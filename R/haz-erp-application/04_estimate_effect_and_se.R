# ==============================================================================
# Setups -----------------------------------------------------------------------
# ==============================================================================

library(ggplot2)

source(here::here("R/haz-erp-application/03_effect_estimation_function.R"))

analysis_df <- readRDS(here::here("R/haz-erp-application/analysis_df.rds"))


# ==============================================================================
# Calculating the ATE (Risk Difference) ----------------------------------------
# ==============================================================================

outcome_mod_formula <- stunt ~
  erp + wealth_index + place_of_res + melevel + (1 | HH7A_ch)

ate_est <- estimate_gcomp_effect(
  data_w_grp = analysis_df,
  outcome_mod_formula = outcome_mod_formula,
  exposure_var = "erp",
  grp_var = "group",
  sampling_wt_var = "chweight"
)

print(ate_est)


# ==============================================================================
# Calculating SE using Bootstrap -----------------------------------------------
# ==============================================================================

unique_clusters <- collapse::funique(analysis_df$HH1)
n_unique_clusters <- collapse::fnunique(analysis_df$HH1)

SEED <- 234
BOOT_ITER <- 1500
N_GRPS <- 5

dqrng::dqset.seed(SEED)
rand_seed <- dqrng::dqsample(1:1e5, size = BOOT_ITER)

boot_results <- collapse::rapply2d(
  l = as.list(rand_seed),
  FUN = \(x) {
    sampled_clusters <- sample(
      unique_clusters,
      n_unique_clusters,
      replace = TRUE
    )

    resamp_df <- purrr::map_dfr(
      sampled_clusters,
      ~ {
        collapse::fsubset(analysis_df, HH1 == .x)
      }
    )

    erp_prop_df <- resamp_df |>
      collapse::fgroup_by(HH7A_ch) |>
      collapse::fsummarise(erp_prop = collapse::fmean(erp))

    grp_kmeans <- kmeans(erp_prop_df$erp_prop, centers = N_GRPS)
    grp_kmeans_df <- broom::augment(grp_kmeans, erp_prop_df) |>
      collapse::frename("group" = .cluster) |>
      collapse::fselect(HH7A_ch, group)

    resamp_df <- resamp_df |>
      collapse::fselect(-group) |>
      collapse::join(
        grp_kmeans_df,
        on = "HH7A_ch",
        how = "left",
        multiple = TRUE,
        validate = "m:1",
        verbose = 0
      )

    res <- safe_and_quietly(
      fun = estimate_gcomp_effect,
      data_w_grp = resamp_df,
      outcome_mod_formula = outcome_mod_formula,
      exposure_var = "erp",
      grp_var = "group",
      sampling_wt_var = "chweight"
    )

    message(paste0("Completed Bootstrap Iteration: ", match(x, rand_seed)))
    return(res$result)
  },
  classes = "numeric"
) |>
  unlist(recursive = FALSE, use.names = FALSE)


# ==============================================================================
# Preparing and storing the bootstrapped results -------------------------------
# ==============================================================================

boot_ate_estimates <- collapse::na_rm(boot_results) |>
  collapse::fslice(n = 1000)

saveRDS(
  object = boot_ate_estimates,
  file = here::here("R/haz-erp-application/boot_ate_estimates.rds")
)


# ==============================================================================
# Calculating Bootstrap SE -----------------------------------------------------
# ==============================================================================

boot_ate_estimates <- readRDS(
  here::here("R/haz-erp-application/boot_ate_estimates.rds")
)

bootstrap_df <- tibble::tibble(ate_est = boot_ate_estimates)

boot_se <- bootstrap_df |>
  collapse::fsummarise(
    boot_mean = collapse::fmean(ate_est),
    boot_se = collapse::fsd(ate_est)
  )

print(boot_se)


# ==============================================================================
# Calculating Non-param Boot CI ------------------------------------------------
# ==============================================================================

# bias corrected boot CI
bca_ci <- coxed::bca(bootstrap_df$ate_est)

ALPHA <- 0.05

est_ci_table <- tibble::tibble(
  ate_est = ate_est,
  lower = quantile(bootstrap_df$ate_est, ALPHA / 2),
  upper = quantile(bootstrap_df$ate_est, 1 - (ALPHA / 2)),
  bca_lower = bca_ci[1],
  bca_upper = bca_ci[2],
) |>
  dplyr::bind_cols(boot_se)

est_ci_table

saveRDS(est_ci_table, here::here("R/haz-erp-application/est_ci_table.rds"))
