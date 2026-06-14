# G Computation Monte Carlo Simulations -----------------------------------
MONTE_CARLO_ITERARIONS = 500
MONTE_CARLO_SEED = 1070
TRUE_ESTIMATE = 1

CLUSTER_NUMBER = 50
CLUSTER_SIZE = 20
TREATMENT_PREVALENCE = 0
N_GROUPS = 5
CRUDE_EST = TRUE
BOOTSTRAP = FALSE
N_BOOT = NULL

gamma_3 = c(-2, -1, 1, 2)
beta_3 = c(-2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2)
THETA = 0

slm_formula <- "Yij ~ Aij + Xij + Wij"
re_formula <- "Yij ~ Aij + Xij + Wij + (1 | cluster)"


# Simulations Starts ------------------------------------------------------
model_type_df <- tibble::tibble(
  model_type = c('SLM', 'MLM'),
  model = c(
    slm_formula,
    re_formula
  ),
  est_type = c('crude-slm', 'crude-re')
)

sim_params_df <- tidyr::expand_grid(
  model = c(
    slm_formula,
    re_formula
  ),
  gamma_3 = gamma_3,
  beta_3 = beta_3
) |>
  collapse::join(
    model_type_df,
    on = 'model',
    how = 'inner',
    verbose = FALSE
  ) |>
  collapse::fselect(
    gamma_3,
    beta_3,
    model,
    model_type,
    est_type
  )

sim_params_list <- asplit(sim_params_df, MARGIN = 1) |>
  lapply(FUN = \(x) unname(as.vector(x)))


tictoc::tic()
mc_res_df <- collapse::rapply2d(
  l = sim_params_list,
  FUN = \(x) {
    mc_res <- run_gcomp_mc_simulations(
      cl_num = CLUSTER_NUMBER,
      cl_size = CLUSTER_SIZE,
      trt_prev = TREATMENT_PREVALENCE,
      gamma_3 = as.numeric(x[1]),
      beta_3 = as.numeric(x[2]),
      theta = THETA,
      ngrps = N_GROUPS,
      outcome_mod_formula = x[3],
      model_type = x[4],
      crude_est = CRUDE_EST,
      n_sims = MONTE_CARLO_ITERARIONS,
      seed = MONTE_CARLO_SEED,
      true_est = TRUE_ESTIMATE,
      bootstrap = BOOTSTRAP,
      n_boot = N_BOOT
    )

    non_null_row_count <- mc_res |>
      collapse::fselect(estimate) |>
      collapse::na_omit() |>
      collapse::fnrow()

    mc_res_extend <- NULL

    # Adding more runs to get valid results for all `MONTE_CARLO_ITERARIONS`
    if (non_null_row_count < MONTE_CARLO_ITERARIONS) {
      new_seed <- MONTE_CARLO_SEED + MONTE_CARLO_ITERARIONS + 1
      extended_iters <- MONTE_CARLO_ITERARIONS - non_null_row_count
      mc_res_extend <- run_gcomp_mc_simulations(
        cl_num = CLUSTER_NUMBER,
        cl_size = CLUSTER_SIZE,
        trt_prev = TREATMENT_PREVALENCE,
        gamma_3 = as.numeric(x[1]),
        beta_3 = as.numeric(x[2]),
        theta = THETA,
        ngrps = N_GROUPS,
        outcome_mod_formula = x[3],
        model_type = x[4],
        crude_est = CRUDE_EST,
        n_sims = extended_iters,
        seed = new_seed,
        true_est = TRUE_ESTIMATE,
        bootstrap = BOOTSTRAP,
        n_boot = N_BOOT
      )
    }

    collapse::rowbind(mc_res, mc_res_extend, idcol = "extend_parts") |>
      collapse::fsummarise(
        est_mean = collapse::fmean(estimate, na.rm = TRUE),
        est_se = collapse::fsd(estimate, na.rm = TRUE),
        mean_prev = collapse::fmean(prevalence, na.rm = TRUE),
        valid_mc_run = collapse::fsum(!is.na(estimate)),
        total_mc_run = collapse::fnobs(estimate)
      )
  },
  classes = "numeric"
) |>
  collapse::unlist2d(idcols = "params_id")
tictoc::toc()

gcomp_sim_res <- sim_params_df |>
  collapse::fmutate(params_id = collapse::seq_row(sim_params_df)) |>
  collapse::fselect(
    params_id,
    gamma_3,
    beta_3,
    model,
    model_type,
    est_type
  ) |>
  collapse::join(
    mc_res_df,
    on = "params_id",
    how = "inner",
    validate = "1:1",
    verbose = 0
  ) |>
  collapse::fmutate(
    bias = est_mean - TRUE_ESTIMATE
  ) |>
  collapse::fselect(
    gamma_3,
    beta_3,
    model,
    model_type,
    est_type,
    bias,
    est_se,
    est_mean,
    mean_prev,
    valid_mc_run,
    total_mc_run
  )

gcomp_sim_res

saveRDS(
  object = gcomp_sim_res,
  file = here::here("R/simulations/case_1_confounder/gamma_beta_crude.rds")
)

library(ggplot2)
gcomp_sim_res |>
  ggplot(aes(beta_3, bias, group = est_type, linetype = est_type)) +
  geom_line() +
  facet_wrap(
    ~gamma_3,
    labeller = label_bquote(gamma[3] == .(gamma_3))
  )
