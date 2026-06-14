# G Computation Monte Carlo Simulations -----------------------------------
MONTE_CARLO_ITERARIONS = 500
MONTE_CARLO_SEED = 1070
TRUE_ESTIMATE = 1

CLUSTER_NUMBER = c(50, 100)
TREATMENT_PREVALENCE = 0
N_GROUPS = 5
CRUDE_EST = FALSE
BOOTSTRAP = FALSE
N_BOOT = NULL

GAMMA_3 = 1.5
BETA_3 = 1.5
THETA = -1

slm_formula <- "Yij ~ Aij + Xij + Wij"
re_formula <- "Yij ~ Aij + Xij + Wij + (1 | cluster)"


# Simulations Starts ------------------------------------------------------

cluster_df <- tidyr::expand_grid(
  cluster_number = CLUSTER_NUMBER,
  cluster_size = seq(15, 150, 15)
)

model_type_df <- tibble::tibble(
  model_type = c('SLM', 'MLM'),
  model = c(
    slm_formula,
    re_formula
  ),
  est_type = c('group-slm', 'group-re')
)

sim_params_df <- tidyr::expand_grid(model_type_df, cluster_df) |>
  collapse::fselect(
    cluster_number,
    cluster_size,
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
      cl_num = as.numeric(x[1]),
      cl_size = as.numeric(x[2]),
      trt_prev = TREATMENT_PREVALENCE,
      gamma_3 = GAMMA_3,
      beta_3 = BETA_3,
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
        cl_num = as.numeric(x[1]),
        cl_size = as.numeric(x[2]),
        trt_prev = TREATMENT_PREVALENCE,
        gamma_3 = GAMMA_3,
        beta_3 = BETA_3,
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
    cluster_number,
    cluster_size,
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
    cluster_number,
    cluster_size,
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
  file = here::here("R/simulations/case_2_conf_eff_mod/slm_vs_mlm_grp.rds")
)
