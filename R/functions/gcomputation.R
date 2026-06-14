#' G-computation estimator for the average treatment effect
#'
#' Fits an outcome model and estimates the ATE via standardisation:
#' \deqn{\hat{\tau} = \frac{1}{n}\sum_i \hat{Y}_i(1) - \frac{1}{n}\sum_i \hat{Y}_i(0)}
#'
#' @param data A data frame containing the outcome, treatment (\code{Aij}), and
#'   all covariates in \code{outcome_mod_formula}.
#' @param outcome_mod_formula A \code{formula} for the outcome model passed
#'   directly to \code{lm()} or \code{lmer()}.
#' @param model_type Character. \code{"SLM"} fits a single-level linear model
#'   via \code{lm()}; \code{"MLM"} fits a mixed-effects model via
#'   \code{lme4::lmer()} with REML and the \code{bobyqa} optimizer.
#'
#' @return A scalar numeric; the estimated ATE \eqn{\hat{\tau}}.
gcomputation <- function(
  data,
  outcome_mod_formula,
  model_type = c("SLM", "MLM")
) {
  model_type <- match.arg(model_type)

  mod <- switch(
    model_type,

    "SLM" = {
      lm(
        formula = outcome_mod_formula,
        data = data
      )
    },

    "MLM" = {
      lme4::lmer(
        formula = outcome_mod_formula,
        data = data,
        REML = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
      )
    }
  )

  pred1 <- collapse::fmean(
    predict(
      mod,
      newdata = data |> collapse::fmutate(Aij = 1),
      type = "response"
    )
  )

  pred0 <- collapse::fmean(
    predict(
      mod,
      newdata = data |> collapse::fmutate(Aij = 0),
      type = "response"
    )
  )

  pred1 - pred0
}


#' Group-stratified g-computation estimator
#'
#' Estimates the ATE either pooled across all data (\code{crude_est = TRUE}) or
#' as a sample-size-weighted average of group-specific ATE estimates:
#' \deqn{\hat{\tau} = \sum_g \frac{n_g}{n} \hat{\tau}_g}
#' where groups are defined by treatment-prevalence k-means clusters from
#' [create_grp_data()].
#'
#' @param grp_data A data frame with a \code{group} column as returned by
#'   [create_grp_data()].
#' @param outcome_mod_formula,model_type Passed to [gcomputation()].
#' @param crude_est Logical. If \code{TRUE}, ignores grouping and fits a single
#'   pooled model. Default \code{FALSE}.
#'
#' @return A scalar numeric; the estimated ATE \eqn{\hat{\tau}}.
#'
#' @seealso [gcomputation()], [create_grp_data()]
grp_gcomputation <- function(
  grp_data,
  outcome_mod_formula,
  model_type = c("SLM", "MLM"),
  crude_est = FALSE
) {
  model_type <- match.arg(model_type)

  if (crude_est) {
    gcomp_est <- gcomputation(
      data = grp_data,
      outcome_mod_formula = outcome_mod_formula,
      model_type = model_type
    )
  } else {
    grp_gcomp <- collapse::rsplit(
      grp_data,
      grp_data$group,
      use.names = FALSE
    ) |>
      collapse::rapply2d(
        FUN = \(x) {
          gcomputation(
            data = x,
            outcome_mod_formula = outcome_mod_formula,
            model_type = model_type
          )
        }
      ) |>
      unlist(recursive = FALSE, use.names = FALSE)

    grp_size <- collapse::GRPN(grp_data$group, expand = FALSE)
    gcomp_est <- collapse::fsum(
      grp_gcomp,
      w = grp_size / collapse::fsum(grp_size)
    )
  }

  gcomp_est
}


#' Cluster bootstrap for the group-stratified g-computation estimator
#'
#' Runs \code{n_boot} bootstrap iterations by resampling clusters with
#' replacement via [create_grp_data()] (\code{resample = TRUE}), applying
#' [grp_gcomputation()] to each resample, and summarising with a BCa confidence
#' interval.
#'
#' @inheritParams create_grp_data
#' @inheritParams grp_gcomputation
#' @param boot_seed Integer. Master seed passed to \code{dqrng::dqset.seed()}
#'   for generating per-bootstrap seeds.
#' @param true_est Numeric. True ATE used to evaluate CI coverage. Default
#'   \code{2}.
#' @param n_boot Integer. Number of bootstrap resamples. Default \code{200}.
#' @param boot_ci_level Numeric. Confidence level for the BCa interval. Default
#'   \code{0.95}.
#'
#' @return A named numeric vector with elements:
#' \describe{
#'   \item{\code{boot_mean}}{Mean of bootstrap estimates.}
#'   \item{\code{boot_se}}{Standard deviation of bootstrap estimates.}
#'   \item{\code{bca_ci_lower}, \code{bca_ci_upper}}{BCa confidence interval bounds.}
#'   \item{\code{ci_covered}}{Logical (0/1); whether \code{true_est} lies in the interval.}
#'   \item{\code{boot_seed}}{The master seed used.}
#'   \item{\code{valid_boot_run}}{Number of non-\code{NA} bootstrap estimates.}
#' }
bootstrap_gcomp <- function(
  cl_num,
  cl_size,
  trt_prev,
  gamma_3,
  beta_3,
  theta,
  ngrps,
  outcome_mod_formula,
  boot_seed,
  model_type,
  crude_est = FALSE,
  true_est = 2,
  n_boot = 200,
  boot_ci_level = 0.95
) {
  dqrng::dqset.seed(boot_seed)
  rand_seed <- dqrng::dqsample(1:1e5, size = n_boot)

  boot_gcomp_est <- collapse::rapply2d(
    as.list(rand_seed),
    FUN = \(x) {
      resamp_df <- create_grp_data(
        cl_num = cl_num,
        cl_size = cl_size,
        trt_prev = trt_prev,
        gamma_3 = gamma_3,
        beta_3 = beta_3,
        theta = theta,
        ngrps = ngrps,
        seed = x,
        resample = TRUE
      )

      res <- safe_and_quietly(
        grp_gcomputation,
        grp_data = resamp_df,
        outcome_mod_formula = outcome_mod_formula,
        model_type = model_type,
        crude_est = crude_est
      )

      return(res$result)
    },
    classes = "numeric"
  ) |>
    unlist(recursive = FALSE, use.names = FALSE)

  est <- collapse::na_rm(boot_gcomp_est)
  boot_mean <- collapse::fmean(est)
  boot_se <- collapse::fsd(est)
  bca_ci <- coxed::bca(est, conf.level = boot_ci_level)
  is_ci_contained <- is_between(true_est, bca_ci)
  valid_boot_run <- sum(!is.na(boot_gcomp_est))

  return(
    c(
      boot_mean = boot_mean,
      boot_se = boot_se,
      bca_ci_lower = bca_ci[1],
      bca_ci_upper = bca_ci[2],
      ci_covered = is_ci_contained,
      boot_seed = boot_seed,
      valid_boot_run = valid_boot_run
    )
  )
}


#' Monte Carlo simulation study for group-stratified g-computation
#'
#' Runs \code{n_sims} independent Monte Carlo iterations. Each iteration
#' generates a dataset via [create_grp_data()], computes a point estimate via
#' [grp_gcomputation()], and optionally runs a cluster bootstrap via
#' [bootstrap_gcomp()].
#'
#' @inheritParams bootstrap_gcomp
#' @param n_sims Integer. Number of Monte Carlo iterations. Default \code{100}.
#' @param seed Integer. Master seed for generating per-simulation seeds.
#'   Default \code{1070}.
#' @param bootstrap Logical. If \code{FALSE}, bootstrap is skipped and all
#'   bootstrap columns are returned as \code{NA}. Default \code{TRUE}.
#'
#' @return A data frame with one row per simulation iteration and columns:
#' \describe{
#'   \item{\code{estimate}}{Point estimate of the ATE from [grp_gcomputation()].}
#'   \item{\code{prevalence}}{Overall treatment prevalence in the simulated dataset.}
#'   \item{\code{boot_mean}, \code{boot_se}}{Bootstrap mean and standard error.}
#'   \item{\code{bca_ci_lower}, \code{bca_ci_upper}}{BCa interval bounds.}
#'   \item{\code{ci_covered}}{CI coverage indicator for \code{true_est}.}
#'   \item{\code{boot_seed}}{Per-iteration seed used for bootstrapping.}
#'   \item{\code{valid_boot_run}}{Number of valid bootstrap resamples.}
#' }
run_gcomp_mc_simulations <- function(
  cl_num,
  cl_size,
  trt_prev,
  gamma_3,
  beta_3,
  theta,
  ngrps,
  outcome_mod_formula,
  model_type = model_type,
  crude_est = FALSE,
  n_sims = 100,
  seed = 1070,
  true_est = 2,
  bootstrap = TRUE,
  n_boot = 200,
  boot_ci_level = 0.95
) {
  dqrng::dqset.seed(seed)
  sim_seed <- dqrng::dqsample(1:1e5, size = n_sims)
  seed_list <- Map(base::c, sim_seed, seq_along(sim_seed))

  sim_res <- collapse::rapply2d(
    l = seed_list,
    FUN = \(x) {
      sim_df <- create_grp_data(
        cl_num = cl_num,
        cl_size = cl_size,
        trt_prev = trt_prev,
        gamma_3 = gamma_3,
        beta_3 = beta_3,
        theta = theta,
        ngrps = ngrps,
        seed = x[1],
        resample = FALSE
      )

      prevalence <- collapse::fmean(sim_df$Aij)

      gcomp_est <- safe_and_quietly(
        grp_gcomputation,
        grp_data = sim_df,
        outcome_mod_formula = outcome_mod_formula,
        model_type = model_type,
        crude_est = crude_est
      )$result

      boot_res <- c(
        boot_mean = NA,
        boot_se = NA,
        bca_ci_lower = NA,
        bca_ci_upper = NA,
        ci_covered = NA,
        boot_seed = x,
        valid_boot_run = NA
      )

      if (bootstrap) {
        boot_res <- bootstrap_gcomp(
          cl_num = cl_num,
          cl_size = cl_size,
          trt_prev = trt_prev,
          gamma_3 = gamma_3,
          beta_3 = beta_3,
          theta = theta,
          ngrps = ngrps,
          outcome_mod_formula = outcome_mod_formula,
          boot_seed = x,
          model_type = model_type,
          crude_est = crude_est,
          true_est = true_est,
          n_boot = n_boot,
          boot_ci_level = boot_ci_level
        )
      }
      message(paste0("MC iteration no ", x[2], " completed"))
      return(
        c(
          estimate = gcomp_est,
          prevalence = prevalence,
          boot_res
        )
      )
    },
    classes = "numeric"
  ) |>
    collapse::unlist2d()
}
