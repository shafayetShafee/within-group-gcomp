#' Simulate hierarchical data with unobserved cluster-level confounding
#'
#' @description
#' Generates two-level observational data with individuals \eqn{i} nested within
#' clusters \eqn{j} according to a prespecified data-generating process (DGP)
#' featuring unobserved cluster-level confounding and nonlinear treatment effect
#' heterogeneity.
#'
#' @section Covariates:
#' For each cluster \eqn{j = 1, \ldots, J} and individual \eqn{i = 1, \ldots, n_j}:
#' \deqn{X_{ij} \sim \mathcal{N}(0, 1)}
#' \deqn{W_j \sim \mathcal{N}(0, 1), \quad W_{ij} = W_j}
#' \deqn{U_j \sim \mathcal{N}(0, 1), \quad U_{ij} = U_j}
#'
#' where \eqn{W_j} is an observed cluster-level covariate and \eqn{U_j} is an
#' **unobserved** cluster-level confounder, both constant within cluster.
#'
#' @section Treatment Assignment Model:
#' Treatment \eqn{A_{ij} \in \{0, 1\}} is drawn from a Bernoulli distribution
#' with cluster-specific logistic regression:
#' \deqn{A_{ij} \mid p_{ij} \sim \text{Bernoulli}(p_{ij})}
#' \deqn{\text{logit}(p_{ij}) = \gamma_{0j} + \gamma_1 X_{ij} + \gamma_2 W_{ij} + \gamma_3 U_{ij}}
#'
#' with fixed coefficients \eqn{\gamma_1 = -1}, \eqn{\gamma_2 = -1}, and a
#' cluster-specific random intercept:
#' \deqn{\gamma_{0j} \sim \mathcal{N}(\texttt{trt\_prev},\ 0.25)}
#'
#' The parameter \code{trt_prev} shifts overall treatment prevalence on the logit
#' scale. The coefficient \eqn{\gamma_3} (\code{gamma_3}) controls the degree of
#' confounding introduced by the unobserved \eqn{U_j}.
#'
#' @section Outcome Model:
#' Outcomes are generated under a potential outcomes framework. The baseline
#' (untreated) potential outcome is:
#' \deqn{Y_{ij}(0) = \beta_{0j} + \beta_1 X_{ij} + \beta_2 W_{ij} + \beta_3 U_{ij} + \varepsilon_{ij}}
#'
#' with fixed coefficients \eqn{\beta_1 = 1}, \eqn{\beta_2 = 1}, individual
#' error \eqn{\varepsilon_{ij} \sim \mathcal{N}(0, 1)}, and cluster random intercept:
#' \deqn{\beta_{0j} \sim \mathcal{N}(5,\ 1)}
#'
#' The treated potential outcome adds a constant treatment effect plus a
#' nonlinear interaction with the unobserved confounder:
#' \deqn{Y_{ij}(1) = Y_{ij}(0) + \tilde{\tau} + \theta \cdot U_{ij}^2}
#'
#' so that the individual treatment effect is:
#' \deqn{Y_{ij}(1) - Y_{ij}(0) = \tilde{\tau} + \theta \cdot U_{ij}^2}
#'
#' @section Average Treatment Effect (ATE) Calibration:
#' The intercept \eqn{\tilde{\tau}} is calibrated so that the marginal ATE equals
#' exactly \eqn{\tau = 1}:
#' \deqn{\tau = \mathbb{E}[Y(1) - Y(0)] = \tilde{\tau} + \theta \cdot \mathbb{E}[U_{ij}^2]}
#' \deqn{\tilde{\tau} = \tau - \theta \cdot \mathbb{E}[U_{ij}^2]}
#'
#' where \eqn{\mathbb{E}[U_{ij}^2]} is estimated from the simulated sample. The
#' observed outcome is then:
#' \deqn{Y_{ij} = Y_{ij}(0) + (\tilde{\tau} + \theta \cdot U_{ij}^2) \cdot A_{ij}}
#'
#' The parameter \code{theta} governs the strength of treatment effect
#' heterogeneity through \eqn{U_j}. When \code{theta = 0}, the individual
#' treatment effect is constant at \eqn{\tau = 1}.
#'
#' @section Cluster Trimming:
#' Clusters with near-deterministic treatment assignment are excluded to ensure
#' sufficient overlap for causal estimation. A cluster \eqn{j} is retained only if:
#' \deqn{0.05 < \bar{p}_j < 0.95}
#' where \eqn{\bar{p}_j = n_j^{-1} \sum_i A_{ij}} is the observed treatment
#' prevalence in cluster \eqn{j}.
#'
#' @param cl_num Integer. Number of clusters \eqn{J}.
#' @param cl_size Integer. Fixed number of individuals per cluster \eqn{n_j}.
#' @param trt_prev Numeric. Mean of the cluster-level random intercept
#'   \eqn{\gamma_{0j}} in the treatment model, shifting overall treatment
#'   prevalence on the logit scale.
#' @param gamma_3 Numeric. Coefficient \eqn{\gamma_3} of the unobserved
#'   cluster-level confounder \eqn{U_j} in the treatment assignment model.
#'   Larger absolute values induce stronger confounding.
#' @param beta_3 Numeric. Coefficient \eqn{\beta_3} of the unobserved
#'   cluster-level confounder \eqn{U_j} in the outcome model.
#' @param theta Numeric. Coefficient \eqn{\theta} governing nonlinear treatment
#'   effect heterogeneity via the interaction \eqn{A_{ij} \times U_{ij}^2}.
#'   When \code{theta = 0}, the ATE is constant at 1 for all individuals.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return
#' A \code{data.frame} of retained individuals with columns:
#' \describe{
#'   \item{\code{cluster}}{Cluster identifier \eqn{j}.}
#'   \item{\code{id}}{Individual identifier \eqn{i}.}
#'   \item{\code{Xij}}{Observed individual-level covariate \eqn{X_{ij}}.}
#'   \item{\code{Wij}}{Observed cluster-level covariate \eqn{W_{ij} = W_j},
#'     constant within cluster.}
#'   \item{\code{Uij}}{Unobserved cluster-level confounder \eqn{U_{ij} = U_j},
#'     constant within cluster. Not available to estimators in practice.}
#'   \item{\code{Aij}}{Binary treatment indicator \eqn{A_{ij} \in \{0, 1\}}.}
#'   \item{\code{Yij}}{Observed outcome \eqn{Y_{ij}}.}
#'   \item{\code{pj}}{Cluster-level treatment prevalence
#'     \eqn{\bar{p}_j = n_j^{-1} \sum_i A_{ij}}.}
#' }
#' Clusters with \eqn{\bar{p}_j \leq 0.05} or \eqn{\bar{p}_j \geq 0.95} are excluded.
create_sim_data <- function(
  cl_num,
  cl_size,
  trt_prev,
  gamma_3,
  beta_3,
  theta,
  seed
) {
  # Coefficients -----------------------------------------------------
  GAMMA_1 <- -1
  GAMMA_2 <- -1
  BETA_1 <- 1
  BETA_2 <- 1
  TAU <- 1
  SIGMA_Y <- 1
  P_EPS <- 0.05

  cl_idx <- rep(1:cl_num, each = cl_size)
  n <- cl_num * cl_size

  set.seed(seed)
  Xij <- rnorm(n, 0, 1)
  Wj <- rnorm(cl_num, 0, 1)
  Uj <- rnorm(cl_num, 0, 1)
  Wij <- Wj[cl_idx]
  Uij <- Uj[cl_idx]

  # Treatment assignment model (logit) ------------------------------
  gamma_0j <- rnorm(cl_num, trt_prev, sqrt(0.25)) # GAMMA_0 = trt_prev
  logit_p_ij <- gamma_0j[cl_idx] +
    GAMMA_1 * Xij +
    GAMMA_2 * Wij +
    gamma_3 * Uij

  p_ij <- stats::plogis(logit_p_ij)
  Aij <- rbinom(n, 1, p_ij)

  # Potential outcomes model ----------------------------------------
  beta_0j <- rnorm(cl_num, 5, 1) # BETA_0 = 5
  eps_ij <- rnorm(n, 0, SIGMA_Y)
  Y0 <- beta_0j[cl_idx] +
    BETA_1 * Xij +
    BETA_2 * Wij +
    beta_3 * Uij +
    eps_ij

  # Maths for adding interaction term
  # Y(1) <- Y0 + adj_tau + theta * Uij^2
  # Y(0) <- Y0
  # Y(1) - Y(0) = adj_tau + theta * Uij^2
  # TAU = ate = E[Y(1) - Y(0)] = adj_tau + theta * E(Uij^2)
  # adj_tau = TAU - theta * E(Uij^2)

  adj_tau <- TAU - theta * collapse::fmean(Uij^2)
  Yij <- Y0 + adj_tau * Aij + theta * Aij * Uij^2

  sim_data <- data.frame(
    cluster = cl_idx,
    id = 1:n,
    Xij = Xij,
    Wij = Wij,
    Uij = Uij,
    Aij = Aij,
    Yij = Yij
  )

  df <- sim_data |>
    collapse::fgroup_by(cluster) |>
    collapse::fsummarise(pj = collapse::fmean(Aij)) |>
    collapse::fsubset(pj > P_EPS & pj < (1 - P_EPS)) |>
    collapse::join(
      sim_data,
      on = "cluster",
      how = "left",
      multiple = TRUE,
      validate = "1:m",
      verbose = 0
    )

  df
}


#' Simulate clustered data with treatment-prevalence-based grouping
#'
#' @description
#' Wraps [create_sim_data()] and assigns each cluster to a treatment-prevalence
#' group via k-means clustering on the cluster-level treatment prevalence
#' \eqn{p_j = n_j^{-1} \sum_i A_{ij}}. Optionally resamples clusters with
#' replacement before grouping, inducing duplicate clusters in the output.
#'
#' @inheritParams create_sim_data
#' @param ngrps Integer. Number of treatment-prevalence groups formed by k-means
#'   on \eqn{p_j}. Default \code{4}.
#' @param resample Logical. If \code{TRUE}, clusters are resampled with
#'   replacement (using \code{seed}) before prevalence computation and grouping,
#'   inducing duplicates. Default \code{FALSE}.
#'
#' @return
#' The individual-level data frame returned by [create_sim_data()] with one
#' additional column:
#' \describe{
#'   \item{\code{group}}{K-means group label for the cluster's treatment
#'     prevalence \eqn{p_j}; factor with \code{ngrps} levels.}
#' }
#'
#' @seealso [create_sim_data()] for the underlying DGP and column definitions.
create_grp_data <- function(
  cl_num,
  cl_size,
  trt_prev,
  gamma_3,
  beta_3,
  theta,
  seed,
  ngrps = 4,
  resample = FALSE
) {
  sim_data <- create_sim_data(
    cl_num = cl_num,
    cl_size = cl_size,
    trt_prev = trt_prev,
    gamma_3 = gamma_3,
    beta_3 = beta_3,
    theta = theta,
    seed = seed
  )

  if (resample) {
    set.seed(seed)
    sampled_cls <- sample(
      collapse::funique(sim_data$cluster),
      collapse::fnunique(sim_data$cluster),
      replace = TRUE
    )

    sim_data <- collapse::rapply2d(
      l = as.list(seq_along(sampled_cls)),
      FUN = \(i) {
        collapse::fsubset(sim_data, cluster == sampled_cls[i]) |>
          collapse::fmutate(cluster = i)
      }
    ) |>
      collapse::unlist2d(idcols = FALSE)
  }

  pj_df <- sim_data |>
    collapse::fgroup_by(cluster) |>
    collapse::fsummarise(pj = collapse::fmean(Aij))

  set.seed(seed + 1000)
  grp_kmeans <- kmeans(pj_df$pj, centers = ngrps)
  grp_kmeans_df <- broom::augment(grp_kmeans, pj_df) |>
    collapse::frename("group" = .cluster) |>
    collapse::fselect(cluster, group)

  data_w_grps <- sim_data |>
    collapse::join(
      grp_kmeans_df,
      on = "cluster",
      how = "left",
      multiple = TRUE,
      validate = "m:1",
      verbose = 0
    )

  data_w_grps
}
