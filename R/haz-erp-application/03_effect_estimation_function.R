# ==============================================================================
# Setups -----------------------------------------------------------------------
# ==============================================================================

library(lme4)
library(WeMix)

source(here::here("R/functions/utils.R"))


# ==============================================================================
# Creating the Execution Function ----------------------------------------------
# ==============================================================================

estimate_gcomp_effect <- function(
  data_w_grp,
  outcome_mod_formula,
  exposure_var = "erp",
  grp_var = "group",
  sampling_wt_var = "chweight"
) {
  cntrl = lme4::lmerControl(check.rankX = 'ignore')

  data_w_grp <- data_w_grp |> collapse::fmutate(ones = 1)

  grp_gcomp <- collapse::rsplit(
    data_w_grp,
    data_w_grp[[grp_var]],
    use.names = FALSE
  ) |>
    collapse::rapply2d(
      FUN = \(x) {
        mod <- WeMix::mix(
          formula = outcome_mod_formula,
          weights = c(sampling_wt_var, "ones"),
          data = x
        )

        pred1 <- collapse::fmean(
          predict(
            mod,
            newdata = x |> dplyr::mutate(!!exposure_var := 1),
            type = "response",
            control = cntrl
          )
        )

        pred0 <- collapse::fmean(
          predict(
            mod,
            newdata = x |> dplyr::mutate(!!exposure_var := 0),
            type = "response",
            control = cntrl
          )
        )

        return(pred1 - pred0)
      }
    ) |>
    unlist(recursive = FALSE, use.names = FALSE)

  grp_size <- collapse::GRPN(data_w_grp[[grp_var]], expand = FALSE)
  gcomp_est <- collapse::fsum(
    grp_gcomp,
    w = grp_size / collapse::fsum(grp_size)
  )

  return(gcomp_est)
}
