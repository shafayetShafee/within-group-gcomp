# utility funs ------------------------------------------------------------

# combined function of purrr::safely() and purrr::quietly() from
# https://github.com/tidyverse/purrr/issues/843 by @Maximilian-Stefan-Ernst
safe_and_quietly <- function(fun, ...) {
  safe_fun <- purrr::quietly(purrr::safely(fun))
  out_safe <- safe_fun(...)

  length_zero_to_na <- function(obj) {
    if (length(obj) == 0) {
      return(NA)
    } else {
      return(obj)
    }
  }

  out <-
    list(
      result = out_safe$result$result,
      error = out_safe$result$error,
      output = out_safe$output,
      warnings = out_safe$warnings,
      messages = out_safe$messages
    )
  if (!is.null(out$error)) {
    out$error <- conditionMessage(out$error)
  }
  out <- purrr::map(out, length_zero_to_na)
  return(out)
}


is_between <- function(x, bounds) {
  x >= collapse::fmin(bounds) & x <= collapse::fmax(bounds)
}


get_base_family <- function(
  preferred = "Libertinus Serif",
  fallback = "Times"
) {
  families <- unique(systemfonts::system_fonts()$family)
  idx <- which(tolower(families) == tolower(preferred))
  if (length(idx) > 0) families[idx[1]] else fallback
}

BASE_FAMILY <- get_base_family()

# copied from:
# https://github.com/koundy/ggplot_theme_Publication/blob/master/ggplot_theme_Publication-2.R
# and then modified & extended
theme_publication <- function(base_size = 14, base_family = BASE_FAMILY) {
  (ggthemes::theme_foundation(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      axis.ticks = element_line(),
      axis.text = element_text(face = "plain"),
      axis.title = element_text(face = "plain", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      panel.grid.major = element_blank(),
      panel.spacing.x = unit(0.75, "cm"),
      panel.border = element_rect(linewidth = 1),
      panel.background = element_rect(colour = NA, fill = "white"),
      plot.background = element_rect(colour = NA, fill = "white"),
      plot.margin = unit(c(2, 3, 2, 3), "mm"),
      legend.box = "vetical",
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.key.width = unit(0.75, "cm"),
      legend.key.height = unit(1.0, "cm"),
      legend.key.spacing.y = unit(0.3, "cm"),
      legend.text = element_text(size = rel(1.1)),
      # legend.background = element_rect(color = "black", linewidth = 0.5),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0")
    ))
}


## Copied and Modified from
## https://github.com/American-Institutes-for-Research/WeMix/blob/main/R/helpers.R
## Whats Changed ?
### Added `lmerControl(check.rankX = 'ignore')` into `lFormula` so that
### `lFormula` does not drop the exposure variable (z) when making predictions
### either for `z = 0` or `z = 1`
predict.WeMixResults <- function(
  object,
  newdata = NULL,
  type = c("link", "response"),
  allow.new.levels = FALSE,
  control = lme4::lmerControl(),
  ...
) {
  type <- match.arg(type)
  family <- attr(object, "resp")$family
  b <- object$coef
  if (is.null(newdata)) {
    if (type == "response") {
      return(attr(object, "resp")$mu)
    } else {
      return(attr(object, "resp")$eta)
    }
  } else {
    form <- as.formula(formula(getCall(object)))
    response <- as.name(form[[2]])
    if (!deparse(response) %in% colnames(newdata)) {
      newdata[[response]] <- 0
    }
    lForm <- lme4::lFormula(formula = form, data = newdata, control = control)
    X_new <- lForm$X
    Zt_new <- lForm$reTrms$Zt
    grps <- names(object$ranefMat)
    ui_offset <- 0
    u <- c()
    for (gi in 1:length(grps)) {
      g <- grps[gi]
      if (any(!newdata[, g] %in% unique(rownames(object$ranefMat[[g]])))) {
        if (!allow.new.levels) {
          stop(
            "found new levels in ",
            g,
            " try setting the argument allow.new.levels to TRUE"
          )
        }
      }
      ui_raw <- object$ranefMat[[gi]]
      ui <- rep(0, ncol(ui_raw) * length(unique(newdata[, g])))
      names(ui) <- rownames(Zt_new)[ui_offset + 1:length(ui)]
      # adjust the offset to account for having filled these positions
      ui_offset <- ui_offset + length(ui)
      for (ii in 1:nrow(ui_raw)) {
        if (sum(names(ui) == rownames(ui_raw)[ii]) == ncol(ui_raw)) {
          ui[names(ui) == rownames(ui_raw)[ii]] <- unlist(ui_raw[ii, ])
        }
      }
      # append ui to the end of the u vector
      u <- c(u, ui)
    }

    if (any(!colnames(X_new) %in% names(b))) {
      notinX <- colnames(X_new)[!colnames(X_new) %in% names(b)]
      stop("cannot find columns in X: ", WeMix:::pasteItems(notinX), ".")
    }
    if (any(!names(b) %in% colnames(X_new))) {
      notinb <- names(b)[!names(b) %in% colnames(X_new)]
      stop("cannot find coefficients in b: ", WeMix:::pasteItems(notinb), ".")
    }
    if (any(!names(lForm$reTrms$cnms) %in% names(object$ranefMat))) {
      notinZ <- names(object$ranefMat)[
        !names(object$ranefMat) %in% names(lForm$reTrms$cnms)
      ]
      stop("cannot find columns in Z: ", WeMix:::pasteItems(notinZ), ".")
    }
    if (any(!names(object$ranefMat) %in% names(lForm$reTrms$cnms))) {
      notinre <- names(object$ranefMat)[
        !names(object$ranefMat) %in% names(lForm$reTrms$cnms)
      ]
      stop(
        "cannot find random effects in ranefMat: ",
        WeMix:::pasteItems(notinre),
        "."
      )
    }
    eta <- as.vector(X_new %*% object$coef + Matrix::t(Zt_new) %*% u)
    if (type == "response") {
      res <- family$linkinv(eta)
    } else {
      res <- eta
    }
    df_res <- data.frame(res = res, row_name = rownames(X_new))
    df_all <- data.frame(row_name = rownames(newdata), order = 1:nrow(newdata))
    df_m <- merge(df_res, df_all, by = "row_name", all.x = TRUE, all.y = TRUE)
    res <- df_m$res
    names(res) <- df_m$row_name
    res <- res[sort(df_m$order, index.return = TRUE)$ix]
    return(res)
  }
}
