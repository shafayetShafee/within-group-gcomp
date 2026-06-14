# Setups ------------------------------------------------------------------
library(ggplot2)
library(collapse)
library(data.table)

source(here::here("R/functions/utils.R"))
fig_path <- here::here("simulation-figures/")
base_family <- get_base_family(
  preferred = "Libertinus Serif",
  fallback = "Times"
)

estimator_types <- c(
  "tilde(tau)[LM]",
  "tilde(tau)[REM]",
  "tilde(tau)[WG-LM]",
  "tilde(tau)[WG-REM]"
)

shapes_types <- c(
  "square open",
  "triangle open",
  "circle open",
  "asterisk"
)

line_types <- c(
  "42",
  "solid",
  "42",
  "solid"
)

scale_shape_values <- rlang::set_names(shapes_types, estimator_types)
scale_linetype_values <- rlang::set_names(line_types, estimator_types)


###########################################################################
# Case 1: Uj is just a confounder -----------------------------------------
###########################################################################

## RMSE Across different values of gamma_3 & beta_3 -----------------------
case_1_gb_crude_sim_res <- readRDS(
  here::here("R/simulations/case_1_confounder/gamma_beta_crude.rds")
)

case_1_gb_grp_sim_res <- readRDS(
  here::here("R/simulations/case_1_confounder/gamma_beta_grp.rds")
)

case_1_gb_sim_res_df <- collapse::rowbind(
  case_1_gb_crude_sim_res,
  case_1_gb_grp_sim_res,
  idcol = "method"
) |>
  fmutate(
    rmse = sqrt(bias^2 + est_se^2),
    model_type_mod = factor(
      data.table::fcase(
        est_type == "crude-slm", "tilde(tau)[LM]",
        est_type == "crude-re",  "tilde(tau)[REM]",
        est_type == "group-slm", "tilde(tau)[WG-LM]",
        est_type == "group-re",  "tilde(tau)[WG-REM]"
      ),
      levels = c(
        "tilde(tau)[LM]",
        "tilde(tau)[WG-LM]",
        "tilde(tau)[REM]",
        "tilde(tau)[WG-REM]"
      )
    )
  )

case_1_gb_rmse_plot <- case_1_gb_sim_res_df |>
  ggplot(
    aes(
      x = beta_3,
      y = rmse,
      group = model_type_mod,
      shape = model_type_mod,
      linetype = model_type_mod
    )
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(
    values = scale_shape_values,
    labels = scales::label_parse()
  ) +
  scale_linetype_manual(
    values = scale_linetype_values,
    labels = scales::label_parse()
  ) +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.25),
    limits = c(0, 1.5)
  ) +
  facet_wrap(
    ~gamma_3,
    scales = "free",
    labeller = label_bquote(gamma[3] == .(gamma_3))
  ) +
  labs(
    title = NULL,
    x = expression(beta[3]),
    y = "RMSE"
  ) +
  theme_publication()

case_1_gb_rmse_plot

ggsave(
  filename = paste0(fig_path, "Figure-1.pdf"),
  plot = case_1_gb_rmse_plot,
  width = 10,
  height = 6,
  units = "in",
  dpi = 800,
  device = grDevices::cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-1.eps"),
  plot = case_1_gb_rmse_plot,
  device = grDevices::cairo_ps,
  width = 10,
  height = 6,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-1.tiff"),
  plot = case_1_gb_rmse_plot,
  device = grDevices::tiff,
  width = 10,
  height = 6,
  units = "in",
  dpi = 800,
  type = "cairo",
  family = base_family,
  symbolfamily = cairoSymbolFont(family = base_family),
  compression = "lzw"
)


## RMESE Across different Cluster Numbers & Cluster Sizes -----------------
case_1_cls_crude_sim_res <- readRDS(
  here::here("R/simulations/case_1_confounder/slm_vs_mlm_crude.rds")
)

case_1_cls_grp_sim_res <- readRDS(
  here::here("R/simulations/case_1_confounder/slm_vs_mlm_grp.rds")
)

case_1_sim_res_df <- collapse::rowbind(
  case_1_cls_crude_sim_res,
  case_1_cls_grp_sim_res,
  idcol = "method"
) |>
  fmutate(
    rmse = sqrt(bias^2 + est_se^2),
    cluster_number_mod = factor(
      data.table::fcase(
        cluster_number == 50  , "Cluster Number: 50"  ,
        cluster_number == 100 , "Cluster Number: 100"
      ),
      levels = c("Cluster Number: 50", "Cluster Number: 100")
    ),
    model_type_mod = factor(
      data.table::fcase(
        est_type == "crude-slm", "tilde(tau)[LM]",
        est_type == "crude-re",  "tilde(tau)[REM]",
        est_type == "group-slm", "tilde(tau)[WG-LM]",
        est_type == "group-re",  "tilde(tau)[WG-REM]"
      ),
      levels = c(
        "tilde(tau)[LM]",
        "tilde(tau)[WG-LM]",
        "tilde(tau)[REM]",
        "tilde(tau)[WG-REM]"
      )
    )
  )

case_1_cls_rmse_plot <- case_1_sim_res_df |>
  ggplot(
    aes(
      x = cluster_size,
      y = rmse,
      group = model_type_mod,
      shape = model_type_mod,
      linetype = model_type_mod
    )
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(
    values = scale_shape_values,
    labels = scales::label_parse()
  ) +
  scale_linetype_manual(
    values = scale_linetype_values,
    labels = scales::label_parse()
  ) +
  scale_x_continuous(
    breaks = seq(15, 150, 15)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.25),
    limits = c(0, 1.5)
  ) +
  facet_wrap(~cluster_number_mod, scales = "free") +
  labs(
    x = "Cluster Size",
    y = "RMSE"
  ) +
  theme_publication()

case_1_cls_rmse_plot


# Figure 2 plots preparing ------------------------------------------------
ggsave(
  filename = paste0(fig_path, "Figure-2.pdf"),
  plot = case_1_cls_rmse_plot,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-2.eps"),
  plot = case_1_cls_rmse_plot,
  device = grDevices::cairo_ps,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-2.tiff"),
  plot = case_1_cls_rmse_plot,
  device = grDevices::tiff,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family,
  symbolfamily = cairoSymbolFont(family = base_family)
)



###########################################################################
# Uj is both confounder & effect modifier ---------------------------------
###########################################################################

## RMSE across different values of theta ----------------------------------
case_2_theta_sim_res <- readRDS(
  here::here("R/simulations/case_2_conf_eff_mod/theta.rds")
) |>
  fmutate(
    abs_bias = abs(bias),
    rmse = sqrt(bias^2 + est_se^2),
    model_type_mod = factor(
      data.table::fcase(
        est_type == "crude-slm", "tilde(tau)[LM]",
        est_type == "crude-re",  "tilde(tau)[REM]",
        est_type == "group-slm", "tilde(tau)[WG-LM]",
        est_type == "group-re",  "tilde(tau)[WG-REM]"
      ),
      levels = c(
        "tilde(tau)[LM]",
        "tilde(tau)[WG-LM]",
        "tilde(tau)[REM]",
        "tilde(tau)[WG-REM]"
      )
    )
  )


case_2_theta_rmse_plot <- case_2_theta_sim_res |>
  ggplot(
    aes(
      x = theta,
      y = rmse,
      group = model_type_mod,
      shape = model_type_mod,
      linetype = model_type_mod
    )
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(
    values = scale_shape_values,
    labels = scales::label_parse()
  ) +
  scale_linetype_manual(
    values = scale_linetype_values,
    labels = scales::label_parse()
  ) +
  scale_x_continuous(breaks = seq(-1.5, -0.5, 0.2)) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.25),
    limits = c(0, 1.5)
  ) +
  labs(
    x = expression(theta),
    y = "RMSE"
  ) +
  theme_publication()

case_2_theta_rmse_plot

ggsave(
  filename = paste0(fig_path, "Figure-3.pdf"),
  plot = case_2_theta_rmse_plot,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-3.eps"),
  plot = case_2_theta_rmse_plot,
  device = grDevices::cairo_ps,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-3.tiff"),
  plot = case_2_theta_rmse_plot,
  device = grDevices::tiff,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family,
  symbolfamily = cairoSymbolFont(family = base_family)
)


## RMESE Across different Cluster Numbers & Cluster Sizes -----------------
case_2_cls_crude_sim_res <- readRDS(
  here::here("R/simulations/case_2_conf_eff_mod/slm_vs_mlm_crude.rds")
)

case_2_cls_grp_sim_res <- readRDS(
  here::here("R/simulations/case_2_conf_eff_mod/slm_vs_mlm_grp.rds")
)

case_2_cls_sim_res_df <- collapse::rowbind(
  case_2_cls_crude_sim_res,
  case_2_cls_grp_sim_res,
  idcol = "method"
) |>
  fmutate(
    rmse = sqrt(bias^2 + est_se^2),
    cluster_number_mod = factor(
      data.table::fcase(
        cluster_number == 50  , "Cluster Number: 50"  ,
        cluster_number == 100 , "Cluster Number: 100"
      ),
      levels = c("Cluster Number: 50", "Cluster Number: 100")
    ),
    model_type_mod = factor(
      data.table::fcase(
        est_type == "crude-slm", "tilde(tau)[LM]",
        est_type == "crude-re",  "tilde(tau)[REM]",
        est_type == "group-slm", "tilde(tau)[WG-LM]",
        est_type == "group-re",  "tilde(tau)[WG-REM]"
      ),
      levels = c(
        "tilde(tau)[LM]",
        "tilde(tau)[WG-LM]",
        "tilde(tau)[REM]",
        "tilde(tau)[WG-REM]"
      )
    )
  )


case_2_cls_rmse_plot <- case_2_cls_sim_res_df |>
  ggplot(
    aes(
      x = cluster_size,
      y = rmse,
      group = model_type_mod,
      shape = model_type_mod,
      linetype = model_type_mod
    )
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(
    values = scale_shape_values,
    labels = scales::label_parse()
  ) +
  scale_linetype_manual(
    values = scale_linetype_values,
    labels = scales::label_parse()
  ) +
  scale_x_continuous(
    breaks = seq(15, 150, 15)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.25),
    limits = c(0, 1.5)
  ) +
  facet_wrap(~cluster_number_mod, scales = "free") +
  labs(
    x = "Cluster Size",
    y = "RMSE"
  ) +
  theme_publication()

case_2_cls_rmse_plot

ggsave(
  filename = paste0(fig_path, "Figure-4.pdf"),
  plot = case_2_cls_rmse_plot,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-4.eps"),
  plot = case_2_cls_rmse_plot,
  device = grDevices::cairo_ps,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "Figure-4.tiff"),
  plot = case_2_cls_rmse_plot,
  device = grDevices::tiff,
  width = 10,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family,
  symbolfamily = cairoSymbolFont(family = base_family)
)

