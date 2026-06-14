# ==============================================================================
# Setups -----------------------------------------------------------------------
# ==============================================================================

library(ggplot2)
library(tidytext)

fig_path <- here::here("analysis-figures/")
base_family <- get_base_family(
  preferred = "Libertinus Serif",
  fallback = "Times"
)


# ==============================================================================
# Estimate & CI ----------------------------------------------------------------
# ==============================================================================

est_ci_table <- readRDS(here::here("R/haz-erp-application/est_ci_table.rds"))

tibble::tibble(est_ci_table)


# ==============================================================================
# Preparing the K-Means Clustering Variance Elbow plot -------------------------
# ==============================================================================

elbow_plot_gg_obj <- readRDS(
  here::here("R/haz-erp-application/elbow_plot_gg_obj.rds")
) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.82, 0.48),
    legend.direction = "vertical",
    legend.key.spacing.y = unit(5, "mm"),
    legend.background = element_blank(),
    legend.key = element_rect(color = "grey", linewidth = 0.5),
    legend.key.width = unit(0.50, "cm"),
    legend.key.height = unit(0.50, "cm"),
    legend.text = element_text(face = "plain", size = rel(0.75)),
    panel.grid.major.y = element_line(colour = "#f0f0f0")
  )

elbow_plot_gg_obj

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-1.pdf"),
  plot = elbow_plot_gg_obj,
  width = 6,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-1.eps"),
  plot = elbow_plot_gg_obj,
  device = grDevices::cairo_ps,
  width = 6,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-1.tiff"),
  plot = elbow_plot_gg_obj,
  device = grDevices::tiff,
  width = 6,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family
)


# ==============================================================================
# Preparing the Mean prevalence of CoC for different # of K-means Centroid -----
# ==============================================================================

kclust_group_preval_gg_obj <- readRDS(
  here::here("R/haz-erp-application/kclust_group_preval_gg_obj.rds")
) +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.border = element_rect(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.5),
    panel.grid.major.y = element_line(colour = "gray80", linewidth = 0.3)
  )

kclust_group_preval_gg_obj

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-2.pdf"),
  plot = kclust_group_preval_gg_obj,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-2.eps"),
  plot = kclust_group_preval_gg_obj,
  device = grDevices::cairo_ps,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-2.tiff"),
  plot = kclust_group_preval_gg_obj,
  device = grDevices::tiff,
  width = 8,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family
)


# ==============================================================================
# Preparing the Bootstrapped ATE Density plot ----------------------------------
# ==============================================================================

boot_ate_estimates <- readRDS(
  here::here("R/haz-erp-application/boot_ate_estimates.rds")
)

bootstrap_df <- tibble::tibble(ate_est = boot_ate_estimates)

boot_est_hist <-
  bootstrap_df |>
  ggplot(aes(ate_est)) +
  geom_density(fill = "#f0f0f0", color = "black") +
  geom_vline(
    xintercept = 0,
    linetype = "dotdash",
    linewidth = 0.6
  ) +
  geom_vline(
    xintercept = as.numeric(est_ci_table["bca_lower"]),
    linetype = "42",
    linewidth = 0.6
  ) +
  geom_vline(
    xintercept = as.numeric(est_ci_table["bca_upper"]),
    linetype = "42",
    linewidth = 0.6
  ) +
  scale_x_continuous(
    limits = c(-.22, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0, 16, 2),
    limits = c(0, 15),
    expand = c(0, 0)
  ) +
  labs(
    x = "Estimated ATE",
    y = "Density"
  ) +
  theme_publication() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5))
  )

print(boot_est_hist)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-3.pdf"),
  plot = boot_est_hist,
  width = 5,
  height = 4,
  units = "in",
  dpi = 800,
  device = cairo_pdf,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-3.eps"),
  plot = boot_est_hist,
  device = grDevices::cairo_ps,
  width = 5,
  height = 4,
  units = "in",
  dpi = 800,
  family = base_family
)

ggsave(
  filename = paste0(fig_path, "analysis-", "Figure-3.tiff"),
  plot = boot_est_hist,
  device = grDevices::tiff,
  width = 5,
  height = 4,
  units = "in",
  dpi = 800,
  compression = "lzw",
  type = "cairo",
  family = base_family
)
