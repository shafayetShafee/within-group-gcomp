# ==============================================================================
# Setups -----------------------------------------------------------------------
# ==============================================================================

library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(ggplot2)
library(tidytext)
library(forcats)
library(tableone)

source(here::here("R/functions/utils.R"))

app_df <- readRDS(
  file = here::here("R/haz-erp-application/application_data.rds")
)

# ==============================================================================
# Outcome, Treatment & Confounders ---------------------------------------------
# ==============================================================================

app_df |> count(erp)
app_df |> count(erp) |> mutate(pct = n / sum(n) * 100)
app_df |> group_by(erp) |> summarise(mean(stunt))

app_df |>
  ggplot(aes(stunt, group = erp, fill = factor(erp))) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  labs(fill = NULL) +
  theme_bw()

# ==============================================================================
# Checking Distribution of Covariates over Early/Adolescent Pregnancy ----------
# ==============================================================================

CreateTableOne(
  vars = c(
    "melevel",
    "wealth_index",
    "place_of_res",
    "stunt"
  ),
  factorVars = c("melevel", 'wealth_index', 'place_of_res'),
  strata = "erp",
  data = app_df,
  test = FALSE
)

# ==============================================================================
# Preparing the groups using Kmeans --------------------------------------------
# ==============================================================================

erp_prop <- app_df |>
  group_by(HH7A_ch) |>
  summarise(trt_prop = mean(erp, na.rm = TRUE))

SEED <- 1234
set.seed(SEED)
seed_number <- sample(100:500, 10)

kgroups <- tibble(
  k = 1:10,
  seed = seed_number
) |>
  mutate(
    kclust = map2(
      .x = k,
      .y = seed,
      .f = \(x, y) {
        set.seed(y)
        kmeans(erp_prop$trt_prop, x)
      }
    ),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, erp_prop)
  )


# ==============================================================================
# Exploratory plots for Choosing the Group Numbers -----------------------------
# ==============================================================================

elbow_plot <- kgroups |>
  unnest(cols = c(glanced)) |>
  select(k, "Within Variance" = tot.withinss, "Between Variance" = betweenss) |>
  pivot_longer(cols = !k, names_to = "variance_type", values_to = "value") |>
  ggplot(aes(x = factor(k), group = variance_type)) +
  geom_line(aes(y = value), linewidth = 0.4) +
  geom_point(aes(y = value, shape = variance_type), size = 2) +
  labs(
    x = "Groups",
    y = "Variance",
    shape = NULL
  ) +
  theme_publication()

elbow_plot

saveRDS(
  elbow_plot,
  file = here::here("R/haz-erp-application/elbow_plot_gg_obj.rds")
)

kclust_group_preval <- kgroups |>
  unnest(cols = c(tidied)) |>
  select(k, cl_mean = x1, cluster, size) |>
  group_by(k) |>
  arrange(cl_mean, .by_group = TRUE) |>
  ungroup() |>
  ggplot(aes(x = reorder_within(cluster, cl_mean, k), y = cl_mean)) +
  geom_col(color = "gray20", fill = "white", width = 0.8, linewidth = 0.5) +
  geom_text(aes(label = size), nudge_y = 0.03, size = 2.5) +
  scale_x_reordered() +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  labs(
    x = "Group ID",
    y = "Mean Prevalence of\nAdolescent Pregnancy"
  ) +
  facet_wrap(~k, scales = "free_x", nrow = 2, ncol = 5) +
  theme_publication()

kclust_group_preval

saveRDS(
  kclust_group_preval,
  file = here::here("R/haz-erp-application/kclust_group_preval_gg_obj.rds")
)


# ==============================================================================
# Creating the groups ----------------------------------------------------------
# ==============================================================================

# From the above graphs, We think we should consider 5 groups.

set.seed(seed_number[5])

grp_kmeans <- kmeans(erp_prop$trt_prop, centers = 5)

grp_kmeans_df <- augment(grp_kmeans, erp_prop) |>
  rename("group" = .cluster)

grp_kmeans_df |>
  group_by(group) |>
  summarise(p = mean(trt_prop))

grp_preval_plot <- tidy(grp_kmeans) |>
  ggplot(aes(x = fct_reorder(cluster, x1), x1)) +
  geom_col(color = "grey10", fill = "white") +
  geom_text(aes(label = size), nudge_y = 0.04) +
  labs(
    x = "Groups",
    y = "Mean prevalence of Adolescent Pregnancy"
  ) +
  theme_publication() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "plain")
  )

grp_preval_plot


analysis_df <- app_df |>
  left_join(grp_kmeans_df, by = join_by(HH7A_ch)) |>
  select(
    stunt,
    erp,
    place_of_res,
    wealth_index,
    melevel,
    chweight,
    HH7A_ch,
    group,
    HH1
  )

saveRDS(
  analysis_df,
  file = here::here("R/haz-erp-application/analysis_df.rds")
)
