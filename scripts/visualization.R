#===========================================
# loading experiment results [Controlled]
load(file.path(data_dir, "result_list_controlled.RData"))

# format data for histogram
df_controlled <- dplyr::bind_rows(
  lapply(
    result_list_controlled, function(l) {
      tibble(
        pvalue = l$p_values,
        effect_size = l$effect_size,
        power = l$power,
        alpha = l$alpha,
        sample_size = l$sample_size,
        iteration = l$iteration
      )
    }
  )
)

# ggplot with facet_wrap
# iterationを固定して、他の条件でfacet_wrap
g1s <- list()
for (iter in unique(df_controlled$iteration)) {
  g1 <- ggplot(
    df_controlled %>% dplyr::filter(iteration == iter),
    aes(x = pvalue)
  ) +
    geom_histogram(binwidth = 0.01) +
    # make bigger plot size
    theme_minimal(base_size = 16) +
    facet_wrap(
      ~effect_size + power + sample_size,
      scales = "free_y",
      labeller = labeller(
        effect_size = label_both,
        power = label_both,
        sample_size = label_both
      )
    ) +
    labs(title = paste("Controlled", iter), x = "p-value", y = "Frequency")
  g1s[[paste0("iteration_", iter)]] <- g1
}

g1 <- ggplot(df_controlled, aes(x = pvalue)) +
  geom_histogram(binwidth = 0.01) +
  # make bigger plot size
  theme_minimal(base_size = 16) +
  facet_wrap(
    ~effect_size + sample_size,
    scales = "free_y",
    labeller = labeller(
      effect_size = label_both,
      sample_size = label_both
    )
  ) +
  labs(title = "with controlled sample size", x = "p-value", y = "Frequency")

g1

fig_path = file.path(fig_dir, "controlled_result_1207.png")
ggsave(fig_path, g1, height = 8, width = 12)

#===========================================
# loading experiment results [Fixed sample size]
load(file.path(data_dir, "result_list_oversampling.RData"))
df_oversampling <- dplyr::bind_rows(
  lapply(
    result_list_oversampling, function(l) {
      tibble(
        pvalue = l$p_values,
        sample_size = l$sample_size,
        iteration = l$iteration
      )
    }
  )
)

g2 <- ggplot(df_oversampling, aes(x = pvalue)) +
  geom_histogram(binwidth = 0.01) +
  theme_minimal(base_size = 16) +
  facet_wrap(
    ~sample_size,
    scales = "free_y",
    labeller = labeller(sample_size = label_both)
  ) +
  labs(title = "with fixed sample size", x = "p-value", y = "Frequency")

g2

fig_path = file.path(fig_dir, "fixed_samplesize_result_1207.png")
ggsave(fig_path, g2, height = 8, width = 12)
