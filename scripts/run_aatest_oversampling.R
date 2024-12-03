# 関数読み込み
sources <- c(
  file.path(base_dir, "scripts/aa_test_modules.R")
)

for (fn in sources) {
  source(fn)
}

set.seed(1111)
population_size <- 100000
x <- rnorm(population_size, mean = 0, sd = 1)
y <- rnorm(population_size, mean = 0, sd = 1)
population <- list(x = x, y = y)

iterations     <- c(10000)
sampling_sizes <- c(100, 1000, 10000)

result_list_oversampling <- list()

# oversampling
iteration_amount <- length(iterations) * length(sampling_sizes)
i <- 1

for (iter in iterations) {
  for (sampling_size in sampling_sizes) {
    # oversampling
    res <- run_aa_test_oversampling(
      x = x,
      y = y,
      sampling_size = sampling_size,
      iteration = iter
    )
    result_list_oversampling[[paste(
      iter, sampling_size
    )]] <- res

    if (i %% 10 == 0) {
      print(paste(i, " / ", iteration_amount))
    }
    i <- i + 1
  }
}

# save results
save(
  result_list_oversampling, 
  file = file.path(base_dir, "data/result_list_oversampling.RData")
)

save(
  population,
  file = file.path(base_dir, "data/population_groups_oversampling")
)

# visualization
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
    ~sample_size + iteration,
    scales = "free_y",
    labeller = labeller(sample_size = label_both, iteration = label_both)
  ) +
  labs(title = "Oversampling", x = "p-value", y = "Frequency")

plot(g2)