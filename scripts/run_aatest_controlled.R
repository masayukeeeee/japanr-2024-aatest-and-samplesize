# 関数読み込み
sources <- c(
  file.path(base_dir, "scripts/aa_test_modules.R")
)

for (fn in sources) {
  source(fn)
}

set.seed(1227)
population_size <- 100000
x <- rnorm(population_size, mean = 0, sd = 1)
y <- rnorm(population_size, mean = 0, sd = 1)
population <- list(x = x, y = y)

effect_sizes   <- c(0.03, 0.5, 1)
powers         <- c(0.8)
alphas         <- c(0.05)
iterations     <- c(10000)
sampling_sizes <- c(100, 1000, 10000)

result_list_controlled   <- list()


# controlled
iteration_amount <- length(iterations) *
  length(effect_sizes) *
  length(powers) *
  length(alphas)
i <- 1
for (iter in iterations) {
  for (effect_size in effect_sizes) {
    for (power in powers) {
      for (alpha in alphas) {
        # controlle sample size
        res <- run_aa_test_controlled(
          x = x,
          y = y,
          effect_size = effect_size,
          alpha = alpha,
          power = power,
          iteration = iter
        )
        result_list_controlled[[paste(
          iter, effect_size, power, alpha
        )]] <- res

        if (i %% 10 == 0) {
          print(paste(i, " / ", iteration_amount))
        }
        i <- i + 1
      }
    }
  }
}

# save results
save(
  result_list_controlled,
  file = file.path(base_dir, "data/result_list_controlled.RData")
)

save(
  population,
  file = file.path(base_dir, "data/population_groups_controlled.RData")
)