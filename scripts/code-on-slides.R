run_aa_test <- function(
    x, y, effect_size, alpha, power, iteration
) {
  n <- calc_sample_size(power, alpha, effect_size)
  # シミュレーションを実行
  p_values <- numeric(iteration)
  for (i in 1:iteration) {
    sample_x <- sample(x, n, replace = TRUE)
    sample_y <- sample(y, n, replace = TRUE)
    p_values[i] <- t.test(x = sample_x, y = sample_y, var.equal = FALSE)$p.value
  }
  
  res <- list(
    p_values = p_values,
    sample_size = n,
    effect_size = effect_size,
    power = power,
    alpha = alpha,
    iteration = iteration,
    x = sample_x,
    y = sample_y
  )
  return(res)
}