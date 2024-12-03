# AAテストを実行するための関数を定義する
calc_sample_size <- function(
  effect_size, power, alpha
) {
  # 効果量を標準正規分布のパーセント点関数を使って計算
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  n <- 2 * ((z_alpha + z_beta)^2) / (effect_size^2)
  return(ceiling(n))
}

run_aa_test_controlled <- function(
  x, y, effect_size, alpha, power, iteration
) {
  # サンプルサイズを計算
  n <- calc_sample_size(
    power = power,
    alpha = alpha,
    effect_size = effect_size
  )
  # シミュレーションを実行
  p_values <- numeric(iteration)
  for (i in 1:iteration) {

    # データをランダムに分割
    sample_x <- sample(x, n, replace = TRUE)
    sample_y <- sample(y, n, replace = TRUE)

    # t検定を実行
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

run_aa_test_oversampling <- function(
  x, y, sampling_size, iteration
) {
  # サンプルサイズを計算
  # シミュレーションを実行
  p_values <- numeric(iteration)
  for (i in 1:iteration) {
    # データをランダムに分割
    # データをランダムに分割
    sample_x <- sample(x, sampling_size, replace = TRUE)
    sample_y <- sample(y, sampling_size, replace = TRUE)

    # t検定を実行
    p_values[i] <- t.test(sample_x, sample_y)$p.value

  }

  res <- list(
    p_values = p_values,
    sample_size = sampling_size,
    iteration = iteration,
    x = sample_x,
    y = sample_y
  )

  return(res)
}