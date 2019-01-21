smallish_natural_number <- function() {
  base::sample.int(n = 300, size = 1)
}

reasonable_replication <- function() {
  base::sample.int(n = 10000, size = 1)
}

testthat::context('generate')

testthat::test_that('generate_probabilities generates valid probabilities', {
  num_groups <- smallish_natural_number()
  probabilities <- generate_probabilities(num_groups)
  testthat::expect_length(probabilities, num_groups)
  testthat::expect_true(base::all(probabilities >= 0))
  testthat::expect_equal(sum(probabilities), 1)
})

testthat::test_that('generate_groups generates valid groups', {
  num_samples <- reasonable_replication()
  num_groups <- smallish_natural_number()
  group_levels <- base::seq_len(num_groups)
  group_samples <- generate_groups(num_samples, num_groups)
  testthat::expect_length(group_samples, num_samples)
  testthat::expect_true(base::all(group_samples %in% group_levels))
})

testthat::test_that('rank_outcomes generates a proper data frame', {
  num_outcomes <- smallish_natural_number()
  outcomes <- stats::rnorm(num_outcomes)
  df <- rank_outcomes(outcomes)
  testthat::expect_equal(df$x, base::seq_len(num_outcomes))
  testthat::expect_equal(df$y, base::sort(df$y))
})

testthat::test_that('replicate_df generates a proper data frame', {
  num1 <- smallish_natural_number()
  df_gen1 <- function() {
    tibble::tibble(x = rnorm(num1))
  }
  num_replications <- smallish_natural_number()
  df <- replicate_df(num_replications, df_gen1)
  testthat::expect_equal(base::nrow(df), num_replications * num1)
  testthat::expect_equal(df$replication, base::rep(base::seq_len(num_replications), each = num1))
})
