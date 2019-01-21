# Generate things

#' Generate relative probabilities according to a distribution
#'
#' The probabilities are intended to be used as weights in
#' functions like \code{sample}.
#'
#' @param num_groups natural number, number of group labels
#' @param distribution distribition, e.g. runif, rnorm
#'     Must accept one parameter. Results should be
#'     non-negative.
#' @param ... further parameters for the distribution
#'
#' @return \code{num_groups} samples from the distribution
#' @export
#'
#' @examples
#' generate_probabilities(10, stats::runif)
#' generate_probabilities(5, stats::rnorm, mean = 3, sd = 0.5)
#' generate_probabilities(6, function(n) { base::abs(stats::rnorm(n)) })
generate_probabilities <- function(num_groups, distribution = stats::runif, ...) {
  weights <- distribution(num_groups, ...)
  probabilities <- weights / sum(weights)
  return(probabilities)
}

#' Generate group indices according to a distribution
#'
#' @param num_samples natural number, number of draws from group labels
#' @param num_groups natural number, number of group labels
#' @param distribution distribition, e.g. runif, rnorm
#'     Must accept one parameter. Results should be
#'     non-negative.
#' @param ... further parameters for the distribution
#'
#' @return \code{num_samples} samples of group indices
#' @export
#'
#' @examples
#' generate_groups(10, 2, stats::runif)
generate_groups <- function(num_samples, num_groups, distribution = stats::runif, ...) {
  groups <- base::seq(1, num_groups)
  probabilities <- generate_probabilities(num_groups, distribution, ...)
  return(
    base::sample(
      x = groups,
      size = num_samples,
      replace = TRUE,
      prob = probabilities
    )
  )
}

#' Generate a tibble of ranked outcomes
#'
#' @param outcomes numeric
#'
#' @return a tibble in which \code{outcomes} (y) are sorted by rank (x)
#' @export
#'
#' @examples
#' rank_outcomes(c(6, 5, 4))
rank_outcomes <- function(outcomes) {
  sorted_outcomes <- base::sort(outcomes)
  return(
    tibble::tibble(
      x = seq_along(sorted_outcomes),
      y = sorted_outcomes
    )
  )
}

#' Repeat a dataframe-generating operation and collect results in a dataframe.
#'
#' @param num_replications natural number
#' @param df_generator function that generates a tibble
#' @param ... additional parameters for df_generator
#'
#' @return a tibble of all rows of each generated tibble, with the replication
#'     index labeled by a new variable, \code{replication}.
#' @export
#'
#' @examples
#' replicate_df(3, function(n) {head(iris, n)}, 1)
#' replicate_df(3, function() { rank_outcomes(generate_probabilities(2)) })
replicate_df <- function(num_replications, df_generator, ...) {
  base::seq_len(num_replications) %>%
    purrr::map(function(replication_index) {
      df_generator(...) %>%
        dplyr::mutate(replication = replication_index)
    }) %>%
    dplyr::bind_rows()
}
