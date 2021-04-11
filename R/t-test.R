#' Student Permutation Test
#'
#' Performs a two-sample permutation test based on either Student or Welch
#' statistic on scalar-valued data. The parameter of interest if the difference
#' between the means of the two distributions.
#'
#' @param x A numeric vector providing the first sample.
#' @param y A numeric vector providing the second sample.
#' @param alternative A string specifying the type of alternative hypothesis.
#'   Choices are `"two_tail"`, `"left_tail"` or `"right_tail"`. Defaults to
#'   `"tow_tail"`.
#' @param mu A numeric value providing the value of the mean difference under
#'   the null hypothesis. Defaults to `0`.
#' @param paired A boolean specifying whether the two samples are paired.
#'   Defaults to `FALSE`. Unused at the moment since one-sample tests are not
#'   yet available.
#' @param var_equal A boolean specifying whether the variances of the two
#'   distributions are equal or not. Defaults to `FALSE`.
#' @param conf_level A numeric value specifying the confidence level for the
#'   computation of a confidence interval for the mean difference.
#' @param B An integer specifying the number of permutations to use. Defaults to
#'   `1000L`.
#'
#' @return A list with class `"htest"` containing the following components:
#' - `statistic`:
#' - `parameter`:
#' - `p.value`:
#' - `estimate`:
#' - `conf.int`:
#' - `null.value`:
#' - `stderr`:
#' - `alternative`:
#' - `method`:
#' - `data.name`:
#' @export
#'
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10)
#' z <- rnorm(10, mean = 3)
#' t1 <- t_test(x, y, B = 100)
#' t2 <- t_test(x, z, B = 100)
t_test <- function(x, y = NULL,
                   alternative = c("two_tail", "left_tail", "right_tail"),
                   mu = 0, paired = FALSE, var_equal = FALSE,
                   conf_level = 0.95, B = 1000) {
  if (is.null(y))
    stop("One-sample test noy yet implemented.")
  if (!is.numeric(x) || !is.numeric(y))
    stop("Both data samples should be numeric and univariate.")

  alternative <- match.arg(alternative)
  stat_fun <- if (var_equal) flipr::stat_t else flipr::stat_welch
  method <- if (var_equal)
    "Two-Sample Student Permutation Test"
  else
    "Two-Sample Welch Permutation Test"
  pf <- flipr::PlausibilityFunction$new(null_spec_t_test, 1, x, y, stats = stat_fun)
  pf$set_nperms(B)
  pf$set_alternative(alternative)
  estimate <- flipr::two_sample_pe(pf)
  statistic <- stat_fun(c(
    purrr::array_tree(x, margin = 1),
    purrr::array_tree(y, margin = 1)
  ), 1:(length(x) + length(y)))
  stderr <- estimate / statistic
  structure(
    list(
      statistic = statistic,
      p.value = pf$get_value(mu),
      estimate = estimate,
      conf.int = flipr::two_sample_ci(pf, alpha = 1 - conf_level, point_estimate = estimate),
      null.value = null_spec_t_test,
      stderr = stderr,
      alternative = alternative,
      method = method,
      data.name = c(names(x), names(y))
    ),
    class = "htest"
  )
}

null_spec_t_test <- function(y, parameters) {
    purrr::map(y, ~ .x - parameters[1])
}
