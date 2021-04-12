#' Inference for the Mean via Permutations
#'
#' Performs statistical inference tasks (point estimation, confidence interval,
#' tests) for the mean of the distributions of scalar-valued data.
#'
#' @param x A numeric vector providing the first sample.
#' @param y A numeric vector providing the second sample.
#' @param alternative A string specifying the type of alternative hypothesis.
#'   Choices are `"two_tail"`, `"left_tail"` or `"right_tail"`. Defaults to
#'   `"tow_tail"`.
#' @param mu A numeric value providing the value of the mean difference `mu_y -
#'   mu_x` under the null hypothesis. Defaults to `0`.
#' @param paired A boolean specifying whether the two samples are paired.
#'   Defaults to `FALSE`. Unused at the moment since one-sample tests are not
#'   yet available.
#' @param conf_level A numeric value specifying the confidence level for the
#'   computation of a confidence interval for the mean difference. Defaults to
#'   `95%`.
#' @param B An integer specifying the number of permutations to use. Defaults to
#'   `1000L`.
#'
#' @return A list with class `"htest"` containing the following components:
#' - `statistic`: the value of the test statistic computed from the original
#' observed samples.
#' - `parameter`: the degrees of freedom of the null distribution of the test
#' statistic (not available for permutation tests).
#' - `p.value`: the p-value for the test.
#' - `estimate`: the estimated mean or mean difference `mu_y - mu_x` depending
#' on whether it was a one-sample test, a paired two-sample test or an
#' independent two-sample test.
#' - `conf.int`: a confidence interval for the mean of for the mean difference
#' appropriate to the specified alternative hypothesis.
#' - `null.value`: the specified hypothesized value of the mean or mean
#' difference depending on whether it was a one-sample test or a two-sample
#' test.
#' - `stderr`: an estimate of the standard deviation of the estimator of the
#' mean or mean difference.
#' - `alternative`: a string describing the alternative hypothesis.
#' - `method`: a string indicating what type of test was performed.
#' - `data.name`: a string giving the name(s) of the data.
#' @name mean
#'
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10)
#' z <- rnorm(10, mean = 3)
#' t1 <- t_test(x, y, B = 100)
#' t2 <- t_test(x, z, B = 100)
#' w1 <- wilcox_test(x, y, B = 100)
#' w2 <- wilcox_test(x, z, B = 100)
NULL

mean_null <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
