#' @rdname mean
#' @export
wilcox_test <- function(x, y = NULL,
                        alternative = c("two_tail", "left_tail", "right_tail"),
                        mu = 0, paired = FALSE,
                        conf_level = 0.95, B = 1000L) {
  if (is.null(y))
    stop("One-sample test not yet implemented.")
  if (!is.numeric(x) || !is.numeric(y))
    stop("Both data samples should be numeric and univariate.")

  alternative <- match.arg(alternative)

  method <- "Two-Sample Wilcoxon Rank Sum (Mann-Whitney) Permutation Test"

  statistic <- stat_wilcox(
    data = c(
      purrr::array_tree(x, margin = 1),
      purrr::array_tree(y, margin = 1)
    ),
    indices = 1:length(x)
  )

  pf <- flipr::PlausibilityFunction$new(mean_null, 1, x, y, stats = stat_wilcox)
  pf$set_nperms(B)
  pf$set_totperms(length(x) * length(y))
  pf$set_alternative(alternative)

  estimate <- stats::optim(
    par = mean(y)- mean(x),
    fn = function(x) -pf$get_value(x),
    method = "BFGS"
  )$par

  stderr <- estimate / statistic

  conf_int <- flipr::two_sample_ci(
    pf = pf,
    conf_level = conf_level,
    point_estimate = estimate
  )
  attr(conf_int, "conf.level") <- conf_level

  structure(
    list(
      statistic = statistic,
      parameter = NULL,
      p.value = pf$get_value(mu),
      estimate = c(`mu_y - mu_x` = estimate),
      conf.int = conf_int,
      null.value = c(`mu_y - mu_x` = mu),
      stderr = stderr,
      alternative = if (alternative == "two_tail")
        "two.sided"
      else if (alternative == "left_tail")
        "less"
      else
        "greater",
      method = method,
      data.name = paste0(
        rlang::enexpr(x) %>% rlang::expr_deparse(width = Inf),
        " and ",
        rlang::enexpr(y) %>% rlang::expr_deparse(width = Inf)
      )
    ),
    class = "htest"
  )
}

stat_wilcox <- function(data, indices) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  r <- rank(c(x1, x2))
  val <- sum(r[1:n1])
  names(val) <- "wilcoxon"
  val
}
