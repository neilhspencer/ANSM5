#' @importFrom stats qnorm
noether <-
  function(p1, alpha = 0.05, power = 0.90) {
    stopifnot(is.numeric(p1), length(p1) == 1, p1 > 0, p1 < 1,
              is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1,
              is.numeric(power), length(power) == 1, power > 0, power < 1)

    zalpha <- qnorm(alpha, lower.tail = FALSE)
    zbeta <- qnorm(1 - power, lower.tail = FALSE)
    n <- ((zalpha + zbeta) ^ 2) / (4 * (p1 - 0.5) ^ 2)

    return(n)
  }
