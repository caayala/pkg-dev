#' Une data.frames con posición
#'
#' @param x data.frame
#' @param y data.frame
#' @param where numeric lugar en que `y` se une a `x`
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' t1 <- data.frame(a = 1, b = 2, c = 3)
#' t2 <- data.frame(X = 8, Y = 9)
#'
#' insert_into(t1, t2, where = 2)
insert_into <- function(x, y, where = 1){
  if (where == 1) { # first
    cbind(y, x)
  } else if (where > ncol(x)) { # last cal
    cbind(x, y)
  } else {
    cbind(x[1:(where-1)], y, x[where:ncol(x)])
  }
}
