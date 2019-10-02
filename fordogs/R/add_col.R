add_col <- function(x, name, vect, where = 1) {

  y <- setNames(data.frame(vect), name)

  if (any(names(x) %in% name)){
    y
  } else {
    insert_into(x, y, where = where)
  }
}
