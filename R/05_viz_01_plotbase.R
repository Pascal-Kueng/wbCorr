
wb_plot <- function(x, y, which = NULL, ...) {
  if (is.null(which)) {
    which <- y
  }


  if ('w' %in% which | 'within' %in% which) {
    plot(x@within_df)
  }
  if ('b' %in% which | 'between' %in% which) {
    plot(x@between_df)
  }

}
