#' save a plot in png format
#'
#' @param plot a plot object
#' @param print_plot a boolean switch to turn on printing to console
#' @param file_name file name of the output file
#' @param width width of the output
#' @param height height of the output
#' @param type the type of png device
#' @param bg the background color
#'
#' @return NULL
#'
save_png <- function(plot, print_plot = TRUE, file_name, width, height, type = "cairo", bg = "white"){
  
  if(isTRUE(print_plot)){
    print(plot)
  }
  
  png(file_name, width = width, height = height, type = type, bg = bg)
  print(plot)
  dev.off()
  
  return(invisible(NULL))
}
