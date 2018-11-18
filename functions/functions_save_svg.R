#' save a plot in svg format
#'
#' @param plot a plot object
#' @param print_plot a boolean switch to turn on printing to console
#' @param file_name file name of the output file
#' @param width width of the output
#' @param height height of the output
#'
#' @return NULL
#'
save_svg <- function(plot, print_plot = TRUE, file_name, width, height){
  
  if(isTRUE(print_plot)){
    print(plot)
  }
  
  svg(file_name, width = width, height = height)
  print(plot)
  dev.off()
  
  return(invisible(NULL))
}
