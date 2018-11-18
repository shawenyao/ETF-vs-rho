#' save a plot in jpeg format
#'
#' @param plot a plot object
#' @param print_plot a boolean switch to turn on printing to console
#' @param file_name file name of the output file
#' @param width width of the output
#' @param height height of the output
#' @param type the type of jpeg device
#' @param bg the background color
#' @param quality the quality of the plot
#'
#' @return NULL
#'
save_jpg <- function(plot, print_plot = TRUE, file_name, width, height, type = "cairo", bg = "white", quality = 75){
  
  if(isTRUE(print_plot)){
    print(plot)
  }
  
  jpeg(file_name, width = width, height = height, type = type, bg = bg, quality = quality)
  print(plot)
  dev.off()
  
  return(invisible(NULL))
}
