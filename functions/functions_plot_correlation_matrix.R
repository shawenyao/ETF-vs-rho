#' plot correlation matrix
#' 
#' @param correlation_matrix the correlation matrix
#' @param title plot title
#' 
#' @return a ggplot object
#' 
plot_correlation_matirx <- function(correlation_matrix, title){
  
  categories <- colnames(correlation_matrix)
  
  correlation_matrix_long <- correlation_matrix %>% 
    data.frame(check.names = FALSE) %>% 
    rownames_to_column(var = "ticker1") %>% 
    gather(ticker2, correlation, -ticker1)
 
  ggplot(correlation_matrix_long, aes(x = ticker1, y = ticker2)) + 
    geom_tile(aes(fill = correlation), color = "gray50", size = 1) +
    scale_y_discrete(limits = rev(categories)) +
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white", 
      midpoint = 0, limit = c(-1, 1)
    ) +
    coord_fixed() +
    labs(x = "", y = "", title = title) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22),
      axis.text.x = element_text(size = 20, angle = 40, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 20)
    )
}
