#' plot for exponential based models models
#' @description plots of different parameter values in one plot, to determin the range
#' of start values, plot_nonrec64 for LI-6400 data, and plot_nonrec68 for LI-6800 data.
#'  
#' @param df dataframe for the LI-6400 or LI-6800 measured data
#' @param x is the PAR intensity of the leaf
#' @param Am is the gross photosynthetic rate
#' @param b an adjusting factor
#' @param Ic light compensation point 
#' 
#' @details use ggplot2 geom_smooth to fit the calculate data, generally the fit method
#' is loess, but it does not matter, as we just want to see these data overlap with the 
#' measured ones as much as possible.
#' @examples
#' \dontrun{
#' plot_exp64(lrc64, Am = 17)
#' plot_exp68(lrc68, Am = 12)
#' }
#' @export
#' @import ggplot2
#' @importFrom purrr pmap

plot_exp68 <- function(df, Am=max(df$A)-min(df$A), Rd=min(df$A), b = 0.001,
                       Ic = 0.01){
  lrc_Q <- df$Qin
  lrc_A <- df$A
  n <- length(lrc_A)
  
  alph <- seq(b, n * b, by = b)
  alp <- paste0("b=", as.character(alph))
  alpn <- rep(alp, each = n)
  
  paras <- data.frame(b = rep(alph, each = n), 
                      x = rep(lrc_Q, n), Am = rep(Am, n), 
                      Ic = rep(Ic, n))
  
  x <- lrc_Q
  y <- unlist(pmap(paras, fun_exp))
  
  plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                          b = factor(c(rep("measured", n), alpn),
                                     level = c("measured", alp)))
  
  plot68 <- ggplot(data = plot_data, aes(x, y, group = b, color=b)) + 
    geom_point() + 
    geom_smooth(se = FALSE) + theme_bw()
  
  plot68
} 


