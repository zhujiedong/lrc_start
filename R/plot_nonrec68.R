#' plot for nonrectangular hyperbola–based models
#' @description plots of different parameter values in one plot, to determin the range
#' of start values, plot_nonrec64 for LI-6400 data, and plot_nonrec68 for LI-6800 data.
#'  
#' @param df dataframe for the LI-6400 or LI-6800 measured data
#' @param x is the PAR intensity of the leaf
#' @param Am is the gross photosynthetic rate
#' @param alpha is  quantum yield at low light intensity
#' @param theta is the convexity
#' @param Rd dark respiration rate 
#' 
#' @details use ggplot2 geom_smooth to fit the calculate data, generally the fit method
#' is loess, but it does not matter, as we just want to see these data overlap with the 
#' measured ones as much as possible.
#' @examples
#' \dontrun{
#' plot_nonrec64(lrc64, Am = 17, Rd = 3)
#' plot_nonrec68(lrc68, Am = 12, alpha = 0.01, theta = 0.8)
#' }
#' @export
#' @import ggplot2
#' @importFrom purrr pmap

plot_nonrec68 <- function(df, Am=max(df$A)-min(df$A), Rd=min(df$A), alpha = 0.01,
                          theta = 0.8){
  lrc_Q <- df$Qin
  lrc_A <- df$A
  n <- length(lrc_A)
  
  alph <- seq(alpha, n * alpha, by = alpha)
  alp <- paste0("a=", as.character(alph))
  alpn <- rep(alp, each = n)
  
  paras <- data.frame(alpha = rep(alph, each = n), 
                      x = rep(lrc_Q, n), Am = rep(Am, n), 
                      Rd = rep(-Rd, n), theta = rep(theta, n))
  
  x <- lrc_Q
  y <- unlist(pmap(paras, fun_nonrec))
  
  plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                          alpha = factor(c(rep("measured", n), alpn),
                                         level = c("measured", alp)))
  
  plot68 <- ggplot(data = plot_data, aes(x, y, group = alpha, color=alpha)) + 
    geom_point() + 
    geom_smooth(se = FALSE) + theme_bw()
  
  plot68
} 






