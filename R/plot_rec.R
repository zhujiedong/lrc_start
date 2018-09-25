#' plot for hyperbolic tangent models models
#' @description plots of different parameter values in one plot, to determin the range
#' of start values, plot_rec64 for LI-6400 data, and plot_rec68 for LI-6800 data.
#'  
#' @param x is the PAR intensity of the leaf
#' @param Am is the gross photosynthetic rate
#' @param alpha is  quantum yield at low light intensity
#' @param Rd dark respiration rate 
#' 
#' @details use ggplot2 geom_smooth to fit the calculate data, generally the fit method
#' is loess, but it does not matter, as we just want to see these data overlap with the 
#' measured ones as much as possible.
#' @examples
#' \dontrun{
#' plot_rec64(lrc64, Am = 23)
#'plot_rec68(lrc68, Am = 15, alpha = 0.01)
#' }
#' @export
#' @import ggplot2
#' @importFrom purrr pmap

plot_rec64 <- function(df, Am=max(df$Photo)-min(df$Photo), Rd=min(df$Photo),
                          alpha = 0.01){
  lrc_Q <- df$PARi
  lrc_A <- df$Photo
  n <- length(lrc_A)
  
  alph <- seq(alpha, n * alpha, by = alpha)
  alp <- paste0("a=", as.character(alph))
  alpn <- rep(alp, each = n)
  
  paras <- data.frame(alpha = rep(alph, each = n), 
                      x = rep(lrc_Q, n), Am = rep(Am, n), 
                      Rd = rep(-Rd, n))
  
  x <- lrc_Q
  y <- unlist(pmap(paras, fun_rec))
  
  plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                          a = factor(c(rep("measured", n), alpn),
                                     level = c("measured", alp)))
  
  plot64 <- ggplot(data = plot_data, aes(x, y, group = a, color=a)) + 
    geom_point() + 
    geom_smooth(se = FALSE) + theme_bw()
  
  plot64
} 


plot_rec68 <- function(df, Am=max(df$A)-min(df$A), Rd=min(df$A), alpha = 0.01){
  lrc_Q <- df$Qin
  lrc_A <- df$A
  n <- length(lrc_A)
  
  alph <- seq(alpha, n * alpha, by = alpha)
  alp <- paste0("a=", as.character(alph))
  alpn <- rep(alp, each = n)
  
  paras <- data.frame(alpha = rep(alph, each = n), 
                      x = rep(lrc_Q, n), Am = rep(Am, n), 
                      Rd = rep(-Rd, n))
  
  x <- lrc_Q
  y <- unlist(pmap(paras, fun_rec))
  
  plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                          a = factor(c(rep("measured", n), alpn),
                                     level = c("measured", alp)))
  
  plot68 <- ggplot(data = plot_data, aes(x, y, group = a, color=a)) + 
    geom_point() + 
    geom_smooth(se = FALSE) + theme_bw()
  
  plot68
} 

