plot_data <- function(df, device = c("LI-6400", "LI-6800"), alpha = 0.01,
         theta = 0.8){
  
  if(device == "LI-6400"){
    lrc_Q <- df$PARi
    lrc_A <- df$Photo
    n <- length(lrc_A)
    
    alph <- seq(alpha, n * alpha, by = alpha)
    alp <- paste0("a=", as.character(alph))
    alpn <- rep(alp, each = n)
    
    paras <- data.frame(alpha = rep(alph, each = n), 
                        x = rep(lrc_Q, n), Am = rep(max(lrc_A)-min(lrc_A), n), 
                        Rd = rep(-min(lrc_A), n), theta = rep(theta, n))
    
    x <- lrc_Q
    y <- unlist(pmap(paras, fun_nonrec))
    
    plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                           a = factor(c(rep("measured", n), alpn),
                                      level = c("measured", alp)))
    plot_data_64 <- plot_data
  } 
  
  else{
    lrc_Q <- df$Qin
    lrc_A <- df$A
    n <- length(lrc_A)
    
    # make a dataframe for ggplot2
    # make alpha the same number with A, Q
    alph <- seq(alpha, n * alpha, by = alpha)
    alp <- paste0("a=", as.character(alph))
    alpn <- rep(alp, each = n)
    
    paras <- data.frame(alpha = rep(alph, each = n), 
                        x = rep(lrc_Q, n), Am = rep(max(lrc_A)-min(lrc_A), n), 
                        Rd = rep(-min(lrc_A), n), theta = rep(theta, n))
    
    x <- lrc_Q
    y <- unlist(pmap(paras, fun_nonrec))
    
    plot_data <- data.frame(x = rep(lrc_Q, n + 1), y = c(lrc_A, y), 
                           a = factor(c(rep("measured", n), alpn),
                                      level = c("measured", alp)))
    plot_data68 <- plot_data
  }
}
