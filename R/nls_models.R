#' functions for different light response models
#' @description fun_nonrec for nonrectangular hyperbola–based models
#' fun_rec for hyperbolic tangent models and
#' fun_exp for exponential based models

#' @param x is the PAR intensity of the leaf
#' @param Am is the gross photosynthetic rate
#' @param alpha is  quantum yield at low light intensity
#' @param theta is the convexity
#' @param Rd dark respiration rate
#' @param Ic light compensation point
#' @param b an adjusting factor
#' 
#' 
#' @details 
#' 
#' aims to help us to confirm the range of the start values for nls analysis
#' base on ggplot2 geom_smooth, as we just need to sure the estimate values are 
#' as close to the measured values as possible, then the value are the start values
#' for nls.
#' 
#' @examples
#' \dontrun{
#' # none, internal use
#' }
#' @export

fun_nonrec <- function(x, Am, alpha, Rd, theta) {
  (1/(2 * theta)) * (alpha * x + Am - sqrt((alpha * x + Am)^2 - 
  4 * alpha * theta * Am * x)) - Rd
}

           
fun_rec <- function(x, Am, alpha, Rd) {(alpha * x * Am) * 
    (1/(alpha * x + Am)) - Rd}


fun_exp <- function(x, Am, Ic, b) {Am*(1-exp((-b)*(x-Ic)))}

# fun_rev_rec <- alpha * ((1 - beta*x)/(1 + gamma * x)) * x - Rd
