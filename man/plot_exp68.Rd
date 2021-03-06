% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_exp68.R
\name{plot_exp68}
\alias{plot_exp68}
\title{plot for exponential based models models}
\usage{
plot_exp68(df, Am = max(df$A) - min(df$A), Rd = min(df$A), b = 0.001,
  Ic = 0.01)
}
\arguments{
\item{Am}{is the gross photosynthetic rate}

\item{b}{an adjusting factor}

\item{Ic}{light compensation point}

\item{x}{is the PAR intensity of the leaf}
}
\description{
plots of different parameter values in one plot, to determin the range
of start values, plot_nonrec64 for LI-6400 data, and plot_nonrec68 for LI-6800 data.
}
\details{
use ggplot2 geom_smooth to fit the calculate data, generally the fit method
is loess, but it does not matter, as we just want to see these data overlap with the 
measured ones as much as possible.
}
\examples{
\dontrun{
plot_exp64(lrc64, Am = 17)
plot_exp68(lrc68, Am = 12)
}
}
