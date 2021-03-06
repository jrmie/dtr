% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render.R
\name{dt_to_flextable}
\alias{dt_to_flextable}
\title{Render a dt object in a word document}
\usage{
dt_to_flextable(x, title = NULL, font_size = NULL)
}
\arguments{
\item{x}{A dt object returned by the \code{dt_create()} function}

\item{title}{A title as charcters for the table.}

\item{font_size}{A numeric value to set the font size.}
}
\description{
Render a dt object in a word document
}
\examples{
dt_create(data, group = treatment, stat_num = "mean") \%>\%
 dt_to_flextable(title = "Compare treated and untreated individuals")

}
