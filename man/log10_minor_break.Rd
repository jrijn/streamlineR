% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log10_minor_break.R
\name{log10_minor_break}
\alias{log10_minor_break}
\title{Log10 minor break function.}
\usage{
log10_minor_break(...)
}
\arguments{
\item{x}{}
}
\value{
minor breaks object
}
\description{
This function is scavenged from Stackoverflow and automates log scale minor
axis lines (https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot).
}
\examples{
ggplot(iris, aes(Petal.Width, Petal.Length, color = Species))+
  scale_x_continuous(trans = scales::log10_trans(),
                     minor_breaks = log10_minor_break())+
  geom_point()+
  publish(major_grid = T,
          minor_grid = T)
}
