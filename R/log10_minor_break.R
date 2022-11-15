log10_minor_break = function (...){
#' Log10 minor break function.
#'
#' This function is scavenged from Stackoverflow and automates log scale minor
#' axis lines (https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot).
#'
#' @param x
#'
#' @return minor breaks object
#' @export
#'
#' @examples
#' ggplot(iris, aes(Petal.Width, Petal.Length, color = Species))+
#'   scale_x_continuous(trans = scales::log10_trans(),
#'                      minor_breaks = log10_minor_break())+
#'   geom_point()+
#'   publish(major_grid = T,
#'           minor_grid = T)
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks =
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}
