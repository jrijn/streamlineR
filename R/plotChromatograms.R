#' @import rawrr
#' @import rstatix
#' @import tidyverse

#' @title plotChromatograms
#'
#' @param lc tibble; Output from importChromatograms.
#'
#' @return Returns a ggplot object.
#' @export
#' @examples
#'
#' p1 <- plotChromatograms(lc)
#' p1
#' ggsave("chromatograms.png", p1)
#'
plotChromatograms <- function(lc){

  lc.max <- lc %>%
    group_by(scan) %>%
    summarise(max = formatC(max(intensities)), format = "e")

  p <- ggplot(lc, aes(retention.time, norm.intensities, color = scan)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = c(0, 0.5, 1)) +
    geom_path() +
    geom_text(data = lc.max,
              aes(label = paste("BP:", max)),
              x = Inf,
              y = Inf,
              hjust = "inward",
              vjust = "inward",
              size = 3) +
    facet_grid(rows = vars(scan))+
    labs(title = "Chromatogram",
         x = "retention time",
         y = "intensity") +
    scale_color_manual(values = colorBlindGrey8) +
    publish(aspect.ratio = 1/6,
            border = F)

  return(p)
}
