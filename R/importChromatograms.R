#' @import rawrr
#' @import rstatix
#' @import tidyverse

#' @title chromatogramDf
chromatogramDf <- function(x) {
  df <- data.frame(
    retention.time = x$times,
    intensities = x$intensities
  )
  return(df)
}

#' @title importChromatograms
#'
#' @param rawfile file path; File path of an Orbitrap raw file.
#'
#' @return Returns a tibble containing the chromatography information.
#' @export
#' @examples
#'
#' rawfile <- "C:/myfile.raw"
#' lc <- importChromatograms(rawfile)
#'
importChromatograms <- function(rawfile){
  # object is .raw file path.
  tic <- rawrr::readChromatogram(rawfile = rawfile,
                                 filter = "ms",
                                 type = "tic")
  bpc <- rawrr::readChromatogram(rawfile = rawfile,
                                 filter = "ms",
                                 type = "bpc")
  bpc2 <- rawrr::readChromatogram(rawfile = rawfile,
                                  filter = "ms2",
                                  type = "bpc")

  lc <- lapply(list(tic = tic, ms1 = bpc, ms2 = bpc2), chromatogramDf) %>%
    bind_rows(., .id = "scan") %>%
    as_tibble() %>%
    group_by(scan) %>%
    mutate(across(!contains("scan"), as.numeric),
           norm.intensities = intensities/max(intensities)) %>%
    reorder_levels(scan, order = c("tic", "ms1", "ms2"))

  return(lc)
}
