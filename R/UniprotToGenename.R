#' @import ggplot2
#'
#' @title UniprotToGenename
#' @description This is a simple function which uses the Uniprot REST API to
#' request the gene names from a list of uniprot IDs.
#' @param ids A character vector of Uniprot IDs.
#' @examples
#' contaminants <- c("P19013", "Q7RTT2", "P15636", "P09870", "Q9R4J5")
#' UniprotToGenename(ids = contaminants)
#'
#' @name UniprotToGenename
#' @rdname UniprotToGenename
#' @export
#'
library(httr)

UniprotToGenename <- function(ids) {

  ids <- paste(ids, collapse = ",")

  isJobReady <- function(jobId) {
    pollingInterval = 5
    nTries = 20
    for (i in 1:nTries) {
      url <- paste("https://rest.uniprot.org/idmapping/status/", jobId, sep = "")
      r <- GET(url = url, accept_json())
      status <- content(r, as = "parsed")
      if (!is.null(status[["results"]]) || !is.null(status[["failedIds"]])) {
        return(TRUE)
      }
      if (!is.null(status[["messages"]])) {
        print(status[["messages"]])
        return (FALSE)
      }
      Sys.sleep(pollingInterval)
    }
    return(FALSE)
  }

  getResultsURL <- function(redirectURL) {
    if (grepl("/idmapping/results/", redirectURL, fixed = TRUE)) {
      url <- gsub("/idmapping/results/", "/idmapping/stream/", redirectURL)
    } else {
      url <- gsub("/results/", "/results/stream/", redirectURL)
    }
  }

  files = list(
    ids = ids,
    from = "UniProtKB_AC-ID",
    to = "Gene_Name")

  r <- POST(url = "https://rest.uniprot.org/idmapping/run", body = files, encode = "multipart", accept_json())
  submission <- content(r, as = "parsed")

  if (isJobReady(submission[["jobId"]])) {
    url <- paste("https://rest.uniprot.org/idmapping/details/", submission[["jobId"]], sep = "")
    r <- GET(url = url, accept_json())
    details <- content(r, as = "parsed")
    url <- getResultsURL(details[["redirectURL"]])
    # Using TSV format see: https://www.uniprot.org/help/api_queries#what-formats-are-available
    url <- paste(url, "?format=tsv", sep = "")
    r <- GET(url = url, accept_json())
    resultsTable = read.table(text = content(r), sep = "\t", header=TRUE)
    print(resultsTable)
  }
}
