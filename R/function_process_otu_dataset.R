#' Fix column typos, remove NAs, and parse taxonomic information in OTU dataset
#'
#' @param otu_data OTU dataset containing abundances and taxonomic classifications
#' @importFrom assertthat assert_that
#' @importFrom dplyr rename
#' @importFrom taxa parse_tax_data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @return taxmap object containing tibble with OTU counts and tibble with class data for each taxon
#'
#' @export
#'

parse_tax <- function(otu_data) {
  assertthat::assert_that(is.data.frame(spartina_otu))
  spartina_otu <- rename(spartina_otu, otu_id = out_id)
  spartina_otu <- spartina_otu %>%
    mutate(taxa_assignment = na_if(taxa_assignment, "None"))
  parsed_data <- parse_tax_data(spartina_otu,
                                class_cols = "taxa_assignment",
                                class_sep = ";",
                                class_regex = "^(.+)__(.+)$",
                                class_key = c(tax_rank = "info",
                                              tax_name = "taxon_name"))
  return(parsed_data)
}

