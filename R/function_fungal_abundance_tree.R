#' Visualize fungal abundance from all samples in taxmap object
#'
#' @param obj_complete taxmap object containing tax_data, class_data, taxon_abundance, and tax_sample tibbles
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom phyloseq filter_taxa
#' @importFrom taxa filter_taxa
#' @importFrom metacoder heat_tree
#'
#' @export
#'


fung_abund_tree <- function(obj_complete) {
  assertthat::assert_that(is.object(obj_complete))
  obj_complete %>%
    filter_taxa(grepl(pattern = "^[a-zA-Z]+$", taxon_names)) %>%
    filter_taxa(taxon_names %in% ("Fungi"), subtaxa = TRUE) %>%
    taxa::filter_taxa(n_obs > 1) %>%
    heat_tree(node_label = taxon_names,
              node_size = n_obs,
              node_color = n_obs,
              title = "Sample read depth",
              title_size = 0.05,
              node_size_range = c(0.01, 0.05),
              edge_size_range = c(0.005, 0.005),
              node_size_axis_label = "OTU count",
              initial_layout = "re", layout = "da")
}
