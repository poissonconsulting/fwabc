#' GNIS_NAME lookup table
#'
#' A dataset of all GNIS_NAMEs and which layers are available for each.
#'
#' @format A data frame with 18614 rows and 12 variables:
#' \describe{
#'   \item{GNIS_NAME}{gnis name}
#'   \item{WATERSHED_KEY}{fwa watershed key}
#'   \item{stream-network}{whether it is available for stream-network layer}
#'   \item{linear-boundaries}{whether it is available for linear-boundaries layer}
#'   \item{watersheds}{whether it is available for watersheds layer}
#'   \item{coastlines}{whether it is available for coastlines layer}
#'   \item{obstructions}{whether it is available for obstructions layer}
#'   \item{lakes}{whether it is available for lakes layer}
#'   \item{wetlands}{whether it is available for wetlands layer}
#'   \item{rivers}{whether it is available for rivers layer}
#'   \item{manmade-waterbodies}{whether it is available for manmade-waterbodies layer}
#'   \item{glaciers}{wwhether it is available for glaciers layer}
#'   ...
#' }
"fwa_lookup_gnis"

#' FWA layer lookup
#'
#' A dataset of all freshwater atlas layers that can be used in `fwa_read()`.
#'
#' @format A data frame with 11 rows and 3 variables:
#' \describe{
#'   \item{layer}{freshwater atlas layer}
#'   \item{WATERSHED_KEY}{whether it can be filtered on WATERSHED_KEY}
#'   \item{WATERSHED_GROUP_CODE}{whether it can be filtered by WATERSHED_GROUP_CODE}
#'   ...
#' }
"fwa_lookup_layer"
