#' GNIS_NAME lookup table
#'
#' A dataset of all GNIS_NAMEs and which layers are available for each.
#'
#' @format A data frame with 18614 rows and 12 variables:
#' \describe{
#'   \item{GNIS_NAME}{gnis name}
#'   \item{WATERSHED_KEY}{fwa watershed key}
#'   \item{stream-network}{whether data is available for stream-network layer}
#'   \item{linear-boundaries}{whether data is available for linear-boundaries layer}
#'   \item{watersheds}{whether data is available for watersheds layer}
#'   \item{coastlines}{whether data is available for coastlines layer}
#'   \item{obstructions}{whether data is available for obstructions layer}
#'   \item{lakes}{whether data is available for lakes layer}
#'   \item{wetlands}{whether data is available for wetlands layer}
#'   \item{rivers}{whether data is available for rivers layer}
#'   \item{manmade-waterbodies}{whether data is available for manmade-waterbodies layer}
#'   \item{glaciers}{wwhether data is available for glaciers layer}
#'   ...
#' }
"fwa_lookup_gnis"

#' WATERSHED_GROUP_CODE lookup table
#'
#' A dataset of all WATERSHED_GROUP_CODES and which layers are available for each.
#'
#' @format A data frame with 18614 rows and 12 variables:
#' \describe{
#'   \item{WATERSHED_GROUP_CODE}{watershed group code}
#'   \item{WATERSHED_GROUP_NAME}{watershed group name}
#'   \item{stream-network}{whether data is available for stream-network layer}
#'   \item{linear-boundaries}{whether data is available for linear-boundaries layer}
#'   \item{watersheds}{whether data is available for watersheds layer}
#'   \item{coastlines}{whether data is available for coastlines layer}
#'   \item{obstructions}{whether data is available for obstructions layer}
#'   \item{lakes}{whether data is available for lakes layer}
#'   \item{wetlands}{whether data is available for wetlands layer}
#'   \item{rivers}{whether data is available for rivers layer}
#'   \item{manmade-waterbodies}{whether data is available for manmade-waterbodies layer}
#'   \item{glaciers}{wwhether data is available for glaciers layer}
#'   \item{watershed-groups}{wwhether data is available for watershed-groups layer}
#'   ...
#' }
"fwa_lookup_watershed_group"

#' FWA layer lookup
#'
#' A dataset of all freshwater atlas layers that can be used in `fwa_read()`.
#'
#' @format A data frame with 11 rows and 3 variables:
#' \describe{
#'   \item{layer}{freshwater atlas layer}
#'   \item{WATERSHED_KEY}{whether it can be filtered by WATERSHED_KEY}
#'   \item{WATERSHED_GROUP_CODE}{whether it can be filtered by WATERSHED_GROUP_CODE}
#'   ...
#' }
"fwa_lookup_layer"
