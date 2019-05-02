#' #' Generate and lable Rkm points at specified distance along a line.
#' #'
#' #' @param x A valid WATERSHED_KEY.
#' #' @param distance A number indicating distance in metres between each point.
#' #' @param label_name A character string of the name of the new column containing km labels.
#' #' @param wskey_name A character string of the name of the new column containing WATERSHED_KEY.
#' #' @param sfc_name A character string of the name of the new sfc column containing geometries.
#' #' @return A sf object.
#' #' @examples
#' #' rkm <- fwa_rkm(360709847, distance = 10)
#' #' @export
#' fwa_rkm <- function(x = 360709847,
#'                     distance = 10, label_name = "Rkm",
#'                     stream_name = "WATERSHED_KEY", sfc_name = "geometry"){
#'
#'   check_vector(x, 1, length = 1)
#'   dat <- fwa_read_stream_network(x)
#'
#'   # get main stem
#'   dat <- dat[dat$BLUE_LINE_KEY == x,]
#'
#'   y <- line_rkm(dat, distance = distance,
#'                 label_name = label_name, sfc_name = sfc_name)
#'
#'   y[[stream_name]] <- x
#'   y
#' }

