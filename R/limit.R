# to_shortcut <- function(x){
#   ex <- grep("_max|_fwa|50K|CODES", x, value = T)
#   x <- setdiff(x, ex) %>%
#     tolower %>%
#     gsub("fwa_|_poly|_sp", "", .)
#   x
# }

# shortcut_to_layer <- function(x){
#   if(is_shortcut(x)){
#     data <- do.call("rbind", lapply(fwa_layers, function(x) x))
#     return(data$layer[which(data$shortcut == x)])
#   }
#   x
# }

# is_shortcut <- function(x){
#   sc <- lapply(fwa_layers, function(x) x$shortcut) %>% unlist(., use.names = FALSE)
#   x %in% sc
# }

#' Read a valid FWA layer limit rows.
#'
#' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' @param layer A character string of a valid FWA layer name from dsn or a valid shortcut layer name (see fwa_shortcuts dataset).
#' @param limit An integer indicating how many rows to limit to.
#' @param
#' @return A sf object.
#' @examples
#' stream1 <- ps_fwa_layer(layer = "routes", limit = 1)
#' @export

### damn! limit doesnt work in geodatabase
# ps_fwa_layer <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", layer = "routes", limit = 5L) {
#   check_layer(layer)
#   check_integer(limit)
#   layer <- shortcut_to_layer(layer)
#   check_dsn(dsn, layer = layer)
#
#   sql <- paste("select * from", layer, "limit", limit)
#   st_read(dsn, layer = layer, query = sql)
# }
