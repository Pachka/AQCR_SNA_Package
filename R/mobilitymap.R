#' Map of the mobility network
#'
#' This function produces a map illustrating the mobility network.
#'
#' Cette fonction sert à produire une carte de mobilité.
#'
#' @importFrom graphics points
#' @importFrom rworldmap getMap
#' @export mobilitymap
#' @param x the data.frame containing the degree (mobility parameter) and the GPS coordiantes (column names: LONGITUDE_X and LATITUDE_Y) \cr
#' le data.frame contenant le degré (paramètre de mobilité) et les coodonnées GPS (nom des colonnes : LONGITUDE_X et LATITUDE_Y)
#' @param y  (par defaut, 0.8) the size of the localities. It can be a parameters contained in x.
#' @keywords plot ggplot sna réseau
#' @usage mobilitymap(x,y)
#'
mobilitymap <- function(x, y = 0.8){
    z = y
  if(y != 0.8 & y %in% names(x))
    z <- (x[,y]*2)/max(x[,y])
  x <- x[x$degree != 0,]
  newmap <- getMap(resolution = "low")
  plot(newmap,
       xlim = c(min(x$LONGITUDE_X) - 1, max(x$LONGITUDE_X) + 1),
       ylim = c(min(x$LATITUDE_Y) - 1, max(x$LATITUDE_Y) + 1),
       asp = 1)
  points(x$LONGITUDE_X, x$LATITUDE_Y, bg="darkgreen", pch=19, cex = z)
}
