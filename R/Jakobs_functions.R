#' Title
#'
#' @param long A numeric value representing the longitude
#'
#' @return A numeric value representing the UTM zone
#' @export
#'
#' @examples
#' long2UTM(long)
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

#' Title
#'
#' @param traj A move object?
#'
#' @importFrom moveVis df2move
#' @importFrom ctmm as.telemetry
#' @importFrom ctmm median
#' @importFrom ctmm projection
#' @importFrom sf st_coordinates
#' @importFrom sf st_crs
#'
#' @return A ctmm object
#' @export
#'
#' @examples
#' sf2ctmm(traj)
sf2ctmm <- function(traj){

  df <- cbind(as.data.frame(traj), sf::st_coordinates(traj))
  df$geometry <- NULL

  # make sure naming conventions match ctmm/move
  if(any(grepl("local-identifier", colnames(df)))){
    df <- df[,-grep("local-identifier", colnames(df))]
  }
  if(any(grepl("individual", colnames(df)))){
    colnames(df)[grep("individual", colnames(df))] <- "individual.local.identifier"
  }else{
    df$individual.local.identifier <- "X1"
  }
  colnames(df)[colnames(df) == "X"] <- "location.long"
  colnames(df)[colnames(df) == "Y"] <- "location.lat"

  traj.m <- moveVis::df2move(df, proj = sf::st_crs(traj), x = "location.long", y = "location.lat", time = "timestamp", track_id = "individual.local.identifier")
  traj.ctmm <- ctmm::as.telemetry(traj.m)

  # center the projection on the geometric median of the data
  ctmm::projection(traj.ctmm) <- ctmm::median(traj.ctmm)

  return(traj.ctmm)
}
