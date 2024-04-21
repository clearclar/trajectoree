#' Title
#'
#' @param movefile a move file
#' @param collection an rgee image collection
#'
#' @import rgee
#' @import sf
#'
#' @return a map with the trajectory points and the images
#'
#' @examples
#' visTraj(movefile, collection)
#'
#' @export
visTrajEe <- function(traj, collection, bands){
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  VisCol <- ee$ImageCollection(collection)
  traj_ee <- rgee::sf_as_ee(traj_sf['geometry'])
  #roi_ee <- ee$FeatureCollection(traj_ee)

  Map$addLayer(VisCol$median(),
               visParams = list(bands = bands, min = 0, max = 3000),
               name = "Satellite imagery") +
    Map$addLayer(traj_ee,
                 visParams = list(color = 'red'),
                 name = "Trajectory points")
  if (length(bands) == 1 & bands[[1]] == 'NDVI' | bands[[1]] == 'NDWI' | bands[[1]] == 'NDSI'){
    Map$addLayer(VisCol$median(),
                 visParams = list(bands = bands, min = 0, max = 1),
                 name = bands[[1]]) +
      Map$addLayer(traj_ee,
                   visParams = list(color = 'red'),
                   name = "Trajectory points")
  }
  else {
    Map$addLayer(VisCol$median(),
                 visParams = list(bands = bands, min = 0, max = 3000),
                 name = "Satellite imagery") +
      Map$addLayer(traj_ee,
                   visParams = list(color = 'red'),
                   name = "Trajectory points")
  }
}

#' Title
#'
#' @param traj a move object of a trajectory
#' @param filename the filename of the downloaded raster
#'
#' @importFrom terra rast
#' @importFrom sf st_as_sf
#' @importFrom terra plotRGB
#'
#' @return a map with the trajectory points and the images using the base package
#' @export
#'
#' @examples
#' visTrajDown_base(traj, 'img')
visTrajDown_base <- function(traj, filename){
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  image <- terra::rast(paste0(filename, '.tif'))
  terra::plotRGB(image, r = 1, g = 2, b = 3, stretch = 'lin', axes=TRUE, mar=c(3.1, 3.1, 2.1, 7.1))
  plot(traj_sf$geometry, add = TRUE, type = 'p', pch = 20, col = 'red', cex = .7)
  }

#' Title
#'
#' @param traj a move object of a trajectory
#' @param filename the filename of the downloaded raster
#'
#' @import ggplot2
#' @import terra
#' @importFrom sf st_as_sf
#' @importFrom tidyterra geom_spatraster_rgb
#'
#' @return a map with the trajectory points and the images using ggplot
#' @export
#'
#' @examples
#' visTrajDown(traj, 'img')
visTrajDown <- function(traj, filename){
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  image <- terra::rast(paste0(filename, '.tif'))
  ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data=image,
                                   interpolate = TRUE,
                                   max_col_value = 3000) +
    geom_sf(data = traj_sf, aes(colour = timestamp)) +
    theme_light()
}
