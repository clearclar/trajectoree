#' Title
#'
#' @param traj A move file
#'
#' @import ctmm
#' @import trajectoree
#' @import sf
#' @import terra
#'
#'
#' @return A list with the following ctmm elements: svf, guess, fits, res, akde, od, sim
#' @export
#'
#' @examples
#' traj_model <- traj_mod(traj)
traj_mod <- function(traj){
  traj_sf <- sf::st_as_sf(traj)
  traj_coords <- sf::st_coordinates(traj_sf)
  utm_zone <- trajectoree::long2UTM(mean(traj_coords[,1]))
  utm_north <- mean(traj_coords[,2]) > 0
  target_crs <- sf::st_crs(paste0("+proj=utm +zone=", utm_zone, if(!utm_north) " +south", " +datum=WGS84 +units=m +no_defs +type=crs"))
  traj_sf <- sf::st_transform(traj_sf, crs = target_crs)

  traj_ctmm <- trajectoree::sf2ctmm(traj_sf)

  traj_mod <- list()
  traj_mod$svf <- ctmm::variogram(traj_ctmm)
  traj_mod$guess <- ctmm::ctmm.guess(traj_ctmm, interactive=FALSE)

  traj_mod$fits <- ctmm::ctmm.select(traj_ctmm, traj_mod$guess, IC = "AIC", trace=1, verbose=TRUE, cores=1)
  # traj_mod$fits[["iid"]] <- ctmm::ctmm.fit(traj_ctmm, ctmm::ctmm(isotropic=TRUE))
  # traj_mod$fits[["iid.anisotropic"]] <- ctmm::ctmm.fit(traj_ctmm)
  # save residuals
  traj_mod$res <- residuals(traj_ctmm, traj_mod$fits[[1]]) # use best fit
  # utilization distribution
  traj_mod$akde <- ctmm::akde(traj_ctmm, traj_mod$fits[[1]])

  # occurrence distribution - using the selected model
  target_rast <- terra::rast(
    terra::ext(sf::st_bbox(traj_sf))*1.20,
    resolution = c(10, 10), crs = as.character(target_crs$wkt)
  )

  # utilization density
  traj_mod$od <- ctmm::occurrence(
    traj_ctmm, traj_mod$fits[[1]]
    ,
    grid = as(terra::project(
      x = target_rast,
      y = ctmm::projection(traj_ctmm), method = "near"), "Raster")
  )

  traj_mod$sim <- ctmm::simulate(traj_ctmm, traj_mod$fits[[1]], dt=5)

  return(traj_mod)
}
