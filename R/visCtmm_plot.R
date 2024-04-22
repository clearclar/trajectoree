#' Title
#'
#' @param traj_model a ctmm model, output of traj_mod()
#' @param img_filename the name of the image file
#' @param band the band to be plotted
#'
#' @importFrom ctmm raster
#' @importFrom trajectoree reclassNA
#' @importFrom terra project app resample values rast
#' @importFrom raster plot
#'
#' @return a plot of the occurence vs the band
#' @export
#'
#' @examples
#' visOcc_AKDE_plot(traj_model, 'img_filename', 'B4')
visOcc_AKDE_plot <- function(traj_model, img_filename, band){
  image <- terra::rast(paste0(img_filename, '.tif'))

  # Autocorrelated Kernel Density Estimation
  akde_rast <- terra::rast(ctmm::raster(traj_model$akde))
  akde_rast_invert <- terra::app(akde_rast, function(i)
    (i-1)*(-1))
  akde_rast_cutoff <- trajectoree::reclassNA(akde_rast_invert, 0.05)
  akde_rast_reproject <- terra::project(akde_rast_cutoff, crs(image))

  image <- terra::resample(image, akde_rast_reproject, method = "bilinear")

  #Create a data frame from the raster values
  akde_df <- cbind.data.frame(values(image[band]), values(akde_rast_reproject))
  names(akde_df) <- c(band, "occ_rev")

  # ggplot(data = akde_df, aes(x = akde_df[band], y = occ_rev)) +
  #   geom_point(alpha = 0.1) +
  #   geom_rug(alpha = 0.01) +
  #   theme_light() +
  #   ggtitle(paste0("Occurence vs ", toString(band)))

  raster::plot(raster(akde_rast_reproject), raster(image[band]), maxpixels = 1e6,
               main = paste0('Occurence vs ', toString(band)), ylab = 'Occurence')
}

#' Title
#'
#' @param traj_model a ctmm model, output of traj_mod()
#' @param img_filename the name of the image file
#' @param band the band to be plotted
#'
#' @importFrom ctmm raster
#' @importFrom trajectoree reclassNA
#' @importFrom terra project app resample values rast
#' @importFrom raster plot
#'
#' @return a plot of the occurence vs the band
#' @export
#'
#' @examples
#' visOcc_UD_plot(traj_model, 'img_filename', 'B4')
visOcc_UD_plot <- function(traj_model, img_filename, band){
  image <- terra::rast(paste0(img_filename, '.tif'))

  # Autocorrelated Kernel Density Estimation
  UD_rast <- trajectoree::rast.UD(traj_model$od)
  UD_rast_invert <- terra::app(UD_rast, function(i)
    (i-1)*(-1))
  UD_rast_cutoff <- trajectoree::reclassNA(UD_rast_invert, 0.05)
  UD_rast_reproject <- terra::project(UD_rast_cutoff, crs(image))

  image <- terra::resample(image, UD_rast_reproject, method = "bilinear")

  #Create a data frame from the raster values
  ud_df <- cbind.data.frame(values(image[band]), values(UD_rast_reproject))
  names(ud_df) <- c(band, "occ_rev")

  # ggplot(data = ud_df, aes(x = ud_df[band], y = occ_rev)) +
  #   geom_point(alpha = 0.1) +
  #   geom_rug(alpha = 0.01) +
  #   theme_light() +
  #   ggtitle(paste0("Occurence vs ", toString(band)))

  raster::plot(raster(UD_rast_reproject), raster(image[band]), maxpixels = 1e6,
               main = paste0('Occurence vs ', toString(band)), ylab = 'Occurence')

}
