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
  names(df) <- c(band, "occ_rev")

  raster::plot(raster(akde_rast_reproject), raster(image[band]), maxpixels = 1e6,
               main = paste0('Occurence vs ', toString(band)), ylab = 'Occurence')
}
