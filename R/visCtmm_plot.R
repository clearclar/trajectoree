#' Title
#'
#' @param traj_model a ctmm model, output of traj_mod()
#' @param img_filename the name of the image file
#' @param band band name to be plotted
#' @param info 'akde' or 'od'
#' @param plottype 'ggplot' or 'raster'
#'
#' @return a plot of the occurence vs the band
#' @export
#'
#' @examples
#' visOcc_plot(traj_model, 'img_filename', 'NDVI', 'akde', 'ggplot')
visOcc_plot <- function(traj_model, img_filename, band, info, plottype='ggplot'){
  image <- terra::rast(paste0(img_filename, '.tif'))

  if (info=='akde'){
    akde_rast <- terra::rast(ctmm::raster(traj_model$akde))
  } else if (info=='od'){
    akde_rast <- trajectoree::rast.UD(traj_model$od)
  }
  # Autocorrelated Kernel Density Estimation

  akde_rast_invert <- terra::app(akde_rast, function(i)
    (i-1)*(-1))
  akde_rast_cutoff <- trajectoree::reclassNA(akde_rast_invert, 0.05)
  akde_rast_reproject <- terra::project(akde_rast_cutoff, crs(image))

  image <- terra::resample(image, akde_rast_reproject, method = "bilinear")

  if (plottype == 'ggplot'){
    #Create a data frame from the raster values
    akde_df <- cbind.data.frame(values(image[band]), values(akde_rast_reproject))
    names(akde_df) <- c(band, "occ_rev")

    ggplot(data = akde_df, aes(x = akde_df[[band]], y = occ_rev)) +
      geom_point(alpha = 0.1) +
      geom_rug(alpha = 0.01) +
      theme_light() +
      ggtitle(paste0("Occurence vs ", band)) +
      xlab(band) + ylab("Occurence") + coord_flip() # Add axis labels
  } else if (plottype == 'raster'){
    raster::plot(raster(akde_rast_reproject), raster(image[band]), maxpixels = 1e6,
                 main = paste0('Occurence vs ', toString(band)),
                 xlab='Occurence', ylab=toString(band))
  }
}

#' Title
#'
#' @param traj_model a ctmm model, output of traj_mod()
#' @param img_filename the name of the image file
#'
#' @importFrom ctmm raster
#' @importFrom trajectoree reclassNA
#' @import terra
#'
#' @return a map of the akde on top of a satellite image
#' @export
#'
#' @examples
#' visOcc_AKDE_map(traj_model, 'img_filename')
visOcc_AKDE_map <- function(traj_model, img_filename){
  image <- terra::rast(paste0(img_filename, '.tif'))

  # Autocorrelated Kernel Density Estimation
  akde_rast <- terra::rast(ctmm::raster(traj_model$akde))
  akde_rast_invert <- terra::app(akde_rast, function(i)
    (i-1)*(-1))
  akde_rast_cutoff <- trajectoree::reclassNA(akde_rast_invert, 0.05)
  akde_rast_reproject <- terra::project(akde_rast_cutoff, crs(image))

  image <- terra::resample(image, akde_rast_reproject, method = "bilinear")

  terra::plotRGB(image, ext = terra::ext(akde_rast_reproject), legend=FALSE, mar=c(3.1, 3.1, 2.1, 7.1), stretch='lin', axes=TRUE)
  terra::plot(akde_rast_reproject, add=TRUE, alpha = 0.9)
}

#' Title
#'
#' @param traj_model a ctmm model, output of traj_mod()
#' @param img_filename the name of the image file
#'
#' @importFrom ctmm raster
#' @importFrom trajectoree reclassNA
#' @import terra
#'
#' @return a map of the occurence on top of a satellite image
#' @export
#'
#' @examples
#' visOcc_UD_map(traj_model, 'img_filename')
visOcc_UD_map <- function(traj_model, img_filename){
  image <- terra::rast(paste0(img_filename, '.tif'))

  # Autocorrelated Kernel Density Estimation
  UD_rast <- trajectoree::rast.UD(traj_model$od)
  UD_rast_invert <- terra::app(UD_rast, function(i)
    (i-1)*(-1))
  UD_rast_cutoff <- trajectoree::reclassNA(UD_rast_invert, 0.05)
  UD_rast_reproject <- terra::project(UD_rast_cutoff, crs(image))

  image <- terra::resample(image, UD_rast_reproject, method = "bilinear")

  terra::plotRGB(image, ext = terra::ext(UD_rast_reproject), legend=FALSE, mar=c(3.1, 3.1, 2.1, 7.1), stretch='lin', axes=TRUE)
  terra::plot(UD_rast_reproject, add=TRUE, alpha = 0.9)
}
