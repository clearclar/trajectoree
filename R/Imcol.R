#' Title
#'
#' @param movefile a move file
#' @param product one of 'S2' or 'L8'
#' @param bands a list of bands to be downloaded
#' @param cloudcover the maximum cloud cover percentage as number
#'
#' @importFrom rgee ee
#' @importFrom rgee sf_as_ee
#' @importFrom utils object.size
#' @importFrom rgee ee_get_assethome
#'
#' @return a collection of images in rgee format
#'
#' @examples
#' defCol(movefile, 'S2', c('B2', 'B3', 'B4'), 10)
#'
#' @export
defCol <- function (traj, product, bands, cloudcover) {
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  df.size <- object.size(traj_sf) %>% format(., units = 'Mb')
  if (df.size > 1){
    resampledec <- readline(prompt = paste0('The size of the input data is too large (is: ', df.size, ', should be < 1 Mb). Do you want to downsample using resampleInterval() (y) or proceed and export the object over Google Cloud Storage (n)?: '))
    if (tolower(resampledec) == 'resample'){
      stop('Please resample the data and try again')
    }
    if (tolower(resampledec) == 'proceed'){
      traj_ee <- sf_as_ee(traj_sf['geometry'], via = 'gcs_to_asset',
                          assetId = sprintf("%s/%s", ee_get_assethome(), 'traj_ee_gcs'),
                          bucket = 'trajectoree')
    }
  } else {
    traj_ee <- sf_as_ee(traj_sf['geometry'])
  }

  roi_ee <- ee$FeatureCollection(traj_ee)

  bands <- ee$List(bands)
  start_date <- as.Date(traj_sf$timestamp[[1]])
  end_date <- as.Date(tail(traj_sf$timestamp, 1)[[1]])
  print(paste0('Start date: ', start_date, ', End date: ', end_date))

  if (product == 'S2') {
    product_collection <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  } else if (product == 'L8') {
    product_collection <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')
  } else {
    stop('Product not available')
  }

  while (TRUE) {
    collection <- product_collection$filterBounds(roi_ee)$
      filter(ee$Filter$lte('CLOUDY_PIXEL_PERCENTAGE', cloudcover))$
      filter(ee$Filter$date(rdate_to_eedate(start_date), rdate_to_eedate(end_date)))$
      select(bands)
    print(paste0('Number of images in the ImageCollection: ', collection$size()$getInfo(), '. A low number of images may be caused by dates before 2019. In this case, please change the time frame.'))
    change_decision <- readline(prompt = "Do you want to change the time frame of the query? (y/n): ")
    if (tolower(change_decision) != 'y') {
      break
    }
    new_start <- readline(prompt = paste0('Please specify a start date no later than: ', start_date, ' and before ', end_date, ' [YYYY-MM-DD]: '))
    new_end <- readline(prompt = paste0('Please specify an end date no earlier than: ', end_date, ' and later than ', new_start, ' [YYYY-MM-DD]: '))
    if (new_start < start_date | new_end > end_date) {
      stop('The new time frame is not within the original time frame')
    } else {
      start_date <- as.Date(new_start)
      end_date <- as.Date(new_end)
    }
    # start_date <- as.Date(new_start)
    # end_date <- as.Date(new_end)
  }

  return(collection)
}

#' Title
#'
#' @param collection an rgee image collection
#' @param indices A list of indices to be calculated. Possible indices are 'NDVI', 'NDWI', 'EVI', 'NDSI'.
#'
#' @importFrom rgee ee
#'
#' @return a collection of images with the calculated indices
#' @export
#'
#' @examples
#' calcIndices(collection, c('NDVI', 'NDWI'))
calcIndices <- function(collection, indices){
  for (i in 1:length(indices)){
    if (indices[i]=='NDVI'){
      index_calc <- function(image){
        image <- ee$Image(image)
        image <- image$addBands(image$normalizedDifference(list('B8', 'B4'))$rename('NDVI'))
        return(image)
      }
    }
    if (indices[i]=='NDWI'){
      index_calc <- function(image){
        image <- ee$Image(image)
        image <- image$addBands(image$normalizedDifference(list('B8', 'B4'))$rename('NDWI'))
        return(image)
      }
    }
    # if (indices[i]=='EVI'){
    #   index_calc <- function(image){
    #     image <- ee$Image(image)
    #     image <- image$addBands(image$expression('2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    #                                             'NIR', image$select('B8'),
    #                                             'RED', image$select('B4'),
    #                                             'BLUE', image$select('B2'))$rename('EVI'))
    #     return(image)
    #   }
    # }
    if (indices[i]=='NDSI'){
      index_calc <- function(image){
        image <- ee$Image(image)
        image <- image$addBands(image$normalizedDifference(list('B11', 'B8'))$rename('NDSI'))
        return(image)
      }
    }
    collection <- collection$map(index_calc)
  }
  return(collection)
}

#' Title
#'
#' @param traj_sf a trajectory in sf format
#' @param collection an rgee image collection
#' @param filename the name of the file to be downloaded
#' @param median a boolean to decide if the median image should be downloaded
#' @param scale the scale of the image in meters
#'
#' @importFrom rgee ee
#' @importFrom rgee sf_as_ee
#' @importFrom rgee ee_imagecollection_to_local
#'
#' @return download the images to the local machine
#' @export
#'
#' @examples
#' downRast(traj_sf, collection, 'img', TRUE, 10)
downRast <- function(traj, collection, filename='img', median=TRUE, scale=30) {
  print(paste0('Saving to current working directory: ', getwd()))
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  traj_ee <- rgee::sf_as_ee(traj_sf['geometry'])
  roi_ee <-  ee$FeatureCollection(traj_ee)
  bounds <- roi_ee$geometry()$bounds()

  # do not create median image, ask if number of images is acceptable
  if(median==FALSE){
    collection_down <- collection
    print(paste0('Number of image in the ImageCollection: ', collection$size()$getInfo()))
    decision <- readline(prompt="Do you want to proceed to download the data? ['hellyeah'/'nope']: ")
    if(decision=='nope'){
      stop('Aborted, please alter your time range or cloud cover percentage.', call. = FALSE)
    }
  }

  # create median image
  if(median==TRUE){
    collection_down <- collection$median()
  }

  # change all band data types to Float32
  downCol <- ee$ImageCollection(collection_down)$map(function(image){
    return(image$toFloat())
  })

  # download to local
  task <- ee_imagecollection_to_local(
    ic = downCol,
    region = bounds,
    scale = scale,
    lazy = FALSE,
    maxPixels = 1e13,
    via = "drive",
    dsn = filename,
    add_metadata = TRUE
  )

}
