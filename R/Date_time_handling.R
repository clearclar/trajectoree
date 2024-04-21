#' Title
#'
#' @param traj a move object
#'
#' @importFrom lubridate years
#'
#' @return a resampled move object
#' @export
#'
#' @examples
#' resampleYears(traj)
resampleYears <- function(traj){
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  start_date <- traj_sf$timestamp[1]
  end_date <- tail(traj_sf$timestamp, 1)
  print(paste0('The start date of the trajectory is ', format(start_date, "%Y-%m-%d"), ' and the end date is ', format(end_date, "%Y-%m-%d")))

  if (as.numeric(format(start_date, "%Y")) < 2019) {
    print('From experience, reliable Sentinel-2 data is available from 2019 onwards, Landsat data from 2013 onwards')
    date_decision <- readline(prompt = 'Would you like to resample the trajectory? (y/n)')
    if (date_decision == 'y') {
      sensor_decision1 <- readline('Would you like to select 2019 (Sentinel-2) or 2013 (Landsat-8) as the reference year? (19/13): ')
      if (sensor_decision1 == '13'){
        if (start_date > as.Date('2013-01-01')){
          stop('The trajectory already starts after 2013')
        }
        else {
          year_delta <- 2013 - as.numeric(format(start_date, "%Y"))
        }
      }
      if (sensor_decision1 == '19'){
        if (start_date > as.Date('2019-01-01')){
          stop('The trajectory already starts after 2019')
        }
        else {
          year_delta <- 2019 - as.numeric(format(start_date, "%Y"))
        }
      }
      if(end_date + lubridate::years(year_delta) <= as.Date(format(Sys.time(), "%Y-%m-%d"))){
        traj_sf$timestamp <- sapply(traj_sf$timestamp, function(timestamp){
          POSIXct_timestamp <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")
          new_timestamp <- POSIXct_timestamp + lubridate::years(year_delta)
          return(new_timestamp)
        })
        traj_sf$timestamp <- as.POSIXct(traj_sf$timestamp)
        new_start <- traj_sf$timestamp[[1]]
        new_end <- tail(traj_sf$timestamp, 1)[[1]]
        print(paste0('The new start date is ', format(new_start, "%Y-%m-%d"), '. The new end date is ', format(new_end, "%Y-%m-%d"), '.'))
        return(traj_sf)
      }
      else {
        select_decision <- readline('The time series cannot simply be verschoben. Would you like to select one year from the dataset? (y/n): ')
        if (select_decision == 'y'){
          yearlist <- lapply(traj_sf$timestamp, function(timestamp){
            return(format(as.Date(timestamp), '%Y'))
          })
          print('The available years are:')
          yearlist_unique <- print(unique(unlist(yearlist)))
          year_select <- readline('Please select one: ')
          filtered_traj_sf <- traj_sf[format(as.Date(traj_sf$timestamp), '%Y') == year_select, ]
          if (sensor_decision1 == '19' & year_select > 2018){
            ref_year <- year_select
          }
          if (sensor_decision1 == '19' & year_select <= 2018){
            ref_year <- 2019
          }
          if (sensor_decision1 == '13' & year_select > 2012){
            ref_year <- year_select
          }
          if (sensor_decision1 == '13' & year_select <= 2012){
            ref_year <- 2013
          }
          filtered_traj_sf$timestamp <- sapply(filtered_traj_sf$timestamp, function(timestamp){
            POSIXct_timestamp <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")
            new_timestamp <- toString(format(POSIXct_timestamp, paste0(ref_year, '-%m-%d %H:%M:%OS')))
            return(new_timestamp)
          })
          filtered_traj_sf$timestamp <- as.POSIXct(filtered_traj_sf$timestamp)
          new_start <- as.POSIXct(filtered_traj_sf$timestamp[[1]])
          new_end <- as.POSIXct(tail(filtered_traj_sf$timestamp, 1)[[1]])
          print(paste0('The new start date is ', format(new_start, "%Y-%m-%d"), '. The new end date is ', format(new_end, "%Y-%m-%d"), '.'))
          return(filtered_traj_sf)
        }
        else {
          stop()
        }
      }
    }
    else {
      print('Trajectory not resampled')
    }
  }
}

#' Title
#'
#' @param traj a move file
#' @param interval The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt.
#'
#'@importFrom padr thicken
#'@importFrom dplyr filter
#'@importFrom sf st_as_sf
#'@importFrom sf st_combine
#'@importFrom sf st_centroid
#'@importFrom sf st_geometry
#'@importFrom utils object.size
#'
#' @return A sf object resampled to the specified interval. The centroid is taken from the points within one interval.
#' @export
#'
#' @examples
#' resampleTimestamp(movefile, 'week')
resampleInterval <- function(traj, interval){
  print(paste0('The object size used to be ', object.size(traj) %>% format(., units = 'Mb')))
  if (unlist(class(traj[1]))[1] == 'sf'){
    traj_sf <- traj
  }
  if (unlist(class(traj[1]))[1] != 'sf') {
    traj_sf <- sf::st_as_sf(traj)
  }

  traj_thickened <- traj_sf %>%  thicken(interval)
  names(traj_thickened)[length(names(traj_thickened))]<-"resample_interval"
  variant_types <- unique(as.character(traj_thickened$resample_interval))
  tmplist <- list()
  for (k in 1:length(variant_types)) {
    resample_interval <- variant_types[k]
    tmplist[[k]] <- traj_thickened %>% filter(resample_interval == variant_types[k]) %>% sf::st_as_sf() %>% sf::st_combine() %>% sf::st_centroid() %>% sf::st_as_sf()
    tmplist[[k]]$resample_interval <- resample_interval
  }
  resampled_traj_sf <- do.call(rbind.data.frame, tmplist)
  sf::st_geometry(resampled_traj_sf) <- 'geometry'
  names(resampled_traj_sf)[2]<-"timestamp"
  print(paste0('The object size is now ', object.size(resampled_traj_sf) %>% format(., units = 'Mb'),
               ', with a resampling interval of ', interval))
  return(resampled_traj_sf)
}
