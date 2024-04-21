#' Title
#'
#' @param move_data A move data frame
#'
#' @importFrom sf st_as_sf st_combine st_centroid st_geometry
#' @importFrom dplyr filter
#' @importFrom lubridate years
#' @importFrom padr thicken
#' @importFrom utils object.size
#'
#'
#' @return An sf object containing the information of the move data, possibly temporally resampled
#' @export
#'
#' @examples
#' integrate_move(move_data)
intergrate_move <- function(move_data) {
  # Convert move_data to sf
  move_sf <- move_data %>% sf::st_as_sf()

  while (TRUE) {
    # Print information on file size, time frame, number of points
    start_date <- min(move_sf$timestamp)
    end_date <- max(move_sf$timestamp)
    df_size <- object.size(move_sf) %>% format(., units = 'Mb')
    no_days <- round(as.numeric(end_date - start_date))
    samples <- nrow(move_sf)
    cat(paste0('The time frame of the data is from ', start_date, ' to ', end_date))
    cat(paste0('. The size of the move data is ', df_size, ', with ', samples, ' measurements over ', no_days, ' days. '))
    cat(paste0('This corresponds to about ', round(samples / no_days, 2), ' measurements per day.'))

    if (df_size > 1) {
      cat(' A size smaller than 1 Mb is recommended.')
    }

    process_decision <- readline('Would you like to resume (0), change the sample interval (1) or change the time frame (2)?')

    if (process_decision == '0') {
      break
    } else if (process_decision == '1') {
      # Resample the data
      interval_decision <- readline('Would you like to resample to an hourly (1), daily (2), weekly (3) or monthly (4) sample interval?')
      interval <- switch(interval_decision,
                         '1' = 'hour',
                         '2' = 'day',
                         '3' = 'week',
                         '4' = 'month')

      traj_thickened <- padr::thicken(move_sf, interval = interval, by = 'timestamp', drop = TRUE)
      names(traj_thickened)[length(names(traj_thickened))] <- "timestamp"
      traj_thickened$timestamp <- as.POSIXct(traj_thickened$timestamp)
      variant_types <- unique(traj_thickened$timestamp)
      tmplist <- list()

      for (k in 1:length(variant_types)) {
        thick_timestamp <- variant_types[k]
        tmplist[[k]] <- dplyr::filter(traj_thickened, timestamp == thick_timestamp) %>%
          sf::st_as_sf() %>% sf::st_combine() %>% sf::st_centroid() %>% sf::st_as_sf()
        tmplist[[k]]$timestamp <- thick_timestamp
      }

      resampled_traj_sf <- do.call(rbind.data.frame, tmplist)
      sf::st_geometry(resampled_traj_sf) <- 'geometry'
      move_sf <- resampled_traj_sf
    } else if (process_decision == '2') {
      # Change the time frame
      cat("Sentinel-2 data is reliably available from 2019 onwards, Landsat data from 2013 onwards.\n")

      sensor_decision <- readline("Would you like to select 2019 (Sentinel-2) or 2013 (Landsat-8) as the reference year? (19/13): ")

      if (sensor_decision %in% c("13", "19")) {
        start_year <- as.numeric(format(start_date, "%Y"))
        ref_year <- switch(sensor_decision,
                           '13' = 2013,
                           '19' = 2019)

        if (start_year >= ref_year) {
          stop("The trajectory already starts in the selected time period")
        } else {
          year_delta <- ref_year - start_year
        }
      } else {
        stop("Invalid input. Please enter either '13' or '19' as the reference year.")
      }
      if (end_date + lubridate::years(year_delta) <= Sys.Date()) {
        move_sf$timestamp <- sapply(move_sf$timestamp, function(timestamp) {
          new_timestamp <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS") + lubridate::years(year_delta)
          return(new_timestamp)
        })

        move_sf$timestamp <- as.POSIXct(move_sf$timestamp)

        new_start <- move_sf$timestamp[[1]]
        new_end <- tail(move_sf$timestamp, 1)[[1]]

        cat("The new start date is ", format(new_start, "%Y-%m-%d"), ". The new end date is ", format(new_end, "%Y-%m-%d"), ".\n")

        move_sf <- move_sf
      } else {
        select_decision <- readline('The time series cannot simply be moved. Would you like to select one year from the dataset? (y/n): ')
        if (tolower(select_decision) == 'y') {
          yearlist <- lapply(move_sf$timestamp, function(timestamp) {
            return(format(as.Date(timestamp), '%Y'))
          })
          cat("The available years are:\n")
          cat(unique(unlist(yearlist)), "\n")
          year_select <- readline('Please select one: ')

          if (sensor_decision == '19' & year_select > 2018){
            ref_year <- year_select
          }
          if (sensor_decision == '19' & year_select <= 2018){
            ref_year <- 2019
          }
          if (sensor_decision == '13' & year_select > 2012){
            ref_year <- year_select
          }
          if (sensor_decision == '13' & year_select <= 2012){
            ref_year <- 2013
          }

          filtered_traj_sf <- move_sf[format(as.Date(move_sf$timestamp), '%Y') == year_select, ]
          filtered_traj_sf$timestamp <- sapply(filtered_traj_sf$timestamp, function(timestamp){
            POSIXct_timestamp <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")
            new_timestamp <- toString(format(POSIXct_timestamp, paste0(ref_year, '-%m-%d %H:%M:%OS')))
            return(new_timestamp)
          })

          filtered_traj_sf$timestamp <- as.POSIXct(filtered_traj_sf$timestamp)

          new_start <- as.POSIXct(filtered_traj_sf$timestamp[[1]])
          new_end <- as.POSIXct(tail(filtered_traj_sf$timestamp, 1)[[1]])

          cat("The new start date is ", format(new_start, "%Y-%m-%d"), ". The new end date is ", format(new_end, "%Y-%m-%d"), ".\n")

          move_sf <- filtered_traj_sf
        } else {
          stop("Process aborted by the user.")
        }
      }
    }
  }
  return(move_sf)
}
