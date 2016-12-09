#' Get Busses for a given location
#'
#' Returns IDs for the bus stop and expected times for the expected five (default)
#' busses. The function returns both timestamps for the bus arrivals as
#' well as minutes.
#'
#' @param lon numeric containing current longitude coordinate in format WGS86
#' @param lat numeric containing current latitude coordinate in format WGS86
#' @time POSIXct time indicating the moment of the client requesting the data.
#' @details
#' This function was built for an API to serve the HBL Commute App
#' @export


get_buses <- function(lon = 24.77770,
                      lat = 60.17746,
                      time = as.POSIXct("2016-12-07 15:00",
                                        format="%Y-%m-%d %H:%M")) {


  closest_stops <- get_closest_stops(lon,lat)

  depts <- get_next_depts(closest_stops$stationIndex[1], time)
  depts <- select(depts, stationIndex, line, dateArrival)
  depts$dateArrival <- as.integer(depts$dateArrival)
  return(jsonlite::toJSON(depts))
}
