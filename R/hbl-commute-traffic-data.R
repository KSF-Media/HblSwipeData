list.of.packages <- c("dplyr", "gmt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)


#' Data for every Buss, Train, Tram and Ferry Service
#'
#' Contains data on each service (i.e. individual departures). For each service the following variables
#' are provided: serviceId containing an identifier id, line indicating which line the service operates,
#' footnoteId used to map the correct schedule to the service and direction indicating the direction
#' of the service.
#'
#' @docType data
#'
#' @usage data(services)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(services)
#' lines <- services$line

#' Timetable Data for every Buss, Train, Tram and Ferry Stop
#'
#' Contains timetable data for the different stops. The variables provided are: serviceID, an identifier id
#' for each service (i.e. individual departure). The variable stationIndex, a unique index for each stop that can be
#' used for mapping the stop schedule to the stops' geographical location. The variable arrival indicating the
#' time when the service is expected to arrive at the stop. The variable stopNr is another unique identifier for the stop.
#'
#' @docType data
#'
#' @usage data(stops)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(services)
#' station_index <- services$stationIndex

#' Location and Name Data for every Buss, Train, Tram and Ferry Stations
#'
#' Contains geographical locations for the different stops. The variables provided are: stationId, an identifier id
#' for the stops. The vaiable stationLon prodiving the longitudinal data (in GWS84) for the stop. The vaiable
#' stationLat prodiving the latitudinal data (in GWS84) for the stop. The variable name providing the name for the
#' stop.
#'
#' @docType data
#'
#' @usage data(stations)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(stations)
#' names <- services$name

#' Data For Mapping Services to Dates
#'
#' Contains data for mapping services (i.e. individual departures) to dates. The variables provided are:
#' footnoteId for mapping the correct schedule to each service. The variable date identifying which dates
#' a certain service is scheduled for.
#'
#' @docType data
#'
#' @usage data(footnotes)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(footnotes)
#' date <- footnote$footnoteId





# ############################################################
# ## Functionality starts:
#
# ## Function that finds the n closest bus stops, n=5 as default
#

get_closest_stops <- function(lon, lat, n=5) {
  data(stations) #stations <- get_stations()

  return(

    stations %>%

      mutate(tmp_lon=lon, tmp_lat=lat, dist=gmt::geodist(stationLat,stationLon, tmp_lat, tmp_lon)*1000) %>%
      top_n(n,-dist) %>%
      select(-tmp_lon, -tmp_lat)

  )

}


# ## Function that returns the n next departures from the selected stop, n=50 as default
#

get_next_depts <- function(stId = 2214206, time, n=50) {
  data(stops) # stops <- get_stops()
  data(services) # services <- get_services()
  data(footnotes) # footnotes <- get_services()

  stops %>%
    filter(stationIndex==stId) %>%
    left_join(services,by="serviceId") %>%
    left_join(footnotes, by="footnoteId") %>%
    filter(as.POSIXct(date)==format(time,"%Y-%m-%d")) %>%
    mutate(dateArrival =
                    as.POSIXct(paste(format(time,"%Y-%m-%d"),
                                     sprintf("%04d",
                                             as.integer(sub("^24","00",
                                                            arrival)))),
                               format="%Y-%m-%d %H%M"),
           time_diff=dateArrival-time) %>%
    filter(time_diff > 0) %>%
    top_n(n,-time_diff)

}


# ## Returns score for given user id and list of tags
#

tag_score <- function(uid, tags){
  # tags <- strsplit(tags, ",")[[1]] # Split string from ,
  score <- filter(swipes, tag %in% tags) %>%
    mutate(tag_score = (pos+1)/(pos+neg+2)) %>%
    summarise(score = mean(tag_score)) %>%
    unlist()
  if (is.nan(score))
    score <- 0.5
  return(score)
}



# ## Update given swipe information
#

add_swipe <- function(in_uid, uuid, in_pos = 0, in_neg = 0){
  # Read tags from xlibris based on given uuid
  tags <- unlist(get_article_by_uuid(uuid))
  # Take subset of swipes which includes only given user and tags from given article
  sw <- filter(swipes, uid == in_uid, tag %in% tags)

  # Increment counts for existing tags
  swipes <- swipes %>% mutate(update_row = ((uid == in_uid) & (tag %in% tags)),  #determine which rows counts are updated
                              pos = ifelse(update_row, pos+in_pos, pos),
                              neg = ifelse(update_row, neg+in_neg, neg)) %>%
    select(-update_row)  #remove unnecessary temporary column

  # Add rows for new tags for this user
  tags_to_add <- setdiff(tags, sw$tag)  # What tags are not included in sw$tag
  swipes <- rbind(swipes, data.frame(uid = rep(in_uid, length(tags_to_add)),
                                     tag = tags_to_add,
                                     pos = rep(in_pos, length(tags_to_add)),
                                     neg = rep(in_neg, length(tags_to_add))))
  # Update global variable
  swipes <<- swipes
}


# ## Get article tags based on uuid from xlibris
#

get_article_by_uuid <- function(uuid, column_selection = c("Tags")){
  url_string <- "http://" %+% xlibris_user %+% ":" %+% xlibris_secret %+% "@" %+%
    xlibris_url %+% "/opencontent/search?q=uuid:" %+% uuid

  data <- jsonlite::fromJSON(URLencode(url_string))
  data <- do.call(dplyr::bind_rows,lapply(data$hits$hits$versions,
                                          function(x) x[[2]]))
  data <- data %>% dplyr::select_(.dots = column_selection)
  return(data)
}


