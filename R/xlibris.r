# defining the operator for concatenating strings
`%+%` <- function(x, y) {paste0(x, y)}

#check and convert date to string
validate_and_convert_date <- function(x, q, error_msg = NULL){
  # x <- ymd("2011-01-01")
  # x <- now()
  # q <- "Pubdate.endexclusive"
  error_msg <- "ERROR"
  if(!is.null(x) & (!lubridate::is.Date(x) & !lubridate::is.POSIXct(x))){
    stop("date_start needs to be a Date object")
  }else if(!is.null(x)){
    return("+&Pubdate." %+% q %+% "=" %+%
      format(x, "%Y-%m-%dT%H:%M:%SZ"))
  }else{
    return("")
  }

}


build_search_string <- function(xlibris_url,
                                date_start = NULL,
                                date_end   = NULL,
                                brand      = NULL,
                                headline   = TRUE,
                                text       = TRUE,
                                limit      = 1000){

  brand_string      <- ifelse(is.null(brand),"","+&SubProduct=\"" %+%
                           brand %+% "\"")

  # check that dates are valid
  date_start_string <- validate_and_convert_date(date_start,
                                                 "startinclusive",
                                                 "date_start needs " %+%
                                                   "to be a Date object")

  date_end_string   <- validate_and_convert_date(date_end,
                                                 "endexclusive",
                                                 "date_end needs " %+%
                                                   "to be a Date object")


  "http://" %+% xlibris_user %+% ":" %+% xlibris_secret %+% "@" %+%
  xlibris_url %+% "/opencontent/search?q=NOT+Status:\"Återförd\"+AND+NOT+" %+%
  "SubProductId:19+AND+NOT+ArticleType:\"Advertorial\"+AND+Headline:*+AND+Headline:*" %+%
  date_start_string %+% date_end_string %+%
  brand_string %+% "&limit=" %+% limit %+%
    "&sort.name=Publiceringsdag"
}


#' Get Article Data From KSF Media xlibris API
#'
#' Fetches and returns data from KSF Media instance of NewsPilot API, which
#' is an Infomaker product. The searches can be limited in several ways.
#'
#' @return a data.frame with selected data.
#' @param column_selection a vector with desired columns. To get all columns leave empty or define as NULL
#' @param brand to select the desired media brand. Options are "hbl.fi", "ostnyland.fi", "vastranyland.fi". "HLK" (for HBL kväll). Leave empty or NULL to get all.
#' @param headline boolean. If TRUE the query only selects observations where the Headline field is not empty.
#' @param text boolean. If TRUE the query only selects observations where the Text field is not empty.
#' @param limit numeric. Define number of observation that will be returned
#' @param date_start lubridate Date or POSIXct object to indicate either date or datetime stamp to start the query with
#' @param date_end lubridate Date or POSIXct object to indicate either date or datetime stamp to end the query with
#' @export
#' @author Sami Kallinen
#' @importFrom magrittr "%>%"
#' @examples
#' # get the all the article uuid's from the past two hours
#' # last_uuids <- get_article_data(c("uuid"),
#' #   date_start = lubridate::now()-lubridate::hours(2),
#' #   date_end = lubridate::now())
#' # get last 1000 articles
#' # last_uuids <- get_article_data(limit=1000)
#' #   get last 100 hbl.fi articles
#' # last_uuids <- get_article_data(c("Headline", "SubProductShort"),
#' #   brand="hbl.fi",limit=100)

get_article_data <- function(column_selection = c("uuid",
                                                   "Headline",
                                                   "Text",
                                                   "UserfieldtimeOnSite",
                                                   "UserfieldpriorityArticle"
                                                   ),
                             date_start = NULL,
                             date_end = NULL,
                             brand    = NULL,
                             headline = TRUE,
                             text     = TRUE,
                             limit    = 1000
                             ){
  url_string <- build_search_string(xlibris_url, date_start, date_end,
                                    brand, headline, text, limit)
  data <- jsonlite::fromJSON(URLencode(url_string))
  data <- do.call(dplyr::bind_rows,lapply(data$hits$hits$versions,
                                          function(x) x[[2]]))
  if(!is.null(column_selection)){
    data <- data %>% dplyr::select_(.dots = column_selection)
  }
  if(!is.null(data$created)){
    data$created <- data$created %>% lubridate::ymd_hms(tz = "EET")
  }
  if(!is.null(data$Pubdate)){
    data$Pubdate <- data$Pubdate %>% lubridate::ymd_hms(tz = "EET")
  }
  if(!is.null(data$Updated)){
    data$Updated <- data$Updated %>% lubridate::ymd_hms(tz = "EET")
  }

  data
}

#' Get Available Columns from xlibris API
#'
#' Returns available columns that can be used for queries.
#'
#' @return a vector containing available columns
#' @param brand to select the desired media brand. Options are "hbl.fi", "ostnyland.fi", "vastranyland.fi". "HLK" (for HBL kväll). Leave empty or NULL to get all.
#' @param limit numeric. Define number of observation that will be returned.
#' @export
#' @author Sami Kallinen
#'
#' @importFrom magrittr "%>%"
#'
get_article_columns <- function(brand      = NULL,
                                limit      = 200){
  date_start = NULL
  date_end   = NULL
  headline   = TRUE
  text       = TRUE

  url_string <- build_search_string(xlibris_url, date_start, date_end,
                                    brand, headline, text, limit)
  data <- jsonlite::fromJSON(URLencode(url_string))
  data <- do.call(dplyr::bind_rows,lapply(data$hits$hits$versions, function(x) x[[2]]))
  data %>% names
}

