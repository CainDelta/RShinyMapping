### TODO: move to '.pkg_env'
api_url_ <- "http://pro.openweathermap.org/data/2.5/"
api = "3161dfe75ae578bb10796fb82bd51df2"

custom_assign_loc <- function(query, loc) {
  # skip in case (lat, lon) or zip code is passed instead of city name or id
  if (is.na(loc)) {
    return(query)
  }

  if (is.numeric(loc)) {
    query$id <- loc
  } else {
    query$q <- loc
  }

  query
}

### TODO: document in order to export
custom_parse <- function(response) {
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
}

### TODO: document in order to export
custom_wrap_get <- function(path = "weather", api_url = api_url_) {
  api_url <- paste0(api_url, path)
  function(loc = NA, ...) {
    query <- list(appid = api, ...) %>%
      custom_assign_loc(loc) # %>% c(list(...))
    httr::GET(api_url, query = query)

  }

}

custom_class <- function(response, class_name) {
  structure(response, class = c(class(response), class_name))
}


##Interim function generates 4 day hourly forecast weather variables. To be updated.
get_hourly_Weather <- function(City){
    url <- "http://pro.openweathermap.org/data/2.5/forecast/hourly?q="
    city <- City
    apikey <- "&appid=3161dfe75ae578bb10796fb82bd51df2&units=metric"
    joinURL <- paste0(url, city,apikey)
    results <- GET(joinURL)
    custom_parse(results)%>%
    custom_class("owmr_hourly_forecast")%>%
    owmr_as_tibble(simplify = TRUE)
    }

#### Export RAIN FUNCTION 
exportSIMPOLRain <- function(rain)

        rain%>%
        select(dt_txt,rain_1h)%>%
        mutate('Date/Time' = as.POSIXct(dt_txt))%>%
        mutate('AveRainIntensity' = replace_na(rain_1h,0))%>%
        mutate('UCWI'= -9999)%>%
        mutate('API30' = -9999)%>%
        select(`Date/Time`,AveRainIntensity,UCWI,API30)
