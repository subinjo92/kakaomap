kakao_geocode_keyword <- function (keyword, kakao_key, category_group_code, lon, lat, radius, rect) {

  if (is.character(keyword) == F){
    stop('address is not character')
  }

  #Encoding with url as required by kakao api
  if (Encoding(keyword) == "UTF-8"){
    enc_keyword <- URLencode(keyword)
  }
  if (Encoding(keyword) != "UTF-8"){
    enc_keyword <- URLencode(iconv(keyword, localeToCharset()[1], to = "UTF-8"))
  }
  url <- "https://dapi.kakao.com/v2/local/search/keyword.json?"

  url_fed_to_get <- paste0(url, "query=", enc_keyword)


  #Options that can be added to a function other than query
  #Add options to url and leave blank if not entered by user
  category_group_code_url <- ifelse(!missing(category_group_code),
                                    paste0('category_group_code=', category_group_code, sep = ""),
                                    "")

  lon_url <- ifelse(!missing(lon), paste0('x=', lon, sep = ""), "")

  lat_url <- ifelse(!missing(lat), paste0('y=', lat, sep = ""), "")

  radius_url <- ifelse(!missing(radius), paste0('radius=', radius, sep = ""), "")

  rect_url <- ifelse(!missing(rect), paste0('rect=', rect, sep = ""), "")


  url_fed_to_get <- paste(url_fed_to_get, category_group_code_url, lon_url, lat_url, radius_url, rect_url,
                          sep = '&')

  #Duplicate items added as "" above, leaving & only one
  url_fed_to_get <- gsub("[&]+", "&", url_fed_to_get)
  #When the last one is & texted, Remove this
  if (substr(url_fed_to_get, nchar(url_fed_to_get), nchar(url_fed_to_get)) == "&") {
    url_fed_to_get <- substr(url_fed_to_get, 1, nchar(url_fed_to_get) - 1)
  }

  #Importing the information using the kakao key
  address_result <- GET(url_fed_to_get, add_headers("Authorization" = str_c("KakaoAK ", kakao_key)))
  message(paste0("LONLAT from URL : ", url_fed_to_get))

  #Get it in json file and preprocess it.
  json <-  content(address_result, as = "text")
  processed_json <- fromJSON(json)

  #Stop or return a warning if there is no result or more than one
  if (processed_json$meta$total_count == 0){
    stop('No results found')
  }

  if (processed_json$meta$total_count != 1){
    warning('There are some results more than 1, please input more detail keyword or category_group_code')
  }

  result <- processed_json$documents[, c('place_name', 'x', 'y')]
  colnames(result) <- c('place_name', 'lon', 'lat')
  result$lat <- as.numeric(result$lat)
  result$lon <- as.numeric(result$lon)
  result
}
