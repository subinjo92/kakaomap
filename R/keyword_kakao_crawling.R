keyword_kakao_crawling <- function (category_group_code,  kakao_key, lon, lat, radius = 1000, rect) {

  url <- "https://dapi.kakao.com/v2/local/search/category.json?"

  #Options that can be added to a function other than query
  #Add options to url and leave blank if not entered by user
  category_group_code_url <- ifelse(!missing(category_group_code),
                                    paste0('category_group_code=', category_group_code, sep = ""),
                                    "")

  lon_url <- ifelse(!missing(lon), paste0('x=', lon, sep = ""), "")

  lat_url <- ifelse(!missing(lat), paste0('y=', lat, sep = ""), "")

  radius_url <- ifelse(!missing(radius), paste0('radius=', radius, sep = ""), "")

  rect_url <- ifelse(!missing(rect), paste0('rect=', rect, sep = ""), "")


  url_fed_to_get <- paste(url, category_group_code_url, lon_url, lat_url, radius_url, rect_url,
                          sep = '&')

  #Duplicate items added as "" above, leaving & only one
  url_fed_to_get <- gsub("[&]+", "&", url_fed_to_get)
  #When the last one is & texted, Remove this
  if (substr(url_fed_to_get, nchar(url_fed_to_get), nchar(url_fed_to_get)) == "&") {
    url_fed_to_get <- substr(url_fed_to_get, 1, nchar(url_fed_to_get) - 1)
  }

  #Importing the information using the kakao key
  address_result <- GET(url_fed_to_get, add_headers("Authorization" = str_c("KakaoAK ", kakao_key)))
  message(paste0("information from URL : ", url_fed_to_get))

  #Get it in json file and preprocess it.
  json <-  content(address_result, as = "text")
  processed_json <- fromJSON(json)

  processed_json$meta$total_count
}
