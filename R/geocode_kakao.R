geocode_kakao <- function (address, kakao_key) {

  #Check if the address is formatted correctly
  if (is.character(address) == F){
    stop('address is not character')
  }

  #Encoding addresses in the format required by kakao api
  if (Encoding(address) == "UTF-8"){
    enc_address <- URLencode(address)
  }
  if (Encoding(address) != "UTF-8"){
    enc_address <- URLencode(iconv(address, localeToCharset()[1], to = "UTF-8"))
  }
  url <- "https://dapi.kakao.com/v2/local/search/address.json"
  url_fed_to_get <- paste0(url, "?query=", enc_address)

  #Import the address using the kakao key
  address_result <- GET(url_fed_to_get, add_headers("Authorization" = str_c("KakaoAK ", kakao_key)))
  #Imported in json format and preprocessed for easy viewing
  json <-  content(address_result , as = "text")
  processed_json <- fromJSON(json)

  #Verify that only one result is normally imported
  if (any(processed_json$meta$total_count == 0)){
    stop('uncorrect address')
  }
  else if (processed_json$meta$total_count != 1){
    stop('there are some result more than 1, please input more detail address')
  }

  #Only latitude and longitude are output from the results.
  result <- processed_json$documents[c('y', 'x')]
  colnames(result) <- c('lat', 'lon')
  result$lat <- as.numeric(result$lat)
  result$lon <- as.numeric(result$lon)
  result
}
