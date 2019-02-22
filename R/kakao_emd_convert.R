kakao_emd_convert <- function (lon, lat, kakao_key)
{
  url <- "https://dapi.kakao.com/v2/local/geo/coord2regioncode.json"
  url_fed_to_get <- paste0(url, "?x=", lon, "&y=", lat)
  address_result <- GET(url_fed_to_get, add_headers(Authorization = str_c("KakaoAK ",
                                                                          kakao_key)))
  json <- content(address_result, as = "text")
  processed_json <- fromJSON(json)
  processed_json$documents[processed_json$documents$region_type == 'H', 'region_3depth_name']
}
