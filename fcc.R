install.packages("RCurl")
library(RCurl)

# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api
latlong2fips <- function(latitude, longitude) {
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$Block['FIPS'])
}

# Orange County
as.numeric(latlong2fips(latitude=38.88393, longitude=-76.97877)) /1000
