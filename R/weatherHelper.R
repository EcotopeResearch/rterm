#Weather Data Functions...


getGHCN <- function(ID) {
  #Read in the country info
  countriesURL <- getURL("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt")
  ct <- readLines(textConnection(countriesURL))
  countryCode <- gsub("^([A-Z]+) (.+)$", "\\1", ct)
  countryName <- gsub("^([A-Z]+) (.+)$", "\\2", ct)
  countryName <- rev(gsub("^ *(.+)$", "\\1", rev(countryName)))
  countries <- data.frame("country" = countryName, "code" = countryCode)

  #Read in the station info
  stations <- getURL("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt")
  ts <- read.fwf(file = textConnection(stations),
                 widths = c(11, -1, 8, -1, 9, -1, 6, -1, 2, -1, 30, -1, 3, -1, 3, -1, 5),fill = TRUE)
  names(ts) <- c("id", "lat", "lon", "elev", "state", "name", "gsnFlag", "hcnFlag", "wmoid")
  head(ts)
  
  ts$elev[ts$elev == "-999.9"] <- NA
  
  st <- read.table(text = stations, header = FALSE, strip.white = TRUE, fill = TRUE)
  

  urlTmp <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/"  
  fnames <- getURL(urlTmp)
  
  #download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", "stations.txt")
  

  
  st$country <- gsub("^([A-Z]+)[0-9].+$", "\\1", st$V1)
  
}

