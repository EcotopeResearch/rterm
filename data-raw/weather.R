# Create stations internal dataset for the stationSearch function


library(rterm)
stations <- read.noaa.json("stations",
                           "datacategoryid=TEMP&datasetid=GHCND&startdate=2010-01-01&limit=1000")
count <- stations$metadata$resultset$count
nQueries <- ceiling(count / 1000)
sts <- lapply(1:nQueries, function(i) {
  read.noaa("stations", 
            paste("datacategoryid=TEMP&",
                  "datasetid=GHCND&",
                  "startdate=2010-01-01&",
                  "limit=1000&",
                  "offset=", 1000 * (i - 1) + 1 * (i > 1), sep = ""))
})

stations <- do.call('rbind', sts)
stations$name <- gsub("\\s+$", "", stations$name)

# Set maxdate to missing for any stations returning data in last month
monthAgo <- lubridate::today() - months(1)
stations$maxdate[stations$maxdate > monthAgo] <- NA


# Read in the country info, we'll merge that to get a country variable
statesURL <- RCurl::getURL("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-states.txt")
st <- readLines(textConnection(statesURL))
stateCode <- gsub("^([A-Z]+) (.+)$", "\\1", st)
stateName <- gsub("^([A-Z]+) (.+)$", "\\2", st)
stateName <- gsub("\\s+$", "", stateName)
states <- data.frame("state" = stateName, "stateCode" = stateCode)


# Read in the state info, we'll merge that to get a state variable
countriesURL <- RCurl::getURL("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt")
ct <- readLines(textConnection(countriesURL))
countryCode <- gsub("^([A-Z]+) (.+)$", "\\1", ct)
countryName <- gsub("^([A-Z]+) (.+)$", "\\2", ct)
countryName <- gsub("\\s+$", "", countryName)
countries <- data.frame("country" = countryName, "countryCode" = countryCode)


# Merge in Country Names
countryRows <- grep(", [A-Z]{2}$", stations$name)
stateRows <- grep(", [A-Z]{2} [A-Z]{2}$", stations$name)
stations$countryCode <- NA
stations$countryCode[countryRows] <- gsub("^.+, ([A-Z]{2})$", "\\1", stations$name[countryRows])
stations$countryCode[stateRows] <- gsub("^.+, [A-Z]{2} ([A-Z]{2})$", "\\1", stations$name[stateRows])
dim(stations)
stations <- merge(stations, countries, all.x = TRUE)
dim(stations)

# Merge in State Names
stateRows <- grep(", [A-Z]{2} [A-Z]{2}$", stations$name)
stations$stateCode <- NA
stations$stateCode[stateRows] <- gsub("^.+, ([A-Z]{2}) ([A-Z]{2})$", "\\1", stations$name[stateRows])

dim(stations)
stations <- merge(stations, states, all.x = TRUE)
dim(stations)

# Clean up
stations <- plyr::arrange(stations, id)
stations$stateCode <- NULL
stations$countryCode <- NULL



# Grab all of the TMY Data!!!

# USA TMY File
read.tmy <- function(wname) {
  print(wname)
  wfile <- paste(("/storage/server/TMY2DATA/"), wname, sep = "")
  weather <- read.table(wfile, fill = TRUE)
  weather <- weather[, 1]
  weather <- weather[-1]
  
  temp <- sapply(1:length(weather), function(i) {
    tmp <- as.numeric(substring(weather[i],67,70))/10
    tmp * 1.8 + 32
  })
  
  hourly8760 <- data.frame("doy" = rep(1:365, each = 24),
             "temp" = temp)
  daily <- aggregate(temp ~ doy, FUN = mean, data = hourly8760)
  daily$fname <- wname
  daily
}

tmyFiles <- dir("/storage/server/TMY2DATA", pattern = "3\\.tm2$")
tmyData <- do.call('rbind', lapply(tmyFiles, read.tmy))
tmyData$fname <- factor(tmyData$fname)
names(tmyData)[names(tmyData) == "fname"] <- "tmyFile"
tmyData <- tmyData[-grep("NOSUN", tmyData$tmyFile), ]


# Read in TMY Station meta data... where does that live?
load("../tmyStations.rda")
tmyStations <- tmyStations[-grep("NOSUN", tmyStations$tmyFile), ]

stations <- stations[, c("name", "id", "latitude", "longitude", "elevation", "elevationUnit",
                         "mindate", "maxdate", "datacoverage", "country", "state")]


devtools::use_data(stations, tmyStations, tmyData, overwrite = TRUE, internal = TRUE)


