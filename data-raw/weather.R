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

devtools::use_data(stations, overwrite = TRUE, internal = TRUE)


