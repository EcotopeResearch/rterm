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

