library(data.table)

wd <- getwd()
air_dir <- paste(wd, 'Data/EnvironmentalData/AirQuality/', sep = '/')
counties <- c("New York", "Kings", "Bronx", "Richmond", "Queens")

files <- list.files(path = air_dir, pattern = '*.csv', recursive = FALSE)
for (filename in files){
  print(filename)
  # if the merged dataset doesn't exist, create it
  if (!exists("air_data")){
    air_data <- fread(paste(air_dir, filename, sep = '/'))
    air_data <- air_data[State == "New York" & County %in% counties,]
  }
  
  # if the merged dataset does exist, append to it
  if (exists("air_data")){
    temp_dataset <-fread(paste(air_dir, filename, sep = '/'))
    temp_dataset <- temp_dataset[State == "New York" & County %in% counties,]
    air_data<-rbind(air_data, temp_dataset)
    rm(temp_dataset)
  }
  
}
head(air_data)
air_data[,Borough := County]
air_data[,c('State', 'County') := NULL]
air_data[,Borough := as.factor(Borough)]
levels(air_data$Borough) <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
head(air_data)
fwrite(x = air_data, file = paste(wd, 'Data/EnvironmentalData/air_data.csv', sep = '/'))



       