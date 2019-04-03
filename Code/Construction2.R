library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(stringr)

wd <- getwd()
job_filings <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))
job_filings[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
job_filings$`Pre- Filing Date` <- as.Date(job_filings$`Pre- Filing Date`)
job_filings[,Year := format(`Pre- Filing Date`, "%Y")]
job_filings[,Month := format(`Pre- Filing Date`, "%m")]

NB_filings <- job_filings[`Job Type` == 'NB' | `Job Type` == 'A1',]
NB_filings[,floors_added := `Proposed No. of Stories` - `ExistingNo. of Stories`]
NB_filings[,sqft_added := `Proposed Zoning Sqft` - `Existing Zoning Sqft`]
NB_filings[,`Constr Job` := 1]
construction_trends <- NB_filings[,.(NoOfConstJobs = .N, 
                                     floors_added = sum(floors_added), 
                                     sqft_added = sum(sqft_added)), 
                                  by = c('Year', 'Borough', 'Constr Job')]
construction_trends[,Year := as.factor(Year)]
construction_trends[,Borough := as.factor(Borough)]
construction_trends[,`Constr Job` := as.factor(`Constr Job`)]
head(construction_trends)


DM_filings <- job_filings[`Job Type` == 'DM',]
DM_filings[,floors_dem := `ExistingNo. of Stories` - `Proposed No. of Stories`]
DM_filings[,sqft_dem :=  `Existing Zoning Sqft` - `Proposed Zoning Sqft`]
DM_filings[,`Dem Job` := 1]
dem_trends <- DM_filings[,.(NoOfDemJobs = .N, 
                            floors_dem = sum(floors_dem), 
                            sqft_dem = sum(sqft_dem)), 
                         by = c('Year', 'Borough', 'Dem Job')]
head(dem_trends)
head(construction_trends)
dem_trends[,Year := as.factor(Year)]
dem_trends[,Borough := as.factor(Borough)]
dem_trends[,`Dem Job` := as.factor(`Dem Job`)]
construction_trends <- merge(construction_trends, dem_trends, 
                             by = c('Borough', 'Year'))
construction_trends <- construction_trends[Year != 2019,]

air_data <- fread(paste(wd,"Data/EnvironmentalData/air_data.csv", sep = '/'))

air_data <- air_data[Year > 1999,]
air_data[,Borough := as.factor(Borough)]
air_data[,Year := as.factor(Year)]
colnames(air_data)
colnames(construction_trends)
combined_data <- merge(construction_trends, air_data,
                       by = c('Borough', 'Year'))

names(combined_data)

glm.fit <- glm(formula = cbind(`Good Days`, `Unhealthy for Sensitive Groups Days`) ~ Borough + Year +
                 NoOfConstJobs + floors_added + sqft_added + NoOfDemJobs + floors_dem +
                 sqft_dem, data = combined_data, family = binomial(link = "logit"))

summary(glm.fit)

glm.fit <- glm(formula = cbind(`Good Days`, `Unhealthy for Sensitive Groups Days`) ~ NoOfConstJobs + floors_added + 
                 sqft_added + NoOfDemJobs + floors_dem +
                 sqft_dem, data = combined_data, family = binomial(link = "logit"))
a <- lm((`Days PM10` + `Days PM10`) ~ NoOfConstJobs + floors_added + sqft_added + NoOfDemJobs + floors_dem +
          sqft_dem, data = combined_data)
summary(a)

##No. of const and dem jobs and the sqft demolished seem to have an impact.

cor(combined_data$`Median AQI`, (combined_data$NoOfDemJobs + combined_data$NoOfConstJobs))

##Strong negative correlation