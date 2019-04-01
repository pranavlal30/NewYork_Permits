library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)

wd <- getwd()

full_data <- fread(paste(wd,"Data/DOB_Permit_Issuance.csv", sep = '/'))

nrow(full_data)
head(full_data)
full_data[`Job #` == 123372466,]
summary(full_data)


#######################################################################################
#################################### 1. BOROUGH #######################################
#######################################################################################

#Convert Borough to factor
full_data[, BOROUGH := as.factor(BOROUGH)]
barplot(table(full_data[,BOROUGH]))
ggplot(data = data.frame(full_data[,.(BOROUGH, `Permit Status`)]), aes(x = full_data[,BOROUGH], fill = full_data[,`Permit Status`])) + 
  geom_bar() +
  ggtitle("Distribution of DOB Permit Issuance in each Borough")+
  xlab("Borough") +
  guides(fill=guide_legend(title="Permit Status"))
ggsave(filename = paste(wd,"Results/PermitDistributionByBorough.png", sep = '/'))

table(full_data[,`Permit Status`])
#There are 10,813 Entries with a missing Permit Status

#Study the Permits that are revoked
revoked <- full_data[`Permit Status` == 'REVOKED',]
revoked

job_permits[`Job #` == 401292365,]
#######################################################################################
#################################### 2. Job ~ Work ####################################
#######################################################################################

Job_frequencies <- data.table(table(full_data[,`Job #`]))
colnames(Job_frequencies) <- c('JobID', 'NoOfWorks')

sprintf("Range of Works Per Job: %i - %i",min(table(full_data[,`Job #`])), max(table(full_data[,`Job #`])))

png(filename = paste(wd,"Results/NoOfWorksPerJob<10.png", sep = '/'))
hist(Job_frequencies[NoOfWorks < 10,NoOfWorks], breaks = 10,
     xlab = "No Of Works per Job", main = "Distribution of No of Works per Job (< 10)")
dev.off()

png(filename = paste(wd,"Results/NoOfWorksPerJob>10<20.png", sep = '/'))
hist(Job_frequencies[NoOfWorks >= 10 & NoOfWorks < 20,NoOfWorks], breaks = 10,
     xlab = "No Of Works per Job", main = "Distribution of No of Works per Job >10 & <20")
dev.off()

png(filename = paste(wd,"Results/NoOfWorksPerJob>20<50.png", sep = '/'))
hist(Job_frequencies[NoOfWorks >= 20 & NoOfWorks < 50,NoOfWorks], breaks = 30,
     xlab = "No Of Works per Job", main = "Distribution of No of Works per Job >10 & <30")
dev.off()

png(filename = paste(wd,"Results/NoOfWorksPerJob>50.png", sep = '/'))
hist(Job_frequencies[NoOfWorks >= 50,NoOfWorks], breaks = 40,
     xlab = "No Of Works per Job", main = "Distribution of No of Works per Job >50")
dev.off()

#######################################################################################
#################################### 3. JOB TYPE ######################################
#######################################################################################

png(filename = paste(wd,"Results/JobTypeDist.png", sep = '/'))
barplot(table(full_data[,`Job Type`]),
        main = "Distribution of Job Types", ylab = "Count", xlab = "Job Type")
dev.off()

png(filename = paste(wd,"Results/PermitTypeDist.png", sep = '/'))
barplot(table(full_data[,`Permit Type`]), 
        main = "Distribution of Permit Types", ylab = "Count", xlab = "Permit Type")
dev.off()

##There is only 1 row with missing Permit Type. What can I do about this?
nrow(full_data[`Permit Type` == ''])      

table(full_data[,`Job Type`])
png(filename = paste(wd,"Results/PermitsForJobTyepDist.png", sep = '/'))
par(mfrow = c(2,3))
barplot(table(full_data[`Job Type` == "A1",`Permit Type`]),
        main = "A1 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(full_data[`Job Type` == "A2",`Permit Type`]),
        main = "A2 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(full_data[`Job Type` == "A3",`Permit Type`]),
        main = "A3 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(full_data[`Job Type` == "DM",`Permit Type`]),
        main = "DM Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(full_data[`Job Type` == "NB",`Permit Type`]),
        main = "NB Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(full_data[`Job Type` == "SG",`Permit Type`]),
        main = "SG Jobs Distribution", ylab = "Count", xlab = "Permit Type")
dev.off()
par(mfrow = c(1,1))


table(full_data[,`Work Type`])
# ~1.5M Entries either do not mention a work type or have a work type as 'Other'
# Of the remaining, Plumbing, Mechanical and Sprinklers accounts for ~50% (1.2M) entries


#######################################################################################
#################################### 1. BOROUGH vs Time ###############################
#######################################################################################


##Evolution of Permits over the years

full_data$Count <- rep(1, nrow(full_data))
evolution <- full_data %>%
  group_by(BOROUGH, `Job Type`, `Issuance Date`) %>%
  summarise(total = sum(Count))
evolution <- as.data.table(evolution)
evolution[, `Issuance Date` := date(mdy_hms(`Issuance Date`))]
evolution$`Issuance Date` <- as.Date(evolution$`Issuance Date`)

sum(evolution[is.na(evolution$`Issuance Date`),total])
#There are ~20K entries with no Issuance Date. For now, I am removing these
evolution <- evolution[!is.na(evolution$`Issuance Date`),]
head(evolution)
trends <- evolution %>%
  mutate(month = format(`Issuance Date`, "%m"), year = format(`Issuance Date`, "%Y")) %>%
  group_by(BOROUGH, month, year) %>%
  summarise(total = sum(total))
head(trends)
trends$Date <- paste(paste(trends$year, trends$month, sep = "-"), "01", sep = "-")

ggplot(trends, aes(x = as.Date(Date), y = total, col = BOROUGH)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Monthly Permits") +
  ggtitle("Permits Evolution in each Borough")
ggsave(filename = paste(wd,"Results/PermitEvolutionByBorough.png", sep = '/'))


##New Buildings added over the years

## No. of NB job types - No. of DM job types = No. of new buildings added
full_data[`Job Type` == "DM", Count := -1]

evolution <- full_data[`Job Type` == 'NB' | `Job Type` == "DM"] %>%
  group_by(BOROUGH, `Issuance Date`) %>%
  summarise(total = sum(Count))
evolution <- as.data.table(evolution)
evolution[, `Issuance Date` := date(mdy_hms(`Issuance Date`))]
evolution$`Issuance Date` <- as.Date(evolution$`Issuance Date`)

evolution <- evolution[!is.na(evolution$`Issuance Date`),]
trends <- evolution %>%
  mutate(month = format(`Issuance Date`, "%m"), year = format(`Issuance Date`, "%Y")) %>%
  group_by(BOROUGH, month, year) %>%
  summarise(total = sum(total))
head(trends)
trends$Date <- paste(paste(trends$year, trends$month, sep = "-"), "01", sep = "-")

ggplot(trends, aes(x = as.Date(Date), y = total, col = BOROUGH)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("New Buildings Added") +
  ggtitle("New Buildings added in each Borough")
ggsave(filename = paste(wd,"Results/New Buildings ByBorough.png", sep = '/'))

#Based on above two plots, we can make the following conclusions:

#Since 1990, Manhattan has seen many more work permits than the other Boroughs, but 
#much of that has been alteration work. The number of New Buildings added is low.
#Queens and Brooklyn, on the other hand, have seen a rapid increase in the number
#of new buildings added.

#Interestingly, a lot of new builing requests were filed around 2007-08.

##Let's look at Manhattan a little closely. What are the works being done there?


manhattan <- full_data[BOROUGH == 'MANHATTAN', ]
head(manhattan)
barplot(table(manhattan[,`Job Type`]),
        main = "Distribution of Job Types in Manhattan", ylab = "Count", xlab = "Job Type")
nrow(manhattan[`Job Type` == 'A2']) / nrow(manhattan)

##77.24% of the jobs are of A2 type

job_apps <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))
job_apps <- job_apps[Borough == "MANHATTAN"]
head(job_apps)
cols <- c('Job #', 'Job Type', 'Pre- Filing Date', 'ExistingNo. of Stories', 
          'Proposed No. of Stories', 'Existing Height', 'Proposed Height', 
          'Existing Dwelling Units', 'Proposed Dwelling Units')
floors_added <- job_apps[`ExistingNo. of Stories` < `Proposed No. of Stories`, (cols), with = FALSE]
nrow(floors_added) / nrow(job_apps)

##24% of the Jobs in Manhattan included adding stories to the 

floors_added[,floors_added := `Proposed No. of Stories` - `ExistingNo. of Stories`]
floors_added[,height_added := `Proposed Height` - `Existing Height`]

head(floors_added[,`Pre- Filing Date`])
floors_added[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
floors_added$`Pre- Filing Date` <- as.Date(floors_added$`Pre- Filing Date`)
floors_added[,year := format(as.Date(`Pre- Filing Date`), "%Y")]



png(filename = paste(wd,"Results/FloorsAddedDist.png", sep = '/'))
barplot(table(floors_added[floors_added < 50,floors_added]),
        main = "Distribution of No. of Floors added",
        xlab = "No. of floors added",
        ylab = "No. of job applications")
dev.off()

ggplot(floors_added[,sum(floors_added), by = year], aes(x = year, y = V1, group = 1)) +
  geom_line() + 
  ggtitle("No. of Floors added by Year in Manhattan") + 
  xlab("Year") + 
  ylab("No. of Floors added")
ggsave(filename = paste(wd,"Results/FloorsAddedByYear.png", sep = '/'))

##Maybe construction in the Manhattan area has become a little saturated over the 
##last decade or so.

##
names(full_data)
full_data[, `Issuance Date` := date(mdy_hms(`Issuance Date`))]
full_data[,year := format(as.Date(`Issuance Date`), "%Y")]

census_tract <- full_data[,.(count=.N, lat = mean(LATITUDE), lon = mean(LONGITUDE)), 
  by = c("year", "BOROUGH", "CENSUS_TRACT")]

census_tract <- census_tract[!is.na(census_tract[,year])]
head(census_tract)

ggpmap_api_key <- 'AIzaSyCkbgPvsO23Ka395-uT8Ugl0lK67iaLdtY'
#NYMap <- get_map(location = "New York")

#install this for api key
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
#register_google(key = "AIzaSyBoJ1aI3DsDP8BiAH1pw33Kb17pfW0fIRo")

#NYMap <- get_map(location = "New York",zoom = 11, maptype = "roadmap", 
#                         source = "google", api_key = ggpmap_api_key)
#save(NYMap, file = paste(wd, '/Code/NYMap.RData', sep  = ""))
load(paste(wd, '/Code/NYMap.RData', sep  = ""))
ggmap(NYMap)

years <- seq(1989, 2019, by = 1)

for(yr in years){
  print(yr)
  permits_map <- ggmap(NYMap, extent = "panel", maprange=FALSE) +
    geom_point(data = census_tract[year == yr,], aes(x = lon, y = lat, col = BOROUGH, size = count))+
    scale_size_continuous(range = c(0,10), limits = c(0,1700))+
    ggtitle(yr)+
    theme(plot.title = element_text(hjust = 0.5))
  filename <- paste(wd, "/Results/PermitsMaps/",toString(yr), ".png", sep = "")
  ggsave(filename = filename, plot = permits_map)
}
permits_2009
min(census_tract[,count])
max(census_tract[,count])
max(census_tract[])
ggmap(NYMap, extent = "panel", maprange=FALSE) +
  geom_point(data = census_tract[year == 1990,], aes(x = lon, y = lat, col = BOROUGH, size = count))+
  scale_size_continuous(range = c(0,10), limits = c(0,1700)) +
  ggtitle(yr)+
  theme(plot.title = element_text(hjust = 0.5))
