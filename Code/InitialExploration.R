library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(plyr)

wd <- getwd()

permit_issuance <- fread(paste(wd,"Data/DOB_Permit_Issuance.csv", sep = '/'))
permit_issuance[, `Issuance Date` := date(mdy_hms(`Issuance Date`))]
permit_issuance$`Issuance Date` <- as.Date(permit_issuance$`Issuance Date`)
permit_issuance <- permit_issuance[!is.na(permit_issuance$`Issuance Date`),]
#range(permit_issuance$`Issuance Date`, na.rm = TRUE) 
permit_issuance[,year := format(`Issuance Date`, "%Y")]
permit_issuance[,month := format(`Issuance Date`, "%m")]

nrow(permit_issuance[BOROUGH == "MANHATTAN"])/nrow(permit_issuance)

summary(permit_issuance)

NoOfJobs <- permit_issuance[year > 1989,.(NoOfJobs = .N), by = c('year', 'BOROUGH')]

NoOfJobs <- NoOfJobs[!year == "NA",]
head(NoOfJobs)
NoOfJobs <- NoOfJobs[order(BOROUGH, year)]

percent_change <- function(x) {
  (x/shift(x)) - 1
}

cols <- c("NoOfJobs")
NoOfJobs[, "percent_change_Jobs" := lapply(.SD, percent_change), by = BOROUGH, .SDcols = cols][]
head(NoOfJobs)

ggplot(NoOfJobs[year < 2019,], aes(x = year, y = NoOfJobs, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("NoOfJobs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Evolution of Jobs filed with the DOB")+
  scale_x_discrete(breaks = c('1989', '1994', '1999', '2004', '2009', '2014', '2018'))
ggsave(filename = paste(wd,"Results/NoOfJobsByBorough.png", sep = '/'))
ggplot(NoOfJobs[year < 2019 & percent_change_Jobs < 1,], aes(x = year, y = (percent_change_Jobs*100), col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("NoOfJobs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Evolution of Jobs filed with the DOB")+
  scale_x_discrete(breaks = c('1989', '1994', '1999', '2004', '2009', '2014', '2018'))

NoOfJobs <- NoOfJobs[year <2018 & year > 1990,]
###Population
#Prepare the population Data
Population <- fread(paste(wd,"Data/Annual_Population_Estimates.csv", sep = '/'))
counties <- c("New York County", "Kings County", "Bronx County", "Richmond County", "Queens County")
Population <- Population[Geography %in% counties,]
Population[,c('Program Type', 'FIPS Code') := NULL]
head(Population)
Population[,Geography := as.factor(Geography)]

levels(Population$Geography) <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
colnames(Population) <- c("Borough", "Year", "Population")
Population <- Population[order(Borough, Year)]
Population <- Population[Year > 1989,]
head(Population)



cols <- c("Population")
Population[, "percent_change_population" := lapply(.SD, percent_change), by = Borough, .SDcols = cols][]
Population <- data.table(ddply(Population,c('Borough', 'Year'), numcolwise(mean)))
Population <- Population[!is.na(percent_change_population),]
ggplot(Population[Year < 2019,], aes(x = Year, y = (percent_change_population*100), col = Borough, group = Borough)) +
  geom_line() +
  ylab("NoOfJobs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Evolution of Jobs filed with the DOB")


#Find correlation between %inc in population and permits
cor(Population$percent_change_population, NoOfJobs$percent_change_Jobs, use = "complete.obs")
cor(Population$Population, NoOfJobs$NoOfJobs)
#0.4302929

Population[,c('Borough', 'Year') := NULL]
head(Population)
nrow(Population)
Population[,MetricType := rep('Population', 135)]
NoOfJobs[,c('BOROUGH', 'year') := NULL]
NoOfJobs[,MetricType := rep('NoOfJobs', 135)]
colnames(Population) <- c("TrueValue", "Percent_Increase", "MetricType")
colnames(NoOfJobs) <- c("TrueValue", "Percent_Increase", "MetricType")
Population[,TrueValue := scale(TrueValue)]
Population[,Percent_Increase := scale(Percent_Increase)]
NoOfJobs[,TrueValue := scale(TrueValue)]
NoOfJobs[,Percent_Increase := scale(Percent_Increase)]
combined_data <- rbind(Population, NoOfJobs)

head(combined_data)
wilcox.test(TrueValue ~ MetricType, data = combined_data)
wilcox.test(Percent_Increase ~ MetricType, data = combined_data)


#######################################################################################
#################################### 1. BOROUGH #######################################
#######################################################################################

#Convert Borough to factor
permit_issuance[, BOROUGH := as.factor(BOROUGH)]
barplot(table(permit_issuance[,BOROUGH]))
ggplot(data = data.frame(permit_issuance[,.(BOROUGH, `Permit Status`)]), aes(x = permit_issuance[,BOROUGH], fill = permit_issuance[,`Permit Status`])) + 
  geom_bar() +
  ggtitle("Distribution of DOB Permit Issuance in each Borough")+
  xlab("Borough") +
  guides(fill=guide_legend(title="Permit Status"))
ggsave(filename = paste(wd,"Results/PermitDistributionByBorough.png", sep = '/'))

table(permit_issuance[,`Permit Status`])
#There are 10,813 Entries with a missing Permit Status

#Study the Permits that are revoked
revoked <- permit_issuance[`Permit Status` == 'REVOKED',]
revoked

job_permits[`Job #` == 401292365,]
#######################################################################################
#################################### 2. Job ~ Work ####################################
#######################################################################################

Job_frequencies <- data.table(table(permit_issuance[,`Job #`]))
colnames(Job_frequencies) <- c('JobID', 'NoOfWorks')

sprintf("Range of Works Per Job: %i - %i",min(table(permit_issuance[,`Job #`])), max(table(permit_issuance[,`Job #`])))

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

nrow(permit_issuance[`Job Type` == 'A2'])/nrow(permit_issuance)

png(filename = paste(wd,"Results/JobTypeDist.png", sep = '/'))
barplot(table(permit_issuance[,`Job Type`]),
        main = "Distribution of Job Types", ylab = "Count", xlab = "Job Type")
dev.off()

png(filename = paste(wd,"Results/PermitTypeDist.png", sep = '/'))
barplot(table(permit_issuance[,`Permit Type`]), 
        main = "Distribution of Permit Types", ylab = "Count", xlab = "Permit Type")
dev.off()

##There is only 1 row with missing Permit Type. What can I do about this?
nrow(permit_issuance[`Permit Type` == ''])      

table(permit_issuance[,`Job Type`])
png(filename = paste(wd,"Results/PermitsForJobTyepDist.png", sep = '/'))
par(mfrow = c(2,3))
barplot(table(permit_issuance[`Job Type` == "A1",`Permit Type`]),
        main = "A1 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(permit_issuance[`Job Type` == "A2",`Permit Type`]),
        main = "A2 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(permit_issuance[`Job Type` == "A3",`Permit Type`]),
        main = "A3 Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(permit_issuance[`Job Type` == "DM",`Permit Type`]),
        main = "DM Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(permit_issuance[`Job Type` == "NB",`Permit Type`]),
        main = "NB Jobs Distribution", ylab = "Count", xlab = "Permit Type")
barplot(table(permit_issuance[`Job Type` == "SG",`Permit Type`]),
        main = "SG Jobs Distribution", ylab = "Count", xlab = "Permit Type")
dev.off()
par(mfrow = c(1,1))

##Borough vs Job Type:
head(permit_issuance)
a <- head(permit_issuance)
ggplot(data = permit_issuance, aes(x =`Job Type`, fill = BOROUGH))+
  geom_bar()+
  ggtitle("Distribution of Job Types")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Dark2")
ggsave(filename = paste(wd,"Results/JobDistributionByBorough.png", sep = '/'))


tbl = table(permit_issuance$`Job Type`, permit_issuance$BOROUGH) 
chisq.test(tbl)

chisq.test(table(permit_issuance$`Job Type`, permit_issuance$BOROUGH)) 
# ~1.5M Entries either do not mention a work type or have a work type as 'Other'
# Of the remaining, Plumbing, Mechanical and Sprinklers accounts for ~50% (1.2M) entries


#######################################################################################
#################################### 1. BOROUGH vs Time ###############################
#######################################################################################


##Evolution of Permits over the years

permit_issuance$Count <- rep(1, nrow(permit_issuance))
evolution <- permit_issuance %>%
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
permit_issuance[`Job Type` == "DM", Count := -1]

evolution <- permit_issuance[`Job Type` == 'NB' | `Job Type` == "DM"] %>%
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


manhattan <- permit_issuance[BOROUGH == 'MANHATTAN', ]
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
names(permit_issuance)
permit_issuance[, `Issuance Date` := date(mdy_hms(`Issuance Date`))]
permit_issuance[,year := format(as.Date(`Issuance Date`), "%Y")]

census_tract <- permit_issuance[,.(count=.N, lat = mean(LATITUDE), lon = mean(LONGITUDE)), 
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
