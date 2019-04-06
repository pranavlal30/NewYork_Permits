library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(stringr)

#######################################################################################
########################### 4. Construction ###########################################
#######################################################################################
wd <- getwd()
job_filings <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))
job_filings[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
job_filings$`Pre- Filing Date` <- as.Date(job_filings$`Pre- Filing Date`)
job_filings[,Year := format(`Pre- Filing Date`, "%Y")]
job_filings[,Month := format(`Pre- Filing Date`, "%m")]

names(job_filings)
NB_filings <- job_filings[`Job Type` == 'NB' | `Job Type` == 'A1',]
nrow(NB_filings)
nrow(NB_filings) / nrow(job_filings)
#New Buildings and A1 jobs account for 15% of the total jobs filed.



plot(NB_filings[,.N, by = c('Year')])

evolution <- job_filings[`Job Type` == 'NB' | `Job Type` == 'A1', .(NoOfJobs = .N), by = c('Borough', 'Year', 'Month')]


evolution <- as.data.table(evolution)


evolution <- evolution[!is.na(evolution$`Issuance Date`),]
head(trends)
evolution$Date <- paste(paste(evolution$Year, evolution$Month, sep = "-"), "01", sep = "-")

ggplot(evolution, aes(x = as.Date(Date), y = NoOfJobs, col = Borough)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("No. of NB/A1 Jobs") +
  ggtitle("New Buildings/A1 Jobs in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/NB_A1_ByBorough.png", sep = '/'))

NB_filings[,change_in_floors := `Proposed No. of Stories` - `ExistingNo. of Stories`]
NB_filings[,change_in_sqft := `Proposed Zoning Sqft` - `Existing Zoning Sqft`]

plot(NB_filings[,.(total_new_floors = sum(floors_added)), by = Year])

construction_trends <- NB_filings[,.(`Job Type` = `Job Type`,
                                     NoOfJobs = .N, 
                                     change_in_floors = sum(change_in_floors), 
                                     change_in_sqft = sum(change_in_sqft)), 
                                  by = c('Year', 'Month', 'Borough')]
head(construction_trends)

ggplot(trends, aes(x = as.Date(paste(paste(trends$Year, trends$Month, sep = "-"), "01", sep = "-")), y = total_new_floors, col = Borough)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Total Floors added") +
  ggtitle("New FLoors added in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/NewFloors_ByBorough.png", sep = '/'))

(sum(trends[Year <= 2009, total_new_floors]) - sum(trends[Year > 2009, total_new_floors])) / sum(trends[Year <= 2009, total_new_floors])

##There has been a 71% decrease in the no. of floors constructed since 2009.
construction_trends[,.N, by = c('Borough', 'Job Type')]
(35975 + 37534) / (37534 + 35975 + 9233 + 20243 + 15000)
(9233) / (37534 + 35975 + 9233 + 20243 + 15000)

##AOV of construnction trends

aov.mod <- aov(construction_trends[,NoOfJobs] ~ construction_trends[,Borough])
aov.mod <- aov(construction_trends[,NoOfJobs] ~ construction_trends[,Borough] + construction_trends[,`Job Type`])
summary(aov.mod)

a <- TukeyHSD(aov.mod)
summary(a)
a$`construction_trends[, Borough]`
###DEMOLITIONS

DM_filings <- job_filings[`Job Type` == 'DM',]
DM_filings[,change_in_floors := `Proposed No. of Stories` - `ExistingNo. of Stories`]
DM_filings[,change_in_sqft := `Proposed Zoning Sqft` - `Existing Zoning Sqft`]
dem_trends <- DM_filings[,.(`Job Type` = `Job Type`,
                        NoOfJobs = .N, 
                        change_in_floors = sum(change_in_floors), 
                        change_in_sqft = sum(change_in_sqft)), 
                     by = c('Year', 'Month', 'Borough')]
dem_trends[,Year := as.factor(Year)]
dem_trends[,Month := as.factor(Month)]
dem_trends[,Borough := as.factor(Borough)]
dem_trends[,`Job Type` := as.factor(`Job Type`)]

aov.mod <- aov(dem_trends[,NoOfJobs] ~ dem_trends[,Borough])
summary(aov.mod)
summary(construction_trends)
nrow(DM_filings)
nrow(DM_filings) / nrow(job_filings)
#Demolition jobs account for only 2.6% of the total jobs filed.



plot(DM_filings[,.N, by = c('Year')])

evolution <- job_filings[`Job Type` == 'DM', .(NoOfJobs = .N), by = c('Borough', 'Year', 'Month')]
evolution <- as.data.table(evolution)

head(trends)
evolution$Date <- paste(paste(evolution$Year, evolution$Month, sep = "-"), "01", sep = "-")

ggplot(evolution, aes(x = as.Date(Date), y = NoOfJobs, col = Borough)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("No. of NB/A1 Jobs") +
  ggtitle("New Buildings/A1 Jobs in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/NB_A1_ByBorough.png", sep = '/'))
head(DM_filings)

head(DM_filings)

trends$Date <- paste(paste(trends$Year, trends$Month, sep = "-"), "01", sep = "-")
ggplot(trends, aes(x = as.Date(Date), y = floors_demolished, col = Borough)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Total Floors added") +
  ggtitle("New FLoors added in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/FloorsDemolished_ByBorough.png", sep = '/'))

(sum(trends[Year <= 2009, floors_demolished]) - sum(trends[Year > 2009, floors_demolished])) / sum(trends[Year <= 2009, floors_demolished])

##There has been a 27% decrease in the no. of floors constructed since 2009.

