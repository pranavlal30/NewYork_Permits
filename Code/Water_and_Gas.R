library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(stringr)

#######################################################################################
########################### 2. Water Consumption#######################################
#######################################################################################


water <- fread(paste(wd,"Data/EnvironmentalData/Water_Consumption_and_Cost.csv", sep = '/'))
head(water)


water <- water[Borough %in% boroughs,]
water[, `Service End Date` := date(mdy(`Service End Date`))]
water$`Service End Date` <- as.Date(water$`Service End Date`)

water_consumption <- as.data.table(water %>%
                                     mutate(month = format(`Service End Date`, "%m"), year = format(`Service End Date`, "%Y")) %>%                                           
                                     group_by(Borough, year, month) %>%
                                     summarise(con = sum(`Consumption (HCF)`),
                                               TotalCharges = sum(`Current Charges`)))

water_consumption$Date <- paste(paste(water_consumption$year, water_consumption$month, sep = "-"), "01", sep = "-")
head(water_consumption)

ggplot(water_consumption, aes(x = as.Date(Date), y = con, col = Borough)) + 
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Water Consumption (HCF)") +
  ggtitle("Water Consumption in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/WaterConsumptionByBorough.png", sep = '/'))

water_consumption[,.(totalcons = sum(con)), by = c('year', 'Borough')]
(311217 - 167522)/311217
(1352800 - 979990) / 1352800
##Remarkarbly, NY has also seen a 27% decrease in Water consumption from 2013 - 2017.

job_filings <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))

toilet_jobs <- job_filings[str_detect(job_filings[,`Job Description`], regex("leaks", ignore_case = TRUE)),]
plumbing_jobs <- job_filings[Plumbing == "X",]
head(plumbing_jobs$`Job Description`)
plumbing_jobs[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
plumbing_jobs$`Pre- Filing Date` <- as.Date(plumbing_jobs$`Pre- Filing Date`)
plumbing_jobs[,year := format(as.Date(`Pre- Filing Date`), "%Y")]

counts <- plumbing_jobs[,.(count = .N), by = year]
plot(counts)

(counts[year == 2014, count] - counts[year == 2013, count]) / counts[year == 2013, count]
((counts[year == 2014, count] + counts[year == 2015, count]) - mean(counts[year < 2019,count])) / counts[year == 2013, count]

(counts[year == 2014, count] + counts[year == 2015, count]) / sum(counts[year < 2019 & year > 2009, count])
##2014 and 2015 account for 25% of the plumbing jobs over the last decade.

#######################################################################################
########################### 3. Heating Gas Consumption#################################
#######################################################################################

gas <- fread(paste(wd,"Data/EnvironmentalData/Heating_Gas_Consumption_And_Cost.csv", sep = '/'))
head(gas)

gas <- gas[`ES Commodity` == 'UTILITY GAS',]
gas <- gas[Borough %in% boroughs,]
gas[, `Service End Date` := date(mdy(`Service End Date`))]
gas$`Service End Date` <- as.Date(gas$`Service End Date`)

gas_consumption <- as.data.table(gas %>%
                                   mutate(month = format(`Service End Date`, "%m"), year = format(`Service End Date`, "%Y")) %>%                                           
                                   group_by(Borough, year, month) %>%
                                   summarise(con = sum(`Consumption (Therms)`),
                                             TotalCharges = sum(`Current Charges`)))

gas_consumption$Date <- paste(paste(gas_consumption$year, gas_consumption$month, sep = "-"), "01", sep = "-")
head(gas_consumption)

ggplot(gas_consumption, aes(x = as.Date(Date), y = con, col = Borough)) + 
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Water Consumption (HCF)") +
  ggtitle("Water Consumption in each Borough")
ggsave(filename = paste(wd,"Results/SolarJobsByBorough.png", sep = '/'))

gas_consumption <- gas_consumption[,.(totalcons = sum(con)), by = c('year', 'Borough')]
gas_consumption[year == 2010,c('Borough', 'totalcons')]
gas_consumption[year == 2017,c('Borough', 'totalcons')]
merged <- merge(gas_consumption[year == 2010,c('Borough', 'totalcons')], gas_consumption[year == 2017,c('Borough', 'totalcons')], by = 'Borough')
merged[,increase := (totalcons.y - totalcons.x) / totalcons.x]
merged

##14% increase in gas consumption overall.
#Borough totalcons.x totalcons.y   increase
#1:         BRONX    41956205    43961966 0.04780607
#2:      BROOKLYN    53817019    60424064 0.12276869
#3:     MANHATTAN    41223823    48088088 0.16651209
#4:        QUEENS    14248443    18575910 0.30371508
#5: STATEN ISLAND     3649224     4281089 0.17315051
