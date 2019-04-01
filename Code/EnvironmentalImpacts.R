library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)

wd <- getwd()


#######################################################################################
########################### 1. Electricity Consumption#################################
#######################################################################################

electricity <- fread(paste(wd,"Data/EnvironmentalData/Electric_Consumption_And_Cost.csv", sep = '/'))
head(electricity)

boroughs <- c('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND')

electricity <- electricity[Borough %in% boroughs,]
electricity[, `Service End Date` := date(mdy(`Service End Date`))]
electricity$`Service End Date` <- as.Date(electricity$`Service End Date`)

electricity[,`KWH Charges` := as.numeric(gsub(",","",gsub("\\$", "", `KWH Charges`)))]
electricity[,`KW Charges` := as.numeric(gsub(",","",gsub("\\$", "", `KW Charges`)))]
electricity[, TotalCharges := `KWH Charges` + `KW Charges`]
electricity[,c('KWH Charges', 'KW Charges') := NULL]

electricity_consumption <- as.data.table(electricity %>%
  mutate(month = format(`Service End Date`, "%m"), year = format(`Service End Date`, "%Y")) %>%                                           
  group_by(Borough, year, month) %>%
  summarise(KWH_Cons = sum(`Consumption (KWH)`),
            KW_Cons = sum(`Consumption (KW)`),
            TotalCharges = sum(TotalCharges)))
electricity_consumption$Date <- paste(paste(electricity_consumption$year, electricity_consumption$month, sep = "-"), "01", sep = "-")
head(electricity_consumption)
electricity_consumption[,year := format(as.Date(`Date`), "%Y")]

ggplot(electricity_consumption, aes(x = as.Date(Date), y = KWH_Cons, col = Borough)) + 
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("KWH Consumption") +
  ggtitle("Electricity Consumption in each Borough")

price_per_kwh <- electricity_consumption[,.(KWH = sum(KWH_Cons), TotalCharges = sum(TotalCharges)), by = year]
price_per_kwh[,price_per_kwh := TotalCharges/KWH]
price_per_kwh

#In 2014, Governor Andrew M. Cuomo launched New York’s signature energy policy, 
#Reforming the Energy Vision (REV). REV will build an integrated energy network 
#able to harness the combined benefits of the central grid with clean, locally 
#generated power.
#https://www.bls.gov/regions/new-york-new-jersey/news-release/averageenergyprices_newyorkarea.htm

job_filings <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))
head(job_filings[,`Job Description`])
library(stringr)
electric_jobs <- job_filings[str_detect(job_filings[,`Job Description`], regex("electric", ignore_case = TRUE)),]
head(electric_jobs)
electric_jobs[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
electric_jobs$`Pre- Filing Date` <- as.Date(electric_jobs$`Pre- Filing Date`)
electric_jobs[,year := format(as.Date(`Pre- Filing Date`), "%Y")]

counts <- electric_jobs[year > 2009,.(count = .N), by = year]
sum(counts[(year == 2014 | year == 2015 | year == 2016), count]) / sum(counts[,count])

##The years 2014-2016 along account for ~47% of the electricity works that took place
##In NY since 2010.


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
  ggtitle("Water Consumption in each Borough")

water_consumption[,.(totalcons = sum(con)), by = year]
(1352800 - 979990) / 1352800
##Remarkarbly, NY has also seen a 27% decrease in Water consumption from 2013 - 2017.

#######################################################################################
########################### 2. Heating Gas Consumption#################################
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
