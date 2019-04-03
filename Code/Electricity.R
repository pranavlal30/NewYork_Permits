library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(stringr)

wd <- getwd()


#######################################################################################
########################### 1. Electricity Consumption#################################
#######################################################################################

electricity <- fread(paste(wd,"Data/EnvironmentalData/Electric_Consumption_And_Cost.csv", sep = '/'))
job_filings <- fread(paste(wd,"Data/DOB_Job_Application_Filings.csv", sep = '/'))

head(electricity)

boroughs <- c('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND')

electricity <- electricity[Borough %in% boroughs,]
head(electricity[,`Service End Date`])
electricity[, `Service End Date` := date(mdy(`Service End Date`))]
electricity$`Service End Date` <- as.Date(electricity$`Service End Date`)

electricity[,`KWH Charges` := as.numeric(gsub(",","",gsub("\\$", "", `KWH Charges`)))]
electricity[,`KW Charges` := as.numeric(gsub(",","",gsub("\\$", "", `KW Charges`)))]
electricity[, TotalCharges := `KWH Charges` + `KW Charges`]
electricity[,c('KWH Charges', 'KW Charges') := NULL]

electricity[, Month := format(electricity[,`Service End Date`], "%m")]
electricity[, Year := format(electricity[,`Service End Date`], "%Y")]
electricity_consumption <- electricity[,.(KWH_Cons = sum(`Consumption (KWH)`),
                                          KW_Cons = sum(`Consumption (KW)`),
                                          TotalCharges = sum(TotalCharges)),
                                       by = c('Borough', 'Year', 'Month')]
electricity[,.(KWH_Cons = sum(`Consumption (KWH)`),
               KW_Cons = sum(`Consumption (KW)`),
               TotalCharges = sum(TotalCharges)),
            by = c('Borough', 'Year', 'Month')]
head(electricity_consumption)
electricity_consumption[,Date := paste(paste(Year, Month, sep = "-"), "01", sep = "-")]
head(electricity_consumption)

ggplot(electricity_consumption[Year < 2018, ], aes(x = as.Date(Date), y = KWH_Cons, col = Borough)) + 
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("KWH Consumption") +
  ggtitle("Electricity Consumption in each Borough")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = paste(wd,"Results/ElectricityConsumptionByBorough.png", sep = '/'))

(sum(electricity_consumption[Year == 2010, KWH_Cons]) - sum(electricity_consumption[Year == 2017, KWH_Cons])) / sum(electricity_consumption[Year == 2010, KWH_Cons])

head(electricity_consumption)
price_per_kwh <- electricity_consumption[,.(KWH = sum(KWH_Cons), TotalCharges = sum(TotalCharges)), by = year]
price_per_kwh[,price_per_kwh := TotalCharges/KWH]
price_per_kwh

#In 2014, Governor Andrew M. Cuomo launched New York’s signature energy policy, 
#Reforming the Energy Vision (REV). REV will build an integrated energy network 
#able to harness the combined benefits of the central grid with clean, locally 
#generated power.
#https://energyplan.ny.gov
#https://www.bls.gov/regions/new-york-new-jersey/news-release/averageenergyprices_newyorkarea.htm


head(job_filings[,`Job Description`])

electric_jobs <- job_filings[str_detect(job_filings[,`Job Description`], regex("electric", ignore_case = TRUE)),]
head(electric_jobs)
electric_jobs[, `Pre- Filing Date` := date(mdy(`Pre- Filing Date`))]
electric_jobs$`Pre- Filing Date` <- as.Date(electric_jobs$`Pre- Filing Date`)
electric_jobs[,Year := format(as.Date(`Pre- Filing Date`), "%Y")]
electric_jobs[,Month := format(as.Date(`Pre- Filing Date`), "%m")]

counts <- electric_jobs[Year > 2009 & Year < 2019,.(count = .N), by = c('Year', 'Month', 'Borough')]

counts <- counts[order(Borough, Year, Month)]
electricity_consumption <- electricity_consumption[order(Borough, Year, Month)]
head(counts)
head(electricity_consumption)
nrow(electricity_consumption[Year > 2009])
nlevels(as.factor(counts$Year))
nlevels(as.factor(electricity_consumption$Year))
nlevels(as.factor(counts[Borough == "STATEN ISLAND"]))
nrow(counts[Borough == "BRONX"])
nrow(counts[Borough == "MANHATTAN"])
nrow(counts[Borough == "BROOKLYN"])
nrow(counts[Borough == "QUEENS"])

nlevels(as.factor(electricity_consumption[Borough == "STATEN ISLAND",Year]))
nrow(electricity_consumption[Borough == "BRONX"])
nrow(electricity_consumption[Borough == "MANHATTAN"])
nrow(electricity_consumption[Borough == "BROOKLYN"])
nrow(electricity_consumption[Borough == "QUEENS"])
nlevels()

years <- seq(2010, 2018, by = 1)
months <- seq(1,12, by = 1)
merge(x = df, y = data.frame(months), by = NULL)


sum(counts[(year == 2014 | year == 2015 | year == 2016), count]) / sum(counts[,count])
a <- job_filings[year > 2011,]
nrow(a[`Job Type` == 'NB' | `Job Type` == 'A1'])/nrow(job_filings[`Job Type` == 'NB' | `Job Type` == 'A1'])

job_filings[year > 2009 & `Job Type` == 'A1',.(count = .N)]
job_filings[`Job Type` == 'NB',.(count = .N)]
counts <- electric_jobs[year > 2009,.(count = .N), by = year]
sum(counts[(year == 2014 | year == 2015 | year == 2016), count]) / sum(counts[,count])

##The years 2014-2016 along account for ~47% of the electricity works that took place
##In NY since 2010.
