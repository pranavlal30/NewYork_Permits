library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

wd <- getwd()
population <- fread(paste(wd,"Data/New_York_City_Population_by_Borough.csv", sep = '/'))

percent_change <- function(x) {
  (x/shift(x)) - 1
}

population[,`Age Group` := NULL]


population <- population[, grep("Boro share ", names(population)) := NULL]

population <- melt(population)
colnames(population) <- c("BOROUGH", "Year", "Population")
population <- population[!BOROUGH == 'NYC Total',]
(sum(population[Year == 2020,Population]) - sum(population[Year == 1990,Population]))/ sum(population[Year == 1990,Population])


population[,Total := sum(Population), by = Year]
population[,Ratio := (Population/Total * 100)]
population <- population[population[,order(BOROUGH)]]
head(population)

cols <- c("Population", "Ratio")
population[, paste0("percent_change_", cols) := lapply(.SD, percent_change), by = BOROUGH, .SDcols = cols][]
head(population)
head(population)

ggplot(population, aes(x = Year, y = Population, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("Population") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(guide = FALSE)

ggplot(population, aes(x = Year, y = percent_change_Population, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("Population") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(guide = FALSE)

g1 <- ggplot(population, aes(x = Year, y = Population, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("Population") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(guide = FALSE)
ggsave(filename = paste(wd,"Results/PopulationEvolutionByBorough.png", sep = '/'))

g2 <- ggplot(population, aes(x = Year, y = Ratio, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ylab("Percentage of Total Population") +
  theme(plot.title = element_text(hjust = 0.5))
library(ggpubr)
ggarrange(g1, g2, nrow = 1, ncol = 2)
ggsave(filename = paste(wd,"Results/PopulationEvolutionByBorough.png", sep = '/'))


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
