library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

wd <- getwd()
population <- fread(paste(wd,"Data/New_York_City_Population_by_Borough.csv", sep = '/'))


population[,`Age Group` := NULL]


population <- population[, grep("Boro share ", names(population)) := NULL]

population <- melt(population)
colnames(population) <- c("BOROUGH", "Year", "Population")
population <- population[!BOROUGH == 'NYC Total',]
population
population[,Total := sum(Population), by = Year]
population[,Ratio := (Population/Total * 100)]

ggplot(population, aes(x = Year, y = Ratio, col = BOROUGH, group = BOROUGH)) +
  geom_line() +
  ggtitle("Population Evolution in each Borough") +
  ylab("Percentage of Total Population")
ggsave(filename = paste(wd,"Results/PopulationEvolutionByBorough.png", sep = '/'))
