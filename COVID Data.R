#analyzed data gathered from NYTimes COVID Data
getwd()
setwd("D:/Research/2020_COVID19_project/COVID-CoDE-Lab/")

USCounties <- read.csv('us-counties.csv')
USStates <- read.csv('us-states.csv')

library(ggplot2, dplyr)
library(gridExtra)
library(lubridate)

View(USCounties)
View(USStates)

#color-blind-friendly color palette
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
               "#CC79A7", "#0072B2", "#D55E00", "#CC79A7")

#separate year from month & day

USCounties$date <- as.Date(USCounties$date)
USStates$date <- as.Date(USStates$date)

#Counties to look at: San Mateo, Contra Costa, SF, Santa Clara, Alameda, Marin

SM <- subset(USCounties, county == 'San Mateo' & state == 'California')
CC <- subset(USCounties, county == 'Contra Costa' & state == 'California')
SF <- subset(USCounties, county == 'San Francisco' & state == 'California')
SC <- subset(USCounties, county == 'Santa Clara' & state == 'California')
AL <- subset(USCounties, county == 'Alameda' & state == 'California')
MA <- subset(USCounties, county == 'Marin' & state == 'California')

BA <- subset(USCounties, county == 'San Mateo' & state == 'California' | 
               county == 'Contra Costa' & state == 'California' | 
               county == 'San Francisco' & state == 'California' |
               county == 'Santa Clara' & state == 'California' |
               county == 'Alameda' & state == 'California' |
               county == 'Marin' & state == 'California')

#Bay Area COVID Cases by County

ggplot(data = BA, aes(x = date, y = cases, color = county)) + 
  geom_line(size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  geom_label(aes(date, cases, color = factor(county), label = sprintf('%0.1d', cases)),
                   data = tail(BA, 6), show.legend = FALSE) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 weeks", ) +
  scale_color_manual(values = cbPalette) +
  xlab("Dates") + ylab('Number of Cases per County per Day') + 
  labs(color = 'Bay Area County') + ggtitle("Bay Area COVID Cases by County") +
  theme_light() + theme(panel.grid.minor.x = element_blank())

#Change in cases over time
##create function and columns for difference by day

f <- function(x) c(NA,(tail(x,-1) - head(x,-1))/head(x,-1))

AL[, paste0(names(AL)[sapply(AL, is.numeric)],'_diff')] <- 
  sapply(AL[,sapply(AL, is.numeric)], f)

CC[, paste0(names(CC)[sapply(CC, is.numeric)],'_diff')] <- 
  sapply(CC[,sapply(CC, is.numeric)], f)

MA[, paste0(names(MA)[sapply(MA, is.numeric)],'_diff')] <- 
  sapply(MA[,sapply(MA, is.numeric)], f)

SC[, paste0(names(SC)[sapply(SC, is.numeric)],'_diff')] <- 
  sapply(SC[,sapply(SC, is.numeric)], f)

SF[, paste0(names(SF)[sapply(SF, is.numeric)],'_diff')] <- 
  sapply(SF[,sapply(SF, is.numeric)], f)

SM[, paste0(names(SM)[sapply(SM, is.numeric)],'_diff')] <- 
  sapply(SM[,sapply(SM, is.numeric)], f)

##plot cases diff

DiffAL <- ggplot(data = AL, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1)) + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("Alameda County Daily Change in Cases") + theme_minimal()

DiffCC <- ggplot(data = CC, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1)) + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("Contra Costa County Daily Change in Cases") + theme_minimal()

DiffMA <- ggplot(data = MA, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1)) + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("Marin County Daily Change in Cases") + theme_minimal()

DiffSC <- ggplot(data = SC, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1))  + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("Santa Clara County Daily Change in Cases") + theme_minimal()

DiffSF <- ggplot(data = SF, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1))  + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("San Francisco County Daily Change in Cases")  + theme_minimal()

DiffSM <- ggplot(data = SM, aes(x = date, y = cases_diff)) + 
  geom_col(stat = 'identity', fill = 'black', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-04")), linetype = 1,
             color = 'palevioletred2', size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")), linetype = 1,
             color = 'blueviolet', size = 1) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  coord_cartesian(ylim = c(0, 1)) + 
  xlab("Dates") + ylab('Change in Cases by Day') + 
  ggtitle("San Mateo County Daily Change in Cases")  + theme_minimal()

grid.arrange(DiffAL, DiffCC, DiffMA, DiffSM, DiffSF, DiffSC, ncol = 2)  
  

#####################################################################
#COVID Data for Udacity Course - Lesson 8
#New cases per day, facet by state
ggplot(USStates, aes(date, cases)) + geom_line(size = 0.5) +
  facet_wrap(~state)
#New cases per day by county, facet by state
ggplot(USCounties, aes(date, cases, color = county)) + geom_point(size = 0.1) + 
  facet_wrap(~state) + theme_minimal() +
  theme(legend.position = 'none')

#New cases per day by county, facet by state (CA, NY, FL, AZ, IL, MT)  
SixStates <- subset(USCounties, state == 'California' | 
                    state == 'New York' | state == 'Florida' | 
                    state == 'Arizona' | state == 'Illinois' |
                    state == 'Montana')  
ggplot(SixStates, aes(date, cases, color = county)) + geom_line(size = 1) + 
  facet_wrap(~state) + theme_minimal() +
  theme(legend.position = 'none')

ggplot(SixStates, aes(date, cases, color = county)) + geom_line(size = 1) + 
  facet_wrap(~state) + 
  coord_cartesian(ylim = c(0, 50000)) +
  theme_minimal() +
  theme(legend.position = 'none')  

summary(SixStates, state$California)
  
