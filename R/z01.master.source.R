




###################################################
# all required packages

library(ggplot2)
library(reshape2)
library(ISOweek)
library(pander)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(scales)


###################################################


setwd(paste0(wd, "/Data"))

data = read.csv(data.file.name)


setwd(paste0(wd, "/R"))


# load functions 

source("z02.load.functions.R")

# clean the data 

source("z03.clean.data.R")

# produce time periods

source("z04.time.periods.R")

# produce bullet points 

source("z05.bullet.points.R")

# set generic ggplot theme

theme = theme(title = element_text(size = 16, colour = "black", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, size = 16,
                                         colour = "black"),
              axis.text.y = element_text(hjust = 1, size = 16,
                                         colour = "black"),
              legend.text= element_text(hjust = 1, size = 16,
                                        colour = "black", face="bold"),
              axis.title = element_text(size=16, face="bold"),
              strip.text.y = element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
              legend.position="bottom")


# source scripts using grep

for(i in list.files()[grep("^zz", list.files())]){
  source(i)
}




################################################################################
# DATES 

# define recent date range 

recent.date.range = paste(format(min(as.Date(hold.time.period[hold.time.period$year.period == 0, "day"])), "%d/%m/%Y"),
                          "to", 
                          format(max(as.Date(hold.time.period[hold.time.period$year.period == 0, "day"])), "%d/%m/%Y"))

# define recent.vs.histroic date range 

recent.vs.historic.date.range = paste(recent.vs.historic.x.axis.day[1], "to",
                                      recent.vs.historic.x.axis.day[length(recent.vs.historic.x.axis.day)])


# define histroic date range 

historic.date.range = paste(paste0("01/01/", historic.year), "to", format(end.reporting.date, "%d/%m/%Y"))


# define entire range of dates covered within report

entire.date.range = paste(format(range(as.Date(hold.time.period$day)), "%d/%m/%Y"), collapse = " to ")





