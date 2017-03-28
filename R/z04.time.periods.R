





#-------------------------------------------------------------------------------
# generate start.reporting.date based on recent.historic.weeks

end.reporting.date = as.Date(end.reporting.date)

temp1 = get.dates(seq(end.reporting.date - 250, end.reporting.date, 1))

temp2 = rev(unique(get.dates(seq(end.reporting.date - 250, end.reporting.date, 1))$iso.year.week))

temp2 = temp2[recent.historic.weeks + 1]

start.reporting.date = as.Date(as.character(temp1[temp1$iso.year.week == temp2, "day"][1]))

#-------------------------------------------------------------------------------
# create a data frame containing all dates 2500 days prior to the end.reporting date 
# and add useful columns to this data frame

# generate dates starting at the end.reporting.date extending back 2500 days

time.period = get.dates(seq(end.reporting.date - 2500, end.reporting.date, 1))

# make a day.month column

time.period$month.day = format(as.Date(time.period$day), "%m%d")

# make a day.month.year column

time.period$day.month.year = paste(format(as.Date(time.period$day), "%d"), 
                                   format(as.Date(time.period$day), "%b"),
                                   format(as.Date(time.period$day), "%Y"))

#-------------------------------------------------------------------------------
# subset this data frame so that it containes only dates of interest based on 
# end.reporting.date, recent.historic.weeks and historic.year supplied by
# the user

# make a vector containing the month-day.of.month of interest based on
# start.reporting.date and end.reporting.date


month.day.of.interest = format(seq(start.reporting.date, end.reporting.date, 1), "%m%d")


# add 29 Feb to day.month to account for possibility of a leap year 

if(sum(month.day.of.interest %in% c("0228", "0301")) == 2){
  month.day.of.interest = c(month.day.of.interest, "0229")
} else {
  NULL
}


# subset the time.period data frame, keeping only months of interest 

time.period = time.period[time.period$month.day %in% month.day.of.interest, ]

# keep only rows with date later than histoic.year - 1

time.period = time.period[as.numeric(as.character(time.period$year)) >= (historic.year - 1), ]


# if there is a big jump in days this means there has been a break in time period of interest
# remove out the early portion of the year before the historic.year, but keep the later half

temp.cut = which(diff(as.Date(time.period$day)) >= 10)[1]

if(is.na(temp.cut)){
  NULL
} else {
  time.period = time.period[(temp.cut + 1):nrow(time.period), ] 
}

#-------------------------------------------------------------------------------
# create year.period column
# this column identifies equal time periods for comparing recent vs historic cases
# it is done in this way to allow for the time period to break over a year and 
# still give correct numbers of cases
# note: most recent time period is 0

time.period$year.period = cumsum(c(1, diff(as.Date(time.period$day))) >= 10)

time.period$year.period = max(time.period$year.period) - time.period$year.period  


# create year.period.label column (either NA, recent or historic)

time.period$year.period.label = NA

time.period[time.period$year.period == 0, "year.period.label"] = "Recent" 

time.period[time.period$year.period >= 1, "year.period.label"] = "Historic" 


# create alternative year.period.label column (NA, recent, recent -1yr, recent -2yr, etc...)

time.period$year.period.label2 = time.period$year.period.label

temp.logic = time.period$year.period.label2 == "Historic"

temp.label =  paste0("Recent -", time.period[temp.logic, "year.period"], "yr")

time.period[temp.logic, "year.period.label2"] = temp.label

#-------------------------------------------------------------------------------
# add a column stating the range of dates covered within each 'week' in format 
# year-month-day for recent period only

# make data frame listing dates between start.reporting.date and end.reporting.date

temp = data.frame(day = seq(start.reporting.date, end.reporting.date, 1))

# add a column with day of week

temp$week.day = weekdays(temp$day)

# add column containing unique identifier for each week, using monday as the
# starting point for each

temp$week.period = cumsum(temp$week.day == "Monday")

# add colun containing day-month (as a numeric)

temp$month.day = format(temp$day, "%m%d")


for(i in unique(temp$week.period)){
  x = format(temp[temp$week.period == i, "day"], "%d %b %Y")
  r = temp[temp$week.period == i, "day"]
  r = format(r, "%d %b %Y")
  r = paste(c(r[1], r[length(r)]), collapse = " to ")  
  time.period[time.period$day.month.year %in% x, "week.year.recent.period"] = r
}

time.period$week.year.recent.period2 = gsub(" to ", "\nto\n", time.period$week.year.recent.period)

# make object to hold time.period to avoid overwriting in subsequent scripts

hold.time.period = time.period


#-------------------------------------------------------------------------------
# merge time.period and time.period.label onto dataset


data = merge(data, time.period[ , c("day", "year.period", "year.period.label", "year.period.label2", "week.year.recent.period", "week.year.recent.period2")], by = "day", all.x = TRUE)





#-------------------------------------------------------------------------------
# convert newly merged columns into factors for when plotting 

# convert year.period, year.period.label and year.period.label2 into factors

data$year.period = factor(data$year.period,
                          levels = unique(time.period$year.period))


data$year.period.label = factor(data$year.period.label,
                                levels = unique(time.period$year.period.label))


data$year.period.label2 = factor(data$year.period.label2,
                                 levels = unique(time.period$year.period.label2))

data$week.year.recent.period = factor(data$week.year.recent.period,
                                      levels = unique(time.period$week.year.recent.period))

data$week.year.recent.period2 = factor(data$week.year.recent.period2,
                                       levels = unique(time.period$week.year.recent.period2))


#-------------------------------------------------------------------------------
# create week.period column within data

# make data frame listing dates between start.reporting.date and end.reporting.date

temp = data.frame(day = seq(start.reporting.date, end.reporting.date, 1))

# add a column with day of week

temp$week.day = weekdays(temp$day)

# add column containing unique identifier for each week, using monday as the
# starting point for each

temp$week.period = cumsum(temp$week.day == "Monday")

# add colun containing day-month (as a numeric)

temp$month.day = format(temp$day, "%m%d")


# for each unique week extract out the first and last day.month
# and the numeric value for that week.period

a = NULL
b = NULL
for(i in unique(temp$week.period)){
  x = format((temp[temp$week.period == i, "day"] ), "%m%d")
  
  r1 = unique(temp[temp$month.day %in% x, "month.day"])
  r1 =c(r1[1], r1[length(r1)])
  a = rbind(a, r1)

  r2 = unique(temp[temp$month.day %in% x, "week.period"])
  b = rbind(b, r2)
}

temp = data.frame(week.period.lower = as.vector(a[, 1]),
                  week.period.higher = as.vector(a[, 2]),
                  week.period = b[, 1])

# if a leap day exists in time.period (i.e. there is a leap day in 
# the time period being reported) adjust the lower/upper week period 
# bounding each week
# if either the lower or upper week period bound contains 2902, do thing
# if the lower week period bound is 0103, convert to 2902
# if the upper week perod bound is 2802, convert to 2902

if(sum(time.period$day.month == "29 Feb") >= 1){
  if(sum(temp$week.period.lower == "0229" | temp$week.period.higher == "0229") >= 1){
    NULL
  } else if(sum(temp$week.period.lower == "0301") >= 1){
    temp$week.period.lower[temp$week.period.lower == "0301"] = "0229"
  } else if(sum(temp$week.period.higher == "0228") >= 1){
    temp$week.period.lower[temp$week.period.higher == "0228"] = "0229"
  } else {
    NULL
  }
  NULL
}

# make wek.period.label

temp1 = paste(substr(temp$week.period.lower, 3, 4),
              month.abb[as.numeric(substr(temp$week.period.lower, 1, 2))])


temp2 = paste(substr(temp$week.period.higher, 3, 4),
              month.abb[as.numeric(substr(temp$week.period.higher, 1, 2))])


temp$week.period.label = paste(temp1, temp2, sep = " to ")

temp$week.period.label2 = paste(temp1, temp2, sep = "\nto\n")


# make new data frame, 
# temp1 contains week.periods where the week.period.lower and week.period.higher break over a year
# temp2 contains week.periods where the week.period.lower and week.period.higher do not break over a year

temp1 = temp[!(as.numeric(as.character(temp$week.period.higher)) - 
                 as.numeric(as.character(temp$week.period.lower)) <= -20), ]

temp2 = temp[as.numeric(as.character(temp$week.period.higher)) - 
               as.numeric(as.character(temp$week.period.lower)) <= -20, ]


# assign week.period.label to data where there is no break over a year

data$week.period.label = NA

for(i in 1:length(temp1$week.period.lower)){
  temp.logic = as.numeric(data$month.day) >= as.numeric(as.character(temp1$week.period.lower)[i]) & 
               as.numeric(data$month.day) <= as.numeric(as.character(temp1$week.period.higher)[i])
  
  if(sum(temp.logic) >= 1){
    data$week.period.label[temp.logic] = temp1[i, "week.period.label"]
  } else {
    NULL
  }
}

# assign week.period.label to data where there is a break over a year

if(nrow(temp2) == 0){
  NULL
} else {
  for(i in 1:length(temp2$week.period.lower)){
    temp.logic = as.numeric(data$month.day) >= as.numeric(as.character(temp2$week.period.lower)[i]) | 
      as.numeric(data$month.day) <= as.numeric(as.character(temp2$week.period.higher)[i])
    
    if(sum(temp.logic) >= 1){
      data$week.period.label[temp.logic] = temp2[i, "week.period.label"]
    } else {
      NULL
    }
  }
}


# add week.period.label2 for alternative formatting 

data$week.period.label2 = gsub(" to ", "\nto\n", data$week.period.label)

# conver to factor 

data$week.period.label = factor(data$week.period.label,
                                levels = temp$week.period.label)

data$week.period.label2 = factor(data$week.period.label2,
                                 levels = temp$week.period.label2)




#-----------
# create x-axis factor labels for comparing recent vs historic data 

## HANDLE LEAP YEAR

# make a vector containing day.month of interest over whole time period 
# of interest

temp = unique(format(as.Date(time.period$day), "%d %b"))

# if Feb 29 is included (i.e. a leap year is included), remove Feb 29 and 
# add back in the correct position, either before 'Mar 01' or after 'Feb 28', 
# this is because if, for example, years of interest are: 2014, 2015, 2016, 2017
# as 2016 contains a leap year, the position of the leap day in the temp vector
# will be in the wrong place as there is no leap days in 2014 or 2015

temp = leap.year.fix(temp)


recent.vs.historic.x.axis.day = temp


data$day2 = format(as.Date(data$day), "%d %b")

data$day2 = factor(data$day2,
                  levels = recent.vs.historic.x.axis.day)







