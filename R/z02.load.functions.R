

################################################################################
# get.dates

get.dates = function(x){  
  if(class(x) == "Date"){
    NULL
  } else{
    stop("x is not a date")
  } 
  library(ISOweek)
  df = data.frame(day = as.character(x),
                  year = format(x, "%Y"), 
                  month = format(x, "%m"))
  df$year.month = paste0(df$year, df$month)
  df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
  df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
  df$iso.year.week = gsub("-W", "", ISOweek(x))
  df$quarter = sprintf("%02d", ceiling(as.numeric(as.character(df$month))/3))
  df$year.quarter = paste0(df$year, df$quarter)
  df  
}

################################################################################
# epicurve.v.1.5



epicurve = function(x, date.col, time.period, fill.by=NULL, split.by=NULL, shade.by=NULL,
                    start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence", 
                    fill.by.legend.title = NULL, shade.by.legend.title = NULL,
                    angle=45, col.pal=1, label.breaks =0, epi.squares = TRUE, na.rm = FALSE) {
  
  library(ggplot2)
  library(ISOweek)
  library(scales)
  
  ## HANDLE ARGUMENTS ##
  
  if(!is.null(fill.by) && is.numeric(fill.by)) fill.by = names(x)[fill.by]
  
  if(!is.null(split.by) && is.numeric(split.by)) split.by = names(x)[split.by]
  
  if(!is.null(shade.by) && is.numeric(shade.by)) shade.by = names(x)[shade.by]
  
  if(!is.null(col.pal) && col.pal==0) col.pal = NULL # 0 will be the default palette
  
  if(!is.null(col.pal) && (col.pal<0 || col.pal>8)) {
    
    col.pal = NULL
    
    warning("col.pal must be an integer from 1 to 8 - setting col.pal=NULL")
  }
  
  if(!(time.period %in% c("day", "year", "month", "quarter", "year.month", "year.quarter", "iso.year", "iso.week", "iso.year.week", "use.date.col.as.is"))){
    stop("time.period must be either: day, year, quarter, month, year.quarter, year.month, iso.year, iso.week, iso.year.week, use.date.col.as.is")
  }
  
  
  ## USE date.dol AS IS FOR X-AXIS IF time.period = use.date.col.as.is ##
  
  if(time.period == "use.date.col.as.is"){
    
    x$date.col.temp = x[, date.col]
    
  } else { 
    
    ## LOAD get.dates FUNCTION ##
    
    get.dates = function(x){  
      
      if(class(x) == "Date"){
        
        NULL
        
      } else {
        
        stop("x is not a date")
      }
      
      df = data.frame(day = as.character(x),
                      year = format(x, "%Y"), 
                      month = format(x, "%m"))
      
      df$year.month = paste0(df$year, df$month)
      
      df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
      
      df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
      
      df$iso.year.week = gsub("-W", "", ISOweek(x))
      
      df$quarter = sprintf("%02d", ceiling(as.numeric(as.character(df$month))/3))
      
      df$year.quarter = paste0(df$year, df$quarter)
      
      df  
    }
    
    ## APPLY get.dates TO DATA FRAME ##
    
    x = data.frame(x, get.dates(x[, date.col]))
    
    
    ## CREATE NEW FACTOR COLUMN FOR X-AXIS ##
    
    start.at = as.Date(start.at) 
    
    stop.at = as.Date(stop.at)  
    
    all.dates = get.dates(seq(start.at, stop.at, 1))
    
    all.dates = unique(all.dates[, time.period]) 
    
    x$date.col.temp = factor(x[, time.period],
                             levels = all.dates)  
    
    
    ## CODE DATES OUTSIDE OF START/STOP PERIOD AS MISSING ##
    
    x[!(as.character(x[, date.col]) %in% 
          as.character(get.dates(seq(start.at, stop.at, 1))$day)), "date.col.temp"] = NA
    
    ## REMOVE MISSING DATES ##
    
    cat(paste(sum(is.na(x$date.col.temp)), "rows have missing dates OR dates outside of the start/stop period"))
    
    if(na.rm) x = x[!is.na(x$date.col.temp), ]
    
    
    ## ORDER DATE COLUMN LEVELS ##
    
    x$date.col.temp = factor(x$date.col.temp,
                             levels = sort(levels(x$date.col.temp)))
    
  }
  
  
  
  ## ORDER DATA FOR PLOTTING ##
  
  if(!is.null(fill.by) & !is.null(shade.by)){
    
    x = x[order(x[, fill.by], x[, shade.by]), ]
    
  } else if(!is.null(fill.by)){
    
    x = x[order(x[, fill.by]), ]
    
  } else if(!is.null(shade.by)){
    
    x = x[order(x[, shade.by]), ]
    
  } else{
    
    NULL
    
  }
  
  # create blocks column
  
  x$blocks = 1:nrow(x)
  
  ## GENERATE THE PLOT ##
  
  p = ggplot(x) 
  
  if(epi.squares){
    
    p = p + geom_bar(aes_string(x="date.col.temp", fill=fill.by, alpha=shade.by, group = "blocks"),
                     colour = "black")
    
  } else {
    
    p = p + geom_bar(aes_string(x="date.col.temp", fill=fill.by, alpha=shade.by),
                     colour = "black")
    
  } 
  
  p = p + labs(x = xlab, y = ylab)
  
  p = p + scale_x_discrete(breaks = levels(x$date.col.temp)[c(T, rep(F, label.breaks))],
                           drop=FALSE)
  
  p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1)*1.1)))))
  
  p = p + theme(title = element_text(size = 11, colour = "black", face="bold"),
                axis.text.x = element_text(angle = angle, vjust = .5, size = 10,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 9,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 11,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom")
  
  p = p + labs(fill = fill.by.legend.title,
               alpha = shade.by.legend.title)
  
  if(!is.null(split.by)) p = p + facet_grid(paste(split.by, ".", sep = "~"),
                                            drop = FALSE)
  
  if(!is.null(col.pal)) p = p + scale_fill_brewer(type = "qual", 
                                                  palette = col.pal, drop = FALSE)
  
  ## RETURN OUTPUT ##
  p
} 


################################################################################
# line.graph.v.1.7


line.graph = function(x, date.col, time.period,
                      colour.by=NULL, split.by=NULL, linetype.by=NULL,
                      start.at, stop.at,
                      xlab=NULL, ylab=NULL, angle=90, 
                      colour.by.legend.title =NULL,
                      linetype.by.legend.title = NULL,
                      col.pal=2, label.breaks = 0, cumulative = FALSE, 
                      future.values.to.NA = TRUE) {
  
  library(ggplot2)
  library(ISOweek)
  
  ## HANDLE ARGUMENTS ##
  
  if(!is.null(colour.by) && is.numeric(colour.by)) colour.by = names(x)[colour.by]
  
  if(!is.null(split.by) && is.numeric(split.by)) split.by = names(x)[split.by]
  
  if(!is.null(linetype.by) && is.numeric(linetype.by)) linetype.by = names(x)[linetype.by]
  
  if(!is.null(col.pal) && col.pal==0) col.pal = NULL # 0 will be the default palette
  
  if(!is.null(col.pal) && (col.pal<0 || col.pal>8)) {
    
    col.pal = NULL
    
    warning("col.pal must be an integer from 1 to 8 - setting col.pal=NULL")
  }
  
  if(!(time.period %in% c("day", "year", "month", "quarter", "year.month", "year.quarter", "iso.year", "iso.week", "iso.year.week", "use.date.col.as.is"))){
    stop("time.period must be either: day, year, quarter, month, year.quarter, year.month, iso.year, iso.week, iso.year.week, use.date.col.as.is")
  }
  
  
  
  ## USE date.col AS IS FOR X-AXIS IF time.period = use.date.col.as.is ##
  
  if(time.period == "use.date.col.as.is"){
    
    x$date.col.temp = x[, date.col]
    
  } else { 
    
    ## LOAD get.dates FUNCTION ##
    
    get.dates = function(x){  
      if(class(x) == "Date"){
        
        NULL
        
      } else {
        
        stop("x is not a date")
      } 
      
      df = data.frame(day = as.character(x),
                      year = format(x, "%Y"), 
                      month = format(x, "%m"))
      
      df$year.month = paste0(df$year, df$month)
      
      df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
      
      df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
      
      df$iso.year.week = gsub("-W", "", ISOweek(x))
      
      df$quarter = sprintf("%02d", ceiling(as.numeric(as.character(df$month))/3))
      
      df$year.quarter = paste0(df$year, df$quarter)
      
      df  
    }
    
    ## APPLY get.dates TO DATA FRAME ##
    
    x = data.frame(x, get.dates(x[, date.col]))
    
    ## CREATE NEW FACTOR COLUMN FOR X-AXIS ##
    
    start.at = as.Date(start.at) 
    
    stop.at = as.Date(stop.at)  
    
    all.dates = get.dates(seq(start.at, stop.at, 1))
    
    all.dates = unique(all.dates[, time.period]) 
    
    x$date.col.temp = factor(x[, time.period],
                             levels = all.dates)  
    
    ## REMOVE MISSING DATES ##
    
    cat(paste(sum(is.na(x$date.col.temp)), "rows have missing dates OR dates outside of the start/stop period"))
    
    x = x[!is.na(x$date.col.temp), ]
    
  }
  
  ##  GENERATE VARIABLE INDICATING IF linetype.by = split.by ##
  
  if(!is.null(linetype.by) & !is.null(split.by)){
    
    update.split.by = (linetype.by == split.by)
    
  } else {
    
    update.split.by = FALSE
  }
  
  
  
  ##  SET LINETYPE LEGEND TITLE ##
  
  if(!is.null(linetype.by) & !is.null(linetype.by.legend.title)){
    
    linetype.by.legend.title = gsub("[^[:alnum:] | _]", "", gsub(" ", "_", linetype.by.legend.title))
    
    linetype.by.legend.title = sub("^+_", "", linetype.by.legend.title )
    
    linetype.by.legend.title = sub("_+$", "", linetype.by.legend.title )
    
    colnames(x)[colnames(x) == linetype.by] = linetype.by.legend.title
    
    linetype.by = linetype.by.legend.title
  } else {
    
    NULL
  }
  
  ##  IF linetype.by = split.by update split.by ##
  
  if(update.split.by) split.by = linetype.by
  
  ## CREATE TABLE OF DATA FOR PLOTTING ##
  
  
  temp = paste("~ date.col.temp", colour.by, linetype.by, split.by, sep = "+")
  
  temp = gsub("[+][+]", "+", temp)
  
  temp = gsub("[+][+]", "+", temp)
  
  temp = gsub("[+]$", "", temp)
  
  temp
  
  table.to.plot = as.data.frame(xtabs(formula(temp), x))
  
  ##  CONVERT TO CUMULATIVE ##
  
  table.to.plot$dummy = 1
  
  table.to.plot$temp = apply(data.frame(table.to.plot[, c(colour.by, linetype.by, split.by, "dummy")]), 1, 
                             function(x) paste0(x, collapse = "."))
  
  if(cumulative){
    y = NULL
    
    for(i in unique(table.to.plot$temp)){
      temp = table.to.plot[table.to.plot$temp == i, ]
      temp$Freq = cumsum(temp$Freq)
      y = rbind(y, temp)
    }
    
    table.to.plot = y
  } else {
    NULL
  }
  
  ##  SET FUTURE VALUES TO NA ##
  
  if(future.values.to.NA & time.period != "use.date.col.as.is"){
    # identify time 
    future = as.numeric(gsub("-", "", as.character(table.to.plot$date.col.temp))) > as.numeric(gsub("-", "", as.character(get.dates(Sys.Date())[, time.period])))
    # set the count and cumulative count for future months to NA (rather than 0)
    table.to.plot[future, "Freq"] = NA
  } else {
    NULL
  }
  
  
  ## SET PRETTY Y-AXIS BREAKS AND LIMITS ##
  
  y.axis.breaks = unique(floor(pretty(seq(0, (max(table.to.plot$Freq, na.rm = TRUE) + 1)*1.1))))
  
  y.axis.limits = range(unique(floor(pretty(seq(0, (max(table.to.plot$Freq, na.rm = TRUE) + 1)*1.1)))))
  
  
  
  ## GENERATE THE PLOT ##
  
  p =  ggplot(table.to.plot, aes_string(x="date.col.temp", y = "Freq")) 
  
  if(!is.null(colour.by) & !is.null(linetype.by)){
    
    p = p + geom_point(aes_string(x="date.col.temp", y = "Freq", 
                                  group = paste0("interaction(", linetype.by, ",", colour.by, ")"),
                                  colour = colour.by, shape = linetype.by), size = 3)
    
    p = p + geom_line(aes_string(x="date.col.temp", y = "Freq", 
                                 group = paste0("interaction(", linetype.by, ",", colour.by, ")"),
                                 colour = colour.by, linetype = linetype.by), size = 1)
  } else if(!is.null(colour.by)){
    p = p + geom_point(aes_string(x="date.col.temp", y = "Freq", 
                                  group = colour.by, colour = colour.by), size = 3)
    
    p = p + geom_line(aes_string(x="date.col.temp", y = "Freq",
                                 group = colour.by, colour = colour.by), size = 1)
  } else if(!is.null(linetype.by)){
    p = p + geom_point(aes_string(x="date.col.temp", y = "Freq", 
                                  group = linetype.by, shape = linetype.by), size = 3)
    
    p = p + geom_line(aes_string(x="date.col.temp", y = "Freq",
                                 group = linetype.by, linetype = linetype.by), size = 1)
  } else {
    p = p + geom_point(aes_string(x="date.col.temp", y = "Freq", group = 1), size = 3)
    
    p = p + geom_line(aes_string(x="date.col.temp", y = "Freq", group = 1), size = 1)
  }
  
  
  p = p + scale_y_continuous(breaks = y.axis.breaks, limits = y.axis.limits)
  
  
  p = p + theme(title = element_text(size = 11, colour = "black", face="bold"),
                axis.text.x = element_text(angle = angle, vjust = .5, hjust = 1, size = 10,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 9,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 11,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom")
  
  p = p + scale_x_discrete(breaks = levels(x$date.col.temp)[c(T, rep(F, label.breaks))],
                           drop=FALSE)
  
  
  if(!is.null(split.by)) p = p + facet_grid(paste(split.by, ".", sep = "~"),
                                            drop = FALSE)
  
  if(!is.null(col.pal)) p = p + scale_colour_brewer(type = "qual", 
                                                    palette = col.pal, drop = FALSE)
  
  p = p + labs(x = xlab, y = ylab, colour = colour.by.legend.title)
  
  p
  
} 

################################################################################
# fix order of 29 Feb 

leap.year.fix = function(temp){
  if(sum(temp == "29 Feb") >= 1){
    temp = temp[temp != "29 Feb"] 
    if(temp[1] == "01 Mar"){
      temp = c("29 Feb", temp)
    } else if(temp[length(temp)] == "28 Feb"){
      temp = c(temp, "29 Feb")
    } else {
      temp = c(temp[1:which(temp == "28 Feb")], "29 Feb", temp[(which(temp == "28 Feb") + 1):length(temp)])
    }
  } else {
    NULL
  }
  temp
}


################################################################################
# tile plot

tile.plot = function(data, x.col, y.col, x.lab = "", y.lab = "", text = FALSE, 
                     colour = "red", label.breaks = 0, rescale.by.row = FALSE, keep.row.order = FALSE){
  
  # load packages
  
  library(ggplot2)
  library(reshape2)
  library(scales)
  library(dplyr)
  
  # assign x and y columns 
  
  data$y = data[, y.col]
  
  data$x = data[, x.col]
  
  
  # tabulate cases by laboratory by week
  
  temp = as.data.frame.array(xtabs(~ y + x, data,
                                   drop.unused.levels = FALSE))
  
  
  # order data 
  
  if(keep.row.order){
    NULL
  } else {
    temp = temp[order(apply(temp, 1, sum)), ]  
  }
  
  
  
  # add y column
  
  temp$y = row.names(temp)
  
  temp.factor = rev(unique(temp$y))
  
  # melt the dataset
  
  temp = melt(temp, id.vars = "y")
  
  temp$y = factor(temp$y,
                  levels = temp.factor)
  
  temp$rescale = temp$value
  
  if(rescale.by.row)  temp = temp %>% group_by(y) %>% mutate(rescale = rescale(value))
  
  
  # plot temp2
  
  p = ggplot(temp, aes(x = variable, y = y, size = rescale, fill = rescale))
  
  p = p + geom_tile(colour = "white", size = 1)
  
  if(text) p = p + geom_text(aes(label = value), vjust = 0.4, size = 4, colour = "black")
  
  p = p + ylab(y.lab)
  
  p = p + xlab(x.lab)
  
  p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 0, size = 9,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom",
                panel.border = element_rect(colour = "grey", fill = NA),
                panel.background = element_blank())
  
  p = p + scale_fill_continuous(low = "white", high = colour)
  
  p = p + scale_x_discrete(breaks = levels(temp$variable)[c(T, rep(F, label.breaks))],
                           drop=FALSE)
  
  p = p + theme(legend.position="none")
  
  
  p
  
}



################################################################################
# age.sex.pyramid v3 

age.sex.pyramid = function(data, age.grp.col, sex.col, 
                           lower.limit = NULL, upper.limit = NULL,
                           split.by = NULL,
                           col.pal = 1) {
  
  # load packages
  
  library(ggplot2)
  library(scales)
  
  
  # assign age.grp and sex columns within the function
  
  data$age.grp = data[, age.grp.col]
  
  data$sex = data[, sex.col]
  
  data$split.by = data[, split.by]
  
  if(is.null(split.by)) data$split.by = "dummy"
  
  # format sex column
  
  data$sex = as.character(data$sex)
  
  data$sex[grep("^M", toupper(data$sex))] = "Male"
  
  data$sex[grep("^F", toupper(data$sex))] = "Female"
  
  data = data[!is.na(data$sex) & data$sex %in% c("Male", "Female"), ]
  
  data$sex = factor(data$sex,
                    levels = c("Male", "Female"))
  
  
  # make table of age.grp vs sex
  
  table.to.plot = as.data.frame(xtabs(~ age.grp + sex + split.by,
                                      data))
  
  # create axis limits to ensure vertical symmetry (using additional 10%)
  
  if(is.null(lower.limit)) lower.limit = round(-max(table.to.plot$Freq)*1.1, 0)
  
  if(is.null(upper.limit)) upper.limit = round(max(table.to.plot$Freq)*1.1, 0)
  
  temp.limits = c(lower.limit, upper.limit)
  
  # create pretty breaks
  
  temp.breaks = pretty(temp.limits)
  
  # plot data
  
  p = ggplot(data = table.to.plot, 
             aes(x = age.grp, y = Freq, fill = sex))
  
  
  p = p + geom_bar(data = subset(table.to.plot, sex=="Female"),
                   stat = "identity", colour = "black")
  
  p = p + geom_bar(data = subset(table.to.plot, sex=="Male"),
                   stat = "identity",
                   position = "identity",
                   mapping = aes(y = -Freq), colour = "black")
  
  
  p = p + scale_y_continuous(labels = abs, limits = temp.limits, breaks = temp.breaks)
  
  p = p + coord_flip() 
  
  p = p + scale_fill_brewer("Sex", type = "qual", palette = col.pal)
  
  p = p + xlab("Age group")
  
  p = p + ylab("count")
  
  p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                axis.text.x = element_text(angle = 0, hjust = 1, size = 16,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 16,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom")
  
  if(!is.null(split.by)) p = p + facet_grid("split.by~.",
                                            drop = FALSE)
  
  p
  
}


################################################################################
# small function to convert first charachter to upper case 

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

