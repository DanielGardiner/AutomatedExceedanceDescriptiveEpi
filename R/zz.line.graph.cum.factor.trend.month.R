

factors.df = data.frame(name1 = c("all", "sex", "age.grp", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 1, 6))


temp.start = min(as.Date(hold.time.period$day))

x = data[as.Date(data$day) <= end.reporting.date & as.Date(data$day) >= temp.start, ]


x$year.month = factor(x$year.month,
                      levels = unique(get.dates(seq(temp.start, end.reporting.date, 1))$year.month))

i = 1
for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("line.graph.cum.", as.character(i$name1), ".trend.month")
  
  p = line.graph(x, date.col = "specimen_date",  time.period = "year.month",
                 colour.by = as.character(i$factor), split.by=NULL, linetype.by=NULL, 
                 start.at = temp.start,
                 stop.at = end.reporting.date, xlab="Specimen date month",
                 ylab="Count", angle=90, col.pal = i$colour, 
                 label.breaks = month.label.break,
                 colour.by.legend.title = as.character(i$name2),
                 cumulative = TRUE)
  
  if(i$factor == "dummy") p = p + scale_color_continuous(guide = FALSE)
  
  #print(p)
  
  assign(plot.name, p)
}

# line.graph.cum.all.trend.month
# line.graph.cum.sex.trend.month
# line.graph.cum.age.grp.trend.month
# line.graph.cum.request.trend.month
# line.graph.cum.spectype.trend.month

