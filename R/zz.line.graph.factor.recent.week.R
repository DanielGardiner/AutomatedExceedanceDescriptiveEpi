


factors.df = data.frame(name1 = c("all", "sex", "age.grp", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 1, 6))


x = data[!is.na(data$week.year.recent.period2), ]

i = 1
for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("line.graph.", as.character(i$name1), ".recent.week")
  
  p = line.graph(x, date.col = "week.year.recent.period2",  time.period = "use.date.col.as.is",
                 colour.by = as.character(i$factor), split.by=NULL, linetype.by=NULL, 
                 start.at = start.reporting.date,
                 stop.at = end.reporting.date, xlab="Specimen date week period",
                 ylab="Count", angle=90, col.pal = i$colour, 
                 label.breaks = day.label.break,
                 colour.by.legend.title = as.character(i$name2))
  
  if(i$factor == "dummy") p = p + scale_color_continuous(guide = FALSE)
  
  #print(p)
  
  assign(plot.name, p)
}

# line.graph.all.recent.week
# line.graph.sex.recent.week
# line.graph.age.grp.recent.week
# line.graph.request.recent.week
# line.graph.spectype.recent.week

