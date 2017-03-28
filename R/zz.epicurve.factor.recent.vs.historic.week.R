

factors.df = data.frame(name1 = c("all", "sex", "age.grp", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 1, 6))

x = data[!is.na(data$year.period), ]


for(i in 1:nrow(factors.df)){
  i = factors.df[i, ] 
  
  plot.name = paste0("epicurve.", as.character(i$name1), ".recent.vs.historic.week")
  
  p = epicurve(x, date.col = "week.period.label2",  time.period = "use.date.col.as.is",
               fill.by = as.character(i$factor), split.by="year.period.label2", shade.by=NULL, start.at = start.reporting.date,
               stop.at = end.reporting.date, xlab="Specimen date week period",
               ylab="Count", angle=90, 
               fill.by.legend.title = as.character(i$name2), shade.by.legend.title = NULL,
               col.pal=i$colour, label.breaks = week.label.break, 
               epi.squares = FALSE, na.rm = TRUE)  
  
  if(i$factor == "dummy") p = p + scale_fill_manual(values = "light blue", guide = FALSE)
  
  #print(p)
  
  assign(plot.name, p)
}



# epicurve.all.recent.vs.historic.week
# epicurve.sex.recent.vs.historic.week
# epicurve.age.grp.recent.vs.historic.week
# epicurve.request.recent.vs.historic.week
# epicurve.spectype.recent.vs.historic.week



