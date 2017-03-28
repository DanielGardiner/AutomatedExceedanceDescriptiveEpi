


factors.df = data.frame(name1 = c("all", "sex", "age.grp", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 1, 6))

x = data[!is.na(data$year.period.label2), ]

for(i in 1:nrow(factors.df)){
  i = factors.df[i, ] 
  
  plot.name = paste0("epicurve.", as.character(i$name1), ".recent.vs.historic")
  
  p = epicurve(x, date.col = "day2", time.period = "use.date.col.as.is", 
               fill.by = as.character(i$factor), split.by = "year.period.label2", shade.by = NULL,
               start.at = start.reporting.date, stop.at = end.reporting.date,
               xlab="Specimen date", ylab="Count", angle=90, 
               fill.by.legend.title = as.character(i$name2), shade.by.legend.title = NULL,
               col.pal=i$colour, label.breaks = day.label.break, epi.squares = FALSE, na.rm = TRUE)  
  
  
  p = p + scale_fill_brewer(name = as.character(i$name2), type = "qual", palette = i$colour, drop = FALSE)
  
  if(i$factor == "dummy") p = p + scale_fill_manual(values = "light blue", guide = FALSE)
  
  #print(p)
  
  assign(plot.name, p)
}

epicurve.all.recent.vs.historic
epicurve.sex.recent.vs.historic
epicurve.age.grp.recent.vs.historic
epicurve.request.recent.vs.historic
epicurve.spectype.recent.vs.historic






