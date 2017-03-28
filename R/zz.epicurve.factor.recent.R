

factors.df = data.frame(name1 = c("all", "sex", "age.grp", "la",  "lab", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "",  "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "local_authority_name",
                                   "lab_name", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 7, 7, 1, 6))

x = data[!is.na(data$year.period), ]


for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("epicurve.", as.character(i$name1), ".recent")
  
  p = epicurve(x, date.col = "specimen_date",  time.period = "day",
               fill.by = as.character(i$factor), split.by=NULL, shade.by=NULL, start.at = start.reporting.date,
               stop.at = end.reporting.date, xlab="Specimen date",
               ylab="Count", angle=90, 
               fill.by.legend.title = as.character(i$name2), shade.by.legend.title = NULL,
               col.pal=i$colour, label.breaks = day.label.break, 
               epi.squares = FALSE, na.rm = TRUE)  
  
  
  if(i$factor == "dummy") p = p + scale_fill_manual(values = "light blue", guide = FALSE)
  
  #print(p)
  
  assign(plot.name, p)
  
}



epicurve.all.recent
epicurve.sex.recent
epicurve.age.grp.recent
epicurve.la.recent
epicurve.lab.recent
epicurve.request.recent
epicurve.spectype.recent



