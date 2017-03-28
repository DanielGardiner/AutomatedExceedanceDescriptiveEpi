


factors.df = data.frame(name1 = c("all", "sex",  "age.grp", "request", "spectype", "local.authority", "laboratory", "travel"),
                        name2 = c("", "Sex", "Age group", "Requesting organisation", "Specimen type", "Local Authority", "Laboratory", "Travel"),
                        factor = c("dummy2", "sex", "age.grp", "requesting_organisation_type_description", 
                                   "specimen_type_description", "local_authority_name", "lab_name", "travel_abroad_indicator"))

x = data[!is.na(data$week.year.recent.period2), ]

x$dummy2 = ""


for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("tile.plot.all.", as.character(i$name1), ".recent.week")
  
  p = tile.plot(x, x.col = "week.year.recent.period2", y.col = as.character(i$factor), 
                x.lab = "Specimen week", y.lab = i$name2,
                text = recent.tile.plot.text, rescale.by.row = FALSE, keep.row.order = TRUE,
                label.breaks = week.label.break)  

  #print(p)
  
  assign(plot.name, p)
}
  
  
# tile.plot.all.all.recent.week
# tile.plot.all.sex.recent.week
# tile.plot.all.age.grp.recent.week
# tile.plot.all.request.recent.week
# tile.plot.all.spectype.recent.week
# tile.plot.all.laboratory.recent.week
# tile.plot.all.local.authority.recent.week


  