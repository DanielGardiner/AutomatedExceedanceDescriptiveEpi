


factors.df = data.frame(name1 = c("all", "sex",  "age.grp", "request", "spectype", "local.authority", "laboratory"),
                        name2 = c("", "Sex", "Age group", "Requesting organisation", "Specimen type", "Local Authority", "Laboratory"),
                        factor = c("dummy2", "sex", "age.grp", "requesting_organisation_type_description", 
                                    "specimen_type_description", "local_authority_name", "lab_name"))

x = data[!is.na(data$week.year.recent.period2), ]

x$dummy2 = ""

for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("tile.plot.row.", as.character(i$name1), ".recent.week")
  
  p = tile.plot(x, x.col = "week.year.recent.period2", y.col = as.character(i$factor), 
                x.lab = "Specimen week", y.lab = i$name2,
                text = recent.tile.plot.text, rescale.by.row = TRUE, keep.row.order = TRUE,
                colour = "orange", label.breaks = week.label.break)  

  #print(p)
  
  assign(plot.name, p)
}
  

# tile.plot.row.all.recent.week
# tile.plot.row.sex.recent.week
# tile.plot.row.age.grp.recent.week
# tile.plot.row.request.recent.week
# tile.plot.row.spectype.recent.week
# tile.plot.row.laboratory.recent.week
# tile.plot.row.local.authority.recent.week


  