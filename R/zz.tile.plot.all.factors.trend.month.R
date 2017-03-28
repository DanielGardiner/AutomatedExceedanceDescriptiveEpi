


factors.df = data.frame(name1 = c("all", "sex", "age.grp", "request", "spectype", "local.authority", "laboratory", "travel"),
                        name2 = c("", "Sex", "Age group", "Requesting organisation", "Specimen type", "Local Authority", "Laboratory", "Travel status"),
                        factor = c("dummy2", "sex", "age.grp", "requesting_organisation_type_description", 
                                    "specimen_type_description", "local_authority_name", "lab_name", "travel_abroad_indicator"),
                        keep.row.order = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))

temp.start = min(as.Date(hold.time.period$day))

x = data[as.Date(data$day) <= end.reporting.date & as.Date(data$day) >= temp.start, ]


x$year.month = factor(x$year.month,
                      levels = unique(get.dates(seq(temp.start, end.reporting.date, 1))$year.month))

x$dummy2 = ""

for(i in 1:nrow(factors.df)){
  i = factors.df[i, ] 
  
  plot.name = paste0("tile.plot.all.", as.character(i$name1), ".trend.month")
  
  p = tile.plot(x, x.col = "year.month", y.col = as.character(i$factor), 
                x.lab = "Specimen month", y.lab = i$name2,
                text = historic.tile.plot.text, rescale.by.row = FALSE, keep.row.order = i$keep.row.order,
                label.breaks = month.label.break)  

  #print(p)
  
  assign(plot.name, p)
}
  
# tile.plot.all.all.trend.month
# tile.plot.all.sex.trend.month
# tile.plot.all.age.grp.trend.month
# tile.plot.all.request.trend.month
# tile.plot.all.spectype.trend.month
# tile.plot.all.laboratory.trend.month
# tile.plot.all.local.authority.trend.month


  