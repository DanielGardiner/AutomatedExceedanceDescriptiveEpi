


# want to present numbers of recent cases reported by la/lab by week

x = data[!is.na(data$week.year.recent.period2), ]

factors.df = data.frame(col.name = c("local_authority_name", "lab_name", 
                                     "requesting_organisation_type_des",
                                     "specimen_type_description"),
                        name = c("local.authority", "lab", "request", "spectype"))


for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ]
  
  temp.formula = as.formula(paste0("~", i$col.name, "+ week.year.recent.period2"))

  # tabulate cases by laboratory by week
  
  temp = as.data.frame.array(xtabs(temp.formula, x,
                            drop.unused.levels = FALSE))
  
  # add local authority column
  
  temp = cbind(data.frame(Local.Authority = row.names(temp)), temp)
  
  # reset row names
  
  row.names(temp) = 1:nrow(temp)

  # assign table name 
  
  assign(paste0("table.", i$name, ".recent.week"), temp)
  
}

 
# table.local.authority.recent.week
# table.lab.recent.week
# table.request.recent.week
# table.spectype.recent.week
