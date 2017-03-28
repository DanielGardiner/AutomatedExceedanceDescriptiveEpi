

x = data[!is.na(data$year.period), ]


age.sex.pyrimid.recent.week = age.sex.pyramid(x, 
                                              age.grp.col = "age.grp", 
                                              sex.col = "sex2",
                                              split.by = "week.year.recent.period2") 

age.sex.pyrimid.recent.week.missing = sum(x$sex2 == "Unknown")
