


x = data[data$year.period == 0 & !is.na(data$year.period), ]

age.sex.pyrimid.recent = age.sex.pyramid(x, 
                                         age.grp.col = "age.grp", 
                                         sex.col = "sex2") 



age.sex.pyrimid.recent.missing = sum(x$sex2 == "Unknown")


