

x = data[!is.na(data$year.period), ]


age.sex.pyrimid.recent.vs.historic.year = age.sex.pyramid(x, 
                                                          age.grp.col = "age.grp", 
                                                          sex.col = "sex2",
                                                          split.by = "year.period.label2") 

#print(age.sex.pyrimid.recent.vs.historic.year)