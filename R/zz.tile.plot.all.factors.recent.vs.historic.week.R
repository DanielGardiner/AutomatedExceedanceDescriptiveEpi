

colour = "red"

factors.df = data.frame(name1 = c("all", "sex",  "age.grp", "travel"),
                        name2 = c("", "Sex", "Age group", "Travel status"),
                        factor = c("dummy2", "sex", "age.grp", "travel_abroad_indicator"))


temp.start = min(as.Date(hold.time.period$day))

x = data[as.Date(data$day) <= end.reporting.date & as.Date(data$day) >= temp.start, ]

x = x[!is.na(x$year.period.label2), ]

x$dummy2 = ""

i = 2

for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("tile.plot.all.", as.character(i$name1), ".recent.vs.historic.week")
  
  # create factor column
  
  x$factor.col = x[, as.character(i$factor)]
  
  
  # summarise number of cases for each factor, year period and week period
  # then add a column with rescaled values for producing colours 
  
  temp = x %>% group_by(factor.col, year.period.label2, week.period.label2) %>% 
    summarise(count = n()) %>%  
    complete(factor.col, year.period.label2, week.period.label2, 
             fill = list(count = 0)) %>% 
    ungroup() %>% 
    group_by(factor.col) %>% 
    mutate(rescale = rescale(count))
  
  
  # set tile.plot theme 
  
  
  tile.plot.theme = theme(title = element_text(size = 16, colour = "black", face="bold"),
                          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10,
                                                     colour = "black", face="bold"),
                          axis.text.y = element_text(hjust = 1, size = 10,
                                                     colour = "black", face="bold"),
                          legend.text= element_text(hjust = 1, size = 16,
                                                    colour = "black", face="bold"),
                          axis.title = element_text(size=16, face="bold"),
                          strip.text.y = element_text(hjust = 1, size = 16,
                                                      colour = "black", face="bold"),
                          legend.position="none",
                          panel.border = element_rect(colour = "grey", fill = NA),
                          panel.background = element_blank(),
                          strip.text.x = element_text(size = 13, colour = "black", face="bold"))
  
  # create plot 
  
  p = ggplot(temp, aes(x = week.period.label2, y = year.period.label2, fill = rescale)) +
    geom_tile() + 
    geom_text(aes(label = count), vjust = 0.4, size = 4, colour = "black") +
    facet_wrap(~ factor.col, ncol = 1) +
    scale_fill_continuous(low = "white", high = colour) +
    geom_hline(yintercept = c(#length(levels(x$year.period.label2)) + 0.5,
                              length(levels(x$year.period.label2)) - 0.5), size = 1.5,
               colour = "black") +
    scale_x_discrete(breaks = levels(temp$week.period.label2)[c(T, rep(F, week.label.break))],
                     drop=FALSE) +
    tile.plot.theme +
    xlab("Specimen date week period") +
    ylab("Year period")
  
  assign(plot.name, p)
}


tile.plot.all.all.recent.vs.historic.week
tile.plot.all.sex.recent.vs.historic.week
tile.plot.all.age.grp.recent.vs.historic.week
tile.plot.all.travel.recent.vs.historic.week









