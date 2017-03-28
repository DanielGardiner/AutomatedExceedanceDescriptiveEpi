


factors.df = data.frame(name1 = c("all", "sex", "age.grp", "la",  "lab", "request", "spectype","travel"),
                        name2 = c("","Sex", "Age group", "", "", "",  "", "Travel"),
                        factor = c("dummy","sex2", "age.grp", "local_authority_name",
                                   "lab_name", "requesting_organisation_type_description", 
                                   "specimen_type_description", "travel_abroad_indicator"),
                        colour = c(0, 1, 2, 7, 7, 7, 1, 6))

x = data[!is.na(data$year.period), ]


for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ] 
  
  plot.name = paste0("line.graph.", as.character(i$name1), ".recent.vs.historic")
  
  # varying filter
  
  split.by = i$factor
  
  # fixed filters
  
  colour.by = "year.period.label2"
  linetype.by = NULL
  xlab = "Specimen date"
  ylab = "Count"
  angle = 90 
  label.breaks = week.label.break
  epi.squares = FALSE
  na.rm = TRUE
  
  x$x.axis = x$day2
  
  ## CREATE TABLE OF DATA FOR PLOTTING ##
  
  temp = paste("~ x.axis", colour.by, linetype.by, split.by, sep = "+")
  
  temp = gsub("[+][+]", "+", temp)
  
  temp = gsub("[+][+]", "+", temp)
  
  temp = gsub("[+]$", "", temp)
  
  temp
  
  table.to.plot = as.data.frame(xtabs(formula(temp), x))
  
  # create recent table.to.plot and historic.table.to.plot
  
  table.to.plot$year.period.label2 = as.character(table.to.plot$year.period.label2)
  
  recent.table.to.plot = table.to.plot[table.to.plot$year.period.label2 == "Recent", ]
  
  historic.table.to.plot = table.to.plot[!(table.to.plot$year.period.label2 == "Recent"), ]
  
  
  ## GENERATE THE PLOT ##
  
  p =  ggplot(table.to.plot) 
  
  # recent year
  
  p = p + geom_point(data = recent.table.to.plot,
                     aes_string(x="x.axis", y = "Freq", 
                                group = colour.by, colour = colour.by), 
                     size = 3.5, colour = "black")
  
  p = p + geom_line(data = recent.table.to.plot,
                    aes_string(x="x.axis", y = "Freq",
                               group = colour.by, colour = colour.by), 
                    size = 1.5, colour = "black")
  
  # historic year 
  
  p = p + geom_point(data = historic.table.to.plot,
                     aes_string(x="x.axis", y = "Freq", 
                                group = colour.by, colour = colour.by), size = 3, 
                     alpha = 0.4)
  
  p = p + geom_line(data = historic.table.to.plot,
                    aes_string(x="x.axis", y = "Freq",
                               group = colour.by, colour = colour.by), size = 1,
                    alpha = 0.4)
  
 
  p = p + labs(x = xlab, y = ylab)
  
  p = p + scale_x_discrete(breaks = levels(x$x.axis)[c(T, rep(F, label.breaks))],
                           drop=FALSE)
  
  time.period.for.colour.brew = unique(c(historic.table.to.plot$year.period.label2, recent.table.to.plot$year.period.label2))
  
  p = p + scale_colour_manual(name = "Year",
                              drop = FALSE,
                              limits = rev(time.period.for.colour.brew),
                              values = c("black", 2:length(time.period.for.colour.brew)))
  
  p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                axis.text.x = element_text(angle = 90, hjust = 1, size = 11,
                                           colour = "black", vjust = .5),
                axis.text.y = element_text(hjust = 1, size = 16,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom")
  

  if(!is.null(split.by) & i$factor != "dummy") p = p + facet_grid(paste(split.by, ".", sep = "~"), scale = "free",
                                                                  drop = FALSE)
  
  
  #print(p)
  
  assign(plot.name, p)

}

# line.graph.all.recent.vs.historic
# line.graph.sex.recent.vs.historic
# line.graph.age.grp.recent.vs.historic
# line.graph.request.recent.vs.historic
# line.graph.spectype.recent.vs.historic
# line.graph.travel.recent.vs.historic



