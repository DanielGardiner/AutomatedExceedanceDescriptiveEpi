


factors.df = data.frame(name1 = c("la",  "lab", "request", "spectype"),
                        name2 = c("Local Authority", "Laboratory", "Requesting Organisation", "Specimen Type"),
                        factor = c("local_authority_name", "lab_name", "requesting_organisation_type_description", 
                                   "specimen_type_description"))

x = data[!is.na(data$year.period), ]

for(i in 1:nrow(factors.df)){
  
  i = factors.df[i, ]
  
  
  
  # want to present numbers of cases reported by each lab , comparing recent period with
  # historic periods
  
  
  # tabulate cases by LA for recent vs historic periods
  
  formula = as.formula(paste0("~", i$factor, " + year.period.label"))
  
  temp = as.data.frame.array(xtabs(formula, x, drop.unused.levels = FALSE))
  
  # add a column with average numbers of cases 
  # i.e. divide total number of historic cases by total number of time periods 
  
  temp$Historic.avg = temp$Historic/(length(unique(data$year.period[!is.na(data$year.period)])) - 1)
  
  # find difference between number of recent and average historic cases
  
  temp$recent.vs.hist.avg.diff = temp$Recent - temp$Historic.avg
  
  # make data frame with nice column names
  
  temp = data.frame(id = row.names(temp),
                    Recent = temp$Recent,
                    Historic.Avg = temp$Historic.avg,
                    Recent.Vs.Historic.Avg.Diff = temp$recent.vs.hist.avg.diff,
                    Historic.Total = temp$Historic)
  
  
  # want to also include numbers of cases within each time period#
  # tabulate cases by LA for each time period 
  
  formula = as.formula(paste0("~", i$factor, " + year.period.label2"))
  
  temp2 = as.data.frame.array(xtabs(formula, x, drop.unused.levels = FALSE))
  
  temp2$id = row.names(temp2)
  
  temp2$Recent = NULL
  
  # merge onto data frame with nice column names
  
  temp = merge(temp, temp2, by = "id", all.x = TRUE)
  
  
  # order data 
  
  temp = temp[rev(order(temp$Recent.Vs.Historic.Avg.Diff)), ]
  
  # remove rows with no recent cases
  
  temp = temp[temp$Recent != 0, ] 
  
  # format Historic.Avg and Recent.Vs.Historic.Avg.Diff
  
  temp$Historic.Avg = round(temp$Historic.Avg, 1)
  
  temp$Recent.Vs.Historic.Avg.Diff = round(temp$Recent.Vs.Historic.Avg.Diff, 1)
  
  # reset row names
  
  row.names(temp) = 1:nrow(temp)
  
  # keep only local authorities where recent > historic avg 
  
  temp = temp[temp$Recent > temp$Historic.Avg, ]
  
  # temporarily change id column name
  
  colnames(temp)[colnames(temp) == "id"] = as.character(i$name2)
  
  # save table to output 
  
  assign(paste0("table.", as.character(i$name1), ".recent.vs.historic"), temp)
  
  # revert back to id column naming
  
  colnames(temp)[1] = "id"
  
  
  # melt data for plotting
  
  temp = melt(temp[, c("id", "Recent", "Historic.Avg")], id.vars = "id")
  
  # order factor levels
  
  temp$id = as.character(temp$id)
  
  temp$id = factor(temp$id, levels = rev(unique(temp$id)))
  
  
  # remove local authorities with no data to present
  
  temp = temp[!is.na(temp$value), ]
  
  # plot 
  
  if(nrow(temp) == 0){
  
    assign(paste0("bar.graph.", as.character(i$name1), ".recent.vs.historic"), "No data to present on graph")
    
  } else {
    p = ggplot(temp, aes(x = id, y = value, fill = variable))
    
    p = p + geom_bar(stat="identity", position = "dodge", colour = "black")
    
    p = p + coord_flip()
    
    p = p + xlab(as.character(i$name2))
    
    p = p + ylab("Count")
    
    p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                  axis.text.x = element_text(angle = 0, hjust = .5, size = 16,
                                             colour = "black"),
                  axis.text.y = element_text(hjust = 1, size = 9,
                                             colour = "black"),
                  legend.text= element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                  axis.title = element_text(size=16, face="bold"),
                  strip.text.y = element_text(hjust = 1, size = 16,
                                              colour = "black", face="bold"),
                  legend.position="bottom") 
    
    
    p = p + scale_fill_manual(name = "", values = c("#822433", "#80D7C8"))
    
    
    #print(p)
    
    assign(paste0("bar.graph.", as.character(i$name1), ".recent.vs.historic"), p)
    
  }
  
  
}


# table.la.recent.vs.historic
# table.lab.recent.vs.historic
# bar.graph.la.recent.vs.historic
# bar.graph.lab.recent.vs.historic
 