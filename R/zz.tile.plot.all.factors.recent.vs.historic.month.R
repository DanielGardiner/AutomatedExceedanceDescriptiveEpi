
library(dplyr)
library(tidyr)

text = TRUE 

colour = "red"

label.breaks = 1




x = data %>% filter(!is.na(year.period))

temp.start = min(as.Date(hold.time.period$day))

x$year.month = factor(x$year.month,
                      levels = unique(get.dates(seq(temp.start, end.reporting.date, 1))$year.month))






# formula = as.formula(paste0("~", i$factor, " + year.period.label"))
# 
# temp = as.data.frame.array(xtabs(formula, x, drop.unused.levels = FALSE))

temp = as.data.frame(xtabs(~ year.month + year.period.label + lab_name, x, 
                           drop.unused.levels = FALSE))

number.of.historic.years = length(levels(x$year.period)) - 1


temp = temp %>% 
  mutate(Freq = ifelse(year.period.label == "Historic", Freq/number.of.historic.years, Freq))



temp1 = temp %>% filter(year.period.label == "Recent") %>% select(-year.period.label)

temp2 = temp %>% filter(year.period.label == "Historic") %>% select(-year.period.label)

temp = temp1

temp$Freq = round(temp1$Freq - temp2$Freq, 1)


head(temp)



# plot temp2

p = ggplot(temp, aes(x = year.month, y = lab_name, colour = Freq, fill = Freq))

p = p + geom_tile(colour = "white", size = 1)


if(text) p = p + geom_text(aes(label = Freq), vjust = 0.4, size = 4, colour = "black")

p = p + ylab("Laboratory")

p = p + xlab("Year month")

p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 10,
                                         colour = "black"),
              axis.text.y = element_text(hjust = 1, size = 9,
                                         colour = "black"),
              legend.text= element_text(hjust = 1, size = 16,
                                        colour = "black", face="bold"),
              axis.title = element_text(size=16, face="bold"),
              strip.text.y = element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
              legend.position="bottom",
              panel.border = element_rect(colour = "grey", fill = NA),
              panel.background = element_blank())

p = p + scale_fill_continuous(low = "white", high = colour)

p

p = p + scale_x_discrete(breaks = levels(temp$year.month)[c(T, rep(F, label.breaks))],
                         drop=FALSE)

p = p + theme(legend.position="none")


p










