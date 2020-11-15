library(tidyverse)
library(lubridate)

dt = read.csv("SWT_EXPORT_01_17_20_17_25.csv", header = TRUE, colClasses=c('Date','numeric'))
dt <- dt %>% 
  mutate(dayWeek=weekdays(Date)) %>%
  mutate(MonthName=months(Date)) %>%
  mutate(Month=month(Date)) %>%
  mutate(Year=year(Date))

levels <- c("luned", "marted", "mercoled", 
          "gioved", "venerd")

levels <- paste(levels,rawToChar(as.raw(236)),sep='')

levels <- c(levels, "sabato", "domenica")

dt$dayWeek <- factor(dt$dayWeek, levels = levels)
#  levels= c("Monday", "Tuesday", "Wednesday", 
#            "Thursday", "Friday", "Saturday", "Sunday"))

dt$MonthName <- factor(dt$MonthName, 
  levels= c("gennaio", "febbraio", "marzo", "aprile", 
            "maggio", "giugno", "luglio", "agosto", 
            "settembre", "ottobre", "novembre", 
            "dicembre"))
#  levels= c("January", "February", "March", "April", 
#            "May", "June", "July", "August", "September",
#            "October", "November", "December"))

dt$MonthName <- as.factor(dt$MonthName)
dt$Year <- as.factor(dt$Year)

#pdf("summary.pdf")

#################################

dt %>% ggplot(aes(x=Weight)) +
  geom_histogram(alpha=0.4, stat = 'bin',
                 binwidth = 0.2) + 
  xlab("Peso (kg)") + 
  ggtitle("Distrubuzione del peso")

#################################

ggplot(data=dt, mapping = aes(x = Date, y = Weight)) + 
  ylab("Peso (kg)") + 
  xlab("Data") + 
  geom_point(shape=3, color = "red") +
  geom_smooth(method = "loess") +
  theme(axis.text.x = element_text(angle=10)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  ggtitle("Serie temporale del peso")

############################################

dt %>% ggplot(aes(x=Weight, fill=Year)) +
  geom_histogram(position="Identity", alpha=0.2, binwidth = 0.2) + 
  xlab("Peso (kg)") + 
  scale_fill_discrete(name='Anno') +
  ggtitle("Peso per anno") 

###############################################

dt %>% ggplot(aes(x=Year,y=Weight,fill=Year)) +
  geom_boxplot() +
  xlab("Anno") +
  ylab("Peso (kg)") + 
  scale_fill_discrete(name='Anno') +
  ggtitle("Peso per anno")

################################################

dt %>% ggplot(aes(x=Weight, fill=MonthName)) +
  #    geom_histogram(data=subset(dt,Year == 2017),fill = "red", alpha = 0.2) + 
  #    geom_histogram(data=subset(dt,Year == 2018),fill = "green", alpha = 0.2) +
  #    geom_histogram(data=subset(dt,Year == 2019),fill = "blue", alpha = 0.2) +
  geom_histogram(alpha=0.2) + 
  xlab("Peso (kg)") + 
  scale_fill_discrete(name='Mese') +
  ggtitle("Peso per mese")

#################################################

dt %>% ggplot(aes(x=MonthName,y=Weight,fill=MonthName)) +
  geom_boxplot() +
  xlab("Mese") +
  ylab("Peso (kg)") + 
  scale_fill_discrete(name='Mese') +
  theme(axis.text.x = element_text(angle=20)) +
  ggtitle("Peso per mese")

#################################################

dt %>% ggplot(aes(x=Weight, fill=dayWeek)) +
  geom_histogram(position="Dodge",alpha=0.2) + 
  xlab("Peso (kg)") + 
  scale_fill_discrete(name='Giorno della settimana') +
  ggtitle("Peso per giorno della settimana")

################################################

dt %>% ggplot(aes(x=dayWeek,y=Weight,fill=dayWeek)) +
  geom_boxplot() +
  xlab("Giorno del Mese") +
  ylab("Peso (kg)") + 
  scale_fill_discrete(name='Giorno della settimana') +
  ggtitle("Peso per giorno della settimana")

##################################################
#str(dt)

#dt[sapply(dt, is.factor)] <- 
#  lapply(dt[sapply(dt, is.factor)],
#         as.numeric)
       
#cor(dt[colnames(dt) != "Date"], use="complete.obs", method="pearson")

#pairs(dt[colnames(dt) != "Date"])

#dev.off()