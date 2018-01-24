
library(dplyr)
library(lubridate)

setwd("D:/dat4")
fd <- read.csv("FlightDelays.csv",sep=',',stringsAsFactors = FALSE )
str(fd)
head(fd)

fd$FL_DATE <- mdy(fd$FL_DATE)
str(fd)

# No. of flights delayed for all week days
fd %>% filter(Flight.Status == 'delayed' &
                weekdays(FL_DATE) != 'Saturday' &
                 weekdays(FL_DATE) != 'Sunday') %>% 
             nrow()


fd1 <- filter(fd,weekdays(FL_DATE) == 'Friday' & 
                Flight.Status == 'delayed') %>% nrow()


 filter(fd,Flight.Status == 'ontime') %>% nrow()
