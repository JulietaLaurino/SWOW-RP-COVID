# Merge raw data from SQL database	
# This file is not part of the SWOW-RP distribution!	
#	
# Author: Simon De Deyne & Alvaro Cabana	
# Adaptation for SWOW-RP-COVID data: Julieta Laurino, julilaurino@gmail.com

library(tidyverse)	
library(lubridate)
library(here)	
source('settings.R', encoding = 'UTF-8')	
file.participants = paste0('../data/SWOW/raw/participants.',release,'.csv') 	
file.responses = paste0('../data/SWOW/raw/responses.',release,'.csv') 	
file.output = '../data/SWOW/raw/SWOW-RP.complete_covid.csv'
participants = read.csv(file.participants,	
                        stringsAsFactors = F,	
                        strip.white = T,	
                        na.strings = c("NULL","NAN"),	
                        encoding = 'UTF-8')	
responses = read.csv(file.responses,	
                     stringsAsFactors = F,	
                     strip.white = T,	
                     na.strings = "NULL",
                     encoding = 'UTF-8')	

# Filter responses during covid times, before update	
covid.start=ymd("2020-03-01")	
covid.end=ymd("2020-12-09")	
responses  = responses %>% filter(created_at < covid.start | created_at > covid.end)	

#Assign "covid" to cues after covid.end	
#Assign "precovid" to cues before covid.end	
responses = responses %>% 	
  ungroup() %>% 	
  rowwise() %>% 	
  mutate(cov.time = ifelse(created_at > covid.end, "covid", "precovid"))	

# Single out responses repeated in covid set and precovid set	
# responses = responses %>% 	
#   group_by(cue) %>% 	
#   mutate (cov.repeat = sum(cov.time %in% "covid")>0 & sum(cov.time %in% "precovid")>0 )	

# Filter variables	
participants = participants %>% select(participantID = id,age,nativeLanguage,	
                                       gender,education,city,country)	
responses = responses %>% select(participantID,responseID = id,section,cue,	
                                 R1 = response1Clean,R2 = response2Clean,	
                                 R3 = response3Clean, cov.time)	
# Join	
X = right_join(participants,responses,by ='participantID')	
write.csv(X,file = file.output,row.names = F, fileEncoding = 'UTF-8')
