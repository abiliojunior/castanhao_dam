library(jsonlite)
library(tidyverse)
library(lubridate)
library(gganimate)



json_file <- "./Data/raw/castanhao01.json"

json_data <- fromJSON(json_file, simplifyVector = FALSE)

json_data_frame <- data.frame(matrix(unlist(json_data), nrow=length(json_data), byrow=T))

names(json_data_frame)<-c("datamedicao", "timestamp","cota","volume","Volume_perc")

as.numeric(str_replace(json_data_frame$cota, ",", "."))-> json_data_frame$cota
as.numeric(str_replace(json_data_frame$volume, ",", "."))-> json_data_frame$volume
as.numeric(str_replace(json_data_frame$Volume_perc, ",", "."))-> json_data_frame$Volume_perc
json_data_frame<-json_data_frame[,-2]


json_data_frame$datamedicao<-as.POSIXct(json_data_frame$datamedicao, format="%Y-%m-%d")
json_data_frame <- na.omit(json_data_frame)

#######################################################################################

df<-json_data_frame

#tranformando em date time posic



lims <- as.POSIXct(strptime(c("2010-01-01","2019-01-01"), format = "%Y-%m-%d")) 


df%>%
  select(datamedicao, Volume_perc)%>% 
  filter(Volume_perc!= 0)%>%
  ggplot(aes(x=datamedicao, y=Volume_perc)) + 
  #geom_point()+
  geom_line(color='steelblue', size=3, alpha=0.4)+
  scale_x_datetime(limits =lims)+
  transition_reveal(datamedicao, lims)+
  labs(title = "Castanhão", subtitle = "{frame_along}")
