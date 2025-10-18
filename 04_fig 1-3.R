packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate','dplyr','readr','gghalves')
lapply(packages, library, character.only = TRUE)

data_all <- read_csv("/Volumes/ssd/02 extreme weather&policy/10 data/data 0918.csv")

frequent1 <- data_all %>%
     filter(`34kt`> 0|`50kt`>0|`64kt`>0|`80kt`>0|`100kt`>0) %>%
     group_by(citycode) %>%
     summarise(exposeall= n_distinct(date)/3)

summary(frequent1$exposeall)

fwrite(frequent1, 'fig 1a.csv', row.names = F)


ggplot(frequent1, aes(x = group, y = exposeall,fill=group)) +
  geom_half_violin(trim = TRUE, 
                   side="r",   
                   scale='width',width=0.7) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.12,fill = "white",) +
  coord_flip() +  
  theme_bw() 

data_all$ts<-0
data_all$ts<-ifelse(data_all$`34kt`>0,34,data_all$ts)
data_all$ts<-ifelse(data_all$`50kt`>0,50,data_all$ts)
data_all$ts<-ifelse(data_all$`64kt`>0,64,data_all$ts)

severity <- data_all %>%
  group_by(citycode) %>%
  summarise(severity= max(ts))
summary(severity$severity)
fwrite(severity, 'fig 1e tc intensity.csv', row.names = F)


warningtype <- data_all %>%
  # Step 1: Find the maximum severity per citycode
  group_by(citycode) %>%
  summarise(severity = max(ts, na.rm = TRUE)) %>%
  # Step 2: Join back to original data on citycode
  left_join(data_all, by = "citycode") %>%
  # Step 3: Filter rows where severity matches the maximum ts
  filter(severity == ts) %>%
  # Step 4: Group by citycode and calculate the mean of Cpolicy
  group_by(citycode) %>%
  summarise(severity = first(severity), warningtype = mean(Cpolicy, na.rm = TRUE))
fwrite(warningtype, 'fig 1e warning intensity.csv', row.names = F)




##########extract mobility data during tc events

data_all <- read_csv("/Volumes/ssd/02 extreme weather&policy/10 data/data 0918.csv")

###choose data of 2023
data <- data_all
data <- data_all[data_all$year==2023,]
data$time0<-ymd(data$time0)
data$intrac<-data$intrac-1
data$inflowc<-data$inflowc-1
data$outflowc<-data$outflowc-1

# lable exposure day
data0 <- data %>%
  mutate(
    exposed = ifelse(`34kt`> 0|`50kt`>0|`64kt`>0 , 1, 0),
    # Set max_exposure_level to the maximum non-zero radius
    max_exposure_level = case_when(
      `64kt` > 0 ~ 64,
      `50kt` > 0 ~ 50,
      `34kt` > 0 ~ 34,
      TRUE ~ 0
    )
  )
# exclude city without ts exposure
data0 <- data0 %>%
  group_by(citycode) %>%
  filter(any(exposed == 1)) %>%
  ungroup()

data <- data0 %>%
  arrange(citycode, time0)

# extract impact period
data <- data %>%
  group_by(citycode) %>%
  mutate(
    event_start = (exposed == 1) & (lag(exposed, default = 0) == 0 | (date - lag(date, default = first(date))) > 2),  # 当 exposed 为1且时间间隔超过3天时，启动新事件
    event = cumsum(event_start) 
  ) %>%
  fill(event, .direction = "downup") %>%  # 填充 NA 值
  ungroup()


# Calculate event periods
events <- data %>%
  group_by(citycode, event) %>%
  summarise(
    start_date = min(time0[exposed == 1], na.rm = TRUE),
    end_date = case_when(
      any(intrac > 0 & time0 >= start_date) ~ min(time0[intrac > 0 & time0 >= start_date], na.rm = TRUE)
    ),
    pre_event_start = min(time0[exposed == 1], na.rm = TRUE) - days(14),
    typhoon_name = first(typhoon[exposed == 1]),
    .groups = 'drop'
  ) %>%
  filter(event > 0)

events <- events %>%  
  group_by(citycode, typhoon_name) %>%  
  summarise(  
    event = min(event, na.rm = TRUE), 
    start_date = min(start_date, na.rm = TRUE), 
    end_date = na.omit(end_date)[1], 
    pre_event_start = min(pre_event_start, na.rm = TRUE) ,
  )  

##subsequent ts
haikui_end_dates <- events %>%  
  filter(typhoon_name == "haikui") %>%  
  group_by(citycode) %>%  
  summarise(haikui_end_date = min(end_date, na.rm = TRUE)) %>%  
  ungroup() 

events <- events %>%  
  left_join(haikui_end_dates, by = "citycode") %>%  
  mutate(  
    end_date = case_when(  
      typhoon_name == "saola" & is.na(end_date) ~ haikui_end_date,  
      TRUE ~ end_date  
    ),  
    haikui_end_date = NULL  
  )  


# event data before exposure
extract_event_data <- function(citycode_param, event, pre_event_start, end_date, typhoon_name) {  
  data %>%  
    filter(citycode == citycode_param & time0 >= pre_event_start & time0 <= end_date) %>%  
    mutate(event = event, typhoon_name = typhoon_name)  
}

event_data <- pmap_dfr(list(events$citycode, events$event, events$pre_event_start, events$end_date, events$typhoon_name),   
                       ~ extract_event_data(..1, ..2, ..3, ..4, ..5))

event_data <- event_data %>%
  group_by(citycode, typhoon_name) %>%
  mutate(days = as.integer(time0 - min(time0[exposed == 1], na.rm = TRUE))) %>%
  ungroup()

event_data<-merge(event_data,events,by=c('citycode','typhoon_name'),all.x=T)


eventX <- event_data %>%
  group_by(citycode, typhoon_name) %>%
  summarise(
    pstart_date = {
      start_date <- min(start_date, na.rm = TRUE) 
      if_else(  
        any(Cpolicy > 0 & time0 >= start_date - 3 & time0 <= start_date), 
        min(time0[Cpolicy > 0 & time0 >= start_date - 3 & time0 <= start_date], na.rm = TRUE),  
        NA_Date_  
      )
    },
    warning_type = case_when(
      max(Cpolicy, na.rm = TRUE) == 4 ~ "Red",
      max(Cpolicy, na.rm = TRUE) == 3 ~ "Orange",
      max(Cpolicy, na.rm = TRUE) == 2 ~ "Yellow",
      max(Cpolicy, na.rm = TRUE) == 1 ~ "Blue",
      max(Cpolicy, na.rm = TRUE) == 0 ~ "No"
    ),
    exposure_type = max(max_exposure_level, na.rm = TRUE)
  )

event_data<-merge(event_data,eventX,by=c('citycode','typhoon_name'),all.x=T)

event_data$diff<-event_data$pstart_date-event_data$start_date
summary(event_data$diff)

fwrite(event_data, 'event_data_all.csv', row.names = F)

event_data <- read_csv("event_data_all.csv")

event_data<-event_data[event_data$year==2023,]

diff<-event_data%>%
  group_by(citycode, typhoon_name) %>%
  summarise(
    pstart_date = min(pstart_date),
    start_date=min(start_date),
    warning_type=first(warning_type),
    exposure_type=max(exposure_type),
    diff=min(diff)
  )

periode<-event_data%>%
  filter(exposed==1)%>%
  group_by(citycode, typhoon_name) %>%
  summarise(
    periode=n_distinct(time0)
  )

diff_filtered <- diff[diff$warning_type != 'No', ] 
warning_order <- c( "Blue", "Yellow", "Orange","Red")  
warning_colors <- c("Red" = "red", "Orange" = "orange", "Yellow" = "yellow", "Blue" = "blue")  
diff_filtered$warning_type <- factor(diff_filtered$warning_type, levels = warning_order)  

##fig. 1c
ggplot(event_data, aes(x = days, y = intrac,group = interaction(citycode, typhoon_name))) +
  geom_line(color = "lightblue",)+
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +  
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  
  labs( x = "Exposure (day)", y = "Baseline (*100%)")+
  xlim(-14,22)+
  ylim(-0.7,0.50)+
  theme_bw()
#exclude sequential tcs
event_data2<-event_data%>%
  filter(typhoon_name!= "saola"&typhoon_name!= "haikui")

average_data <- event_data2 %>%
  group_by(days) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')

ggplot(average_data, aes(x = days, y = avg_intrac)) +
  geom_line(color = "brown") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +  
  geom_vline(xintercept = -2, color = "black", linetype = "solid") +  
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  
  labs( title='outflow',x = "Exposure (day)", y = "Baseline (*100%)")+
  #xlim(-14,22)+
  #ylim(-0.7,0.50)+
  theme_bw()


###fig. 1d mobility during different tc intensity
average_data <- event_data %>%
  group_by(days, exposure_type) %>%
  summarise(avg_intrac = mean(intrac), sd=sd(intrac),.groups = 'drop')


ggplot(average_data, aes(x = days, y = avg_intrac, color = as.factor(exposure_type))) +  
  #geom_ribbon(aes(ymin = avg_intrac-sd, ymax = avg_intrac+sd, fill = exposure_type), alpha = 0.2,color=NA) +
  geom_line(size=1) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +  
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  
  labs(x = "Exposure (day)", y = "Baseline (*100%)") +  
  #scale_y_continuous(limits = c(-0.52, 0.10), expand = c(0, 0))+
  #scale_x_continuous(limits = c(-10, 5), expand = c(0, 0)) +  
  theme_bw()

###fig. 1f mobility during different warning intensity
average_data2 <- event_data2 %>%
  group_by(days, warning_type) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), sd=sd(intrac, na.rm = TRUE),.groups = 'drop')

ggplot(average_data2, aes(x = days, y = avg_intrac, color = warning_type)) +  
  geom_ribbon(aes(ymin = avg_intrac-sd, ymax = avg_intrac+sd, fill = warning_type), alpha = 0.2,color=NA) +
  geom_line(size = 1) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +  
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  
  labs(x = "Exposure (day)", y = "Baseline (*100%)") +  
  #scale_color_manual(values = colors) +  
  scale_y_continuous(limits = c(-0.52, 0.10), expand = c(0, 0))+
  scale_x_continuous(limits = c(-10, 10), expand = c(0, 0)) +  
  theme_bw()


####fig 2a
er_intra <- read_excel("23/3/er intra.xlsx")
er_intra_sorted <- er_intra %>%  
  arrange(cat, cat2) %>%  
  mutate(cat_cat2=interaction(cat, cat2, sep = " "))

order <- c( "prec 1", "64kt 1","50kt 1","34kt 1",
            "RAIN 1","RAIN 2","RAIN 3","RAIN 4",
            "national 1","national 2","national 3","national 4",
            "provincial 1","provincial 2","provincial 3","provincial 4",
            "citylevel 1","citylevel 2","citylevel 3","citylevel 4")  
er_intra_sorted$cat_cat2<-factor(er_intra_sorted$cat_cat2,levels=order)

ggplot(er_intra_sorted, aes(x = cat_cat2, y = er,group=cat)) +  
  geom_point(size =2, color = "black") + 
  geom_errorbar(aes( ymin = low, ymax = up), width = 0.6, color = "black") + 
  labs(y = "Maximum mobility reduction(*100%)") +  
  theme_bw()+coord_flip()


###fig s5 and results for fig. 3
##recovery time for different tc intensity exposure
colors <- c("red" = "red", "orange"="orange","yellow"="yellow","blue"="blue","without"='grey') 
ts34<-event_data2[event_data2$exposure_type=='34kt',]
average_ts34 <- ts34 %>%
  group_by(days, warning_type) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')

warning_order <- c( "Red","Orange","Yellow", "Blue","Without")  
average_ts34$warning_type <- factor(average_ts34$warning_type, levels = warning_order)  

ggplot(average_ts34, aes(x = days, y = avg_intrac,color = warning_type)) +  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='34kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  #scale_color_manual(values = colors) + 
  scale_y_continuous(limits = c(-0.35, 0.1), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 9), expand = c(0, 0)) +  
  theme_bw()


ts50<-event_data2[event_data2$exposure_type=='50kt',]
average_ts50 <- ts50 %>%
  group_by(days, warning_type) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')
warning_order <- c( "Red","Orange","Yellow", "Blue","Without")  
average_ts50$warning_type <- factor(average_ts50$warning_type, levels = warning_order)  
 
ggplot(average_ts50, aes(x = days, y = avg_intrac, group=warning_type,color = warning_type)) +  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='50kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  #scale_color_manual(values = colors) + 
  scale_y_continuous(limits = c(-0.35, 0.1), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 5), expand = c(0, 0)) +  
  theme_bw()

ts64<-event_data2[event_data2$exposure_type=='64kt',]
average_ts64 <- ts64 %>%
  group_by(days, warning_type) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')
warning_order <- c( "Red","Orange","Yellow", "Blue","Without")  
average_ts64$warning_type <- factor(average_ts64$warning_type, levels = warning_order)  

ggplot(average_ts64, aes(x = days, y = avg_intrac, group=warning_type,color = warning_type)) +  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='64kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  #scale_color_manual(values = colors) +  
  scale_y_continuous(limits = c(-0.35, 0.1), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 16), expand = c(0, 0)) +  
  theme_bw()

##recovery time for different city-level tc warning lead time
average_data2 <- event_data2 %>%
  group_by(days, diff) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')


ggplot(average_data2, aes(x = days, y = avg_intrac,color= as.factor(diff)))+  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='All',x = "Exposure (day)", y = "Baseline (*100%)") +  
 # scale_color_manual(values = colors) +  
  scale_y_continuous(limits = c(-0.3, 0.05), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 9), expand = c(0, 0)) +  
  theme_bw()

average_data34 <- event_data2 %>%
  filter(exposure_type=='34kt')%>%
  group_by(days, diff) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')

ggplot(average_data34, aes(x = days, y = avg_intrac,color= as.factor(diff)))+  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='34kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  # scale_color_manual(values = colors) +  
  scale_y_continuous(limits = c(-0.3, 0.05), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 9), expand = c(0, 0)) +  
  theme_bw()

average_data50 <- event_data2 %>%
  filter(exposure_type=='50kt')%>%
  group_by(days, diff) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')

ggplot(average_data50, aes(x = days, y = avg_intrac,color= as.factor(diff)))+  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='34kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  # scale_color_manual(values = colors) + 
  scale_y_continuous(limits = c(-0.3, 0.05), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 9), expand = c(0, 0)) +  
  theme_bw()

average_data64 <- event_data2 %>%
  filter(exposure_type=='64kt')%>%
  group_by(days, diff) %>%
  summarise(avg_intrac = mean(intrac, na.rm = TRUE), .groups = 'drop')

ggplot(average_data64, aes(x = days, y = avg_intrac,color= as.factor(diff)))+  
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") + 
  labs(title='34kt',x = "Exposure (day)", y = "Baseline (*100%)") +  
  # scale_color_manual(values = colors) + # 使用自定义颜色  
  scale_y_continuous(limits = c(-0.3, 0.05), expand = c(0, 0))+
  scale_x_continuous(limits = c(-3, 9), expand = c(0, 0)) +  
  theme_bw()
