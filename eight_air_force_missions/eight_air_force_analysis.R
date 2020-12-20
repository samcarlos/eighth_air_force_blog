earth.dist <- function (long1, lat1, long2, lat2){
  #from https://www.r-bloggers.com/2011/05/r-functions-for-earth-geographic-coordinate-calculations/
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


library(dplyr)
library(ggplot2)
library(reshape)
library(zoo)
library(ggrepel)
library(scales)

setwd('C:/Users/Larry/Documents/Github/eighth_air_force_blog')

missions_lat_long = read.csv('Eight_air_force_missions/Data/8thAFMissions_lat_long.csv')
missions_lat_long[,'Date'] = as.Date(missions_lat_long[,'Date'],'%m/%d/%Y')

#plots for particular destinations  
missions_lat_long_agg = missions_lat_long %>% group_by(target_country , Date , Mission.Number , target_type_2 , 
                                                       A.C.Type , lon , lat, distance)%>%
  summarise(Dispatched = sum(Dispatched),
            Escorts = sum(Escorts),
            Recalled = sum(Recalled), 
            RTB = sum(RTB),
            Lost.en.route = sum(Lost.en.route),
            Bombed = sum(Bombed), 
            Total.Lost = sum(Total.Lost),
            Cat.E = sum(Cat.E),
            Damaged = sum(Damaged),
            KIA = sum(KIA),
            WIA = sum(WIA),
            MIA = sum(MIA),
            has_weather = sum(has_weather),
            germany = sum(germany),
            norway_denmark = sum(norway_denmark),
            target_type = paste0(unique(target_type_2),collapse = ','),
            Notes = paste0(Notes, collapse = ",")) %>% 
  mutate(lost_dispatched_ratio = Total.Lost / Dispatched ) %>% 
  as.data.frame()



#distance from london 
#convert distance to freedoms
missions_lat_long_agg[,'distance'] = missions_lat_long_agg[,'distance']*0.621371
missions_lat_long_agg[,'heavies']=0
missions_lat_long_agg[which(missions_lat_long_agg[,'A.C.Type'] %in% c("B-17","B-24")),'heavies'] = 1

#focusing on heavy bombers
missions_lat_long_agg_heavies = missions_lat_long_agg %>% filter(A.C.Type %in% c('B-17','B-24')) %>%
  group_by(target_country , Date , Mission.Number , target_type_2 , lon , lat,distance) %>% 
  summarise(Dispatched = sum(Dispatched),
            Escorts = sum(Escorts),
            Recalled = sum(Recalled), 
            RTB = sum(RTB),
            Lost.en.route = sum(Lost.en.route),
            Bombed = sum(Bombed), 
            Total.Lost = sum(Total.Lost),
            Cat.E = sum(Cat.E),
            Damaged = sum(Damaged),
            KIA = sum(KIA),
            WIA = sum(WIA),
            MIA = sum(MIA),
            has_weather = sum(has_weather),
            germany = sum(germany),
            norway_denmark = sum(norway_denmark),
            target_type = paste0(unique(target_type_2),collapse = ','),
            Notes = paste0(Notes, collapse = ",")) %>%
  mutate(lost_dispatched_ratio = Total.Lost / Dispatched ) %>% 
  as.data.frame()

#subtract recalled and returned to base aircraft
missions_lat_long_agg_heavies = mutate(missions_lat_long_agg_heavies, Dispatched = Dispatched - Recalled - RTB)


missions_lat_long_agg_heavies[,'dates_cut'] =cut(missions_lat_long_agg_heavies[,'Date'], breaks = as.Date(c('1943-01-01','1943-06-01','1943-12-31', '1944-07-01', '1944-10-01')))
dates_cut_names = c("1943-06-01 to 1943-12-31", "1944-07-1 to 1944-08-15")
names(dates_cut_names) = c('1943-06-01','1944-07-01')
graph_by_timecut = ggplot(subset(missions_lat_long_agg_heavies, Dispatched>20 & distance < 1000 & lost_dispatched_ratio>=0) %>% filter(dates_cut %in% c('1943-06-01','1944-07-01')), 
                          aes(x = distance, y = lost_dispatched_ratio*100 ))+ facet_grid(.~dates_cut,labeller = labeller(dates_cut = dates_cut_names))+
  ggtitle("Heavy Bomber Losses By Distance")+
  geom_point()+
  xlab("Distance From London (Miles)")+
  ylab('Heavy Bomber Losses (%)')+
  theme_bw()+ theme(legend.position = "none")+
  theme(text = element_text(size=20))

ggsave(filename = 'plots/graph_by_time_cut.png', plot = graph_by_timecut, width = 9, height = 5, dpi =300)
#timeline 
timeline_df = data.frame( 
  start_date = as.Date(c('1944-01-21', '1944-02-20','1944-03-05', '1943-06-14', '1944-06-06', '1943-10-14','1943-08-17','1943-12-05')),
  end_date = as.Date(c('1944-01-21', '1944-02-25','1944-03-08','1944-04-19', '1944-06-06','1943-10-14','1943-08-17','1943-12-05')),
  event = c('First Duty Change','Operation Argument','Battle of Berlin','Operation Pointblank','D-Day','2nd Schweinfurt','1st Scwheinfurt', 'First Mustang Escort'),
  position = c(-.5,-1,-1,-2,-.5,-1,-1, 
               -.5),
  text_position = -c(.55,1.05,1.05,2.05,.95,1.05,1.05,.55)
)


timeline_df = timeline_df %>% mutate(mean_date = start_date + (end_date-start_date)/2)
timeline = ggplot(timeline_df,aes(x=start_date,y=0))+theme_bw()+
  geom_hline(yintercept=0, color = "black", size=0.3)+
  #geom_point()+
  #geom_segment(aes(y=position,yend=0, xend=start_date), color='black', size=0.2)+
  geom_text_repel(aes(y=text_position,x = mean_date, label=event),size=3.5)+
  geom_segment(mapping=aes(x=start_date, y=position, xend=end_date, yend=position))+
  xlim(as.Date('1943-01-01'),as.Date('1944-08-15'))+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank()
        )+xlab('')+ylab('Timeline')+
  scale_y_continuous(breaks=c(-0.5),
                   labels=c("      "))



distance_data = data.frame(
  distance = c(580, 464, 170, 1272, 212, 1049, 718, 310, 397),
  location = c("Berlin", "Schweinfurt", 'SaintLo', 'Silistea ,Romania', 'Paris', 'Drohobycz', 'Oslo', 'Cologne','Frankfurt'),
  x = rep(as.Date('1942-121-01'), 9)
  
)
distance_data[,'location'] = paste0('italic(',distance_data[,'location'],')')

big_plot = ggplot(subset(missions_lat_long_agg_heavies, Dispatched>20 & distance < 800 & lost_dispatched_ratio>0 & Date > '1943-01-01'), 
                  aes(x = Date, y = distance ,
                      label = round(as.numeric(lost_dispatched_ratio)*100), colour = log(lost_dispatched_ratio),
                      size = (lost_dispatched_ratio+1)))+
  geom_text_repel()+
  ggtitle("Heavy Bomber Percent Losses For Each Raid by Distance and Time")+
  xlab("Date")+
  ylab('Distance From London (Miles)')+
  theme_bw()+ theme(legend.position = "none")+
  theme(text = element_text(size=20))+
#  annotate('text',label = distance_data[1,'location'], y = distance_data[1,'distance'], x = distance_data[1,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[2,'location'], y = distance_data[2,'distance'], x = distance_data[2,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[3,'location'], y = distance_data[3,'distance'], x = distance_data[3,'x'], parse = TRUE)+
#  #annotate('text',label = distance_data[4,'location'], y = distance_data[4,'distance'], x = distance_data[4,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[5,'location'], y = distance_data[5,'distance'], x = distance_data[5,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[8,'location'], y = distance_data[8,'distance'], x = distance_data[8,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[9,'location'], y = distance_data[9,'distance'], x = distance_data[9,'x'], parse = TRUE)+
#  annotate('text',label = distance_data[7,'location'], y = distance_data[7,'distance'], x = distance_data[7,'x'], parse = TRUE)+
  coord_cartesian(xlim = c(as.Date('1943-01-01'),as.Date('1944-08-15')), clip = 'off' )+
  scale_color_gradient(low="blue", high="red" )

raids_distance_plot = grid.arrange(big_plot, timeline, ncol=1, heights = c(800,200))

ggsave(filename = 'plots/raids_distance_plot.png', plot = raids_distance_plot, width = 12, height = 10, dpi =300)


month_heavies = missions_lat_long_agg %>% filter( (A.C.Type %in% c('B-17', 'B-24'))) %>% 
  mutate(yearmon = as.yearmon(Date)) %>% 
  group_by(yearmon) %>%
  summarise(dispatched = sum(Dispatched), recalled = sum(Recalled+RTB), lost = sum(Total.Lost)) %>%
  mutate(dispatched_2 = dispatched - recalled) %>%
  mutate(lost_ratio = lost  / dispatched_2) %>%
  as.data.frame()
month_heavies_melt = melt(month_heavies, id.vars= 'yearmon')
new_labels_heavy_month <- c("Total Lost",'Dispatched','Lost Ratio')

names(new_labels_heavy_month) <- c("lost",'dispatched_2','lost_ratio')

heavy_dispatched_loss_by_month_graph = subset(month_heavies_melt, as.Date(yearmon) < as.Date('1944-08-01') & as.Date(yearmon) > as.Date('1942-12-31')) %>%
  filter(variable %in% c('dispatched_2', 'lost','lost_ratio')) %>% 
  ggplot( aes(x = as.Date(yearmon), y= value))+
  geom_bar(stat = 'identity')+
  facet_grid(variable~., scales = 'free', labeller = labeller(variable = new_labels_heavy_month))+ylab('Count')+xlab('Date (Month)')+theme_bw()+
  theme(text = element_text(size=20))+
  ggtitle('Heavy Bomber Dispatched and Lossed by Month')


month_fighters = missions_lat_long_agg %>% filter( (A.C.Type %in% c('P-47', 'P-38','P-51'))) %>% 
  mutate(yearmon = as.yearmon(Date)) %>% 
  group_by(yearmon, A.C.Type) %>%
  summarise(dispatched = sum(Dispatched), recalled = sum(Recalled+RTB), lost = sum(Total.Lost)) %>%
  mutate(dispatched_2 = dispatched - recalled) %>%
  mutate(lost_ratio = lost  / dispatched_2) %>%
  as.data.frame()
month_fighters_melt = melt(month_fighters, id.vars = c('yearmon', 'A.C.Type'))
head(month_fighters_melt)
month_fighters_melt = month_fighters_melt %>% 
  mutate(AC_Type = ifelse( (as.Date(yearmon) >= as.Date('1944-07-01'))*(A.C.Type == 'P-47'| A.C.Type == 'P-38' | A.C.Type == 'P-51'), 'all_fighters', A.C.Type) )%>% 
  group_by(AC_Type,yearmon, variable) %>%
  summarise(value = sum(value)) %>% 
  as.data.frame()
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fighter_dispatched_by_month_graph  = subset(month_fighters_melt, as.Date(yearmon) < as.Date('1944-08-01') & as.Date(yearmon) > as.Date('1942-12-31') & variable == 'dispatched_2') %>%
  ggplot( aes(x = as.Date(yearmon), y= value, fill = AC_Type))+
  geom_bar(stat = 'identity')+theme_bw()+
  ggtitle('Escort Dispatched by Month')+
  ylab('Count')+
  xlab('Date (Month)')+ theme(legend.position = "bottom")+
  theme(text = element_text(size=20))+  scale_fill_manual(values=cbbPalette)



ggsave(filename = 'plots/heavy_dispatched_loss_by_month_graph.png', plot = heavy_dispatched_loss_by_month_graph, width = 9, height = 7, dpi =300)
ggsave(filename = 'plots/fighter_dispatched_by_month_graph.png', plot = fighter_dispatched_by_month_graph, width = 9, height = 5, dpi = 300)


grid.arrange(heavy_dispatched_loss_by_month_graph,fighter_dispatched_by_month_graph, heights = c(500,500))


##
month_heavies_target_type = missions_lat_long_agg %>% filter( (A.C.Type %in% c('B-17', 'B-24'))) %>% 
  mutate(yearmon = as.yearmon(Date)) %>% 
  group_by(yearmon, target_type_2) %>%
  summarise(dispatched = sum(Dispatched), recalled = sum(Recalled+RTB)) %>%
  as.data.frame()


#target_type_2_to_keep = gsub('.PNG','',list.files('C:/Users/Larry/google drive/8th_air_force_data/images'))
target_type_2_to_keep = c('Airfield','Transportation','Industry','V-Weapons','Aircraft','Uboats','Oil','Tactical')
missions_lat_long_agg_heavies[,'image_id_loc'] = paste0('C:/Users/Larry/google drive/8th_air_force_data/images/', missions_lat_long_agg_heavies[,'target_type_2'], '.PNG')
library(ggimage)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

big_plot_target_type = (subset(missions_lat_long_agg_heavies, Dispatched>20 & distance < 800 & lost_dispatched_ratio>0 & Date > '1943-01-01')%>% filter(target_type_2 %in% target_type_2_to_keep )) %>%
  ggplot( aes(x = Date, y = distance, label = target_type_2, colour = target_type_2) )+
  ggtitle("Heavy Bomber Percent Losses For Each Raid by Distance and Time")+
  xlab("Date")+
  ylab('Distance From London (Miles)')+
  theme_bw()+ theme(legend.position = "none")+
  theme(text = element_text(size=20))+
  #geom_image(aes(image = image_id_loc), size = .04)
  geom_text_repel(size = 3)+
  scale_colour_manual(values=cbbPalette)

total_aces_by_day_rd = pilot_counts_by_day_front %>%
subset(unit_type != "NJG" & rd == 1) %>%
group_by( date, rd) %>%
summarise(aces = sum(aces)) %>%
as.data.frame()
