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

missions_lat_long[,'no_escorts'] = 0
missions_lat_long[grep('No escorts', missions_lat_long[,'Notes'], ignore.case = TRUE),'no_escorts'] = 1
missions_lat_long[grep('Escorts fog ', missions_lat_long[,'Notes'], ignore.case = TRUE),'no_escorts'] = 1
missions_lat_long[grep('Escorts miss ', missions_lat_long[,'Notes'], ignore.case = TRUE),'no_escorts'] = 1

missions_lat_long[,'heavies'] = 0
missions_lat_long[which(missions_lat_long[,'A.C.Type'] %in% c("B-17", "B-24")), "heavies"] = 1

missions_lat_long[,'aborted_recalled'] = missions_lat_long[,'Recalled']+missions_lat_long[,'RTB']+missions_lat_long[,'Aborted']
missions_lat_long[,'scrubed_mission'] = (missions_lat_long[,'aborted_recalled']/missions_lat_long[,'Dispatched']>.9)

avg_distance_heavies_mission = subset(missions_lat_long, heavies ==1 & Dispatched > 0) %>% select(Mission.Number, distance) %>%
  group_by(Mission.Number) %>%  mutate(mean_distance_heavies = mean(distance)) %>% as.data.frame()

missions_lat_long_agg_mission = missions_lat_long %>% group_by(Date , Mission.Number ,  A.C.Type, scrubed_mission, no_escorts)%>%
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
            min_distance = min(distance),
            max_distance = max(distance),
            norway_denmark = sum(norway_denmark),
            target_type = paste0(unique(target_type_2),collapse = ','),
            Notes = paste0(Notes, collapse = ",")) %>% as.data.frame()

missions_lat_long_agg_mission = merge(missions_lat_long_agg_mission, unique(avg_distance_heavies_mission[,-2]), by = 'Mission.Number', all.x = TRUE)
missions_lat_long_agg_wide = reshape(subset(missions_lat_long_agg_mission[,c('Date','Mission.Number','Dispatched','Total.Lost','A.C.Type', 
                                                                             'mean_distance_heavies','no_escorts','scrubed_mission', 'min_distance','max_distance'),] , 
                                            Dispatched> 5 & scrubed_mission == 0 & Mission.Number !=0),
                                     idvar = c("Date", 'Mission.Number', 'mean_distance_heavies','no_escorts','scrubed_mission','min_distance','max_distance'),
                                     timevar = c("A.C.Type"),
                                     direction = "wide")



colnames(missions_lat_long_agg_wide) = gsub('\\.','',colnames(missions_lat_long_agg_wide))
colnames(missions_lat_long_agg_wide) = gsub('\\-','',colnames(missions_lat_long_agg_wide))

missions_lat_long_agg_wide[is.na(missions_lat_long_agg_wide)] = 0

missions_lat_long_agg_wide = mutate(missions_lat_long_agg_wide, heavies_dispatched = DispatchedB17+ DispatchedB24, 
                                    heavies_lost = TotalLostB24+TotalLostB17,
                                    has_p51 = (DispatchedP51>0)*1,
                                    has_p47 = (DispatchedP47>0)*1,
                                    has_p38 = (DispatchedP38>0)*1) %>%
  mutate(heavies_lost_ratio =heavies_lost/heavies_dispatched )%>% 
  mutate(heavies_los_ratio_odds = heavies_lost_ratio/(1-heavies_lost_ratio))

missions_lat_long_agg_wide[,'Date'] = as.Date(missions_lat_long_agg_wide[,'Date'])

pilot_counts_by_day_front = read.csv('luftwaffe_data/data/num_pilots_by_day_reich_defense.csv')
total_aces_by_day_rd = pilot_counts_by_day_front %>% 
  subset(unit_type != "NJG" & rd == 1) %>%
  group_by( date, rd) %>% 
  summarise(aces = sum(aces)) %>% 
  as.data.frame()
total_aces_by_day_rd[,'date'] = as.Date(total_aces_by_day_rd[,'date'])
#ggplot(aes(x = as.Date(date), y = aces)) + geom_line()

missions_lat_long_agg_wide_merged_luftwaffe = merge(missions_lat_long_agg_wide, total_aces_by_day_rd,
                                                    by.x = 'Date',by.y = 'date')

pairs(missions_lat_long_agg_wide[,c('heavies_los_ratio_odds','heavies_dispatched', 'heavies_lost','heavies_lost_ratio', 'DispatchedP51', 'DispatchedP47', 'DispatchedP38')])
missions_lat_long_agg_wide[,'mean_distance_heavies_500'] = (missions_lat_long_agg_wide[,'mean_distance_heavies'] > 500)*1
library(mgcv)

gam1 = gam(heavies_lost ~ s(as.numeric(as.Date(Date))) +s(mean_distance_heavies) + s(heavies_dispatched)+ min_distance+max_distance+
             (DispatchedP51*(mean_distance_heavies))+(DispatchedP47*(mean_distance_heavies))+(DispatchedP38*(mean_distance_heavies))+ 
             aces, data = subset(missions_lat_long_agg_wide, (DispatchedB17+DispatchedB24) >0 & heavies_lost_ratio<1 & mean_distance_heavies <1000 & DispatchedP51< 400))
summary(gam1)


reg_luft = lm(heavies_los_ratio  ~  (log(heavies_dispatched))+((as.numeric(as.Date(Date)))) +((mean_distance_heavies))*
                (log(1+DispatchedP51/heavies_dispatched) + log(1+DispatchedP47/heavies_dispatched) +
                log(1+DispatchedP38/heavies_dispatched))*
                aces,
              data = subset(missions_lat_long_agg_wide_merged_luftwaffe, (DispatchedB17+DispatchedB24) >0 & 
                            heavies_lost_ratio<1   & DispatchedP51< 400 & Date > as.Date('1943-06-01') & Date < as.Date('1944-06-01')))

reg_luft = gam(log(heavies_lost+1)  ~  s(log(heavies_dispatched))+s((as.numeric(as.Date(Date)))) +((mean_distance_heavies)) +
                (log(1+DispatchedP51)) + log(1+DispatchedP47) +
                log(1+DispatchedP38)+(has_p51)+(has_p38)+(has_p47)+
                s(aces),
              data = subset(missions_lat_long_agg_wide_merged_luftwaffe, (DispatchedB17+DispatchedB24) >0 & 
                              heavies_lost_ratio<1   & DispatchedP51< 400& DispatchedP38< 400 & Date > as.Date('1943-06-01')& Date < as.Date('1944-06-01') ))
summary(reg_luft)

reg_luft = randomForest(log(heavies_lost+1)  ~  (log(heavies_dispatched))+((as.numeric(as.Date(Date)))) +((mean_distance_heavies)) +min_distance+max_distance+
                log(1+DispatchedP51) + log(1+DispatchedP47) +
                log(1+DispatchedP38)+
                log(aces),
              data = subset(missions_lat_long_agg_wide_merged_luftwaffe, (DispatchedB17+DispatchedB24) >20 & 
                              heavies_lost_ratio<1   & DispatchedP51< 400& DispatchedP38< 400 & Date > as.Date('1943-06-01')& Date < as.Date('1944-06-01')))

head(subset(missions_lat_long_agg_wide_merged_luftwaffe,
            (DispatchedB17+DispatchedB24) >20 & heavies_lost_ratio<1   & DispatchedP51< 400 & Date > as.Date('1943-06-01') )[order(resids),
                                    c('mean_distance_heavies','DispatchedP47','DispatchedP51', 'DispatchedP38','DispatchedB17','DispatchedB24', 'heavies_lost', 'Date')])
par(mfrow = c(2,2))
plot(reg_luft)
summary(reg_luft)



missions_lat_long_agg_wide[,'numeric_date'] = as.numeric(as.Date(missions_lat_long_agg_wide[,'Date']))
missions_lat_long_agg_wide_merged_luftwaffe[,'numeric_date'] = as.numeric(as.Date(missions_lat_long_agg_wide_merged_luftwaffe[,'Date']))
library(randomForest)
rf = randomForest(heavies_lost_ratio~ Dispatched+distance + Date + 
                    p47 , data = subset(missions_lat_long_agg_wide_merged_luftwaffe,Dispatched > 0 ),
                  maxnodes  = 10)
summary(reg_luft)

resids = rf$y - rf$predicted
rf_2 = randomForest(resids~ heavies_dispatched+numeric_date+
                      mean_distance_heavies+(DispatchedP51)+(DispatchedP47)+(DispatchedP38)+aces,
                  data =subset(missions_lat_long_agg_wide_merged_luftwaffe, heavies_lost_ratio<1 & mean_distance_heavies <1000 ), maxnodes = 4)
