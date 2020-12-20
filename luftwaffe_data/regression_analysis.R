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
library(stargazer)

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

pilot_counts_by_day_front = read.csv('luftwaffe_data/data/num_pilots_by_day_reich_defense.csv')
##
missions_lat_long_agg_wide_loc = reshape(subset(missions_lat_long_agg[,c('Date','Mission.Number','Dispatched','Total.Lost','A.C.Type', 'distance',
                                                                         'lon','lat','target_type_2', 'target_country')] , Dispatched> 5),
                                         idvar = c("Date", 'Mission.Number', 'distance', 'lon','lat','target_type_2','target_country'),
                                         timevar = c("A.C.Type"),
                                         direction = "wide")

missions_lat_long_agg_heavies = filter(missions_lat_long_agg, A.C.Type %in% c("B-17", "B-24")) %>% 
  subset(Mission.Number !=0) %>%
  group_by(Mission.Number, lon,lat, target_country,distance, Date)%>%
  summarise(Dispatched = sum(Dispatched), Recalled = sum(Recalled), total_lost = sum(Total.Lost)) %>% 
  as.data.frame()


heavies_per_mission = filter(missions_lat_long_agg, A.C.Type %in% c("B-17", "B-24")) %>% 
  subset(Mission.Number !=0) %>%
  group_by(Mission.Number)%>%
  summarise(total_heavies_dispatched_mission = sum(Dispatched)) %>% 
  as.data.frame()

missions_fighters = filter(missions_lat_long_agg, A.C.Type %in% c("P-51", "P-47","P-38")) %>% 
  subset(Mission.Number !=0) %>%
  group_by(Mission.Number, A.C.Type) %>% 
  summarise(Dispatched = sum(Dispatched)) %>% 
  as.data.frame() %>%
  reshape(idvar = 'Mission.Number',timevar ='A.C.Type',direction = 'wide')
missions_fighters[is.na(missions_fighters)] = 0

exits_by_day = pilot_counts_by_day_front %>% 
  group_by(rd, date) %>% 
  subset(unit_type != "NJG" & rd == 1) %>%
  summarise( ace_exits = sum(ace_exits), exits = sum(exits)) %>% 
  as.data.frame() %>% subset(rd == 1)

total_aces_by_day_rd = pilot_counts_by_day_front %>%
  subset(unit_type != "NJG" & rd == 1) %>%
  group_by( date, rd) %>%
  summarise(aces = sum(aces), pilots = sum(pilots_w_kills), total_kills = sum(total_kills)) %>%
  as.data.frame()

missions_lat_long_agg_heavies = merge(missions_lat_long_agg_heavies, heavies_per_mission, on = 'Mission.Number')
missions_lat_long_agg_heavies = merge(missions_lat_long_agg_heavies, missions_fighters, on = 'Mission.Number')



colnames(missions_lat_long_agg_heavies) = gsub('\\.','',colnames(missions_lat_long_agg_heavies))
colnames(missions_lat_long_agg_heavies) = gsub('\\-','',colnames(missions_lat_long_agg_heavies))
missions_lat_long_agg_heavies[,'Date'] = as.Date(missions_lat_long_agg_heavies[,'Date'])
missions_lat_long_agg_heavies = mutate(missions_lat_long_agg_heavies, ratio_dispatched = Dispatched / total_heavies_dispatched_mission) %>%
  mutate(weighted_p38 = ratio_dispatched* DispatchedP38,
         weighted_p51 = ratio_dispatched* DispatchedP51, 
         weighted_p47 = ratio_dispatched* DispatchedP47)

total_aces_by_day_rd[,'date'] = as.Date(total_aces_by_day_rd[,'date'])
exits_by_day[,'date'] = as.Date(exits_by_day[,'date'])

missions_lat_long_agg_heavies = merge(missions_lat_long_agg_heavies, total_aces_by_day_rd,  by.x = 'Date',by.y = 'date')

missions_lat_long_agg_heavies[is.na(missions_lat_long_agg_heavies)] = 0

missions_lat_long_agg_heavies[,'no_p51'] = (missions_lat_long_agg_heavies[,'weighted_p51']==0)*1
missions_lat_long_agg_heavies[,'no_p38'] = (missions_lat_long_agg_heavies[,'weighted_p38']==0)*1
missions_lat_long_agg_heavies[,'no_p47'] = (missions_lat_long_agg_heavies[,'weighted_p47']==0)*1


reg_1 = gam(log(total_lost+1)~ log(Dispatched + 1)  +
              ratio_dispatched+(log(weighted_p38+1)+log(weighted_p51+1) + log(weighted_p47+1))+(distance)+(as.numeric(Date))+ no_p51+
              no_p38+no_p47+
              total_kills, data = subset(missions_lat_long_agg_heavies,Dispatched > 0 & Date < '1944-06-05' & DispatchedP38<400 ))


summary(reg_1)

reg_output = stargazer(reg_1, out = 'plots/model_regression.html')
