library(dplyr)
library(ggmap)
library(ggplot2)
#data http://www.8thafhsoregon.com/8th/
missions = read.csv('C:/Users/Larry/Google Drive/8thAFMissions.csv')
to_remove_cols = which(colMeans(is.na(missions))==1)
missions = missions[,-to_remove_cols]



missions = mutate(missions, target_country = paste0(Target, ' ,', Country))


missions[which(missions[,'target_country'] == 'Augsburg ,'),'target_country'] = 'Augsburg, Germany'
missions[which(missions[,'target_country'] == 'Brunswick Area ,'),'target_country'] = 'Brunswick, Germany'
missions[which(missions[,'target_country'] == 'Diversion ,Holland'),'target_country'] = 'Holland'
missions[which(missions[,'target_country'] == 'Orleans ,'),'target_country'] = 'Orleans, France'
missions[which(missions[,'target_country'] == 'Watten ,'),'target_country'] = 'Watten, France'
missions[which(missions[,'target_country'] == 'Germany ,Germany'),'target_country'] = 'Germany'
missions[which(missions[,'target_country'] == 'Yugoslavia ,Yugoslavia'),'target_country'] = 'Yogoslavia'

missions[grep('Avions Potez airc',missions[,'target_country']),'target_country'] = 'Meaulte, france'
missions[grep('Ft Rouge',missions[,'target_country']),'target_country'] = 'St. Omer, France'
missions[grep('Bretuit',missions[,'target_country']),'target_country'] = 'Frankfurt, Germany'

#missions[grep('Liotard',missions[,'target_country']),'target_country'] =

missions[grep('Roye/Amy',missions[,'target_country']),'target_country'] = 'Roye, France'
missions[grep('Scottvast',missions[,'target_country']),'target_country'] = 'Sottevast, France'
missions[grep('Lutzkendorf',missions[,'target_country']),'target_country'] = 'Halle ,Germany'

#missions[grep('Tuton',missions[,'target_country']),'target_country'] = 'Halle ,Germany'
missions[grep('Pix ,France',missions[,'target_country']),'target_country'] = 'Arfeuille-Ch?tain, France'
#missions[grep('Oeske',missions[,'target_country']),'target_country'] = 
missions[grep('La Frilliere ,France',missions[,'target_country']),'target_country'] = 'La Frilli?re, 37210, France'
#missions[grep('Le Coulet ,Belgium',missions[,'target_country']),'target_country'] = 'La Frilli?re, 37210, France'
#missions[grep('Le Pecrone ,France',missions[,'target_country']),'target_country'] = 'La Frilli?re, 37210, France'
#missions[grep('Nunque ,France',missions[,'target_country']),'target_country'] = 'La Frilli?re, 37210, France'
#missions[grep('Folous',missions[,'target_country']),'target_country'] = 'La Frilli?re, 37210, France'
missions[grep('Limbeck ,Germany',missions[,'target_country']),'target_country'] = 'Limbach ,Germany'
#missions[grep('Leopdoldshall',missions[,'target_country']),'target_country'] = 'Limbach ,Germany'
missions[grep('Blengermont',missions[,'target_country']),'target_country'] = 'blangerval, france'
missions[grep('Courbronne',missions[,'target_country']),'target_country'] = 'coubronne france'
#missions[grep('Melslorek ,Nether',missions[,'target_country']),'target_country'] = 'coubronne france'
#missions[grep('Tuelmont ,Netherl',missions[,'target_country']),'target_country'] = 'coubronne france'
#missions[grep('Schore ,France',missions[,'target_country']),'target_country'] = 'coubronne france'
#missions[grep('Welle ,France',missions[,'target_country']),'target_country'] = 'coubronne france'
missions[grep('Jaigle',missions[,'target_country']),'target_country'] = "l'aigle, france"
missions[grep('Villeneuve',missions[,'target_country']),'target_country'] = 'Villeneuve france'
missions[grep('Merkwille ,Germany',missions[,'target_country']),'target_country'] = 'Merkwiller, Pechelbronn, France'
missions[grep('Zlistea',missions[,'target_country']),'target_country'] = 'Silistea ,Romania'

missions[grep('Brunswick',missions[,'target_country']),'target_country'] = "Brunswick ,Germany"
missions[grep('Yogoslavia',missions[,'target_country']),'target_country'] = 'belgrade'
missions[grep('Bizau ,Romania',missions[,'target_country']),'target_country'] = 'Buzau, romania'
missions[grep('Hamm',missions[,'target_country']),'target_country'] = 'Hamm, Germany'
missions[grep('Bois Coquerel ,France',missions[,'target_country']),'target_country'] = 'pas de calais, France'
missions[grep('Various ,France',missions[,'target_country']),'target_country'] = ' ,France'
missions[grep('Lutz Kendoprf ,Germany',missions[,'target_country']),'target_country'] = ' ,Wettin, Germany'
missions[grep('Schore ,France',missions[,'target_country']),'target_country'] = 'Chandai ,France'
missions[grep('Jaigle ,France',missions[,'target_country']),'target_country'] = ' ,France'
missions[grep('Usine Liotard Air Dpot ,France',missions[,'target_country']),'target_country'] = ' ,France'
missions[grep('Signal, Fuel, V weapon depots and bridges ,France',missions[,'target_country']),'target_country'] = ' ,France'
missions[grep('Road traffic ,France',missions[,'target_country']),'target_country'] = ' ,France'
missions[grep('Rouglaf',missions[,'target_country']),'target_country'] = 'Châteaudun, France'
missions[grep('Rouglaf',missions[,'target_country']),'target_country'] = 'Châteaudun, France'
missions[grep('Salex ,France',missions[,'target_country']),'target_country'] = 'Châteaudun, France'
missions[grep('Plantlunne',missions[,'target_country']),'target_country'] = 'Lingen, Germany'
missions[grep('Eingwarden',missions[,'target_country']),'target_country'] = 'Warder, Netherlands'
missions[grep('Tuton',missions[,'target_country']),'target_country'] = 'Tutow, Germany'
missions[grep('Evere, Brussels, Melsbroek',missions[,'target_country']),'target_country'] = 'Brussels, Belgium'
missions[grep('Marienburg Rahmel',missions[,'target_country']),'target_country'] = 'Marienburg, Germany'
missions[grep('Diversion ,Germany',missions[,'target_country']),'target_country'] = 'Germany'
missions[grep('Carpetbagger',missions[,'target_country']),'target_country'] = 'France'
missions[grep('Pecrone',missions[,'target_country']),'target_country'] = 'Peronne, France'

missions[which(missions[,'Mission.Number']==507 & missions[,'A.C.Type'] == 'P-38'),'target_country'] 
missions[which(missions[,'Mission.Number']==481 & missions[,'target_country'] == 'France ,France'),'target_country'] = 'Caen, France'


key = #enter your personal key here 
register_google(key, write = TRUE)

unique_targets = unique(missions[,'target_country'])



q = c()
for(x in unique_targets){
  q = rbind(q, geocode(x))
  
}

q = as.data.frame(q)
q[,'target_country'] = unique_targets


missions_lat_long = merge(missions,q , by.x = 'target_country', by.y = 'target_country')


missions_lat_long[,'Total.Lost'] = as.numeric(as.character(missions_lat_long[,'Total.Lost']))
missions_lat_long[,'Recalled'] = as.numeric(as.character(missions_lat_long[,'Recalled']))
missions_lat_long[,'Cat.E'] = as.numeric(as.character(missions_lat_long[,'Cat.E']))

missions_lat_long[is.na(missions_lat_long)] = 0
missions_lat_long = mutate(missions_lat_long, lost_dispatched_ratio = Total.Lost / Dispatched)

missions_lat_long[,'has_weather'] = 0 
missions_lat_long[grep('weather',tolower(missions_lat_long[,'Notes'])),'has_weather'] = 1
missions_lat_long[grep('clouds',tolower(missions_lat_long[,'Notes'])),'has_weather'] = 1
missions_lat_long = mutate(missions_lat_long, norway_denmark = (lat > 55)*1)
missions_lat_long[,'Mission.Target.Type'] = as.character(missions_lat_long[,'Mission.Target.Type'])

missions_lat_long[which(missions_lat_long[,'Mission.Number'] == 88 & missions_lat_long[,'target_country'] == "Amiens-Gilsy ,France"),'Mission.Target.Type'] = 'Airfield'
missions_lat_long[which(missions_lat_long[,'Mission.Number'] == 43 & missions_lat_long[,'target_country'] == "Amiens/Longeau ,France"),'Mission.Target.Type'] = 'Railroad'
missions_lat_long[which(missions_lat_long[,'Mission.Number'] == 134 & missions_lat_long[,'target_country'] == "Gelsenkirchen ,Germany"),'Mission.Target.Type'] = 'Railroad'
missions_lat_long[which(missions_lat_long[,'Mission.Number'] == 206 & missions_lat_long[,'A.C.Type']=='B-17'),'Mission.Target.Type'] = 'Port'
missions_lat_long[which(missions_lat_long[,'Mission.Number'] == 206 & missions_lat_long[,'A.C.Type']=='B-24'),'Mission.Target.Type'] = 'Port'

missions_lat_long[grep('Diversion', missions_lat_long[,'target_country']),'Mission.Target.Type'] = 'diversion'

missions_lat_long[,'target_type_2'] = as.character(missions_lat_long[,'Mission.Target.Type'])
missions_lat_long[grep('Airfield',missions_lat_long[,'target_type_2']),'target_type_2'] = "Airfield"
missions_lat_long[grep('Aircraft',missions_lat_long[,'target_type_2']),'target_type_2'] = "Aircraft"
missions_lat_long[grep('Air Depot',missions_lat_long[,'target_type_2']),'target_type_2'] = "Air Depot"

missions_lat_long[grep('Ball',missions_lat_long[,'target_type_2']),'target_type_2'] = "Industry"
missions_lat_long[grep('Chemical',missions_lat_long[,'target_type_2']),'target_type_2'] = "Industry"

missions_lat_long[grep('boat',missions_lat_long[,'target_type_2']),'target_type_2'] = "Uboats"
missions_lat_long[grep('Naval',missions_lat_long[,'target_type_2']),'target_type_2'] = "Uboats"
missions_lat_long[grep('Port',missions_lat_long[,'target_type_2']),'target_type_2'] = "Uboats"
missions_lat_long[grep('Ship',missions_lat_long[,'target_type_2']),'target_type_2'] = "Uboats"
missions_lat_long[grep('Boat',missions_lat_long[,'target_type_2']),'target_type_2'] = "Uboats"
missions_lat_long[grep('OIL',missions_lat_long[,'target_type_2']),'target_type_2'] = "Oil"
missions_lat_long[grep('Ground',missions_lat_long[,'target_type_2']),'target_type_2'] = "Tactical"
missions_lat_long[grep('Factory',missions_lat_long[,'target_type_2']),'target_type_2'] = "Industry"
missions_lat_long[grep('Weapons',missions_lat_long[,'target_type_2']),'target_type_2'] = 'V-Weapons'
missions_lat_long[which(missions_lat_long[,'target_type_2'] == 'V'),'target_type_2'] = 'V-Weapons'
missions_lat_long[which(missions_lat_long[,'target_type_2'] == 'NOBALL'),'target_type_2'] = 'V-Weapons'

missions_lat_long[grep('Rail',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Transportation'
missions_lat_long[grep('Bridge',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Transportation'

missions_lat_long[grep('NEPTUNE',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Tactical'
missions_lat_long[grep('Beaches',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Tactical'
missions_lat_long[grep('Gun Battery',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Tactical'
missions_lat_long[grep('Coastal Targets',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Tactical'
missions_lat_long[grep('Power',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Industry'
missions_lat_long[grep('oss',missions_lat_long[,'target_type_2']),'target_type_2'] = 'Industry'
missions_lat_long[grep('Carpet',missions_lat_long[,'target_type_2']),'target_type_2'] = "Carpetbagger"
missions_lat_long[grep('OSS',missions_lat_long[,'target_type_2']),'target_type_2'] = "Carpetbagger"

missions_lat_long = subset(missions_lat_long, lat > 5 & lon > -20)
missions_lat_long[,'A.C.Type'] = as.character(missions_lat_long[,'A.C.Type'])
missions_lat_long[grep('51',missions_lat_long[,'A.C.Type']),'A.C.Type'] = "P-51"
missions_lat_long[,'Date']
missions_lat_long[,'distance'] = apply(missions_lat_long[,c('lon','lat')],1,function(x) earth.dist(x[1],x[2],-0.128 , 51.5 ))
missions_lat_long[,'germany'] = 0
missions_lat_long[grep('Germany',missions_lat_long[,'target_country']),'germany'] = 1
missions_lat_long[grep('germany',missions_lat_long[,'target_country']),'germany'] = 1

missions_lat_long[which(missions_lat_long[,'Dispatched']==0 & missions_lat_long[,'Escorts']>0),'Dispatched'] = missions_lat_long[which(missions_lat_long[,'Dispatched']==0 & missions_lat_long[,'Escorts']>0),'Escorts']
missions_lat_long[,'distance'] = apply(missions_lat_long[,c('lon','lat')],1,function(x) earth.dist(x[1],x[2],-0.128 , 51.5 ))


write.csv(missions_lat_long, '8thAFMissionslatlong.csv')
