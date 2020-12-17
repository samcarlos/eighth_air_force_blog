library(dplyr)
library(zoo)
library(gtools)
library(ggplot2)
library(lubridate)

setwd('C:/Users/Larry/Documents/Github/eighth_air_force_blog')

claims_east = read.csv('luftwaffe_data/data/claims_east.txt')
claims_west = read.csv('luftwaffe_data/data/claims_west.txt')

claims_east = mutate(claims_east, front = 'east')
claims_west = mutate(claims_west, front = 'west')

df = smartbind(claims_east, claims_west)
colnames(df)[c(3:4, 6, 7, 9)] = c('first_name','last_name','unit','aircraft','victory_time' )
df[,'dates'] = dmy(unlist(lapply(strsplit(df[,'Date'], ' '), function(x) x[[1]])))
#remove duplicates
df = unique(df)
df[,'yearmon'] = as.yearmon(df[,'dates'])
df = mutate(df, full_name = paste0(first_name, last_name))
df[,'full_name'] = tolower(iconv(df[,'full_name'],to="ASCII//TRANSLIT"))

#number of claims by time per full_name
df = df %>% arrange(full_name,dates)
df[,'counts'] = 1:nrow(df)
df = df %>% group_by(full_name)%>% mutate(rank = dense_rank(counts)) %>% as.data.frame()

#
max_date = df %>% group_by(full_name) %>% 
  filter(dates == max(dates))%>% mutate(max_date = dates)%>%select(full_name, max_date)  %>% distinct() %>%as.data.frame()

df = merge(df, max_date, by = c('full_name'), all.x = TRUE)
df = mutate(df, date_diff = max_date - dates)

min_date = df %>% group_by(full_name) %>% 
  filter(dates == min(dates))%>% mutate(min_date = dates)%>%select(full_name, min_date)  %>% distinct() %>%as.data.frame()

df = merge(df, min_date, by = c('full_name'), all.x = TRUE)

lw_aces = read.csv('luftwaffe_data/data/lw_aces_text.txt')
lw_aces = mutate(lw_aces, full_name = paste0(First.Name, Pilot.Name))

test = lapply(lw_aces[,'Remarks'], function(q) na.omit(do.call(c,(lapply(strsplit(q,' '), function(D) lubridate::dmy(D) )))))
test_1 = lapply(test, function(x) unlist(x[1]))
test_1[sapply(test_1, is.null)] = NA
test_1 = unlist(test_1)

lw_aces[,'date'] = as.Date(test_1, origin="1869-12-31")
lw_aces[,'status'] = 'none'
lw_aces[grep('KIA',lw_aces[,'Remarks']),'status'] = 'KIA'
lw_aces[grep('KILLED',lw_aces[,'Remarks'], ignore.case = TRUE),'status'] = 'KILLED'
lw_aces[grep('MIA',lw_aces[,'Remarks']),'status'] = 'MIA'
lw_aces[grep('POW',lw_aces[,'Remarks']),'status'] = 'POW'
lw_aces[grep('KIFA',lw_aces[,'Remarks']),'status'] = 'KIFA'
lw_aces[grep('Injured',lw_aces[,'Remarks']),'status'] = 'Injured'
lw_aces[grep('WIA',lw_aces[,'Remarks']),'status'] = 'WIA'
lw_aces[grep('KIC',lw_aces[,'Remarks']),'status'] = 'KIC'
lw_aces[grep('WIFA',lw_aces[,'Remarks']),'status'] = 'WIFA'
lw_aces[grep('Died',lw_aces[,'Remarks']),'status'] = 'Died'
lw_aces[grep('Surrender',lw_aces[,'Remarks']),'status'] = 'Surrender'
lw_aces[grep('suicide',lw_aces[,'Remarks']),'status'] = 'suicide'


lw_aces[,'full_name'] = tolower(iconv(lw_aces[,'full_name'],to="ASCII//TRANSLIT"))
lw_aces_1 = lw_aces[,c('full_name','date','status')]
colnames(lw_aces_1) = c('full_name','status_date','status')

#remove duplicate names. should find match better but its only ~66 names lost.

lw_aces_1 = lw_aces_1 %>% filter(!full_name %in% lw_aces_1[which(duplicated(lw_aces_1[,'full_name'])),'full_name'])

lw_aces_1[which(as.numeric(lw_aces_1[,'status_date'])< -30000),'status_date'] = lw_aces_1[which(as.numeric(lw_aces_1[,'status_date'])< -30000),'status_date']+years(100)
df_1 = merge(df, lw_aces_1, by = 'full_name', all.x = TRUE)

df_1[is.na(df_1[,'dates']),'dates'] = as.Date('1945-06-01')

#nag close recon

df_1[,'aircraft'] = gsub(' *', '',df_1[,'aircraft'])
df_1[,'aircraft'] = gsub('*', '',df_1[,'aircraft'])
df_1[,'aircraft'] = gsub('*', '',df_1[,'aircraft'])

df_1[grep('B-17',df_1[,'aircraft']),'aircraft'] = 'B-17'
df_1[grep('Fortress',df_1[,'aircraft']),'aircraft'] = 'B-17'
df_1[grep('Liberator',df_1[,'aircraft']),'aircraft'] = 'B-24'
df_1[grep('P-47',df_1[,'aircraft']),'aircraft'] = 'P-47'
df_1[grep('B-24',df_1[,'aircraft']),'aircraft'] = 'B-24'
df_1[grep('P-51',df_1[,'aircraft']),'aircraft'] = 'P-51'
df_1[grep('P-38',df_1[,'aircraft']),'aircraft'] = 'P-38'
df_1[grep('P-25',df_1[,'aircraft']),'aircraft'] = 'P-25'
df_1[grep('Il-2',df_1[,'aircraft']),'aircraft'] = 'Il-2'
df_1[grep('Spitfire',df_1[,'aircraft']),'aircraft'] = 'Spitfire'
df_1[grep('Hurric',df_1[,'aircraft']),'aircraft'] = 'Hurricane'
df_1[grep('P-40',df_1[,'aircraft']),'aircraft'] = 'P-40'
df_1[grep('Airacobra',df_1[,'aircraft']),'aircraft'] = 'P-39'
df_1[grep('P-39',df_1[,'aircraft']),'aircraft'] = 'P-39'
df_1[grep('Lancaster',df_1[,'aircraft']),'aircraft'] = 'Lancaster'
df_1[grep('Mig',df_1[,'aircraft']),'aircraft'] = 'Mig'
df_1[grep('Yak',df_1[,'aircraft']),'aircraft'] = 'Yak'
df_1[grep('Boston',df_1[,'aircraft']),'aircraft'] = 'Boston'
df_1[grep('Jak',df_1[,'aircraft']),'aircraft'] = 'Jak'
df_1[grep('Mustang',df_1[,'aircraft']),'aircraft'] = 'P-51'
df_1[grep('LaGG',df_1[,'aircraft']),'aircraft'] = 'LaGG'
df_1[grep('Typhoon',df_1[,'aircraft']),'aircraft'] = 'Typhoon'
df_1[grep('B-26',df_1[,'aircraft']),'aircraft'] = 'B-26'
df_1[grep('Thunderb',df_1[,'aircraft']),'aircraft'] = 'P-47'
df_1[grep('Spif',df_1[,'aircraft']),'aircraft'] = 'Spitfire'
df_1[grep('Wellington',df_1[,'aircraft']),'aircraft'] = 'Wellington'

df_1[,'heavy']= 0
df_1[grep('B-24',df_1[,'aircraft']),'heavy']=1
df_1[grep('B-17',df_1[,'aircraft']),'heavy']=1


df_1[,'gruppe'] = ''
df_1[,'gruppe'] = ''
df_1[grep('1[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('2[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('3[.]',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('4[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('5[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('6[.]',df_1[,'Unit']),'gruppe'] = "II"
df_1[grep('7[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('8[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('9[.]',df_1[,'Unit']),'gruppe'] = "III"
df_1[grep('10[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('11[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('12[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('13[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('14[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('15[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('16[.]',df_1[,'Unit']),'gruppe'] = "IV"
df_1[grep('I',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "I"
df_1[grep('III',df_1[,'Unit']),'gruppe'] = "I"
# njg 2, 3, 45, all west 
#NJG 4, 5, west
#jg 300, 301, 302 west wild sau 
#jg4 reich 
# ZG 26,76 , 1  heavy units disbanded 9/44
#need to work on jg 51,53, 3
#jg3 I france 6/6 - 8/8
#jg3 II france 6/6 - 10/10
#jg3 II france 6/6 - 9/11

unit = c('JG 54',  'JG 3',
         'JG 3', 'JG 3','JG 3', 
         "JG 53", "JG 27", 'JG 27', 'JG 27', 'JG 26', 'JG 2', 'NLG 1', 'JG 1',
         'JG 11', 'NJG 2', 'NJG 3', 'NJG 45', 'NJG 4', 'NJG 5', 'JG 300', 'JG 301', 'JG 302', 'JG 4', 'ZG 26','ZG 76', 'ZG 1',
         'JG 27', 'NJG 1', 'NJG 6', 'JG 106')
gruppe = c('III', 'I', 
           'II','III','IV', 
           'II', 'I', 'II', "III", 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA' ,
           'IV', 'NA','NA', 'NA')
enter_reich_defense = c('1943-06-01',  '1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01',
                        '1943-10-01', '1943-01-01', '1943-08-01', '1944-03-01', '1943-06-01',
                        '1943-06-01', '1943-06-01', '1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1943-06-01','1943-06-01','1943-06-01','1943-06-01',
                        '1944-03-23', '1943-06-01', '1943-06-01','1943-06-01')
exit_reich_defense =  c('1944-06-07', '1944-06-06', 
                        '1944-06-06', '1944-06-06', '1944-10-01',
                        '1944-06-06', '1944-06-06', '1944-10-01', '1944-06-06', '1944-06-06', 
                        '1944-06-06', '1944-10-01', '1944-06-06','1944-06-06', '1944-10-01', 
                        '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', 
                        '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01', '1944-10-01',
                        '1944-10-01','1944-10-01','1944-10-01','1944-10-01')

groups_enter_leave_reich_defense = data.frame(unit, gruppe, enter_reich_defense, exit_reich_defense)
groups_enter_leave_reich_defense = mutate(groups_enter_leave_reich_defense, 
                                          enter_reich_defense = as.Date(enter_reich_defense), 
                                          exit_reich_defense = as.Date(exit_reich_defense))

#jg 52 Russian
##jg54 III, 7,8,9, exited 7 Jun 1944 to France https://asisbiz.com/luftwaffe/jg54.html

##jg51 was med  ...
##jg51 II, 4,5,6  18 Aug 1943 https://asisbiz.com/luftwaffe/jg51.html. italian theater so will omit 
#j3 west 3 moved to france june 6
#jg 53 group II 16 Oct 1943 to austria then june 6 to lemans. 4,5,6
#JG 77 medit theater 
#JG 27  1943 jan  I./JG 27 1,2,3 was posted France 
#JG 27 august 1943 4,5,6 II./JG 27 was posted Germany
#njg 100 East
#jg4 reich 
##JG 2 all west than france. 
##NLG 1 night western  
# JG 5 norway finland
# JG 1 reich until June 1944 
# JG 11 reich until June 1944 
# njg 2, 3, 45, all west 
#NJG 4, 5, west
#jg 300, 301, 302 west wild sau 
# ZG 26,76 , 1  heavy units disbanded 9/44
#njg 100 East
#jg4 reich 
# SG2 east 
#JG 27 I, III, IV to go to france 
##JG 26 all west. focused on france after dday
#JG 27  7,8,9 III./JG 27 was posted Austria march 1944 t- june 6



df_1[,'unit_type'] = "JG"
df_1[grep('NJG',df_1[,'unit']),'unit_type'] = "NJG"
df_1[grep('JG 30',df_1[,'unit']),'unit_type'] = "wilde_sau"
df_1[grep('ZG',df_1[,'unit']),'unit_type'] = "ZG"


counts_by_group_month_rm_28_w_exit = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  
  temp_df_1 = df_1[-which(is.na(df_1[,'status'])),]
  
  temp_df_1 = temp_df_1 %>%mutate(last_kill_date_diff = x - dates)
  temp_df_1[is.na(temp_df_1[,'status_date']),'status_date'] = as.Date("1946-06-01")
  temp_df_1[,'status_date_diff'] = x-temp_df_1[,'status_date']
  
   get_counts_day = subset(temp_df_1, dates<=x)%>%
    mutate(last_kill_date_diff = x - dates) %>% 
    group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    subset( status_date_diff <= 0)  %>%
    #subset(last_kill_date_diff <= 28*2) %>% 
    as.data.frame()
  
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe'))
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  get_counts_day_1[,'rd'] = 1
  
  get_counts_day_not_in_rd = get_counts_day %>% filter(!full_name %in% get_counts_day_1[,'full_name']) %>% mutate(rd = 0 )
  
  get_counts_day_1 = rbind(get_counts_day_1, get_counts_day_not_in_rd)
  
  get_counts_day_1 = get_counts_day_1 %>% 
    mutate(entry = (rank==1)*(dates == x)*1, exit = (status_date_diff == 0)*1, lapsed_exit = (last_kill_date_diff == 28*2)*1, claims_day = (dates==x)*1) %>% 
    mutate(ace_entry = (rank==5)*(dates == x)*1, ace_exit =  (rank>4)*((exit == 1) )*1 )
  
  
  sum_counts = get_counts_day_1%>% group_by(unit,rd,unit_type) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_20 = sum((rank>19)*1),
              entries = sum(entry), exits = sum(exit), ace_entries = sum(ace_entry), ace_exits = sum(ace_exit),
              sum_claims = sum(claims_day))  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

unique(df[,c('max_date','min_date', 'full_name')])[which(duplicated(unique(df[,c('max_date','min_date', 'full_name')])[,3])),]
counts_by_group_month_rm_28 = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'day'), function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  df_1 = df_1 %>%mutate(last_kill_date_diff = x - dates)
  get_counts_day = subset(df_1, dates<=x)%>%
    mutate(last_kill_date_diff = x - dates) %>% 
    group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    #subset( date_diff < 7)  %>%
    subset(last_kill_date_diff < 28) %>% 
    as.data.frame()

  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)

  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe'))
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  get_counts_day_1[,'rd'] = 1
  
  get_counts_day_not_in_rd = get_counts_day %>% filter(!full_name %in% get_counts_day_1[,'full_name']) %>% mutate(rd = 0 )
  
  get_counts_day_1 = rbind(get_counts_day_1, get_counts_day_not_in_rd)
  get_counts_day_1 = get_counts_day_1 %>% 
    mutate(entry = (rank==1)*(dates == x)*1, exit = (max_date == x)*1) %>% 
    mutate(ace_entry = (rank==5)*(dates == x)*1, ace_exit =  (rank==5)*(max_date == x)*1 )
  
  sum_counts = get_counts_day_1%>% group_by(unit,rd,unit_type) %>% 
    summarise(total_kills = sum(rank), pilots_w_kills = sum((rank>0)*1), aces = sum((rank>4)*1), gt_20 = sum((rank>19)*1),
            entries = sum(entry), exits = sum(exit), ace_entries = sum(ace_entry), ace_exits = sum(ace_exit)  )  %>% 
    as.data.frame()
  sum_counts[,'date'] = x
  return(sum_counts)
})

counts_by_day_1 = function(x){
  print(x)
  df_1[,'date_diff'] = x-df_1[,'max_date']
  
  temp_df_1 = df_1[-which(is.na(df_1[,'status'])),]
  
  temp_df_1 = temp_df_1 %>%mutate(last_kill_date_diff = x - dates)
  temp_df_1[is.na(temp_df_1[,'status_date']),'status_date'] = as.Date("1946-06-01")
  temp_df_1[,'status_date_diff'] = x-temp_df_1[,'status_date']
  
  get_counts_day = subset(temp_df_1, dates<=x)%>%
    mutate(last_kill_date_diff = x - dates) %>% 
    group_by(full_name)%>% filter(dates == max(dates))%>%
    #subset(rank > 4) %>% 
    filter(rank == max(rank))%>%
    subset( status_date_diff <= 0)  %>%
    #subset(last_kill_date_diff <= 28*2) %>% 
    as.data.frame()
  
  
  to_keep_unit = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe == 'NA') %>% select(unit)
  to_keep_unit_w_gruppe = subset(groups_enter_leave_reich_defense, enter_reich_defense < x & exit_reich_defense > x & gruppe != 'NA') %>% select(unit, gruppe)
  
  get_counts_day_units = merge(get_counts_day, to_keep_unit, by = 'unit')
  get_counts_day_units_gruppe = merge(get_counts_day, to_keep_unit_w_gruppe, by = c('unit', 'gruppe'))
  get_counts_day_1 = rbind(get_counts_day_units, get_counts_day_units_gruppe)
  get_counts_day_1[,'rd'] = 1
  
  get_counts_day_not_in_rd = get_counts_day %>% filter(!full_name %in% get_counts_day_1[,'full_name']) %>% mutate(rd = 0 )
  
  get_counts_day_1 = rbind(get_counts_day_1, get_counts_day_not_in_rd)
  
  get_counts_day_1 = get_counts_day_1 %>% 
    mutate(entry = (rank==1)*(dates == x)*1, exit = (status_date_diff == 0)*1, lapsed_exit = (last_kill_date_diff == 28*2)*1, claims_day = (dates==x)*1) %>% 
    mutate(ace_entry = (rank==5)*(dates == x)*1, ace_exit =  (rank>4)*((exit == 1) )*1 )
  get_counts_day_1 = get_counts_day_1 %>% select(full_name, rd)
  
  colnames(get_counts_day_1)= c('full_name',paste0( x, '_rd'))
  all_pilots_w_status = unique(temp_df_1[,c('full_name','min_date','status_date')])
  get_counts_day_2 = merge(all_pilots_w_status, get_counts_day_1, by = 'full_name',all.x = TRUE)
  get_counts_day_2[which(get_counts_day_2[,'min_date'] - x > 0),4] = -1
  get_counts_day_2[which(get_counts_day_2[,'status_date'] - x < 0),4] = 2
  
  return(get_counts_day_2[,c(1,4)])
}
pilots_2_44 = lapply(seq.Date(as.Date("1943-08-01"),as.Date("1944-09-30"), by = 'month'), counts_by_day_1)

temp_df = Reduce(function(x, y) merge(x, y, all=TRUE, by = 'full_name'), pilots_2_44)

temp_df_melt = melt(temp_df, id.vars = 'full_name')
temp_df_melt[,'value'] = as.factor(temp_df_melt[,'value'])
library(ggalluvial)
ggplot(temp_df_melt,
       aes(x = variable, stratum = as.factor(value), alluvium = full_name,
           fill = as.factor(value), label = as.factor(value))) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters")

temp_df_melt_1 = temp_df_melt
temp_df_melt_1[,'date'] = as.Date(gsub('_rd','',temp_df_melt_1[,'variable']))
temp_df_melt_1[,'date_sub'] = temp_df_melt_1[,'date'] %m-% months(1)

temp_df_melt_2 = merge(temp_df_melt_1[,c('full_name','date_sub','value')], temp_df_melt_1[,c('full_name','date','value')], 
                       by.x = c('full_name','date_sub'),
                        by.y = c('full_name','date'))
temp_df_melt_2_agg = temp_df_melt_2 %>% group_by(date_sub, value.x, value.y) %>% summarise(counts = n()) %>% as.data.frame()
temp_df_melt_3_agg = (temp_df_melt_2_agg %>% filter(value.x ==1 | value.y == 1) %>% mutate(groups = as.factor(paste0(value.x, value.y))))
temp_df_melt_3_agg[which(temp_df_melt_3_agg[,'groups'] == '01'),'counts'] = -temp_df_melt_3_agg[which(temp_df_melt_3_agg[,'groups'] == '01'),'counts']
temp_df_melt_3_agg[which(temp_df_melt_3_agg[,'groups'] == '21'),'counts'] = -temp_df_melt_3_agg[which(temp_df_melt_3_agg[,'groups'] == '21'),'counts']

ggplot(subset(temp_df_melt_3_agg, groups !='11'),aes(x = date_sub, y = counts, fill = groups))+geom_bar(stat = 'identity')

pilots_5_44 = counts_by_day_1(as.Date('1944-05-01'))%>% mutate(gruppe_5_44 = gruppe, unit_5_44 = unit, rank_5_44=rank, rd_5_44 = rd) %>%
  select(full_name, status_date, status, gruppe_5_44, unit_5_44, rank_5_44, rd_5_44)


pilots_2_5 = merge(pilots_2_44, pilots_5_44, by = c('full_name','status','status_date'), all = TRUE)
pilots_2_5[is.na(pilots_2_5)] = -1
pilots_2_5 %>% group_by(rd_2_44, rd_5_44) %>% summarise(counts = n())

requires(ggplot2)
library(ggalluvial)

ggplot(df_1, aes(x = dates, stratum = gruppe, alluvium = full_name, fill = gruppe)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("Treatment across observation period")


library(alluvial)


alluvial((pilots_2_5 %>% group_by(rd_2_44, rd_5_44) %>% summarise(counts = n()) %>% as.data.frame())[,1:2],
         freq=(pilots_2_5 %>% group_by(rd_2_44, rd_5_44) %>% summarise(counts = n()) %>% as.data.frame())$counts,
         cex = 0.7
)

alluvial_ts((pilots_2_5 %>% group_by(rd_2_44, rd_5_44) %>% summarise(counts = n()) %>% as.data.frame()),
            wave = .3, ygap = 5,  plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='white', 
            title = "UNHCR-recognised refugees\nTop 10 countries (2003-13)\n")

counts_by_group_month_rm_28_w_exit = do.call(rbind,counts_by_group_month_rm_28_w_exit)
ggplot(counts_by_group_month_rm_28_w_exit, aes(x = date, y= aces ))+facet_wrap(unit_type~.)+geom_line()

ggplot((subset(counts_by_group_month_rm_28_w_exit , rd == 1) %>% group_by(unit_type, date) %>% summarise(aces = sum(aces)) %>% as.data.frame()),
       aes(x = date, y= aces, ))+facet_wrap(unit_type~.)+geom_line()

ggplot((subset(counts_by_group_month_rm_28_w_exit ) %>% group_by(unit_type, date, rd) %>% summarise(aces = sum(aces)) %>% as.data.frame()),
       aes(x = date, y= aces, colour = as.factor(rd)))+facet_wrap(unit_type~.)+geom_line()

counts_by_group_month_rm_28  = do.call(rbind,counts_by_group_month_rm_28)
ggplot(counts_by_group_month_west_rm_28_days, aes(x = date, y= aces, ))+facet_wrap(unit_type~.)+geom_line()
ggplot((subset(counts_by_group_month_rm_28 ) %>% group_by(unit_type, date,rd) %>% summarise(aces = sum(aces)) %>% as.data.frame()),
       aes(x = date, y= aces, ))+facet_wrap(unit_type~.)+geom_line()


claims_by_unit = df_1 %>% group_by(unit) %>% summarise(n_claimed_in_unit = n()) %>% as.data.frame()
claims_by_unit[,'unit'] = factor(claims_by_unit[,'unit'], claims_by_unit[order(claims_by_unit[,'n_claimed_in_unit']),'unit'])
counts_by_group_month_rm_28 = merge(counts_by_group_month_rm_28,claims_by_unit, by = 'unit', all.x = TRUE)
counts_by_group_month_rm_28_w_exit = merge(counts_by_group_month_rm_28_w_exit,claims_by_unit, by = 'unit', all.x = TRUE)

ggplot(subset(counts_by_group_month_rm_28, n_claimed_in_unit > 100), aes(x = date, y = aces, colour = as.factor(rd)))+geom_line()+facet_wrap(unit~., scales = 'free')

ggplot(subset(counts_by_group_month_rm_28, n_claimed_in_unit > 100), aes(x = date, y = exits, colour = as.factor(rd)))+geom_line()+facet_wrap(unit~., scales = 'free')


changes_in_pilots_by_month = (subset(counts_by_group_month_rm_28_w_exit, n_claimed_in_unit > 100 & rd ==1) %>%#mutate(date = as.yearmon(date)) %>% 
                                group_by(unit,date) %>% 
                                summarise(exits=sum(exits), entries = sum(entries), ace_exits = sum(ace_exits), ace_entries = sum(ace_entries)) %>% 
                                mutate(exits = cumsum(exits), entries = cumsum(entries), ace_exits  =cumsum(ace_exits), ace_entries = cumsum(ace_entries)) %>% as.data.frame())
library(reshape)

changes_in_pilots_by_month = mutate(changes_in_pilots_by_month, ace_diff = ace_entries-ace_exits)

test_merge = merge(subset(changes_in_pilots_by_month, unit == 'JG 26'), 
                   subset(counts_by_group_month_rm_28_w_exit, rd == 1 & unit == 'JG 26'), by = c('date')) 

changes_in_pilots_by_month_melt = melt(changes_in_pilots_by_month, id.vars = c('date','unit_type'))
ggplot(changes_in_pilots_by_month_melt, 
       aes(x = date, y = value, colour = variable))+
    geom_line()+facet_grid(unit_type~variable, scales = 'free')

