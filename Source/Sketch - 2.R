#
# Source -National Center for Chronic Disease Prevention and Health Promotion | Division of Population Health
# Behavioral Risk Factor Surveillance System (BRFSS)
# http://www.cdc.gov/sleep/data_statistics.html
#
# READ BEFORE EXECUTION
# Pre-required steps:
# download dataset from http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP 
# extract and copy LLCP2014.XPT into /Sleeping/BRFSS/2014/

library(Hmisc)
library(maps)
library(ggplot2)
library(gplots)
library(maptools)
require(gridExtra)


setwd('/Sleeping/')

# convert sas format to dataframe
dfcol <- sasxport.get('BRFSS/2014/LLCP2014.XPT', as.is = TRUE)
df2014 <- sasxport.get('BRFSS/2014/LLCP2014.XPT', as.is = TRUE)
#df2013 <- sasxport.get('BRFSS/2013/LLCP2013.XPT', as.is = TRUE)
#df2012 <- sasxport.get('BRFSS/2012/LLCP2012.XPT', as.is = TRUE)
#df2011 <- sasxport.get('BRFSS/2011/LLCP2011.XPT', as.is = TRUE)
#df2010 <- sasxport.get('BRFSS/2010/CDBRFS10.XPT', as.is = TRUE)
#df2009 <- sasxport.get('BRFSS/2009/CDBRFS09.XPT', as.is = TRUE)

# interrogation 
colnames(dfcol$x.rfdrhv4)

# keep only used columns - data optimization - fucking lighter-faster, damn it
df2014 <- dfcol[,c('x.state', 'sleptim1', 'x.age.g', 'x.race', 'x.rfdrhv4', 'sex', 'smokday2',
                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]
#df2013 <- df2013[,c('x.state', 'sleptim1', 'x.age.g',  'x.race', 'sex', 'smokday2',
#                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]

#df2012 <- df2012[,c('x.state', 'sleptime', 'x.age.g',  'x.race.g', 'sex', 'smokday2',
#                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]
#df2011 <- df2011[,c('x.state', 'sleptime', 'x.age.g',  'x.race.g', 'sex', 'smokday2',
#                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]
#df2010 <- df2010[,c('x.state', 'sleptime', 'x.age.g',  'x.race.g', 'sex', 'smokday2',
#                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]
#df2009 <- df2009[,c('x.state', 'sleptime', 'x.age.g',  'x.race.g', 'sex', 'smokday2',
#                    'exerany2', 'genhlth', 'physhlth', 'menthlth')]

# data cleanup - remove 2 external states, remove null sleeping data records
stateMapping <- read.csv('BRFSS/StateMapping.csv', stringsAsFactors = F)
df2014 <-  subset(df2014, x.state != 66 & x.state != 72)
df2014 <- merge(df2014, stateMapping, by = 'x.state')
df2014 <- subset(df2014, sleptim1 <=24)

#df2013 <-  subset(df2013, x.state != 66 & x.state != 72)
#df2013 <- merge(df2013, stateMapping, by = 'x.state')
#df2013 <- subset(df2013, sleptim1 <=24)

#df2012 <-  subset(df2012, x.state != 66 & x.state != 72)
#df2012 <- merge(df2012, stateMapping, by = 'x.state')
#df2012 <- subset(df2012, sleptime <=24)

#df2011 <-  subset(df2011, x.state != 66 & x.state != 72)
#df2011 <- merge(df2011, stateMapping, by = 'x.state')
#df2011 <- subset(df2011, sleptime <=24)

#df2010 <-  subset(df2010, x.state != 66 & x.state != 72)
#df2010 <- merge(df2010, stateMapping, by = 'x.state')
#df2010 <- subset(df2010, sleptime <=24)

#df2009 <-  subset(df2009, x.state != 66 & x.state != 72)
#df2009 <- merge(df2009, stateMapping, by = 'x.state')
#df2009 <- subset(df2009, sleptime <=24)

#counting related parameters
# 'x.state', 'sleptim1', 'x.age.g', 'x.race', 'x.rfdrhv4 - alcohol', 'sex', 'smokday2',
# 'exerany2', 'genhlth', 'physhlth', 'menthlth'
table(df2014$x.age.g)
table(df2014$sex)
table(df2014$x.rfdrhv4)
table(df2014$smokday2)
table(df2014$exerany2)
table(df2014$genhlth)
table(df2014$physhlth)

#convert HR to Min -> precision
df2014$sleptim1_min <- df2014$sleptim1 *60

# 6 years average
#years_avg <- data.frame('avg' = c(mean(df2014$sleptim1), mean(df2013$sleptim1), mean(df2012$sleptime), 
#               mean(df2011$sleptime), mean(df2010$sleptime), mean(df2009$sleptime)))
#years_avg <- cbind(years_avg, year=c(2014:2009))

#plot(years_avg)

#p <- ggplot(years_avg, aes(x = year, y = avg))
#p + geom_line() + ylab("y")

#histogram
hist(df2014$sleptim1)

# average sleeping hours by state
g_state_avg <- aggregate(df2014[, 'sleptim1'], list(df2014$StateAbbv), mean)
g_state_avg <- g_state_avg[with(g_state_avg, order(x)),]

plot(g_state_avg)
ggplot(g_state_avg, aes(reorder(Group.1,x), x)) +geom_bar(stat ='identity') + coord_cartesian(ylim=c(6.5, 7.5))

#--------------------------------------------------------
# one diemnsion plot - not used
# average sleeping hours by age group
age_group_avg <- aggregate(df2014[, 'sleptim1'], list(df2014$x.age.g), mean)
qplot(factor(age_group_avg$Group.1), age_group_avg$x, geom = 'bar', stat = 'identity') + coord_cartesian(ylim=c(6.5, 7.5))


# average sleeping hours by physical activity
phys_act_group_avg <- aggregate(df2014[, 'sleptim1'], list(df2014$exerany2), mean)
qplot(factor(phys_act_group_avg$Group.1), phys_act_group_avg$x, geom = 'bar', stat = 'identity') + coord_cartesian(ylim=c(6.5, 7.5))
#--------------------------------------------------------

# average sleeping hours by physical activity vs age groups
data <- df2014[df2014$exerany2 == 1 | df2014$exerany2==2, ]
phys_act_age_group_avg <- aggregate(data[, 'sleptim1_min'], list(exercise = data$exerany2, age = data$x.age.g), mean)
phys_act_age_group_avg$x <- round(phys_act_age_group_avg$x,0)
phys_act_age_group_avg$exercise <- as.factor(phys_act_age_group_avg$exercise)
phys_act_age_group_avg$age <- as.factor(phys_act_age_group_avg$age)

ggplot(phys_act_age_group_avg, aes(x=age, y=x, fill=exercise)) +
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(ylim=c(360, 480))

# average sleeping hours by sex vs age groups
sex_age_group_avg <- aggregate(df2014[, 'sleptim1_min'], list(sex = df2014$sex, age = df2014$x.age.g), mean)
sex_age_group_avg$x <- round(sex_age_group_avg$x,0)
sex_age_group_avg$sex <- as.factor(sex_age_group_avg$sex)
sex_age_group_avg$age <- as.factor(sex_age_group_avg$age)

ggplot(sex_age_group_avg, aes(x=age, y=x, fill=sex)) +
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(ylim=c(360, 480))

# tobacco and alcohol consumption - percentage pie chart - cutoff by average sleeping hrs.
# group by consumption rates
# as in sketches
absoluteAverage = mean(df2014$sleptim1)

#tobacco consumption
smokeBehav <- df2014[df2014$smokday2 == 1 | df2014$smokday2 == 2 | df2014$smokday2 == 3,c('smokday2', 'sleptim1')]
totalCount <- nrow(smokeBehav)
dailySmoker <- nrow(smokeBehav[smokeBehav$smokday2==1,])
occasionSmoker <- nrow(smokeBehav[smokeBehav$smokday2==2,])
nonSmoker <- nrow(smokeBehav[smokeBehav$smokday2==3,])

head(smokeBehav)
table(smokeBehav$smokday2)
smokerAvg <-  data.frame(type=c(), count=c())
smokerAvg <- rbind(smokerAvg, data.frame(type='dailySmoker', class='belowAvg',
                                                   count=nrow(smokeBehav[smokeBehav$smokday2==1 & smokeBehav$sleptim1 < absoluteAverage,])))
smokerAvg <- rbind(smokerAvg, data.frame(type='occasionSmoker',  class='belowAvg',
                                                   count= nrow(smokeBehav[smokeBehav$smokday2==2 & smokeBehav$sleptim1 < absoluteAverage,])))
smokerAvg <- rbind(smokerAvg, data.frame(type='nonSmoker',  class='belowAvg',
                                                   count=nrow(smokeBehav[smokeBehav$smokday2==3 & smokeBehav$sleptim1 < absoluteAverage,])))
smokerAvg <- rbind(smokerAvg, data.frame(type='dailySmoker',  class='aboveAvg',
                                         count=nrow(smokeBehav[smokeBehav$smokday2==1 & smokeBehav$sleptim1 >= absoluteAverage,])))
smokerAvg <- rbind(smokerAvg, data.frame(type='occasionSmoker',  class='aboveAvg',
                                         count=nrow(smokeBehav[smokeBehav$smokday2==2 & smokeBehav$sleptim1 >= absoluteAverage,])))
smokerAvg <- rbind(smokerAvg, data.frame(type='nonSmoker',  class='aboveAvg',
                                         count=nrow(smokeBehav[smokeBehav$smokday2==3 & smokeBehav$sleptim1 >= absoluteAverage,])))

#ggplot
# non smoker
p <- ggplot(smokerAvg[smokerAvg$type=='nonSmoker',], aes(x='nonSmoker', y= count, fill=class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  annotate(geom = "text", y = c(smokerAvg[smokerAvg$type=='nonSmoker',]$count[1], 
                                sum(smokerAvg[smokerAvg$type=='nonSmoker',]$count)),
           x = 1, label = smokerAvg[smokerAvg$type=='nonSmoker',]$count / sum(smokerAvg[smokerAvg$type=='nonSmoker',]$count) * 100)
# occasion smoker
q <- ggplot(smokerAvg[smokerAvg$type=='occasionSmoker',], aes(x='occasionSmoker', y=count, fill=class)) +
  geom_bar(width = 1, stat = "identity", xlab= 'occasional smoker') +
  coord_polar("y", start=0) +
  annotate(geom = "text", y = c(smokerAvg[smokerAvg$type=='occasionSmoker',]$count[1], 
                                sum(smokerAvg[smokerAvg$type=='occasionSmoker',]$count)),
           x = 1, label = smokerAvg[smokerAvg$type=='occasionSmoker',]$count / sum(smokerAvg[smokerAvg$type=='occasionSmoker',]$count) * 100)
# daily smoker
r <- ggplot(smokerAvg[smokerAvg$type=='dailySmoker',], aes(x='dailySmoker', y=count, fill=class)) +
  geom_bar(width = 1, stat = "identity", xlab= 'daily smoker') +
  coord_polar("y", start=0) +
  annotate(geom = "text", y = c(smokerAvg[smokerAvg$type=='dailySmoker',]$count[1], 
                                sum(smokerAvg[smokerAvg$type=='dailySmoker',]$count)),
           x = 1, label = smokerAvg[smokerAvg$type=='dailySmoker',]$count / sum(smokerAvg[smokerAvg$type=='dailySmoker',]$count) * 100)

#simple plots
par(mfrow = c(1,3))
cols <- c('lightblue', 'lightpink')
pie(smokerAvg[smokerAvg$type=='nonSmoker',]$count, 
    label = smokerAvg[smokerAvg$type=='nonSmoker',]$count / sum(smokerAvg[smokerAvg$type=='nonSmoker',]$count) * 100
    , main = 'Non Smoker', col = cols)
pie(smokerAvg[smokerAvg$type=='occasionSmoker',]$count, 
    label = smokerAvg[smokerAvg$type=='occasionSmoker',]$count / sum(smokerAvg[smokerAvg$type=='occasionSmoker',]$count) * 100
    , main = 'Occasional Smoker', col = cols)
pie(smokerAvg[smokerAvg$type=='dailySmoker',]$count, 
    label = smokerAvg[smokerAvg$type=='dailySmoker',]$count / sum(smokerAvg[smokerAvg$type=='dailySmoker',]$count) * 100
    , main = 'Daily Smoker', col = cols)
legend(0,1, c("belowAvg", 'aboveAvg'), cex=0.8, 
       fill = cols)


# alcohol consumption
drinkBehav <- df2014[df2014$x.rfdrhv4 == 1 | df2014$x.rfdrhv4 == 2 ,c('x.rfdrhv4', 'sleptim1')]

drinkAvg <-  data.frame(type=c(), count=c())
drinkAvg <- rbind(drinkAvg, data.frame(type='heavyDrinker', class='belowAvg',
                                       count=nrow(drinkBehav[drinkBehav$x.rfdrhv4==2 & drinkBehav$sleptim1 < absoluteAverage,])))
drinkAvg <- rbind(drinkAvg, data.frame(type='non-heavyDrinker', class='belowAvg',
                                       count=nrow(drinkBehav[drinkBehav$x.rfdrhv4==1 & drinkBehav$sleptim1 < absoluteAverage,])))
drinkAvg <- rbind(drinkAvg, data.frame(type='heavyDrinker', class='aboveAvg',
                                       count=nrow(drinkBehav[drinkBehav$x.rfdrhv4==2 & drinkBehav$sleptim1 >= absoluteAverage,])))
drinkAvg <- rbind(drinkAvg, data.frame(type='non-heavyDrinker', class='aboveAvg',
                                       count=nrow(drinkBehav[drinkBehav$x.rfdrhv4==1 & drinkBehav$sleptim1 >= absoluteAverage,])))
# simple plot - not the different size of sample datasets
par(mfrow = c(1,2))
cols <- c('lightblue', 'lightpink')
pie(drinkAvg[drinkAvg$type=='heavyDrinker',]$count, 
    label = drinkAvg[drinkAvg$type=='heavyDrinker',]$count / sum(drinkAvg[drinkAvg$type=='heavyDrinker',]$count) * 100
    , main = 'Heavy Drinker', col = cols)
pie(drinkAvg[drinkAvg$type=='non-heavyDrinker',]$count, 
    label = drinkAvg[drinkAvg$type=='non-heavyDrinker',]$count / sum(drinkAvg[drinkAvg$type=='non-heavyDrinker',]$count) * 100
    , main = 'Non Heavy Drinker', col = cols)
legend(0,1, c("belowAvg", 'aboveAvg'), cex=0.8, 
       fill = cols)

#--------------------------------------------------------

# results of sleeping - to group by range of sleeping hours, 3 groups
# physical health condition good-bad df2014$genhlth
# percentage plot
absoluteAverage = mean(df2014$sleptim1)

physHealth <- df2014[df2014$genhlth %in% c(1,2,3,4,5) ,c('genhlth', 'sleptim1')]

#below average sleeping time
physHealthBelowAvg <-  data.frame(type=c(), count=c())
physHealthBelowAvg <- rbind(physHealthBelowAvg, data.frame(type='Exellent', 
                                                     count=nrow(physHealth[physHealth$genhlth==1 & physHealth$sleptim1 < absoluteAverage,])))
physHealthBelowAvg <- rbind(physHealthBelowAvg, data.frame(type='Very Good', 
                                                     count= nrow(physHealth[physHealth$genhlth==2 & physHealth$sleptim1 < absoluteAverage,])))
physHealthBelowAvg <- rbind(physHealthBelowAvg, data.frame(type='Good', 
                                                           count= nrow(physHealth[physHealth$genhlth==3 & physHealth$sleptim1 < absoluteAverage,])))
physHealthBelowAvg <- rbind(physHealthBelowAvg, data.frame(type='Fair', 
                                                           count= nrow(physHealth[physHealth$genhlth==4 & physHealth$sleptim1 < absoluteAverage,])))
physHealthBelowAvg <- rbind(physHealthBelowAvg, data.frame(type='Poor', 
                                                           count= nrow(physHealth[physHealth$genhlth==5 & physHealth$sleptim1 < absoluteAverage,])))

# over average sleeping time
physHealthOverAvg <-  data.frame(type=c(), count=c())
physHealthOverAvg <- rbind(physHealthOverAvg, data.frame(type='Exellent', 
                                                           count=nrow(physHealth[physHealth$genhlth==1 & physHealth$sleptim1 >= absoluteAverage,])))
physHealthOverAvg <- rbind(physHealthOverAvg, data.frame(type='Very Good', 
                                                           count= nrow(physHealth[physHealth$genhlth==2 & physHealth$sleptim1 >= absoluteAverage,])))
physHealthOverAvg <- rbind(physHealthOverAvg, data.frame(type='Good', 
                                                           count= nrow(physHealth[physHealth$genhlth==3 & physHealth$sleptim1 >= absoluteAverage,])))
physHealthOverAvg <- rbind(physHealthOverAvg, data.frame(type='Fair', 
                                                           count= nrow(physHealth[physHealth$genhlth==4 & physHealth$sleptim1 >= absoluteAverage,])))
physHealthOverAvg <- rbind(physHealthOverAvg, data.frame(type='Poor', 
                                                           count= nrow(physHealth[physHealth$genhlth==5 & physHealth$sleptim1 >= absoluteAverage,])))

p <- ggplot(physHealthBelowAvg, aes(x='', y=count, fill=type)) +
  geom_bar(width = 1, stat = "identity") +xlab('physical health, below avg sleep')

q <- ggplot(physHealthOverAvg, aes(x='', y=count, fill=type)) +
  geom_bar(width = 1, stat = "identity") +xlab('physical health, over avg sleep')
grid.arrange(arrangeGrob(p, q, ncol=2))

# mental health condition - number of days with bad emotaion df2014$menthlth
# percentage plot
mentHealth <- df2014[df2014$menthlth %in% c(1:30, 88) ,c('menthlth', 'sleptim1')]

#below average sleeping time
mentHealthBelowAvg <-  data.frame(type=c(), count=c())
mentHealthBelowAvg <- rbind(mentHealthBelowAvg, data.frame(type='none', 
                                                           count=nrow(mentHealth[mentHealth$menthlth  %in% c(88) & mentHealth$sleptim1 < absoluteAverage,])))
mentHealthBelowAvg <- rbind(mentHealthBelowAvg, data.frame(type='1-7 days', 
                                                           count=nrow(mentHealth[mentHealth$menthlth  %in% c(1:7) & mentHealth$sleptim1 < absoluteAverage,])))
mentHealthBelowAvg <- rbind(mentHealthBelowAvg, data.frame(type='8-14 days', 
                                                           count= nrow(mentHealth[mentHealth$menthlth %in% c(8:14) & mentHealth$sleptim1 < absoluteAverage,])))
mentHealthBelowAvg <- rbind(mentHealthBelowAvg, data.frame(type='15-21 days', 
                                                           count= nrow(mentHealth[mentHealth$menthlth %in% c(15:21) & mentHealth$sleptim1 < absoluteAverage,])))
mentHealthBelowAvg <- rbind(mentHealthBelowAvg, data.frame(type='22-30 days', 
                                                           count= nrow(mentHealth[mentHealth$menthlth %in% c(22:30) & mentHealth$sleptim1 < absoluteAverage,])))

# over average sleeping time
mentHealthOverAvg <-  data.frame(type=c(), count=c())
mentHealthOverAvg <- rbind(mentHealthOverAvg, data.frame(type='none', 
                                                           count=nrow(mentHealth[mentHealth$menthlth  %in% c(88) & mentHealth$sleptim1 >= absoluteAverage,])))
mentHealthOverAvg <- rbind(mentHealthOverAvg, data.frame(type='1-7 days', 
                                                         count=nrow(mentHealth[mentHealth$menthlth %in% c(1:7) & mentHealth$sleptim1 >= absoluteAverage,])))
mentHealthOverAvg <- rbind(mentHealthOverAvg, data.frame(type='8-14 days', 
                                                         count= nrow(mentHealth[mentHealth$menthlth %in% c(8:14) & mentHealth$sleptim1 >= absoluteAverage,])))
mentHealthOverAvg <- rbind(mentHealthOverAvg, data.frame(type='15-21 days', 
                                                         count= nrow(mentHealth[mentHealth$menthlth %in% c(15:21) & mentHealth$sleptim1 >= absoluteAverage,])))
mentHealthOverAvg <- rbind(mentHealthOverAvg, data.frame(type='22-30 days', 
                                                         count= nrow(mentHealth[mentHealth$menthlth %in% c(22:30)& mentHealth$sleptim1 >= absoluteAverage,])))

p <- ggplot(mentHealthBelowAvg, aes(x='', y=count, fill=type)) +
  geom_bar(width = 1, stat = "identity") +xlab('number of days with emotional problems, below avg sleep')

q <- ggplot(mentHealthOverAvg, aes(x='', y=count, fill=type)) +
  geom_bar(width = 1, stat = "identity") +xlab('number of days with emotional problems, over avg sleep')
grid.arrange(arrangeGrob(p, q, ncol=2))


# new plot - combining physical - mental health - sleeping hr
absoluteAverage = mean(df2014$sleptim1_min)
nrow(df2014)
healthData <- df2014[df2014$menthlth %in% c(1:30, 88), c('genhlth','menthlth', 'sleptim1_min')]
healthData <- healthData[healthData$genhlth %in% c(1:5), c('genhlth','menthlth', 'sleptim1_min')]

# classify mental health
healthData[, 'menthlthClass'] <- 0
healthData[healthData$menthlth %in% c(1:7),'menthlthClass'] <- 1
healthData[healthData$menthlth %in% c(8:14),'menthlthClass'] <- 2
healthData[healthData$menthlth %in% c(15:21),'menthlthClass'] <- 3
healthData[healthData$menthlth %in% c(22:30),'menthlthClass'] <- 4

# average values
healthTable <- table(healthData$menthlthClass, healthData$genhlth)
healthTableLabel <- table(healthData$menthlthClass, healthData$genhlth)
healthTable # row: mental health condition 0 = best, col: gen health condition 1 = best 

for(i in 1:nrow(healthTable)) {
  for(j in 1:ncol(healthTable)) {
    healthTable[i,j] <- round(mean(healthData[healthData$menthlthClass==i-1 & healthData$genhlth==j,'sleptim1_min']), 0)
    healthTableLabel[i,j] <- paste(healthTable[i,j]%/%60,':',sprintf("%02d", healthTable[i,j] %% 60), sep='')
  }
}

#heatmap( healthTable, Rowv=NA, Colv=NA, col = heat.colors(256))
heatmap.2( healthTable, Rowv = F, Colv = F,
           cellnote=healthTableLabel, col = cm.colors(256),
           notecol="black", trace='none', dendrogram = 'none',
           key=FALSE,lwid = c(1,4),lhei = c(1,4), margins = c(5,5), 
           main = 'Mental-Physical Health and Average sleeping time')

# recoloring
#heatmap( healthTable, Rowv=NA, Colv=NA, col = heat.colors(256))
heatmap.2( healthTable, Rowv = F, Colv = F,
           cellnote=healthTableLabel, col = colorRampPalette(c('#C9ADA7','#22223B'))(25),
           notecol="black", trace='none', dendrogram = 'none',
           key=FALSE,lwid = c(1,4),lhei = c(1,4), margins = c(5,5), 
           main = 'Mental-Physical Health and Average sleeping time')


#--------------------------------------------------------

#complementary plots
sc2014 <- read.csv('~/Documents/IM Vault/IST 719/Poster/Sample Dataset/Sleeping/SleepCycle-Country.csv', stringsAsFactors = F)
sc2014$timeDecimal <- sapply(strsplit(sc2014$Average.time.in.bed,":"),
                             function(x) {
                               x <- as.numeric(x)
                               x[1]*60+x[2]
                             })

avg_sleep <- mean(sc2014$timeDecimal)
sc2014$belowAvg <- sc2014$timeDecimal < avg_sleep

for( i in sc2014$belowAvg) {
  sc2014[i,'avgDiff'] = sc2014[i,'timeDecimal'] - avg_sleep
}

ggplot(sc2014, aes(reorder(Country, timeDecimal), avgDiff, fill=belowAvg)) +
  geom_bar(stat="identity", position="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

