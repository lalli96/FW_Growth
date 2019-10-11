
rm(list = ls()) # clear workspace
library(tidyverse)
library(lubridate)
library(usethis)

updateR()

usethis::use_git()

###all the "date" shit in the following code is to divide everything up into 
  ## 10 differnt breeding seasons, and not calendar years (ie season 1 is like 
  ## dec 2010 to feb 2011)

### the first big block of Mutate stuff is to average the SVL 1 and SVL 2 
 ##   measuremnts into one value, its complicated because not all have the 2nd
 ##   SVL measurement and just have NA instead, and cant take average of NA

######### Main Growth Dataframe #####
growth.dat2 <- read.csv('SVL_Weight_Vol_FWGrowth data.csv') %>%
  mutate_at(vars(SVL_1stMeasurement, SVL_2ndMeasurement,
                 next.first.svl, next.second.svl),
            funs(ifelse(is.na(.), 0, .))) %>%
  filter(next.first.svl!=0, in.out.out.in==1, days.between.captures>4, 
         is.training==1) %>%
  mutate(is.first.svl=ifelse(SVL_1stMeasurement!=0, 1, 0),
         is.second.svl=ifelse(SVL_2ndMeasurement!=0, 1, 0),
         is.last.svl1=ifelse(next.first.svl!=0, 1, 0),
         is.last.svl2=ifelse(next.second.svl!=0, 1, 0),
         mean.first.svl=(SVL_1stMeasurement +
                            SVL_2ndMeasurement)/(is.first.svl+is.second.svl),
         mean.last.svl=(next.first.svl+next.second.svl)/(is.last.svl1+is.last.svl2),
         diff.svl=mean.last.svl-mean.first.svl,
         Date=mdy(date),
         breeding.season=ifelse(Date>=ymd('2012-06-01') &
                                  Date<ymd('2013-06-01'), 4,
                                ifelse(Date>=ymd('2013-06-01') &
                                         Date<ymd('2014-06-01'), 5,
                                       ifelse(Date>=ymd('2014-06-01') &
                                                Date<ymd('2015-06-01'), 6,
                                              ifelse(Date>=ymd('2015-06-01') &
                                                       Date<ymd('2016-06-01'), 7,
                                                     ifelse(Date>=ymd('2016-06-01') &
                                                              Date<ymd('2017-06-01'), 8,
                                                            ifelse(Date>=ymd('2017-06-01') &
                                                                     Date<ymd('2018-06-01'), 9, 
                                                                   ifelse(Date>=ymd('2018-06-01') &
                                                                            Date<ymd('2019-06-01'), 10,
                                                                          ifelse(Date>=ymd('2011-06-01') &
                                                                                   Date<ymd('2012-06-01'), 3,
                                                                                 ifelse(Date>=ymd('2010-06-01') &
                                                                                          Date<ymd('2011-06-01'), 2,
                                                                                        ifelse(Date>=ymd('2009-06-01') &
                                                                                                 Date<ymd('2010-06-01'), 1, 
                                                                                               NA)))))))))),
         growth.type=ifelse(is.pond.growth, 'pond',
                            ifelse(is.upland.growth, 'upland', NA)),
         breeding.season=as.factor(breeding.season),
         days.standardized=(days.between.captures-mean(days.between.captures, na.rm=TRUE))/
                              sd(days.between.captures, na.rm=TRUE),
         first.svl.standardized=(mean.first.svl-mean(mean.first.svl, na.rm=TRUE))/
                                    sd(mean.first.svl, na.rm=TRUE),
         diff.svl.transformed=log(diff.svl-min(diff.svl)+0.001),
         diff.svl.inverse=1/(diff.svl+0.001),
         diff.weight=next.weight-Weight,
         first.weight.standardized=(Weight-mean(Weight, na.rm=TRUE))/
           sd(Weight, na.rm=TRUE),
         diff.weight.transformed=log(diff.weight-min(diff.weight, na.rm=TRUE)+0.001),
         diff.weight.inverse=1/(diff.weight+0.001),
         breeding.season=factor(breeding.season, levels=c('1','2','3',
                                                           '4','5','6',
                                                           '7','8','9',
                                                           '10')))

final.sex <- growth.dat %>% ### gets to actaul sex, since could be M/F or U
  select(MasterID, Sex) %>%
  unique() %>%
  arrange(MasterID) %>%
  filter(Sex %in% c('M','F')) %>%
  group_by(MasterID) %>%
  mutate(counts=n(),
         Sex=as.character(Sex)) %>%
  ungroup() %>%
  mutate(Sex=ifelse(MasterID==854, 'M', Sex)) %>%
  unique() %>%
  select(-counts) %>%
  rename(sex2=Sex)

bci.dat <- read.csv('bci_data.csv')

growth.dat <- growth.dat2 %>%
  left_join(., final.sex, by='MasterID') %>%
  mutate(sex2=ifelse(is.na(sex2), 'U', sex2)) %>%
  left_join(., bci.dat, by=c('MasterID','date'))%>%
  mutate(diff.bci=next.bci-bci)
head(growth.dat)  

notfemale.dat <- growth.dat %>%
  filter(sex2 %in% c('U','M'))


###########wt vs dry year break up############
growth.dat$is.dry <- ifelse(growth.dat$breeding.season==2 & 3 & 6, 1, 0) 


training.dat <- growth.dat %>%
  filter(is.training==1, in.out.out.in==1)  %>%
  select(diff.svl, growth.type, breeding.season, first.svl.standardized,
         days.standardized) %>%
  na.omit() #filters out everything thats
                        # not going to be used to train the model ie that 
                        # has a 0 in the is.training column in the csv

################ SVL - Growth Model (all sexes) #######
svlallgrowth.mod <- aov(diff.svl ~ growth.type + breeding.season + first.svl.standardized +
                   days.standardized, growth.dat) # this is the model! its
                       # a linear model (what the lm stand for), and each 
                       # following argument are the factors that need to be
                       # considered in the model 
summary(svlallgrowth.mod)
plot(svlallgrowth.mod) #shows 4 graphs of 3 tests of assumptions for the model 
#growth not sig
anova(svlallgrowth.mod)
anova(svlallgrowth.mod, update(svlallgrowth.mod, .~. -growth.type), test='Chisq')
anova(svlallgrowth.mod, update(svlallgrowth.mod, .~. -breeding.season), test='Chisq')

########### SVL - Growth Model (notfemale) ##########
svlnotfemalegrowth.mod <- aov(diff.svl ~ growth.type + breeding.season + first.svl.standardized +
                                days.standardized, notfemale.dat)
#growth not sig
summary(svlnotfemalegrowth.mod) #no signif between growth type
plot(svlnotfemalegrowth.mod) 
anova(svlnotfemalegrowth.mod)
anova(svlnotfemalegrowth.mod, update(svlnotfemalegrowth.mod, .~. -growth.type), test='Chisq')
anova(svlnotfemalegrowth.mod, update(svlnotfemalegrowth.mod, .~. -breeding.season), test='Chisq')

##looking at data distribution, trying to figure out what our 
### min date cutoff should be to minimize the effects of measuremnt error
notfemale.dat%>% 
    select(MasterID, diff.svl, diff.weight, days.between.captures)%>%
    View('Selected columns from growth.dat')

growth.dat%>% 
  select(MasterID, diff.svl, days.between.captures, Date,last.date, 
         breeding.season, SVL_1stMeasurement, next.first.svl, 
         mean.first.size, mean.next.first.svl)%>%
  filter(breeding.season==6)%>%
  View()


training1.dat <- growth.dat %>%
  filter(in.out.out.in==1, days.between.captures>4) %>%
  select(MasterID, SVL_1stMeasurement, next.first.svl, mean.first.size, mean.last.size, diff.svl, days.between.captures)


################SVL-Scatterplot####
ggplot(growth.dat, aes (x=breeding.season, y=diff.svl))+
  stat_smooth(method='lm', formula=y~x )+
  geom_hline(yintercept = 0, linetype=2, color="#495B64")+
  geom_point()


length(unique(growth.dat[["MasterID"]]))
mean(growth.dat[["days.between.captures"]])


######### weight growth model (nonfemales) ########
## growth model <- only nonfemales
weightnotfemalegrowth.mod <- aov(diff.weight ~ growth.type + breeding.season + first.weight.standardized +
                                days.standardized, notfemale.dat)

##SIG growth type
summary(weightnotfemalegrowth.mod)
plot(weightnotfemalegrowth.mod) 
anova(weightnotfemalegrowth.mod)
anova(weightnotfemalegrowth.mod, update(weightnotfemalegrowth.mod, .~. -growth.type), test='Chisq')
anova(weightnotfemalegrowth.mod, update(weightnotfemalegrowth.mod, .~. -breeding.season), test='Chisq')

hist(notfemale.dat$diff.weight)

expand.grid(growth.type=c('upland','pond'),
            breeding.season='2',
            first.weight.standardized=0, days.standardized=0) %>%
  mutate(preds=predict(weightnotfemalegrowth.mod, newdata=.),
         se=predict(weightnotfemalegrowth.mod, newdata=., se=TRUE)$se.fit) %>%
  ggplot(., aes(x=growth.type)) +
  geom_errorbar(aes(ymin=preds-se, ymax=preds+se), width=0.5) +
  geom_point(aes(y=preds)) 

ggplot(notfemale.dat, aes (x=days.between.captures, y=diff.weight, 
                      color=growth.type))+
  stat_smooth(method='lm', formula=y~x)+
  geom_hline(yintercept = 0, linetype=2, color="#495B64")+
  geom_point()+
  labs(y="Standardized Difference in Mass (g)", x="Number of Days Between Captures")+
  scale_color_manual("Period", values=c("#495B64","#D86128"),
                     labels=c("Breeding\nSeason", "Non-Breeding\nSeason"))


######### weight growth model (all sexes) #######
weightallgrowth.mod <- aov(diff.weight ~ growth.type + breeding.season + first.weight.standardized +
                                   days.standardized, growth.dat)

summary(weightallgrowth.mod)
plot(weightallgrowth.mod) 
anova(weightallgrowth.mod)
anova(weightallgrowth.mod, update(weightallgrowth.mod, .~. -growth.type), test='Chisq')
anova(weightallgrowth.mod, update(weightallgrowth.mod, .~. -breeding.season), test='Chisq')

######## BCI model ######## 
bcigrowth.mod <- aov(diff.bci ~ growth.type + breeding.season + bci +
                             days.standardized, growth.dat)

summary(bcigrowth.mod)
plot(bcigrowth.mod) 
anova(bcigrowth.mod)
anova(bcigrowth.mod, update(bcigrowth.mod, .~. -growth.type), test='Chisq')
anova(bcigrowth.mod, update(bcigrowth.mod, .~. -breeding.season), test='Chisq')

######## weights v SVL Scatterplot######## 

ggplot(growth.dat, aes (x=SVL_1stMeasurement, y=Weight, color=sex2))+
  stat_smooth(method='lm', formula=y~x )+
  geom_hline(yintercept = 0, linetype=2, color="#495B64")+
  geom_point()

############# Weight Box Plot ################
ggplot(.dat, aes(x=growth.type, y=diff.weight)) + 
  geom_hline(yintercept = 0, linetype=2, color="#495B64")+
  geom_boxplot(color="#4E4748")



