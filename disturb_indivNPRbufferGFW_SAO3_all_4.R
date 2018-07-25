

# Compare rates of disturbances between NPR and surrounding buffers (500, 1000, 5000m)
# make one by one NPR and its buffers (100, 500, 1000, 2000 m)
# disturbnace data: SAO and GFW
# buffers do not overlap!! - 

#Need to accomplish:
#   read npr - get shape_Area (total NPR)
#   read npr_buff - get shape area (total buffer)
#   read finBUffSMK - sum SMK per buffer (get sum area per buff)
#   read finNPRSMK - calculate sum SMK per NPR (get sum area per NPR) (teh same for various buffers)
#   calculate % of damage by years between NPR and BUFFERS
#   buffer data caontains buffers 100, 500, 1000 and 2000 m
# example - for only one buffer: 500 m 

# PROCESS:
# need to join the tables by common field
# two gridcodes !! use the one with 0-17 values 
# (0 = forest,  NO DAMAGE !!!
#  1 = 2001
#  2 = 2002...
# 16 = 2016

# Keep only SAO

# needs to be the same: Buffer (ORIG_FID) = NPR (FID_NPRSmrecinyAll) 
# calculate % of damage by years between NPR and BUFFERS
# area: in m2

# Example to set the correct encoding
#Encoding(nprSMK$NAM)<-"UTF-8"
#Encoding(nprSMK$PSB)<-"UTF-8"


# *---------------------------------------

require(foreign)

Sys.setlocale(category = "LC_ALL", locale = "Slovak")

# set working directory
setwd("C:/Users/maycca/disk/Projects/2017_deforestSVKchu2/FinLoc_2/outTables")

# Read nprSMK - for smk area per NPR - all are the same
nprSMK <- read.dbf("iNprSMK.dbf", as.is = T) # read characters as characters, not as factors

# replace NA values (not considered as character to character NA values (NoValue))
nprSMK[is.na(nprSMK)] <- "NoValue"


# Read buffSMK - # for smk area per buffer for disturbances
buffSMK <- read.dbf("iBuffSMK.dbf", as.is = T)
buffSMK[is.na(buffSMK)] <- "NoValue"

# Read disturbnaces data: NPR, buffs
# ======================================

# Read intersected NPR, SMK and SAO
nprSMKsao <-read.dbf("iNPRSmkSAO.dbf", as.is = T)  # gridcode_1 = age (vek porastov)
nprSMKsao[is.na(nprSMKsao)] <- "NoValue"

# Read intersected BUFF, SMK and SAO
buffSMKsao <- read.dbf("iBuffSmkSAO.dbf", as.is = T)
buffSMKsao[is.na(buffSMKsao)] <- "NoValue"


# Aggregate disturbance area by distances, by location
# -----------------------------

# for NPR
nprSMKsao2<-aggregate(Shape_Area ~ gridcode + NAM + PSB, nprSMKsao, sum)  #, na.action = na.pass

# for buff
buffSMKsao_2<-aggregate(Shape_Area ~ gridcode + distance + NAM + PSB, buffSMKsao, sum) 




# Normalize data
# ----------------------------------
# : calculate the % of forest damage (SAO) per 
# undisturbed forest for
 # NPR
 # for buffer 100, 500, 1000, 2000  
 # change columns names

# sum original forest for each location 
sum_NPRforest <-aggregate(Shape_Area ~ NAM + PSB, nprSMK, sum)

# for original forest for buffers for each location
sum_buffSMK<-aggregate(Shape_Area ~ NAM + distance + PSB, buffSMK, sum)

# rename columns names
names(sum_NPRforest) <- c("NAM", "PSB", "origForest")
names(sum_buffSMK) <- c("NAM", "distance", "PSB", "origForest")


# add original forest area to disturbances in NPR, Buff
# for NPR
finNPRsao <-merge(nprSMKsao2,     # 109 
                  sum_NPRforest,  # 117
                  by = c("NAM", "PSB"), all = T)

# replace NA by 0, as there are more localities than there are disturbances
finNPRsao$Shape_Area[is.na(finNPRsao$Shape_Area)]<-0
# anyNA(finNPRsao$Shape_Area)

# for buffers
finBUFFsao <-merge(buffSMKsao_2, sum_buffSMK, by = c("NAM", "PSB", "distance"), all = T)
finBUFFsao$Shape_Area[is.na(finBUFFsao$Shape_Area)]<-0

#anyNA(finBUFFsao$Shape_Area)

length(unique(paste(finNPRsao$NAM, finNPRsao$PSB)))

length(unique(paste(finBUFFsao$NAM, finBUFFsao$PSB)))


# Convert Distance to a factor, 
# add factor NPR to natural reserves
finNPRsao$distance <- rep("NPR", nrow(finNPRsao))

# Change columns order
finNPRsao<- finNPRsao[c("NAM", "PSB","distance", "gridcode", 
                        "Shape_Area", "origForest")] 

# rename NPR and BUFF again
names(finNPRsao)<-c("NAM", "PSB","distance", "gridcode", 
                    "Shape_Area", "origForest")

names(finBUFFsao)<-c("NAM","PSB", "distance", "gridcode",
                     "Shape_Area", "origForest")




# =================================
# Create final tab
# =================================


# put tables together
tab<-rbind(finNPRsao, finBUFFsao)

# Set correct encoding for Slovak language
Encoding(tab$NAM)<-"UTF-8"
Encoding(tab$PSB)<-"UTF-8"


(head(tab))

# check reserves with NoValues (PSB); complete those data
unique(subset(tab, PSB == "NoValue")$NAM)
unique(tab$PSB)

# check replace statement
tab$PSB[tab$NAM == "Ladonhora"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Hricovec"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Javorinka"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Klokocovsk� sk�lie"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Kloptan"] <- "S NP Slovensky kras"
tab$PSB[tab$NAM == "Mal� Polom"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Velk� Raca"] <- "S CHKO Kysuce"
tab$PSB[tab$NAM == "Velk� Polom"] <- "S CHKO Kysuce"


# add 2000 to indicate the year
tab$gridcode <- tab$gridcode + 2000


# keep data from original forest, keeep only origForest columns and PSB, NAM
tab.orig.forest<-subset(tab, select=-c(gridcode,Shape_Area))

# get rid of duplicated values
tab.orig.forest<-tab.orig.forest[!duplicated(tab.orig.forest), ]


head(tab.orig.forest)



# calculate cumulative values for rates and areas
# need to cumulate values by NAM, PSB and by location
#########################################################

tab$comb<-paste(tab$NAM, tab$PSB, tab$distance, sep = "\n")




#####################################
# Complete years per locations
#####################################

# complete years per each comb (distance & PSB & NAM)
# it will remove the NAM, DIST, and PSB - will be added later

require(tidyr)

tab<-as.data.frame(complete(tab, comb, gridcode = 2006:2017, fill = list(Shape_Area = 0)))


# convert back NAm & PSB by comb column
# split string by character

library(dplyr)

tab<-tab %>%
  separate(comb, c("NAM", "PSB", "distance"), "\n")

# add indication of uniqueLoc & combination
tab$uniqueLoc<-paste(tab$NAM, tab$PSB, sep = "\n")
tab$comb<-paste(tab$NAM, tab$PSB, tab$distance, sep = "\n")


# merge values of original forest
tab<-merge(tab, tab.orig.forest, by = c("NAM", "PSB", "distance"), all = F)

head(tab)


# caculate rate of disturbed forests over years
tab$rate <- tab$Shape_Area * 100 / tab$origForest.y

# Calculate cumulative absolute area values by name
tab$cumArea<-ave(tab$Shape_Area, tab$comb, FUN = cumsum) 


# Calculate cumulative rates  by name
tab$cumRate<-ave(tab$rate, tab$comb, FUN = cumsum) 

head(tab)

# reorder the factors (distance)
tab$distance<-factor(tab$distance,
                     levels = c("NPR", "100", "500", "1000", "2000"))




#####################################
# Export final table to create plots
#####################################

tab.e<-subset(tab, select = -c(origForest.x))

colnames(tab.e) <- c("NAM", "PSB","distance", "year",  
                     "area_m2", "uniqueLoc","comb",
                     "origForest_m2", "forestRate", 
                     "cumArea","cumRate")

write.table(tab.e, "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/tabFinSAO.csv", sep = ",")



#####################################
# Export list of locations
#####################################


library(stringr)

tab.loc<-str_split_fixed(unique(tab$uniqueLoc), "\n", 2)
colnames(tab.loc)<-c("Nazov", "Posobisko")

write.table(tab.loc, "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/tab_zoznam.csv", sep = ",")



















######################################
# check NR without disturbances
######################################

unique(subset(tab, cumRate == 0 & distance == "NPR" & gridcode == 2017)$comb)

# [1] "Havranie skaly\nS CHKO Polana\nNPR"          
# [2] "Javorinka\nS CHKO Kysuce\nNPR"               
# [3] "Kunovo\nS TANAP\nNPR"                        
# [4] "Lubietovsk� Vepor\nS CHKO Polana\nNPR"       
# [5] "Masianske skalky\nS NP Mur�nska planina\nNPR"
# [6] "Pod Dud�som\nS CHKO Polana\nNPR"             
# [7] "Sokolec\nS CHKO Ponitrie\nNPR"               
# [8] "Star� hrad\nS NP Mal� Fatra\nNPR"   


########################################
# the highest peak of disturbances???
########################################

# define colours
cols = c("NPR" = "green",
         "100" = "orange",
         "500" = "red",
         "1000" = "blue",
         "2000" = "black")



head(tab)



# cumRate
#####################

# find cumul.max per locality, find 
# in which distance the cumul.max is found


# fidn max CumRate per locality
tab.max<-tab[with(tab, cumRate == ave(cumRate, uniqueLoc, FUN= max)),]


aggregate(cumRate ~ uniqueLoc + distance, data = tab, max)



# remove duplicated values
tab.max<-tab.max[!duplicated(tab.max$cumRate), ]

(t<-table(tab.max$distance))

# NPR  100  500 1000 2000 
#  12   19   23   23   40

round(prop.table(t),2)

# NPR  100  500 1000 2000 
# 0.10 0.16 0.20 0.20 0.34 

require(ggplot2)
require(reshape2)

windows(4, 3)
ggplot(melt(t),
       aes(x = Var1, 
           y = value,
           colour = 'black')) +
  geom_bar(stat = "identity",
           fill = cols) +
  scale_colour_manual(values = cols) +
  theme_bw() + 
  ggtitle('Počet najvyšších kumulatívnych podielov\npoškodenia v lokalite a jej buffroch') + 
  labs(y = "počet",  x = "NPR a buffre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none",
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 




# Cumulative values by years
tt<-table(tab.max$distance, tab.max$gridcode)

# 2006 2009 2012 2013 2015 2016 2017
# NPR     0    0    1    3    1    1    6
# 100     0    0    2    5    1    0   11
# 500     0    0    0    0    1    4   18
# 1000    0    1    0    1    1    2   18
# 2000    1    0    0    3    0    3   33

# export table to clipboard, keep formatting
write.csv(tt, "clipboard", row.names = TRUE)




# create matrix plot
###############################

require(reshape2)


# set zero value to no data
tt[tt == 0]<-NA

# reverse row order in table (matrix)
tt<-tt[nrow(tt):1, ]


windows(3.5,3)
ggplot(melt(tt), 
       aes(x = factor(Var2), 
           y = Var1)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  theme_bw() + 
  xlab("") + 
  ylab("") +
  scale_fill_gradient("Počet najvyššieho\npodielu poškodenia", 
                      low = "yellow", 
                      high = "red", 
                      na.value = "white") +
  theme(legend.position = "bottom")








# Rate
#####################

# find max rate per locality
tab.max<-tab[with(tab, rate == ave(rate, uniqueLoc, FUN= max)),]

(t<-table(tab.max$distance))

windows()
barplot(t)

# NPR  100  500 1000 2000 
#   8   23   26   26   34

barplot(round(prop.table(t),2))


(round(prop.table(t),2))
# NPR  100  500 1000 2000 
# 0.07 0.20 0.22 0.22 0.29 

# convert t to long format
require(reshape2)

melt(t)


windows(4, 3)
ggplot(melt(t),
       aes(x = Var1, 
           y = value,
           colour = 'black')) +
  geom_bar(stat = "identity",
           fill = cols) +
  scale_colour_manual(values = cols) +
  theme_bw() + 
  ggtitle('Počet najvyšších podielov poškodenia\nv NPR a jej buffroch') + 
  labs(y = "počet",  x = "NPR a buffre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none",
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 





tab.max<-tab[with(tab, rate == ave(rate, uniqueLoc, FUN= max)),]



tt<-table(tab.max$distance, tab.max$gridcode)
tt

# 2006 2007 2009 2010 2011 2012 2013 2015 2016 2017
# NPR     3    0    1    1    0    1    0    1    1    0
# 100     4    1    2    5    1    2    1    5    2    0
# 500     3    1    4    4    2    4    1    5    1    1
# 1000   10    1    5    0    1    2    1    5    0    1
# 2000    7    2    3    0    1    5    3    9    1    3


# export table to clipboard, keep formatting
write.csv(tt, "clipboard", row.names = TRUE)



# create matrix plot

require(reshape2)


# create heat map with values
######################################

tt[tt == 0]<-NA

# reverse row order in table (matrix)
tt<-tt[nrow(tt):1, ]


windows(4,3)
ggplot(melt(tt), 
            aes(x = factor(Var2), 
                y = Var1)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  theme_bw() + 
  xlab("") + 
  ylab("") +
  scale_fill_gradient("Počet najvyššieho\npodielu poškodenia", 
                        low = "yellow", 
                        high = "red", 
                      na.value = "white") +
  theme(legend.position = "bottom")






# ==========================================================
# beginning of disturbance: where?? in buff or in NPR???
# ==========================================================


# find the max value and the zone where the max belongs to
# https://stackoverflow.com/questions/51045930/r-find-the-year-when-one-value-is-higher-then-other-by-category/51046127?noredirect=1#comment89094139_51046127

startSpread<-tab %>% group_by(uniqueLoc, gridcode) %>%
  filter(max(cumRate) != min(cumRate)) %>%
  arrange(uniqueLoc, gridcode, desc(cumRate)) %>%
  group_by(uniqueLoc) %>%
  slice(1)


# Export table of locations with indication of distances
outLoc <- data.frame(startSpread[,1:4])

# add unique ID
outLoc$id<-1:nrow(outLoc)

outLoc$unique<-paste(outLoc$NAM, outLoc$PSB, sep = " ")


(outLoc)

# export table of start of disturbnace by location - to GIS as well
write.table(outLoc, "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/loc_startGIS.csv", 
            sep = ",", col.names = T)



# A tibble: 1 x 12
# Groups:   uniqueLoc [1]
#NAM     PSB      distance gridcode Shape_Area origForest.x uniqueLoc           comb                     origForest.y   rate cumArea cumRate
#<chr>   <chr>    <fct>       <dbl>      <dbl>        <dbl> <chr>               <chr>                           <dbl>  <dbl>   <dbl>   <dbl>
#  1 Salatín S NAPANT NPR          2006      8100.     8135738. "Salatín\nS NAPANT" "Salatín\nS NAPANT\nNPR"     8135738. 0.0996   8100.  0.0996

# get summary values per unique location 
tt<-table(startSpread$uniqueLoc, startSpread$distance)


# replace \n by space to copy table easier
rownames(tt)<-gsub("\n", " ", rownames(tt))
tt<-cbind(tt, id = c(1:nrow(tt)))

# export table of start of disturbnace by location
write.table(tt, "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/loc_start.csv", sep = ",")



# get summary valuer per year
tt<-table(startSpread$gridcode, startSpread$distance)




# Get data for salatin
tt<-subset(tab, NAM == "Salatín" & gridcode == 2006, select = c("NAM", "distance", "cumRate"))


# export table to clipboard, keep formatting
write.csv(tt, "clipboard", row.names = TRUE)




# create lollipop plot

head(tab)

# add values for two catergories: NPR or buffer
tab$npr_buff<-ifelse(tab$distance == "NPR", "NPR", "buffer")


# Library
library(tidyverse)
library(ggplot2)


# calculate for each location: NPR - cum rate, mean values cum rate for buffers
head(tab)

# get cumRate in 2017 for NPR per location
npr_cumRate<- aggregate(cumRate ~ uniqueLoc, subset(tab, distance == "NPR"& gridcode  == 2017), max)

# rename dataframe
names(npr_cumRate)<-c("uniqueLoc", "npr")

buff_cumRate<-aggregate(cumRate ~ uniqueLoc, subset(tab, distance != "NPR" & gridcode  == 2017), mean)

#rename dataframe
names(buff_cumRate)<-c("uniqueLoc", "buff")


# merge by uniqueLoc
lollipop <- merge(npr_cumRate,buff_cumRate,by="uniqueLoc")

# split uniqueLoc into two columns: name + PSB

library(stringr)
lollipop<-cbind(lollipop, str_split_fixed(lollipop$uniqueLoc, "\n", 2))

names(lollipop)<-c( "uniqueLoc", "npr",  "buff","Nazov",  "PSB")

# subset lollipop by PSB
lollipop = subset(lollipop, PSB == "S TANAP")


# Reorder data using average?
lollipop = lollipop %>% rowwise() %>% mutate( mymean = mean(c(npr,buff) )) %>% arrange(mymean) %>% mutate(Nazov=factor(Nazov, Nazov))

# plot
windows()
ggplot(lollipop) +
  geom_segment( aes(x=Nazov, 
                    xend=Nazov, 
                    y=npr, 
                    yend=buff), 
                color="darkgrey") +
  geom_point( aes(x=Nazov, 
                  y=npr), 
              color="#F0D154", size=3 ) +
  geom_point( aes(x=Nazov, 
                  y=buff), 
              color= "#323FEF", 
              size=3 ) + # rgb(0.7,0.2,0.1,0.5)
  #scale_color_manual(values = c("Women" = '#ff00ff','Men' = '#3399ff'))
  coord_flip() +
  theme_bw() +
  theme_minimal() +
  theme(panel.border = element_blank(),
        legend.position = "bottom",
        panel.grid.minor=element_blank()) +
  xlab("S TANAP") +
  ylab("kumulat�vna hodnota\npodielu poskodenia")





# ============================================================================
# classify the disturbances by beginning of disturbances - in NPR, in Buff
# ============================================================================

# where the disturbnaces started first?
# use absolute areas (m2):
# find locations 
# 

head(tab)

tab.sub<- tab[tab$distance!="1000" & tab$distance !="2000", ]

# split data in 2 categories: NPR, buff
tab.sub$npr_buff<-ifelse(tab.sub$distance == "NPR", "NPR", "buffer")


# bark beetle spread only locations <500m within 1 year
# keep only buffers 100 & 500m
#tab.sub<-subset(tab, distance != "1000" & distance != "2000")# 
tab.sub$distance<-factor(tab.sub$distance)

# tab with summary values for shape_area NPR, buff
tab2<-aggregate(Shape_Area ~ uniqueLoc + npr_buff + gridcode, tab.sub, sum)


str(tab2)





# create a function to evaluate the where disturbnace started
# check values in first year (t1): are not 0 (Shape_area is > 0)

# if NPR = 0 OR buff = 0: 

#         if NPR > 0 and buff = 0 -> source == "NPR"
#         if NPR = 0 & buff > 0 -> source == "buff"

# if NPR > 0 & buff > 0 OR NPR = 0  & buff = 0 

# keep value of disturbance in NPR
# sum disturbances in t2
# get ratio between dist in NPR t1

#          if sum t2:NPR <= 5 -> source = "NPR"
#          if sum t2:NPR > 5 -> source = "buff"



# example on one locality
# ======================================

# try on one location
df<-subset(tab2, uniqueLoc == unique(tab2$uniqueLoc)[6])

# create column to source of infestations
df$infSource<-rep("NA", nrow(df))

# get the first and second year of disturbnaces
t1 = min(df$gridcode[df$Shape_Area > 0]) # min(df$gridcode)
t2 = t1 + 1

# include the year of disturbance
df$distYear<- rep(t1, nrow(df))

# compare Shape_area values in first year with zero
npr_t1 <- subset(df, gridcode == t1 & npr_buff == "NPR")$Shape_Area
buff_t1<-subset(df, gridcode == t1 & npr_buff == "buffer")$Shape_Area


# Sum disturbances in buff in t2
npr_t2 <- subset(df, gridcode == t2 & npr_buff == "NPR")$Shape_Area
buff_t2<-subset(df, gridcode == t2 & npr_buff == "buffer")$Shape_Area
sum_t2<-npr_t2 + buff_t2

# get ratio between sum_t2 and npr_t1 
ratio = sum_t2/npr_t1

if (npr_t1 == 0 | buff_t1 == 0)

  if (npr_t1 > 0 & buff_t1 == 0)
    df$infSource<-rep("NPR", nrow(df))

  if (npr_t1 == 0 & buff_t1 > 0)
    df$infSource<-rep("buff", nrow(df))


ifelse(ratio <= 5, 
       df$infSource<-rep("NPR",nrow(df)), 
       df$infSource<-rep("buff",nrow(df)))


df

unique(tab$uniqueLoc)

# try to make function

# create a function 

getDisturbOrigin<-function(df, na.rm = TRUE, ...) {
  
  # create column to source of infestations
  df$infSource<-rep("NA", nrow(df))
  
  # get the first and second year
  t1 = min(df$gridcode[df$Shape_Area > 0])
  t2 = t1 + 1
  
  # add disturbance year to locality
  df$distYear <- rep(t1, nrow(df))
  
  # compare Shape_area values in first year with zero
  npr_t1 <- subset(df, gridcode == t1 & npr_buff == "NPR")$Shape_Area
  buff_t1<-subset(df, gridcode == t1 & npr_buff == "buffer")$Shape_Area
  
  
  # if NPR & buff > 0 - go to next year
  if (npr_t1 == 0 | buff_t1 == 0) 
    
    if (npr_t1 > 0 & buff_t1 == 0) 
      df$infSource<-rep("NPR", nrow(df))
  
    if (npr_t1 == 0 & buff_t1 > 0) 
      df$infSource<-rep("buff", nrow(df))
  
  
  # Sum disturbances in buff in t2
  npr_t2 <- subset(df, gridcode == t2 & npr_buff == "NPR")$Shape_Area
  buff_t2<-subset(df, gridcode == t2 & npr_buff == "buffer")$Shape_Area
  sum_t2<-npr_t2 + buff_t2
  
  # get ratio between sum_t2 and npr_t1 
  ratio = sum_t2/npr_t1
  
  # complete the infestation sources based on the ratio 
  ifelse(ratio <= 5, 
         df$infSource<-rep("NPR",nrow(df)), 
         df$infSource<-rep("buff",nrow(df)))
  
  
  df
}
  
  
getDisturbOrigin(df)







# split tab in multiple dataframes by locations, get the values
locOrig<-lapply(split(tab2, tab2$uniqueLoc), function(d) { getDisturbOrigin(d) })

# convert list to dataframe
locOrig.df<-do.call(rbind.data.frame, locOrig)


# get location and inf source
locInfSource<-unique(cbind(locOrig.df$uniqueLoc, locOrig.df$infSource, locOrig.df$distYear ))


(locInfSource)

# summarize results
table(locInfSource[,2]) 


# export table of start of disturbnace by location
# add names, NAZOV and PSB,

outTab<-cbind(locInfSource, data.frame(do.call('rbind', strsplit(as.character(locInfSource[,1]),"\n",fixed=TRUE))))

# change order of columns
outTab<-outTab[,c("X1","X2",1,2,3)]

# get rid of the unique columns (third columns, unique combination location and PSB)
outTab<-outTab[,-3]

# rename output table
names(outTab)<-c("NAZOV", "PSB", "zaciatok", "rok")



write.csv(outTab, "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/disturb_startComp2years.csv")



 

# ?? what if there are multiple locations without disturbnaces in 2006, 2007??
# split tab in list by uniqueLoc, apply the function to every uniqueLoc and merge list in one table
# get table of locations: where disturbnaces started first?? in NPR or in buff?




# try to get a function
# ===========================









# Create data
value1=abs(rnorm(26))*2
data=data.frame(x=LETTERS[1:26], value1=value1, value2=value1+1+rnorm(26, sd=1) )

# Reorder data using average?
data = data %>% rowwise() %>% mutate( mymean = mean(c(value1,value2) )) %>% arrange(mymean) %>% mutate(x=factor(x, x))

# plot
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip() 

# With a bit more style
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")





###########################################
#    Create plots
##########################################




require(ggplot2)

# folder to store output files
outPath <- "C:/Users/maycca/disk/Projects/2017_deforestSVKchu3/FinLoc_2/outTables/" 

#####

# hard define colors
cols = c("NPR" = "green",
         "100" = "orange",
         "500" = "red",
         "1000" = "blue",
         "2000" = "black")


# Format number as fixed width, with leading zeros
# https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros



#############################
#
# Line Culmul rate Plot
#
############################


# modify the output name of file
a <- seq(1,101,25)
sprintf("name_%03d", a)
#  "name_001" "name_026" "name_051" "name_076" "name_101"


################################
# try
################################

tab1= subset(tab, NAM == "Salatín")
#windows(4.5, 3.5)

lineCumRateGraph <- function(tab, na.rm = TRUE, ...) {
  
  # Create list of locations
  type_list <-unique(tab$uniqueLoc)
  
  # Create a for loop to produce ggpot plots
  for (i in seq_along(type_list)) {
    
    # create a plot for each loc in df
    plot<-
      
      ggplot(subset(tab, tab$uniqueLoc == type_list[i]),
             aes(x = gridcode, 
                 y = cumRate, 
                 colour = distance,
                 group = distance)) +
      geom_line(aes(linetype = distance),
                size = 1) +
      scale_x_continuous(breaks = c(2006:2017),
                         labels = c(2006:2017),
                         limits = c(2006, 2017)) +
      #xlim(2006,2017) + 
      ggtitle(type_list[i]) +
      labs(y = "kumulatívny podiel\npoškodenia lesa",  
           x = "roky",
           linetype = "NPR a buffre")  +
      scale_colour_manual(name= "NPR a buffre",
                          values = cols) + #  c("green", "orange", "red", "blue", "black")
      theme_bw() +
      scale_shape(name= "NPR a buffre",
                  solid=FALSE) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="bottom") 
    
    print(plot)
    ggsave(plot,
           width = 4.5, height = 3.5,
           file = paste(outPath,
                        sprintf("lineCumRate_%03d", i),
                        ".png",
                        sep=''),
           dpi = 300)
  }
}

lineCumRateGraph(tab)












#############################
#
# Bar Rate Plot
#
############################



barRateGraph <- function(tab, na.rm = TRUE, ...) {
  
  # Create list of locations
  type_list <-unique(tab$uniqueLoc)
  
  # Create a for loop to produce ggpot plots
  for (i in seq_along(type_list)) {
    
    # create a plot for each loc in df
    plot<-
      ggplot(subset(tab, tab$uniqueLoc == type_list[i]),
             aes(x = gridcode, 
                 y = rate)) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = c(2007:2017),
                         labels = c(2007:2017),
                         limits = c(2007, 2017)) +
      facet_grid( .~ distance) +
      ggtitle(type_list[i]) +
      theme_bw() + 
      labs(y = "podiel poškodenia lesa (%)",  
           x = "roky") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="none",
            #panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black")) 
    
    #windows(width = 8, height = 3.5)
    print(plot)
    ggsave(plot,
           width = 8, height = 3.5,
           file = paste(outPath,
                        sprintf("barRate_%03d", i),
                        ".png",
                        sep=''),
           dpi = 300)
  }
}


#barRateGraph(subset(tab, uniqueLoc == "Äumbier\nS NAPANT"))
# TANAP: subset only data with gridcode > 2005
#barRateGraph(subset(tab, gridcode > 2006))
barRateGraph(tab)





##########################
#
# Bar absolute area
#
#####################


barAbsoluteGraph <- function(tab, na.rm = TRUE, ...) {
  
  # Create list of locations
  type_list <-unique(tab$uniqueLoc)
  
  # Create a for loop to produce ggpot plots
  for (i in seq_along(type_list)) {
    
    # create a plot for each loc in df
    plot<-
      ggplot(subset(tab, tab$uniqueLoc == type_list[i]),
             aes(x = gridcode, 
                 y = Shape_Area/10000)) +
      geom_bar(stat = "identity") +
      ggtitle(type_list[i]) +
      facet_grid( .~ distance) +
      scale_x_continuous(breaks = c(2007:2017),
                         labels = c(2007:2017),
                         limits = c(2007, 2017)) +
      theme_bw() + 
      labs(y = "plocha (ha)",  
           x = "roky") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="none",
            #panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black")) 
    
    #windows(width = 8, height = 3.5)
    print(plot)
    ggsave(plot,
           width = 8, height = 3.5,
           file = paste(outPath,
                        sprintf("barArea_%03d", i),
                        ".png",
                        sep=''),
           dpi = 300)
  }
}

#barAbsoluteGraph(subset(tab, gridcode > 2006))
barAbsoluteGraph(tab)



###################################################
#
#        CUMULATIVE AREAS LINE PLOTS
#
###################################################

windows()

lineCumAreaGraph <- function(tab, na.rm = TRUE, ...) {
  
  # Create list of locations
  type_list <-unique(tab$uniqueLoc)
  
  # Create a for loop to produce ggpot plots
  for (i in seq_along(type_list)) {
    
    # create a plot for each loc in df
    plot<-
      
      ggplot(subset(tab, tab$uniqueLoc == type_list[i]),
             aes(x = gridcode, 
                 y = cumArea/10000, 
                 colour = distance,
                 group = distance)) +
      geom_line(aes(linetype = distance),
                size = 1) +
      scale_x_continuous(breaks = c(2006:2017),
                         labels = c(2006:2017),
                         limits = c(2006, 2017)) +
      ggtitle(type_list[i]) +
      
     ## facet_grid(~NAM, 
      #           scales = "free") +
      labs(y = "plocha (ha)",  
           x = "roky",
           linetype = "NPR a buffre") +
      scale_colour_manual(name= "NPR a buffre",
                          values = cols) + #  c("green", "orange", "red", "blue", "black")
      theme_bw() +
      scale_shape(name= "NPR a buffre",
                  solid=FALSE) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="bottom")
    
   # windows(width = 4, height = 3.5)
    print(plot)
    ggsave(plot,
           width = 4, height = 3.5,
           file = paste(outPath,
                        sprintf("lineCumArea_%03d", i),
                        ".png",
                        sep=''),
           dpi = 300)
  }
}

lineCumAreaGraph(tab)





########################################
#
#           Summary plots
#
########################################



library(ggplot2)


# podiel poskodenia v jednotlivych rokoch a lokalitach
############################################


windows(7,3.5)
ggplot(tab[complete.cases(tab),],  # remove rows containing NA values (in gridcode)
       aes(x = factor(gridcode),
           y = rate)) + #fill = gridcode 
  geom_boxplot( aes(fill = factor(gridcode)),
               #notch = T,
               outlier.shape = "NA",
               outlier.size = 0.1) + #   #
  facet_grid(.~distance) +
  ggtitle("Podiel poÅ¡kodenia v jednotlivÃ½ch rokoch") +
  labs(fill = "roky",
       x = "vzdialenosÅ¥",
       y = "podiel poÅ¡kodenia (%)") + 
  theme_bw() +
  coord_cartesian(ylim = c(0,65)) +

  # stat_summary(fun.y=mean, # median 
  #              geom="smooth", 
  #              aes(group=distance,
  #                  colour = distance)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none") 


# podiely poskodenia podla lokalit
##################################3
windows(4.5,3)
ggplot(tab[complete.cases(tab),],  # remove rows containing NA values (in gridcode)
       aes(x = factor(distance),
           y = rate)) + 
  geom_boxplot( aes(fill = factor(distance)),
                fill = cols,
                colour = "black",
                notch = T,
                outlier.shape = "NA",
                outlier.size = 0.0) + #   #
  
  ggtitle("Podiel poÅ¡kodenia v NPR\na jej buffroch") +
  theme_bw() + 
  labs(fill = "vzdialenosÅ¥",
       x = "vzdialenosÅ¥",
       y = "podiel poÅ¡kodenia (%)") +
  coord_cartesian(ylim = c(0,15)) +
  # stat_summary(fun.y=mean, # median 
  #              geom="smooth", 
  #              aes(group=distance,
  #                  colour = distance)) +
  # labs(y = "% from undisturbed forest",  x = "years") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none") 



# podiely poskodenia podla rokov
##################################3
windows(4.5,3)
ggplot(tab[complete.cases(tab),],  # remove rows containing NA values (in gridcode)
       aes(x = factor(gridcode),
           y = rate,
           fill = factor(gridcode))) + 
  geom_boxplot( #aes(fill = factor(distance)),
               # fill = cols,
                #colour = factor(gridcode),
                notch = T,
                outlier.shape = "NA",
                outlier.size = 0.0) + #   #
  
  ggtitle("Podiel poÅ¡kodenia v NPR\na jej buffroch") +
  theme_bw() + 
  labs(fill = "vzdialenosÅ¥",
       x = "roky",
       y = "podiel poÅ¡kodenia (%)") +
  coord_cartesian(ylim = c(0,25)) +
  # stat_summary(fun.y=mean, # median 
  #              geom="smooth", 
  #              aes(group=distance,
  #                  colour = distance)) +
  # labs(y = "% from undisturbed forest",  x = "years") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="none") 



##### NO TANAP
windows()
ggplot(subset(tab, tab$PSB != "S TANAP"),
       aes(x = factor(distance),
           y = rate, 
           fill = distance)) + 
  geom_boxplot( #aes(fill = factor(gridcode)),
    notch = T,
    #outlier.shape = "NA",
    outlier.size = 0.1) + #   #
  
  # ggtitle() +
  theme_bw() + 
  coord_cartesian(ylim = c(0,20)) +
  # stat_summary(fun.y=mean, # median 
  #              geom="smooth", 
  #              aes(group=distance,
  #                  colour = distance)) +
  # labs(y = "% from undisturbed forest",  x = "years") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") 
































###################################################
#             EXAMPLES
###################################################


df<- data.frame(loc = rep(c("l1", "l2"), each = 3),
                name = rep(c("A", "B"), 3),
                grid = c(5,6,7,2,3,5),
                area = c(5,10,1,1,3,1),
                areaOrig = rep(c(20, 10, 5), each = 2))

df2<-rbind(df, df)

df2$type = rep(c("y", "z"), each = 6)

# calculate rate:
df2$rate = df2$area / df2$areaOrig *100

# Calculate cumulative values by name
df2$cumArea<-ave(df2$area, df2$name, FUN = cumsum) 

# Calculate cumulative values by name
df2$cumRate<-ave(df2$rate, df2$name, FUN = cumsum) 


# create qqplot by function?

require(ggplot2)

ggplot(subset(df2, type == "y"), 
       aes(x = grid, 
           y = area)) +
  geom_bar(stat = "identity") +
  facet_grid(loc ~ name)



# create plots

require(ggplot2)

type.graph <- function(df2, na.rm = TRUE, ...) {
  
  # Create list of locations
  type_list <-unique(df2$type)
  
  # Create a for loop to produce ggpot plots
  for (i in seq_along(type_list)) {
    
    # create a plot for each loc in df
    plot<-

      ggplot(subset(df2, df2$type == type_list[i]),
             aes(x = grid, 
                 y = area)) +
        geom_bar(stat = "identity") +
        ggtitle(type_list[i]) +
        facet_grid(loc ~name)
    windows()
    
    print(plot)
  }
}


type.graph(df2)


###############################

#significant SNPs
type_list <- unique(df2$type)

#create list of ggplots per type
p_re <-
  lapply(type_list, function(i){
    
    ggplot(subset(df2, type == type_list[i]), 
           aes(x = grid, 
               y = area)) +
      geom_bar(stat = "identity")
    
  })

#assign names
names(p_re) <- type_list

#plot
p_re$y


##################################

# https://stackoverflow.com/questions/48527446/subset-and-plot-data-by-for-loop-lappy-r-ggplot2/48527782?noredirect=1#comment84051781_48527782

library(tidyverse)

by_type <- df2 %>% 
  group_by(type) %>% 
  nest() %>% 
  mutate(plot = map(data, 
                    ~ggplot(. ,aes(x = grid, y = area)) +
                      geom_bar(stat = "identity") +
                      ggtitle(.) +
                      facet_grid(loc ~name)))

by_type$plot
# A tibble: 2 x 3



##################################






ggplot(subset(df2, df2$type == "y"),
       aes(x = grid, 
           y = area)) +
  geom_bar(stat = "identity") 




ggplot(df, aes(x = grid, 
               y = area)) +
  geom_bar(stat = "identity") +
  facet_grid(loc ~ name)



# working example Merge tables

df1<-data.frame(grid = c(1:5),
                n = c("a", "a", "a", "b", "b"),
                area = c(1,1,1,3,7))

df2 <- data.frame(n = c("a", "b"),
                  areaOrig = c(10, 20))

#> merge(df1, df2, key = "n")
#n grid area areaOrig
#1 a    1    1       10
#2 a    2    1       10
#3 a    3    1       10
#4 b    4    3       20
#5 b    5    7       20

# identical IDs should be: buffSMK: ORIG_FID, nprSMK: FID_NPRsmr








# -------------------------------------------
# CHECK which NRs are missing,
# not run otherwise

# -------------------------------------------
# SKIPP !!
# -------------------------------------------

length(unique(paste(nprSMKsao$NAM, nprSMKsao$PSB, sep = "_")))
# 109 - v rezervaciach sice bol smrek, ale nebolo tam poskodenia
# check in ArcGIS !!

Encoding(nprSMKsao$NAM)<-"UTF-8"
Encoding(buffSMK$NAM)<-"UTF-8"

Encoding(nprSMKsao$PSB)<-"UTF-8"
Encoding(buffSMK$PSB)<-"UTF-8"


df1<-as.data.frame(sort(unique(paste(buffSMK$NAM, buffSMK$PSB, sep = "_"))))
df2<-as.data.frame(sort(unique(paste(nprSMKsao$NAM, nprSMKsao$PSB, sep = "_"))))

names(df1)<-"name"
names(df2)<-"name"

df2$y = rep(0, nrow(df2))

# identify which localities are missing: 117-109 = 8

merge(df1,df2, by ="name", all = T)


# --------------------------------------------------------



