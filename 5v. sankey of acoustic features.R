
##
## 5v. parallel co-ordinates for af over time (10-year bands)
##

install.packages("GGally")
library(GGally)

parCor <- sp_ArtSongAF
ggparcoord(parCor,columns=c(13,14,15,16,17,18,19,20),groupColumn=4)+
  labs(title="Parallel CoordinateMap of Acoustic Features")

# put year into 10-year bands
parCor1 <- parCor %>% mutate(
  year_group=dplyr::case_when(
    year <= 1969 ~ "< 1969",
    year >= 1970 & year < 1979 ~ "1970-1979",
    year >= 1980 & year < 1989 ~ "1980-1989",
    year >= 1990 & year < 1999 ~ "1990-1999",
    year >= 2000 & year < 2009 ~ "2000-2009",
    year >= 2010 & year < 2019 ~ "2010-2019"
  ),
  year_group = factor(
    year_group,
    level=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019")
  )
)

View(parCor1)
ggparcoord(parCor1,columns=c(13,14,15,16,17,18,19,20),groupColumn=21)+
  labs(title="Parallel CoordinateMap of Acoustic Features")

# get random sample of 1000 records

subset(parCor,duration_ms>250000) # 5000 rows
subset(parCor,duration_ms>300000) # 1748 rows
subset(parCor,duration_ms>350000)
parCor2 <- subset(parCor,duration_ms>250000) 
nrow(parCor2) # 1127 rows

# create year groups
parCor2 <- parCor2 %>% mutate(
  year_group=dplyr::case_when(
    year <= 1969 ~ "< 1969",
    year >= 1970 & year < 1979 ~ "1970-1979",
    year >= 1980 & year < 1989 ~ "1980-1989",
    year >= 1990 & year < 1999 ~ "1990-1999",
    year >= 2000 & year < 2009 ~ "2000-2009",
    year >= 2010 & year < 2019 ~ "2010-2019"
  ),
  year_group = factor(
    year_group,
    level=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019")
  )
)

# remove "-"
parCor2 <-parCor2[parCor2$year_group != "-",]
View(parCor2)

ggparcoord(parCor2,columns=c(13,14,15,16,17,18,19,20),groupColumn=21)+
  labs(title="Parallel CoordinateMap of Acoustic Features")

# reduce number of rows
parCor30 <- subset(parCor2,duration_ms>300000) # 1748 rows
parCor35 <- subset(parCor2,duration_ms>350000) # 613 rows
parCor40 <- subset(parCor2,duration_ms>400000) # 260 rows
parCor50 <- subset(parCor2,duration_ms>500000) # 64 rows
# tracks tended to be longer in the 1970s! 

# scale instrumentalness to between 0 and 1
parCor40$instrumentalness <- (parCor40$instrumentalness-min(parCor40$instrumentalness))/max(parCor40$instrumentalness)-min(parCor40$instrumentalness)
parCor50$instrumentalness <- parCor50$instrumentalness * 10

install.packages("ggsci")
library(ggsci)
ggparcoord(parCor50,columns=c(13,14,15,16,17,18,19,20),groupColumn=21)+
  labs(title="Parallel Coordinate Map of Acoustic Features")+
  scale_color_npg()
# tracks tended to be longer in the past!  more red lines

# plot duration over time
ggplot(parCor2,aes(x=year,y=duration_ms))+geom_bar(stat="identity")

# get random sample of data and plot parallel co-ords again
# use original df with year_group added
parCorRand <- parCor1 %>% slice_sample(n=200)
parCorRand250 <- parCor1 %>% slice_sample(n=250)
parCorRand250 <- na.omit(parCorRand200)

# plot in default order
ggparcoord(parCorRand,
           columns=c(13,14,15,16,17,18,19,20),
           groupColumn=21,
           showPoints=TRUE,
           scale="centerObs")+
  labs(title="Parallel Coordinate Map of Acoustic Features")+
  theme_minimal()

# plot in custom order
ggparcoord(parCorRand250,
          columns=c(16,19,17,13,20,14,15,18),
          groupColumn=21,
          showPoints=TRUE,
          scale="centerObs")+
  labs(title="Parallel Coordinate Map of Acoustic Features",x="Acoustic Feature",y="Value",legend="Year Group")+
  scale_color_viridis_d()+
  theme_minimal()
  
# filter by decade - facet plot
# plot in custom order
ggparcoord(parCorRand250,
           columns=c(16,19,17,13,20,14,15,18),
           groupColumn=21,
           showPoints=TRUE,
           scale="centerObs")+
  labs(title="Parallel Coordinate Map of Acoustic Features",x="Acoustic Feature",y="Value",legend="Year Group")+
  scale_color_viridis_d()+
  facet_grid(year_group ~ .)






















