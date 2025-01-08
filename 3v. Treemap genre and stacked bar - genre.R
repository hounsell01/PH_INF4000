
##
## Artist total popularity by year
## and Treemap of genres
##
## Rapper = {Accessibility}
## Treemap = {Theoretical Frameworks} ??

install.packages("ggplot2")
library(ggplot2)
# how does type change by year? Bar charts 1967-2017
type_df <- ss_ArtSongAF %>% filter(artist_type != "-") %>% group_by(artist_type, year) %>% count()

print(type_df,n=200)
genrey67 <- type_df %>% filter(year==1967)
ggplot(ss_ArtSongAF,aes(x=artist_type),colour=year)+geom_bar()


# plot artist popularity by genre by year
type_df <- ss_ArtSongAF %>% filter(artist_type != "-")
ggplot(type_df,aes(x=year,y=popularity.x,fill=artist_type))+geom_bar(stat="identity")+
  labs(title="Raw popularity of artist type by year", caption = "MusicOSet data")
  scale_fill_viridis_d()

# popularity is weighted towards more recent songs - calcilate mean popularity by year and replot
# mean poularity by year
mean_pop <- type_df %>% group_by(year) %>% summarise(mean(popularity.x))
print(mean_pop,n=55)

names <- names(mean_pop)
names[2] <- "mean"
names(mean_pop) <- names

ggplot(mean_pop,aes(x=year,y=mean))+geom_point()

# join mean population score back into main table
ss_mp <- left_join(ss_ArtSongAF,mean_pop,by="year")
View(ss_mp)

# plot (artist popularity / mean popularity for that year) by genre by year
ssmp_div <- ss_mp %>% mutate(ss_mp$adj_pop <-  popularity.x/mean)
View(ssmp_div)
names <- names(ssmp_div)
names[29] <- "adj_pop"
names(ssmp_div) <- names

# remove NA and "-"
ssmp_div <- na.omit(ssmp_div)
ssmp_div <- ssmp_div[ssmp_div$artist_type != "-",]

ggplot(ssmp_div,aes(x=year,y=adj_pop,fill=artist_type))+geom_bar(stat="identity")+
  labs(title="Modified popularity of artist by year", caption="MusicOset data")
  theme_minimal()
  scale_fill_viridis_d()+
  scale_x_continuous(name="Year",n.breaks=10)

ggplot(ssmp_div,aes(x=year,y=adj_pop,fill=artist_type))+geom_bar(stat="identity")+
  labs(title="The Rise of the Rapper",subtitle="Popularity of artist type through the years, adjusted for recency bias", caption="MusicOSet data",
       x= "Year", y="Total artist popularity divided by mean popularity for that year",colour="Artist\nType")+
  theme_minimal()+
  scale_fill_viridis_d()+
  scale_x_continuous(name="Year",n.breaks=10)

# better colour contrast - DJ/duo:
# this looks too much like a 3d atempt and is confusing
ggplot(ssmp_div,aes(x=year,y=adj_pop,fill=artist_type))+geom_bar(stat="identity")+
  labs(title="The Rise of the Rapper",subtitle="Popularity of artist type through the years, adjusted for recency bias", caption="MusicOSet data",
       x= "Year", y="Total artist popularity divided by mean popularity for that year",colour="Artist\nType")+
  theme_minimal()+
  scale_fill_brewer()+
  scale_x_continuous(name="Year",n.breaks=10)

library(RColorBrewer)
display.brewer.all(colorblindFriendly=TRUE)

# This one is better (and cbf too):
ggplot(ssmp_div,aes(x=year,y=adj_pop,fill=artist_type))+geom_bar(stat="identity")+
  labs(title="The Rise of the Rapper",subtitle="Popularity of artist type through the years, adjusted for recency bias", caption="Billboard top 100 chart, MusicOSet data",
       x= "Year", y="Total artist popularity divided by mean popularity for that year",fill="Artist\nType")+
  theme_minimal()+
  scale_fill_brewer(palette="Set2")+
  scale_x_continuous(name="Year",n.breaks=10)


# radar charts by year? Or by each decade?
# group into 10-year bands
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

# subset to reduce data
# radar chart for each  decade
# place 9 plots on a grid sequentially
# pos: animate?

#########################
# treemap showing genre
#########################

library(treemapify)

# count genres by type
tm_data <- ss_ArtSongAF %>% group_by(artist_type,main_genre) %>% count(artist_type)
tail(ss_ArtSongAF)
# remove "-"
tm_data <- tm_data[tm_data$artist_type != "-",]
tm_data <- tm_data[tm_data$main_genre != "-",]

# plot treemap
ggplot(tm_data,aes(area=n,fill=artist_type,label=main_genre))+geom_treemap_text()

tail(tm_data)

# order by count and remove 80:20
tm_data <- arrange(tm_data,desc(n))
tm_data <- na.omit(tm_data)

# try n>5
tm_data <- tm_data[tm_data$n>5,] # 195 rows

# plot treemap
ggplot(tm_data,aes(area=n,fill=artist_type,label=main_genre))+geom_treemap_text()

# try n>10
tm_data <- tm_data[tm_data$n>10,] # 147 rows

# plot treemap
ggplot(tm_data,aes(area=n,fill=artist_type,label=main_genre))+geom_treemap()

# try n>50
tm_data <- tm_data[tm_data$n>50,] # 44 rows

# plot treemap
ggplot(tm_data,aes(area=n,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  labs(title="",subtitle="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

View(tm_data)

# duplicate genres due to artist type - colour by artist type?
# get numbered types 
tm_types <- tm_data %>% group_by(artist_type) %>% count(artist_type)
tm_types[,2] <- c(1,4,3,2)
  
# join type numbers back to data
tm_dataWtype_num <- left_join(tm_data,tm_types,by="artist_type")

# make more sensible manes
tmnm <- names(tm_dataWtype_num)
tmnm[4] <- "type_num"
tmnm[3] <- "genre_num"
names(tm_dataWtype_num) <- tmnm

# plot treemap with type number
ggplot(tm_dataWtype_num,aes(area=genre_num,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_brewer(palette="Blues")+
  labs(title="Song Genres and Type of Artist",subtitle="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")
  
ggplot(tm_dataWtype_num,aes(area=genre_num,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_viridis_d()+
  labs(title="Song Genres and Type of Artist",subtitle="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

# animated - year by year??
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("gapminder")
library(gapminder)

tm_data_a <- ss_ArtSongAF %>% group_by(artist_type,main_genre,year) %>% count(artist_type)

# remove "-"
tm_data_a <- tm_data[tm_data_a$artist_type != "-",]
tm_data_a <- tm_data[tm_data_a$main_genre != "-",]

# reduce (n > 5)
tm_data_a <- tm_data_a[tm_data_a$n>5,] # 44 rows

# join type numbers back to data
tmdataaWtype_num <- left_join(tm_data_a,tm_types,by="artist_type")
View(tmdataaWtype_num)
tmdataaWtype_num <- na.omit(tmdataaWtype_num)

# make more sensible names
tmdataaWtype_num
tmnm_a <- names(tmdataaWtype_num)
tmnm_a[3] <- "year"
tmnm_a[4] <- "genre_num"
tmnm_a[5] <- "type_num"
names(tmdataaWtype_num) <- tmnm_a

# plot treemap with type number animated by year
ggplot(tmdataaWtype_num,aes(area=genre_num,fill=artist_type,label=main_genre))+
  geom_treemap(layout="fixed")+
  geom_treemap_text(layout="fixed",place="center",grow=TRUE)+
  scale_fill_brewer(palette="Blues")+
  transition_time(year)+
  transition_states(year,transition_length=100,state_length=100)+
  ease_aes("linear")+
  labs(subtitle="Year:{frame_time}",title="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")+
  ggtitle("Year:{frame_time}")
  
labs(title="Song Genres and Type of Artist",subtitle="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")
getwd()
anim_save("C:/Users/peteh/OneDrive/Documents/Data Science/Data Visualisation/Coursework Semester 1/anim1.gif")



