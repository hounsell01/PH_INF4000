
##
## 2. treemap of genre by popularity
##

library(ggplot2)
library(tidyverse)

# plot histogram of popularity descending - song
spop_desc <- ss_ArtSongAF %>% arrange(desc(ss_ArtSongAF$popularity.y))
ggplot(pop_desc, aes(y=popularity.y))+geom_bar()+
  labs(title="Histogram of song popularity scores", caption="MusicOSet data", y="Song Popularity")
hist(ss_ArtSongAF$popularity.x) # artist popularity
hist(ss_ArtSongAF$popularity.y) # song popularity

# plot histogram of popularity descending - artist
apop_desc <- ss_ArtSongAF %>% arrange(desc(ss_ArtSongAF$popularity.x))
ggplot(apop_desc, aes(y=popularity.x))+geom_bar()+
  labs(title="Histogram of artist popularity scores", caption="MusicOSet data", y="Artist Popularity")+
  xlim(0,500)+ylim(0,100)

# count artist
 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# subset data
art_count <- art_count[art_count$n>30,]
art_count <- art_count %>% arrange(desc(n))
art_count <- art_count[-1,]
ggplot(art_count, aes(x=reorder(name,n),y=n))+geom_bar(stat="identity")+
  labs(title="Ranking of artist popularity scores", caption="MusicOSet data")+
  theme(axis.text.x=element_text(angle=90))+
  coord_flip()

# cut down to popularity >x
ss_new <- ss_ArtSongAF[ss_ArtSongAF$popularity.x>10,]
ss_new <- ss_new[9:11]
ss_new <- ss_new[,-2]

# get first value only
ss_sample <- ss_new %>% distinct(name,.keep_all=TRUE)
View(ss_sample)
# sample some rows
ss_sample <- sample_n(ss_sample,60)

# plot popularity descending
ggplot(ss_sample,aes(x=reorder(name, popularity.x),y=popularity.x))+
  geom_bar(stat="identity")+
  labs(title="Artist Popularity (60 randome values)",x="Popularity Score",caption="MusicOSet data")+
  theme(legend.position="none", axis.text.x=element_text(angle=90))+
  coord_flip()

# cut down data
sub_art <- sub_art[1:6]
sub_art <- art[art$popularity>75,]

# plot treemap with popularity
ggplot(sub_art,aes(area=popularity,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  labs(title="Song Genres and Popularity",subtitle="Most popular artists 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

tm_genrepop <- sub_art %>% group_by(artist_type,main_genre) %>% summarise(sum_pop=sum(popularity))
tm_genrepop <- tm_genrepop[tm_genrepop$artist_type!="-",]

# intial plot - genre and popularity
ggplot(tm_genrepop,aes(area=sum_pop,fill=main_genre,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  labs(title="Song Genres and Popularity",subtitle="Most popular Genres 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

ggplot(tm_genrepop,aes(area=sum_pop,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  labs(title="Song Genres and Popularity",subtitle="Most popular Genres 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

arrange(tm_genrepop,(tm_genrepop$sum_pop))
tm_genrepopCOPY <- tm_genrepop
tm_genrepop <- tm_genrepopCOPY[tm_genrepopCOPY$sum_pop>200,]
tm_genrepop

ggplot(tm_genrepop,aes(area=sum_pop,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  labs(title="Song Genres and Popularity",subtitle="Most popular Genres 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

ggplot(tm_genrepop,aes(area=sum_pop,fill=artist_type,label=main_genre))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_brewer(palette="Blues")+
  labs(title="Song Genres and Popularity",subtitle="Most popular Genres 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

ggplot(tm_genrepop,aes(area=sum_pop,fill=artist_type,label=main_genre,subgroup=artist_type))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_brewer(palette="Blues")+
  labs(title="Song Genres and Popularity",subtitle="Most popular Genres 1964-2018",caption="Billboard top 100 chart, MusicOSet data",fill="Artist\nType")

## plot treemap with type number
# ggplot(tm_dataWtype_num,aes(area=genre_num,fill=artist_type,label=main_genre))+
#   geom_treemap()+
#   geom_treemap_text()+
#   scale_fill_brewer(palette="Blues")+
#   labs(title="Song Genres and Type of Artist",subtitle="Most prevelant genres of music 1964-2018",caption="Billboard top 100 chart, MusicOSet data")

art


