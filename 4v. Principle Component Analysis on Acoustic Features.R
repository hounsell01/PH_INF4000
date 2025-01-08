
##
## 4. PCA on Acoustic Features
##
## {Visualisation Choice}

library(gridExtra)

# calculate the PCA on the acoustic features
pca <- prcomp(sp_ArtSongAF[,13:20])

sp_ArtSongAF.pca <- data.frame(type=sp_ArtSongAF$artist_type,genre=sp_ArtSongAF$main_genre, PCA1=pca$x[,1], PCA2=pca$x[,2])

# way too many records
ggplot(sp_ArtSongAF.pca, aes(PCA1, PCA2, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features", caption="musicOset data")

#####################

### cut data down to 1967
data67 <- sp_ArtSongAF %>% filter(year==1967)   # 248 rows
# omit "-"
data67 <- data67[data67$artist_type != "-",]
data67 <- data67[data67$main_genre != "-",]
# calculate the PCA on the acoustic features of 1967 songs
pca67 <- prcomp(data67[,13:20])
# extract data
data67.pca <- data.frame(type=data67$artist_type, genre=data67$main_genre, PCA1_67=pca67$x[,1], PCA2_67=pca67$x[,2])
# plot data
ggplot(data67.pca, aes(PCA1_67, PCA2_67, label = genre))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1967 Songs", caption="musicOset data")+
  xlim(-1,1)+ylim(-1,1)

### cut data down to 1972
data72 <- sp_ArtSongAF %>% filter(year==1972)   # 248 rows
# omit "-"
data72 <- data72[data72$artist_type != "-",]
data72 <- data72[data72$main_genre != "-",]
# calculate the PCA on the acoustic features of 1972 songs
pca72 <- prcomp(data72[,13:20])
# extract data
data72.pca <- data.frame(type=data72$artist_type, genre=data72$main_genre, PCA1_72=pca72$x[,1], PCA2_72=pca72$x[,2])
# plot data
ggplot(data72.pca, aes(PCA1_72, PCA2_72, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1972 Songs", caption="musicOset data")+
  xlim(-1,1)+ylim(-1,1)


### cut down data to 1977
data77 <- sp_ArtSongAF %>% filter(year==1977)   # 259 rows
# omit "-"
data77 <- data77[data77$artist_type != "-",]
data77 <- data77[data77$main_genre != "-",]
# calculate the PCA on the acoustic features of 1977 songs
pca77 <- prcomp(data77[,13:20])
# extract data
data77.pca <- data.frame(type=data77$artist_type, genre=data77$main_genre, PCA1_77=pca77$x[,1], PCA2_77=pca77$x[,2])
# plot data
ggplot(data77.pca, aes(PCA1_77, PCA2_77, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1977 Songs", caption="musicOset data")

### cut data down to 1982
data82 <- sp_ArtSongAF %>% filter(year==1982)   # 248 rows
# omit "-"
data82 <- data82[data82$artist_type != "-",]
data82 <- data82[data82$main_genre != "-",]
# calculate the PCA on the acoustic features of 1982 songs
pca82 <- prcomp(data82[,13:20])
# extract data
data82.pca <- data.frame(type=data82$artist_type, genre=data82$main_genre, PCA1_82=pca82$x[,1], PCA2_82=pca82$x[,2])
# plot data
ggplot(data82.pca, aes(PCA1_82, PCA2_82, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1982 Songs", caption="musicOset data")+
  xlim(-1,1)+ylim(-1,1)


### cut down data to 1987
data87 <- sp_ArtSongAF %>% filter(year==1987)   # 259 rows
# omit "-"
data87 <- data87[data87$artist_type != "-",]
data87 <- data87[data87$main_genre != "-",]
# calculate the PCA on the acoustic features of 1987 songs
pca87 <- prcomp(data87[,13:20])
# extract data
data87.pca <- data.frame(type=data87$artist_type, genre=data87$main_genre, PCA1_87=pca87$x[,1], PCA2_87=pca87$x[,2])
# plot data
ggplot(data87.pca, aes(PCA1_87, PCA2_87, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1987 Songs", caption="musicOset data")


### cut down data to 1992
data92 <- sp_ArtSongAF %>% filter(year==1992)   # 259 rows
# omit "-"
data92 <- data92[data92$artist_type != "-",]
data92 <- data92[data92$main_genre != "-",]
# calculate the PCA on the acoustic features of 1987 songs
pca92 <- prcomp(data92[,13:20])
# extract data
data92.pca <- data.frame(type=data92$artist_type, genre=data92$main_genre, PCA1_92=pca92$x[,1], PCA2_92=pca92$x[,2])
# plot data
ggplot(data92.pca, aes(PCA1_92, PCA2_92, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1992 Songs", caption="musicOset data")


### cut down data to 1997
data97 <- sp_ArtSongAF %>% filter(year==1997)   # 259 rows
# omit "-"
data97 <- data97[data97$main_genre != "-",]
data97 <- data97[data97$artist_type != "-",]
# calculate the PCA on the acoustic features of 1997 songs
pca97 <- prcomp(data97[,13:20])
# extract data
data97.pca <- data.frame(type=data97$artist_type, genre=data97$main_genre, PCA1_97=pca97$x[,1], PCA2_97=pca97$x[,2])
# plot data
ggplot(data97.pca, aes(PCA1_97, PCA2_97, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 1997 Songs", caption="musicOset data")


### cut data down to 2002
data02 <- sp_ArtSongAF %>% filter(year==2002)   # 259 rows
# omit "-"
data02 <- data02[data02$artist_type != "-",]
data02 <- data02[data02$main_genre != "-",]
# calculate the PCA on the acoustic features of 2007 songs
pca02 <- prcomp(data02[,13:20])
# extract data
data02.pca <- data.frame(type=data02$artist_type, genre=data02$main_genre, PCA1_02=pca02$x[,1], PCA2_02=pca02$x[,2])
# plot data
ggplot(data02.pca, aes(PCA1_02, PCA2_02, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 2002 Songs", caption="musicOset data")


### cut data down to 2007
data07 <- sp_ArtSongAF %>% filter(year==2007)   # 259 rows
# omit "-"
data07 <- data07[data07$artist_type != "-",]
data07 <- data07[data07$main_genre != "-",]
# calculate the PCA on the acoustic features of 2007 songs
pca07 <- prcomp(data07[,13:20])
# extract data
data07.pca <- data.frame(type=data07$artist_type, genre=data07$main_genre, PCA1_07=pca07$x[,1], PCA2_07=pca07$x[,2])
# plot data
ggplot(data07.pca, aes(PCA1_07, PCA2_07, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 2007 Songs", caption="musicOset data")


### cut data down to 2012
data12 <- sp_ArtSongAF %>% filter(year==2012)   # 259 rows
# omit "-"
data12 <- data12[data12$artist_type != "-",]
data12 <- data12[data12$main_genre != "-",]
# calculate the PCA on the acoustic features of 2012 songs
pca12 <- prcomp(data12[,13:20])
# extract data
data12.pca <- data.frame(type=data12$artist_type, genre=data12$main_genre, PCA1_12=pca12$x[,1], PCA2_12=pca12$x[,2])
# plot data
ggplot(data12.pca, aes(PCA1_12, PCA2_12, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 2012 Songs", caption="musicOset data")


### cut data down to 2017
data17 <- sp_ArtSongAF %>% filter(year==2017)   # 259 rows
# omit "-"
data17 <- data17[data17$artist_type != "-",]
data17 <- data17[data17$main_genre != "-",]
# calculate the PCA on the acoustic features of 2017 songs
pca17 <- prcomp(data17[,13:20])
# extract data
data17.pca <- data.frame(type=data17$artist_type, genre=data17$main_genre, PCA1_17=pca17$x[,1], PCA2_17=pca17$x[,2])
# plot data
ggplot(data17.pca, aes(PCA1_17, PCA2_17, label = type))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for 2017 Songs", caption="musicOset data")


##########################################
# plot with coloured points - artist type
##########################################

d67 <- ggplot(data67.pca, aes(PCA1_67, PCA2_67))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1967", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d72 <- ggplot(data72.pca, aes(PCA1_72, PCA2_72))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1972", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d77 <- ggplot(data77.pca, aes(PCA1_77, PCA2_77))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1977", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d82<- ggplot(data82.pca, aes(PCA1_82, PCA2_82))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1982", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d87 <- ggplot(data87.pca, aes(PCA1_87, PCA2_87))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1987", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d92<- ggplot(data92.pca, aes(PCA1_92, PCA2_92))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1992", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d97 <- ggplot(data97.pca, aes(PCA1_97, PCA2_97))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1997", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d02<- ggplot(data02.pca, aes(PCA1_02, PCA2_02))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2002", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d07 <- ggplot(data07.pca, aes(PCA1_07, PCA2_07))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2007", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d12<- ggplot(data12.pca, aes(PCA1_12, PCA2_12))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2012", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
d17 <- ggplot(data17.pca, aes(PCA1_17, PCA2_17))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2017", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="yellow","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)

grid.arrange(d67,d72, d77, d82, d87, d92, d97, d02, d07, d12, d17,ncol=4, top="PCA of acoustic features by Genre, single years 1967 through 2017")

##########################################
# plot with coloured points - genre
##########################################

dg67 <- ggplot(data67.pca, aes(PCA1_67, PCA2_67))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1967", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg72 <- ggplot(data72.pca, aes(PCA1_72, PCA2_72))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1972", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg77 <- ggplot(data77.pca, aes(PCA1_77, PCA2_77))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1977", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg82<- ggplot(data82.pca, aes(PCA1_82, PCA2_82))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1982", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg87 <- ggplot(data87.pca, aes(PCA1_87, PCA2_87))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1987", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg92<- ggplot(data92.pca, aes(PCA1_92, PCA2_92))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1992", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg97 <- ggplot(data97.pca, aes(PCA1_97, PCA2_97))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 1997", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg02<- ggplot(data02.pca, aes(PCA1_02, PCA2_02))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2002", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg07 <- ggplot(data07.pca, aes(PCA1_07, PCA2_07))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2007", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg12<- ggplot(data12.pca, aes(PCA1_12, PCA2_12))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2012", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)
dg17 <- ggplot(data17.pca, aes(PCA1_17, PCA2_17))+geom_point(aes(colour=genre),size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2017", caption="musicOset data")+
  scale_colour_viridis_d()+
  xlim(-1,1)+ylim(-1,1)

grid.arrange(dg67,dg72, dg77, dg82, dg87, dg92, dg97, dg02, dg07, dg12, dg17,ncol=4, top="PCA of acoustic features by Genre, five-yearly, 1967 through 2017")

#############################
# loading plots
pca67 <- prcomp(data67[,13:20])
data67.loading <- data.frame(
  dimensions=colnames(data67)[13:20],
  PC1=pca67$rotation[,1],
  PC2=pca67$rotation[,2])
lp67 <- ggplot(data67.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="1967",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

pca77 <- prcomp(data77[,13:20])
data77.loading <- data.frame(
  dimensions=colnames(data77)[13:20],
  PC1=pca77$rotation[,1],
  PC2=pca77$rotation[,2])
lp77 <- ggplot(data77.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="1977",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

pca87 <- prcomp(data17[,13:20])
data87.loading <- data.frame(
  dimensions=colnames(data87)[13:20],
  PC1=pca87$rotation[,1],
  PC2=pca87$rotation[,2])
lp87 <- ggplot(data87.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="1987",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

pca97 <- prcomp(data17[,13:20])
data97.loading <- data.frame(
  dimensions=colnames(data97)[13:20],
  PC1=pca97$rotation[,1],
  PC2=pca97$rotation[,2])
lp97 <- ggplot(data97.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="1997",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

pca07 <- prcomp(data07[,13:20])
data07.loading <- data.frame(
  dimensions=colnames(data07)[13:20],
  PC1=pca07$rotation[,1],
  PC2=pca07$rotation[,2])
lp07 <- ggplot(data07.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="2007",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

pca17 <- prcomp(data17[,13:20])
data17.loading <- data.frame(
  dimensions=colnames(data17)[13:20],
                  PC1=pca17$rotation[,1],
                  PC2=pca17$rotation[,2])
lp17 <- ggplot(data17.loading, aes(PC1,PC2,label=dimensions))+geom_text(size=3)+
  labs(title="2017",caption="MusicOset")+
  xlim(-1,1)+ylim(-1,1)

grid.arrange(lp67,lp77,lp87,lp97,lp07,lp17,nrow=2,top="Loading plots of acoustic features over the years")

#############################
# animate 1990 to 1999

install.packages("gganimate")
library(gganimate)

# get data for 1990-1999
data9099 <- sp_ArtSongAF %>% filter(year>=1990 & year < 2000)  # 2270 rows 
# arrange 1990 first
data9099 <- arrange(data9099,year,ascending=FALSE)
tail(data9099)

# calculate pca for AF's for 1990 to 1999
# calculate the PCA on the acoustic features of 2007 songs
pca9099 <- prcomp(data9099[,13:20])

### ten single years
data90 <- sp_ArtSongAF %>% filter(year==1990)
pca90 <- prcomp(data90[,13:20])
data90.pca <- data.frame(type=data90$artist_type, genre=data90$main_genre, PCA1_90=pca90$x[,1], PCA2_90=pca90$x[,2])
data91 <- sp_ArtSongAF %>% filter(year==1991)
pca91 <- prcomp(data91[,13:20])
data91.pca <- data.frame(type=data91$artist_type, genre=data91$main_genre, PCA1_91=pca91$x[,1], PCA2_91=pca91$x[,2])
data92 <- sp_ArtSongAF %>% filter(year==1992)
pca92 <- prcomp(data92[,13:20])
data92.pca <- data.frame(type=data92$artist_type, genre=data92$main_genre, PCA1_92=pca92$x[,1], PCA2_92=pca92$x[,2])
data93 <- sp_ArtSongAF %>% filter(year==1993)
pca93 <- prcomp(data93[,13:20])
data93.pca <- data.frame(type=data93$artist_type, genre=data93$main_genre, PCA1_93=pca93$x[,1], PCA2_93=pca93$x[,2])
data94 <- sp_ArtSongAF %>% filter(year==1994)
pca94 <- prcomp(data94[,13:20])
data94.pca <- data.frame(type=data94$artist_type, genre=data94$main_genre, PCA1_94=pca94$x[,1], PCA2_94=pca94$x[,2])
data95 <- sp_ArtSongAF %>% filter(year==1995)
pca95 <- prcomp(data95[,13:20])
data95.pca <- data.frame(type=data95$artist_type, genre=data95$main_genre, PCA1_95=pca95$x[,1], PCA2_95=pca95$x[,2])
data96 <- sp_ArtSongAF %>% filter(year==1996)
pca96 <- prcomp(data96[,13:20])
data96.pca <- data.frame(type=data96$artist_type, genre=data96$main_genre, PCA1_96=pca96$x[,1], PCA2_96=pca96$x[,2])
data97 <- sp_ArtSongAF %>% filter(year==1997)
pca97 <- prcomp(data97[,13:20])
data97.pca <- data.frame(type=data97$artist_type, genre=data97$main_genre, PCA1_97=pca97$x[,1], PCA2_97=pca97$x[,2])
data98 <- sp_ArtSongAF %>% filter(year==1998)
pca98 <- prcomp(data98[,13:20])
data98.pca <- data.frame(type=data98$artist_type, genre=data98$main_genre, PCA1_98=pca98$x[,1], PCA2_98=pca98$x[,2])
data99 <- sp_ArtSongAF %>% filter(year==1999)
pca99 <- prcomp(data99[,13:20])
data99.pca <- data.frame(type=data99$artist_type, genre=data99$main_genre, PCA1_99=pca99$x[,1], PCA2_99=pca99$x[,2])

### manual plots
p0 <- ggplot(data90.pca, aes(PCA1_90, PCA2_90))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1990 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p1 <- ggplot(data91.pca, aes(PCA1_91, PCA2_91))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1991 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p2 <- ggplot(data92.pca, aes(PCA1_92, PCA2_92))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1992 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p3 <- ggplot(data93.pca, aes(PCA1_93, PCA2_93))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1993 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p4 <- ggplot(data94.pca, aes(PCA1_94, PCA2_94))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1994 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p5 <- ggplot(data95.pca, aes(PCA1_95, PCA2_95))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1995 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p6 <- ggplot(data96.pca, aes(PCA1_96, PCA2_96))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1996 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p7 <- ggplot(data97.pca, aes(PCA1_97, PCA2_97))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1997 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p8 <- ggplot(data98.pca, aes(PCA1_98, PCA2_98))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1998 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)
p9 <- ggplot(data99.pca, aes(PCA1_99, PCA2_99))+geom_point(aes(colour=type),size=3)+
  labs(title="PCA of Acoustic Features for 1999 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
  xlim(-1,1)+ylim(-1,1)

grid.arrange(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9)

# add years to data files
# now undone
data90.pca$year=1990
data91.pca$year=1991
data92.pca$year=1992
data93.pca$year=1993
data94.pca$year=1994
data95.pca$year=1995
data96.pca$year=1996
data97.pca$year=1997
data98.pca$year=1998
data99.pca$year=1999

# work out how to append DF's
cp1 <- data90.pca
cp2 <- data91.pca
nrow(cp1)==nrow(cp2)
head(cp1)
head(cp2)
# change col names to same
allnames=c("type","genre","PCA1","PCA2","year")
names(cp1) <- allnames
names(cp2) <- allnames
cp3 <- rbind(cp1,cp2)
head(cp3)
tail(cp3)

# change all column names to the same
# now undone
names(data90.pca) <- allnames
names(data91.pca) <- allnames
names(data92.pca) <- allnames
names(data93.pca) <- allnames
names(data94.pca) <- allnames
names(data95.pca) <- allnames
names(data96.pca) <- allnames
names(data97.pca) <- allnames
names(data98.pca) <- allnames
names(data99.pca) <- allnames

# append years 1991-1999 to 1990
data90s.pca <- rbind(data90.pca,data91.pca)
data90s.pca <- rbind(data90s.pca,data92.pca)
data90s.pca <- rbind(data90s.pca,data93.pca)
data90s.pca <- rbind(data90s.pca,data94.pca)
data90s.pca <- rbind(data90s.pca,data95.pca)
data90s.pca <- rbind(data90s.pca,data96.pca)
data90s.pca <- rbind(data90s.pca,data97.pca)
data90s.pca <- rbind(data90s.pca,data98.pca)
data90s.pca <- rbind(data90s.pca,data99.pca)
head(data90s.pca)
tail(data90s.pca)

# data90s.pca has PCA numbers by song by year so ready for animation (??)
ggplot(data90s.pca, aes(PCA1, PCA2))+
    geom_point(aes(colour=type),size=3)+
    labs(title="year: {frame_time}",x="PCA1",y="PCA2")+
    scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))+
    transition_time(year)+
    ease_aes('linear')



#############################################
# PCA with is_pop based on acoustic features

# way too many records
ggplot(sp_ArtSongAF.pca, aes(PCA1, PCA2, label = genre))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features for Popular songs", caption="musicOset data")

# copy data to preserve original
copy_ispop <- sp_ArtSongAF


# perform PCA on acoustic features
popular <- prcomp(copy_ispop[,13:20])

# extract the data
popular.pca <- data.frame(popular_yn=copy_ispop$is_pop,year=copy_ispop$year, PCA1p=popular$x[,1], PCA2p=popular$x[,2])
head(popular.pca)

ggplot(popular.pca, aes(PCA1p, PCA2p, label = popular_yn))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features", caption="musicOset data")
# too many records - subset


#1967
# subset data
ispop67 <- sp_ArtSongAF %>% filter(year==1967)     # 248 rows

# perform PCA
popular67 <- prcomp(ispop67[,13:20])

# extract data
popular67.pca <- data.frame(popular_yn = ispop67$is_pop, year = ispop67$year, PCA1p67=popular67$x[,1], PCA2p67=popular67$x[,2])
length(ispop67$is_pop)
length(ispop67$year)
pca

ggplot(popular67.pca, aes(PCA1p67, PCA2p67, label = popular_yn, colour=popular_yn))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features", caption="musicOset data")+
  xlim(-1,1)+ylim(-1,1)


# 2017
# subset data
ispop17 <- sp_ArtSongAF %>% filter(year==2017)

# perform PCA
popular17 <- prcomp(ispop17[,13:20])

# extract data
popular17.pca <- data.frame(popular_yn = ispop17$is_pop, year = ispop17$year, PCA1p17=popular17$x[,1], PCA2p17=popular17$x[,2])

ggplot(popular17.pca, aes(PCA1p17, PCA2p17, label = popular_yn, colour=popular_yn))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features", caption="musicOset data")


### smaller PCA on just main_genre, artist_type, duration - use 2017 to test
genre_test17 <- sp_ArtSongAF %>% filter(year==2017)

#####################
# build new genre df
#####################

genre_test17 <- genre_test17[,-1]
genre_test17 <- genre_test17[,-1]
genre_test17a <- genre_test17[,1:2]
genre_test17a <- genre_test17a %>% mutate(genre_test17[,10])


#### 1. transform genre to numerical categorical (1): just give it a number
genre <- sp_ArtSongAF[,10] 
genre <- genre %>% group_by(main_genre) %>% summarise(n())
genre$num <- c(1:305)

# join genres to original file to get numerical value for genre
genre_test17a <- left_join(genre_test17a,genre,by="main_genre")

# rename column
nm <- names(genre_test17a)
nm <- c("is_pop","year","artist_type","main_genre","duration_ms","n()","genre_num")
names(genre_test17a) <- nm

# rm col n()
genre_test17a <- genre_test17a[,-6]
  
# perform PCA on genre,duration
test17 <- prcomp(genre_test17a[,5:6])

# extract data
test17.pca <- data.frame(genre = genre_test17a$genre_num, duration = genre_test17a$duration_ms, PCA1g=test17$x[,1], PCA2g=test17$x[,2])

# plot data
ggplot(test17.pca, aes(PCA1g, PCA2g, label = genre, colour=genre))+geom_text(size=3)+
  labs(title="PCA of Genre and Duration", caption="musicOset data")

genre_test17aT <- genre_test17a

# scale duration to 200 and try again
genre_test17a$duration_msT <- genre_test17aT$duration_ms/mean(genre_test17aT$duration_ms)
genre_test17a$duration_msT <- genre_test17a$duration_msT * 200

# perform PCA on genre,scaled duration
test17 <- prcomp(genre_test17a[,6:7])
# extract data
test17.pca <- data.frame(genre = genre_test17a$genre_num, duration = genre_test17a$duration_ms, PCA1g=test17$x[,1], PCA2g=test17$x[,2])
# plot data
ggplot(test17.pca, aes(PCA1g, PCA2g, label = genre, colour=genre))+geom_text(size=3)+
  labs(title="PCA of Genre and Duration", caption="musicOset data")

# join back to names
nm <- names(genre)
nm <- c("main_genre","n()","genre")
names(genre) <- nm
test17.pca <- left_join(test17.pca,genre,by="genre")
# plot with genre name
ggplot(test17.pca, aes(PCA1g, PCA2g, label = main_genre, colour=genre))+geom_text(size=3)+
  labs(title="PCA of Genre and Duration 2017", caption="musicOset data")



# PCA of acoustic features for 2017 Billboard songs, classified by main_genre
# subset data
genre17 <- sp_ArtSongAF %>% filter(year==2017)

# perform PCA
gen17 <- prcomp(genre17[,13:20])

# extract data
gen17.pca <- data.frame(genre = genre17$main_genre, year = genre17$year, PCA1g17=gen17$x[,1], PCA2g17=gen17$x[,2])

ggplot(gen17.pca, aes(PCA1g17, PCA2g17, label = genre, colour=genre))+geom_text(size=3)+
  labs(title="PCA of Acoustic Features by Genre for 2017 Billboard Songs", caption="musicOset data")





# animate pca for 2007 to 2017
### cut data down to 2007
data07 <- sp_ArtSongAF %>% filter(year==2007)   # 248 rows
# omit "-"
data07 <- data07[data07$artist_type != "-",]
# calculate the PCA on the acoustic features
pca07 <- prcomp(data07[,13:20])
# extract data
data07.pca <- data.frame(type=data07$artist_type, genre=data07$main_genre, PCA1_07=pca07$x[,1], PCA2_07=pca07$x[,2])
# plot data
cp7 <- ggplot(data07.pca, aes(PCA1_07, PCA2_07, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2007 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2008
data08 <- sp_ArtSongAF %>% filter(year==2008)   # 248 rows
# omit "-"
data08 <- data08[data08$artist_type != "-",]
# calculate the PCA on the acoustic features
pca08 <- prcomp(data08[,13:20])
# extract data
data08.pca <- data.frame(type=data08$artist_type, genre=data08$main_genre, PCA1_08=pca08$x[,1], PCA2_08=pca08$x[,2])
# plot data
cp8 <- ggplot(data08.pca, aes(PCA1_08, PCA2_08, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2008 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2009
data09 <- sp_ArtSongAF %>% filter(year==2009)   # 248 rows
# omit "-"
data09 <- data09[data09$artist_type != "-",]
# calculate the PCA on the acoustic features
pca09 <- prcomp(data09[,13:20])
# extract data
data09.pca <- data.frame(type=data09$artist_type, genre=data09$main_genre, PCA1_09=pca09$x[,1], PCA2_09=pca09$x[,2])
# plot data
cp9 <- ggplot(data09.pca, aes(PCA1_09, PCA2_09, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2009 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2010
data10 <- sp_ArtSongAF %>% filter(year==2010)   # 248 rows
# omit "-"
data10 <- data10[data10$artist_type != "-",]
# calculate the PCA on the acoustic features
pca10 <- prcomp(data10[,13:20])
# extract data
data10.pca <- data.frame(type=data10$artist_type, genre=data10$main_genre, PCA1_10=pca10$x[,1], PCA2_10=pca10$x[,2])
# plot data
cp10 <- ggplot(data10.pca, aes(PCA1_10, PCA2_10, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2010 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2011
data11 <- sp_ArtSongAF %>% filter(year==2011)   # 248 rows
# omit "-"
data11 <- data11[data11$artist_type != "-",]
# calculate the PCA on the acoustic features
pca11 <- prcomp(data11[,13:20])
# extract data
data11.pca <- data.frame(type=data11$artist_type, genre=data11$main_genre, PCA1_11=pca11$x[,1], PCA2_11=pca11$x[,2])
# plot data
cp11 <- ggplot(data11.pca, aes(PCA1_11, PCA2_11, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2011 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2012
data12 <- sp_ArtSongAF %>% filter(year==2012)   # 248 rows
# omit "-"
data12 <- data12[data12$artist_type != "-",]
# calculate the PCA on the acoustic features
pca12 <- prcomp(data12[,13:20])
# extract data
data12.pca <- data.frame(type=data12$artist_type, genre=data12$main_genre, PCA1_12=pca12$x[,1], PCA2_12=pca12$x[,2])
# plot data
cp12 <- ggplot(data12.pca, aes(PCA1_12, PCA2_12, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2012 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2013
data13 <- sp_ArtSongAF %>% filter(year==2013)   # 248 rows
# omit "-"
data13 <- data13[data13$artist_type != "-",]
# calculate the PCA on the acoustic features
pca13 <- prcomp(data13[,13:20])
# extract data
data13.pca <- data.frame(type=data13$artist_type, genre=data13$main_genre, PCA1_13=pca13$x[,1], PCA2_13=pca13$x[,2])
# plot data
cp13 <- ggplot(data13.pca, aes(PCA1_13, PCA2_13, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2013 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2014
data14 <- sp_ArtSongAF %>% filter(year==2014)   # 248 rows
# omit "-"
data14 <- data14[data14$artist_type != "-",]
# calculate the PCA on the acoustic features
pca14 <- prcomp(data14[,13:20])
# extract data
data14.pca <- data.frame(type=data14$artist_type, genre=data14$main_genre, PCA1_14=pca14$x[,1], PCA2_14=pca14$x[,2])
# plot data
cp14 <- ggplot(data14.pca, aes(PCA1_14, PCA2_14, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2014 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2015
data15 <- sp_ArtSongAF %>% filter(year==2015)   # 248 rows
# omit "-"
data15 <- data15[data15$artist_type != "-",]
# calculate the PCA on the acoustic features
pca15 <- prcomp(data15[,13:20])
# extract data
data15.pca <- data.frame(type=data15$artist_type, genre=data15$main_genre, PCA1_15=pca15$x[,1], PCA2_15=pca15$x[,2])
# plot data
cp15 <- ggplot(data15.pca, aes(PCA1_15, PCA2_15, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2015 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2016
data16 <- sp_ArtSongAF %>% filter(year==2016)   # 248 rows
# omit "-"
data16 <- data16[data16$artist_type != "-",]
# calculate the PCA on the acoustic features
pca16 <- prcomp(data16[,13:20])
# extract data
data16.pca <- data.frame(type=data16$artist_type, genre=data16$main_genre, PCA1_16=pca16$x[,1], PCA2_16=pca16$x[,2])
# plot data
cp16 <- ggplot(data16.pca, aes(PCA1_16, PCA2_16, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2016 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

### cut data down to 2017
data17 <- sp_ArtSongAF %>% filter(year==2017)   # 248 rows
# omit "-"
data17 <- data17[data17$artist_type != "-",]
# calculate the PCA on the acoustic features
pca17 <- prcomp(data17[,13:20])
# extract data
data17.pca <- data.frame(type=data17$artist_type, genre=data17$main_genre, PCA1_17=pca17$x[,1], PCA2_17=pca17$x[,2])
# plot data
cp17 <- ggplot(data17.pca, aes(PCA1_17, PCA2_17, colour = type))+geom_point(size=3)+
  labs(title="PCA of Acoustic Features for 2017 Songs", caption="musicOset data")+
  scale_colour_manual(values=c("band"="black","DJ"="blue","duo"="green","rapper"="red","singer"="purple","-"="orange"))

p7
p8
p9
p10
p11
p12
p13
p14
p15
p16
p17


cp7
cp8
cp9
cp10
cp11
cp12
cp13
cp14
cp15
cp16
cp17

grid.arrange(cp8,cp9,cp10,cp11,cp12,cp13,cp14,cp15,cp16,cp17,nrow=3)


