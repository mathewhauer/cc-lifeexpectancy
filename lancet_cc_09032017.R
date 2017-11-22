rm()
library(tidyverse)
library(HMDHFDplus)
library(LifeTables)
library(xlsx)
library(tmap)
library(tmaptools)
library(RColorBrewer)


myusername <- userInput()
mypassword <- userInput()

lancetdata <- read.csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/LANcET_CCMORTDATA.csv",colClasses=c("CNTRY"="character"))

Countries <- getHMDcountries()

deaths <- do.call(rbind, lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Deaths_5x1", fixup=TRUE, username = myusername, password = mypassword)
Dat$CNTRY <- CNTRY
Dat}))

pops <- do.call(rbind, lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Population", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

lt <- do.call(rbind, lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "bltper_5x1", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))


lt2 <- lt%>%
  mutate(Age = ifelse(Age >= 80, 80, Age)) %>%
  group_by(CNTRY, Year, Age)%>%
  summarise(ax = sum(ax))

deaths2 <- deaths %>%
  mutate(Age = ifelse(Age >=80, 80, Age)) %>%
  group_by(CNTRY, Year, Age) %>%
  summarise(deaths = sum(Total) )


pops2 <- pops %>%
  mutate(Age = ifelse(Age >= 80, 80, 
               ifelse(Age >= 75, 75,
               ifelse(Age >= 65, 65,
               ifelse(Age >= 60, 60,
               ifelse(Age >= 55, 55,
               ifelse(Age >= 50, 50,
               ifelse(Age >= 45, 45,
               ifelse(Age >= 40, 40,
               ifelse(Age >= 35, 35,
               ifelse(Age >= 30, 30,
               ifelse(Age >= 25, 25,
               ifelse(Age >= 20, 20,
               ifelse(Age >= 15, 15,
               ifelse(Age >= 10, 10,
               ifelse(Age >= 5,  5,
               ifelse(Age >= 1,  1, Age))))))))))))))))) %>%
  group_by(CNTRY, Year, Age) %>%
  summarise(Pop= sum(Total1))





a <- left_join(pops2, deaths2)
a <- left_join(a, lt2)
a <- group_by(a, CNTRY, Age) %>%
  filter(Year == max(Year)) 



GBD_data <- read.csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/GBD_data.csv") %>%
  #mutate(perdist = log(val)) %>%
  group_by(countrycode, Age) %>%
  summarise(meanval = mean(val),
            upperval = mean(upper),
            lowerval = mean(lower)) %>%
  group_by(countrycode) %>%
  mutate(countmeanval = sum(meanval),
         countupperval = sum(upperval),
         countlowerval = sum(lowerval),
         perdist = (meanval / countmeanval)) %>%
  rename(CNTRY = countrycode)


a <-  left_join(a, lancetdata)  %>%
  select(Age, Pop, CNTRY, deaths, ax, MID, LOW, HIGH) %>%
   filter(!is.na(MID))
a2 <- left_join(a, GBD_data) %>%
  mutate(width = ifelse(Age == 0, 1,
                 ifelse(Age == 1, 4,5)),
         mx_base = deaths/Pop,
         mx_low = (deaths + LOW * perdist)/Pop,
         mx_mid = (deaths + MID * perdist)/Pop,
         mx_high = (deaths + HIGH * perdist)/Pop,
         qx_base = ifelse(Age == 80, 1, mx_base/(1+((width-ax)*mx_base))),
         qx_low = ifelse(Age == 80, 1, mx_low/(1+((width-ax)*mx_low))),
         qx_mid = ifelse(Age == 80, 1, mx_mid/(1+((width-ax)*mx_mid))),
         qx_high = ifelse(Age == 80, 1, mx_high/(1+((width-ax)*mx_high)))
  )

write.table(a2, "C:/Users/Matt/Documents/Matt - Work Stuff/r code/r_output2.txt", sep="\t")

lt_lancet <- read.csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/lt_afterr_09032017.csv")
lt_lifetables <- read.csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/lancet_lifetables_09032017.csv")

lt_lancet$'country name' <- lt_lancet$COUNTRY
lt_lancet<- lt_lancet[order(lt_lancet$DIF_MID), ]
lt_lancet$'country name' <- factor(lt_lancet$'country name', levels = lt_lancet$'country name')

ggplot(lt_lancet, aes(x=RANK, y=DIF_MID, ymin = DIF_LOW, ymax = DIF_HIGH, label=DIF_MID)) +
  geom_pointrange(stat="identity") +
  geom_point(stat="identity", fill="black", size=8) +
   geom_hline(yintercept=0) +
  geom_text(color="white", size=2) +
  #scale_x_discrete(limits = rev(levels(the_factor)))
  scale_x_reverse(breaks = lt_lancet$RANK, labels=lt_lancet$COUNTRY) +
  coord_flip() +
  theme_bw() +
  #ylim(-2, 2) +
  theme(panel.grid.minor = element_blank()) +
 
   labs(y=expression(Delta* e[0]),
        x= "Country")
        #title="Change in Life Expectancy due to Climate Change in 28 European Countries",
        #caption="Based on data from the Human Mortality Database and Forzieri et al (2017)"

selection <- lt_lifetables %>%
  filter(CNTRY %in% c("FRATNP", "IRL", "ESP", "DEUTNP")) %>%
  mutate(DIF_MID =(ex_mid - ex_base ),
         DIF_LOW = (ex_low - ex_base),
         DIF_HIGH = (ex_high - ex_base)
         )

ggplot(selection) +
  geom_ribbon(aes(x = Age, ymin=DIF_LOW, ymax=DIF_HIGH, fill=factor(CNTRY,labels= c("Germany", "Spain", "France", "Ireland"))), alpha=0.2) +
 geom_line(aes(x = Age, y = DIF_MID, colour=factor(CNTRY,labels= c("Germany", "Spain", "France", "Ireland"))), size=0.9) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2), legend.title=element_blank()) +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(palette="Set1")+
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(limits = c(0,80), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.5,0.1), expand = c(0, 0)) +
 # scale_shape_discrete(labels= c("Germany", "Spain", "France", "Ireland"))+
  labs(y = expression(Delta* e[x]),
       x = "Age"
       )
 

ggplot(lt_lancet) +
  geom_segment(aes(x=0, xend=MID_e0,y=BASE_e0, yend=MID_e0))
  

ggplot(data=lt_lancet) +
  geom_point(aes(x = long , y = lat,  size=abs(DIF_MID))) +
  geom_contour(aes(x = long, y = lat, z =(DIF_MID)))
  stat_density_2d(aes(x = long, y =lat, fill = ..level..), geom = "polygon")
  geom_density2d(aes(x = long, y = lat, z=DIF_MID))
  geom_smooth(aes(x = DIF_MID, y =lat), method="lm")  +
  geom_point(aes(x = DIF_MID, y = long, color="red")) +
  geom_smooth(aes(x = DIF_MID, y =long), method="lm")

  ggplot(lt_lancet, aes(x = long, y = lat, z = DIF_MID))+ 
    #geom_contour(aes(x=long, y=lat, weight =DIF_MID), data= lt_lancet)
  geom_density2d(aes(x=long, y=lat, weight =DIF_MID))
  
data(Europe)
Europe2 <- append_data(Europe, lt_lancet, key.shp = "iso_a3", key.data = "ISO3", ignore.na = TRUE)

bb(Europe2)

  tm_shape(Europe2) +
  tm_fill("DIF_MID", title = expression(Delta* e[0 ~italic(MID)]), palette = "YlOrBr", style = "jenks") +
  tm_borders(alpha = 0.5) +  
  tm_text("Test" , size="AREA", root=5) +
  tm_format_Europe() 
        

  
