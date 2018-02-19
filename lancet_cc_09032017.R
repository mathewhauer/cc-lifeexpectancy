rm()
library(tidyverse)
library(HMDHFDplus)
library(LifeTables)
library(xlsx)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(data.table)

###   Inputting user info for HMD
myusername <- userInput()
mypassword <- userInput()

###   Importing the data from Forizeri et al.
lancetdata <- tribble(
  ~CNTRY,~MID,~LOW,~HIGH,
  "AUT",467,121,797,
  "BEL",2841,91,5007,
  "BGR",44,36,63,
  "CHE",1981,497,3247,
  "CYP",143,62,279,
  "CZE",375,48,628,
  "DEUTNP",7319,620,12132,
  "DNK",24,4,56,
  "EST",12,0,22,
  "ESP",41326,33495,53802,
  "FIN",23,3,44,
  "FRATNP",38728,14235,72947,
  "GRC",2838,1520,5606,
  "HRV",1145,624,1811,
  "HUN",559,191,985,
  "IRL",1566,476,4195,
  "ISL",3,1,5,
  "ITA",41965,23694,57401,
  "LTU",15,1,25,
  "LUX",392,12,820,
  "LVA",17,1,30,
  "MLT",0,0,0,
  "NLD",1984,126,4114,
  "NOR",15,4,34,
  "POL",313,52,561,
  "PRT",4573,3808,6284,
  "ROU",177,97,301,
  "SWE",52,10,113,
  "SVN",561,251,847,
  "SVK",63,17,118,
  "GBR_NP",2515,388,7484
)
#  read.csv("data/LANcET_CCMORTDATA.csv",colClasses=c("CNTRY"="character"))
 
###   Getting a country list from the HMD
Countries <- getHMDcountries()

###   Downloading the DEATHS data from the HMD in 5-year age groups by single-year.
deaths <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Deaths_5x1", fixup=TRUE, username = myusername, password = mypassword)
Dat$CNTRY <- CNTRY
Dat}))

###   Downloading the POPULATION data from the HMD.
pops <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Population", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

###   Downloading the LIFE TABLE data from the HMD in 5-year age groups by single-year.
lt <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "bltper_5x1", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

###   Topcoding the age groups to 80+ of the LIFE TABLE data, and summing the ax value.
lt2 <- lt%>%
  mutate(Age = ifelse(Age >= 80, 80, Age)) %>%
  group_by(CNTRY, Year, Age)%>%
  summarise(ax = sum(ax))

###   Topcoding the age groups of the DEATHS data and summing.
deaths2 <- deaths %>%
  mutate(Age = ifelse(Age >=80, 80, Age)) %>%
  group_by(CNTRY, Year, Age) %>%
  summarise(deaths = sum(Total) )

###   Recoding the Population data to single year of age
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

###   Joining the POPULATION, DEATHS, and LIFE TABLE data and subseting to the maximum year in the data.
a <- left_join(pops2, deaths2)
a <- left_join(a, lt2)
a <- group_by(a, CNTRY, Age) %>%
  filter(Year == max(Year)) 

###   Importing the GBD data.
###   Data is 5-year age groups of mortality rates for Environmental heat and cold exposure for 2006-2015. 
###   Analysis uses the mean of the last 10 years of data for this death distribution.
GBD_data <- read.csv("data/GBD_data.csv") %>%
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

ggplot(data=GBD_data, aes(x=Age, y=perdist)) +
  geom_line(aes(group =CNTRY, col=CNTRY), alpha=0.7) +
  geom_smooth(span=0.3, se=F, col="black") +
  theme_bw() +
  scale_y_log10()+
  labs(y="log(Mortality Percentage Distribution)")


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
         qx_high = ifelse(Age == 80, 1, mx_high/(1+((width-ax)*mx_high))))


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
        

  
