# exploratory data analysis of global terrorism database
library(tidyverse)
library(plotly)
library(maps)

gtd <- read_csv('globalterrorismdb_0617dist.csv') %>% as_tibble()

ggplot(data=gtd) + geom_bar(mapping= aes(x=iyear)) + labs(x='Year',y='Number of terror incidents')

# plot the net number of terrorist attacks per year per country
perYear <- gtd %>% group_by(iyear,region_txt) %>% summarise(nIncidents=n())
ggplot(data=perYear) + 
  geom_area(mapping = aes(x=iyear,y=nIncidents,group=region_txt,fill=region_txt),position='fill') + 
  labs(x='Year',y='Proportion of terror incidents',fill='Region')

# examining the periods of terrorism in western europe from 1970 to 1980
we <- gtd %>% filter(region_txt == 'Western Europe',between(iyear,1970,1980)) # %>% group_by(country)

# analyzing the relationship between country, attacktype, and number of victims
# number of attacks per type and country
ggplot(data=we) + geom_count(mapping = aes(x=country_txt,y=attacktype1_txt)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Country',y='Attack type')

weByMean <- we %>% group_by(country_txt,attacktype1_txt) %>% summarise(meanKills = mean(nkill,na.rm=TRUE))
weBySum <- we %>% group_by(country_txt,attacktype1_txt) %>% summarise(nKills = sum(nkill,na.rm=TRUE))
# in terms of efficacy of attack type
ggplot(data=weByMean,mapping = aes(x = country_txt, y = attacktype1_txt)) +
  geom_tile(mapping = aes(fill = meanKills)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Country',y='Attack type', fill='Mean killed')

# in termns of total killed
ggplot(data=weBySum,mapping = aes(x = country_txt, y = attacktype1_txt)) +
  geom_tile(mapping = aes(fill = nKills)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x='Country',y='Attack type', fill='Number of killed')

# Let's take a deeper look at UK assasinations
# against whom where they commited? Which weapons were used?
ukAssas <- filter(we,country_txt=='United Kingdom',attacktype1_txt == 'Assassination')
ggplot(data = ukAssas) + geom_count(mapping = aes(x=targtype1_txt,y=gname)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Type of target',y='Name of perpetrator group')

# Now let's switch topic and take a look at south from 1970 to 2005
sa <- filter(gtd,region_txt == 'South America',between(iyear,1970,2005))

# plotting country, attacktype, and number of victims
ggplot(data=sa) + geom_count(mapping = aes(x=country_txt,y=attacktype1_txt)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Country',y='Attack type')

# which organizations killed the most people in south america during this period?
saDeadliest <- sa %>% group_by(gname,country_txt) %>% 
  summarise(killed=sum(nkill)) %>% arrange(desc(killed)) %>% filter(killed>5)

ggplot(data = saDeadliest) + geom_tile(mapping = aes(x=country_txt,y=gname,fill=killed)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Country',y='Name of perpetrator group',fill='Number of killed')

# now let's switch regions again and take a look at the middle east and North Africa from 1987 to 2016
mena <- filter(gtd,region_txt == 'Middle East & North Africa',between(iyear,1987,2016))

# plotting country, attacktype, and number of victims
ggplot(data=mena) + geom_count(mapping = aes(x=country_txt,y=attacktype1_txt)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Country',y='Attack type')

# let's look at the cities with the most bombings in this region where more than 200 people died
menaBombs <- mena %>% filter(attacktype1_txt == 'Bombing/Explosion') %>%
  group_by(country_txt,city) %>% summarise(killed=sum(nkill)) %>% arrange(desc(killed)) %>% filter(killed>200)

ggplot(data=menaBombs) + 
  geom_col(mapping = aes(x=city,y=killed,fill=country_txt)) + 
  coord_flip() +
  labs(y='Number killed in bombing',x='City',fill='Country')

# Let's now take a look at the number of suicide attacks around the world throughout time
# We will also make this an interactive plot using plotly
suicide <- gtd %>% filter(suicide==1) %>% group_by(iyear,country_txt) %>% summarise(n=n())

# converting years to categorical variables to create tile plot
yearBreaks <- 1980:2016
labels <- 1981:2016
suicideCat <- suicide %>% mutate(yearCategory=cut(iyear, breaks=yearBreaks,labels=labels))
# tile plot
ggplot(data=suicideCat,mapping = aes(x = yearCategory, y = country_txt)) +
  geom_tile(mapping = aes(fill = n)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='Year',y='Country',fill='Number of suicide attacks')

# Looking at the Middle East and North Africa and removing Iraq from plot since it's off the chart
suicide1 <- gtd %>% 
    filter(suicide==1,region_txt== 'Middle East & North Africa' & country_txt != 'Iraq') %>%
    group_by(iyear,country_txt) %>% summarise(n=n())

# creating a ggplot2 plot but not showing it
ggSuicide1 <- ggplot(data=suicide1) + 
    geom_line(mapping = aes(x=iyear,y=n,colour=country_txt,group=country_txt)) +
    guides(colour=FALSE) + 
    labs(x='Year',y='Number of suicide attacks')
# showing an interactive plot
ggplotly(ggSuicide1,tooltip = c('x','colour','y'))

# Now we will plot the location of all suicide terrorist attacks on a map
world_map <- map_data("world")

# We create a base plot with gpplot2
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

# Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

suicideLoc <- gtd %>% filter(suicide==1) %>% group_by(latitude,longitude,city) %>% summarise(n=n(),killed=sum(nkill,na.rm=TRUE))

map_data <- 
  base_world +
  geom_point(data=suicideLoc, 
             aes(x=longitude, y=latitude), colour="Red", 
             fill="Pink",pch=21, size=1, alpha=I(0.7)) +
  labs(title='Suicide attacks from 1980 to 2016')

map_data
