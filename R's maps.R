library(tidyverse)
library(maps)
library(gganimate)
library(gifski)

##tell us about what is in the library 'maps'
help(package="maps")
?maps_data
my_world_map = map_data('world')

##
ggplot(data = my_world_map, mapping = aes(x=long, y=lat, group=group))+
  geom_polygon(fill='black', color='white')


##let's look at a country or many countries
germany = filter(my_world_map, region=="Italy")

ggplot(data = germany, mapping = aes(x=long, y=lat, group=group))+
  geom_polygon(fill='green', color='black')


  ##
#germany_france = filter(my_world_map, region=="Germany" region =="France")

#ggplot(data = germany_france, mapping = aes(x=long, y=lat, group=group))+
#geom_polygon(fill='white', color='black')



##get a region by filterring the lat and the long
a_region = filter(my_world_map, long>-10 & long<15.1 & lat>32 & lat<55)

#
ggplot(data = a_region, mapping = aes(x=long, y=lat, group=group))+
  geom_polygon(fill='white', color='black')




mauritania = filter(my_world_map, region=="Japan")

l = ggplot(data = mauritania, mapping = aes(x=long, y=lat, group=group))+
  geom_polygon(fill='white', color='black')

l
###US states
my_state_map=map_data("state")
#
ggplot(data = my_state_map, mapping = aes(x=long, y=lat, group=group))+
  geom_polygon(fill='white', color='black')



##making gganimate graphsn
data("mtcars")
mtcars$gear
ggplot(mtcars, aes(x=factor(cyl), y=mpg))+
  geom_boxplot()

#facetted box plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg))+
  geom_boxplot()+
  facet_wrap(~gear)


#animating 
my_anime = ggplot(mtcars, aes(x=factor(cyl), y=mpg))+
  geom_boxplot()+
  transition_states(gear)
my_anime

anime1 = my_anime+
  enter_fade()+
  exit_fade()
anime1
#install.packages('plotly')

