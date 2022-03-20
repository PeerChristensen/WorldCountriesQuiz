
library(maps)
library(ggthemes)
library(tidyverse)
library(plotly)

world <- map_data("world") 

countries <- c("Denmark", "Argentina")
countries <- world %>% filter(region %in% countries)

p=ggplot() +
	geom_polygon(data = world, 
							 aes(x = long, y = lat, group=group, text="?"),
							 colour="snow",fill="grey",size=.1) +
	geom_polygon(data=countries,
							 aes(x= long, y = lat, group=group,text=region),
							 fill="blue",text=countries$region) +
	theme_map()

ggplotly(p)


