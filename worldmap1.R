library(plotly)
library(tidyverse)


df <- read_csv("countries.csv")

g <- list(
	scope = 'world',
	visible = F,
	showcountries = T,
	countrycolor = toRGB("grey"),
	resolution = 50,
	showland = TRUE,
	hovertext=T,
	landcolor = toRGB("white")
)


fig <- plot_ly(type = 'scattergeo', 
							 mode = 'markers',
							 text = df$COUNTRY) %>% 
	layout(geo = g)
fig

fig <- plot_ly(df, type='choropleth', locations=df$CODE, z="white", text=df$COUNTRY, colorscale="Blues")
fig


library(rnaturalearth)
world <- ne_countries(returnclass = "sf")

#> [1] "sf"    "data.frame"
data=world %>% filter(name=="Argentina")
plot_ly(world, 
				color = I("gray90"), 
				stroke = I("black"), 
				span = I(1))


map_data("world", c("canada","denmark")) %>%
				 	group_by(group) %>%
				 	plot_geo(x = ~long, y = ~lat) %>%
				 	add_markers(size = I(1))



# subset for all countries that are filled by a color
library(dplyr)


world <- map_data("world") 

ggplot(world, aes(x = long, y = lat, group = group)) +
	geom_polygon(col = "white")

ggplot() +
	# first layer: all countries, no fill, no white outlines
	geom_polygon(data = world, 
							 aes(x = long, y = lat)) +
	# second layer: only countries with a fill
	geom_polygon(data = world5, 
							 aes(x = long, y = lat, group = group, fill = interest),
							 color = "white") +
	# apply the color scale
	scale_fill_manual(breaks = c("interest", "past", "current"), 
										values = c("#4dc11d","#26660b","#9def7a"))   