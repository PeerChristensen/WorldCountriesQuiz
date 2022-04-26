
library(shiny)
library(shinyWidgets)
library(maps)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinyjs)

world <- map_data("world") 
countries <- world %>% distinct(region) %>% pull()
n_countries <- length(countries)

p <- ggplot() +
	geom_polygon(data = world, 
							 aes(x = long, y = lat, group=group, text="?"),
							 colour="snow",fill="grey",size=.2) +
	theme_map()

ui <- fluidPage(useShinyjs(),
								title = "World Countries Quiz",
								titlePanel(h1("How many countries do you know? - test", align="center",tags$title('This is my page'))),
								fluidRow(
									column(12,
												 textInput("input",label = "", placeholder = "Type country names here"),
												 h4(textOutput("count")),
												 align = "center")
								),
								fluidRow(
									column(12,
												 plotlyOutput("map", height="550px", width="1300px"),
												 align = "center")
								),
								fluidRow(
									column(12,
												 actionButton("show_missing", "Show missing country names"),
												 align = "center")
								),
								fluidRow(
									column(12,
												 tableOutput("missing_countries"),
												 align="center")
								),
								br(),
								br()
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
	
	show("hide_missing")
	
	output$count <- renderText(glue::glue("{length(rv$named_countries)} / {n_countries}"))
	
	output$map <- renderPlotly({
		p 
	})
	
	rv <- reactiveValues(
		named_countries = c(),
		new_country = c()
	)
	
	observeEvent(input$input, {
		
		show("show_missing")
		hide("missing_countries")
		
		#rv$inputs <- append(rv$inputs, input$input)
		
		if (input$input %in% countries) {
			
			updateTextInput(session,"input", "", value="", placeholder="")
			
			if (!input$input %in% rv$named_countries) {
				rv$named_countries <- append(rv$named_countries, input$input)
				rv$new_country <- input$input
			}
			
			named_countries_df <- world %>% filter(region %in% rv$named_countries)
			named_new_country_df <- world %>% filter(region %in% rv$new_country)
			
			output$map <- renderPlotly({
				
				p + geom_polygon(data = named_new_country_df,
												 aes(x = long, y = lat, group = group, text = region),
												 fill = "slateblue4", colour = "snow")
			})
			output$count <- renderText(glue::glue("{length(rv$named_countries)} / {n_countries}"))
			
		}
	})
	
	observeEvent(input$show_missing, {
		
		hide("show_missing")
		show("missing_countries")
		
		output$missing_countries <- renderTable({
			
			missing <- setdiff(countries, rv$named_countries)
			missing_df <- tibble(Country = missing) %>% arrange(Country)
		})
	})
}

shinyApp(ui = ui, server = server)
