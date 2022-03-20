
library(shiny)
library(shinyWidgets)
library(maps)
library(ggthemes)
library(tidyverse)
library(plotly)

world <- map_data("world") 
countries <- world %>% distinct(region) %>% pull()
n_countries <- length(countries)

p <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group=group, text="?"),
                 colour="snow",fill="grey",size=.2) +
    theme_map()

ui <- fluidPage(title = "World Countries Quiz",

    titlePanel(h1("How many countries do you know?", align="center",tags$title('This is my page'))),
    fluidRow(
        column(12,
        textInput("input",""),
        h4(textOutput("count")),
        align = "center")
        ),
    fluidRow(
        column(12,
        plotlyOutput("map", height="600px"),
        align = "center")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$count <- renderText(glue::glue("{length(rv$named_countries)} / {n_countries}"))
    
    output$map <- renderPlotly({
        p 
    })
    
    rv <- reactiveValues(
        named_countries = c()
    )
    
    observeEvent(input$input, {
        
        rv$inputs <- append(rv$inputs, input$input)

        if (input$input %in% countries) {
            
            updateTextInput(session,"input", "Input", value="")
            
            if (!input$input %in% rv$named_countries) {
                rv$named_countries <- append(rv$named_countries, input$input)
            }
            named_countries_df <- world %>% filter(region %in% rv$named_countries)
            
            output$map <- renderPlotly({
                
                p + geom_polygon(data=named_countries_df,
                                 aes(x= long, y = lat, group=group,text=region),
                                 fill="slateblue4",colour="snow")
            })
            output$count <- renderText(glue::glue("{length(rv$named_countries)} / {n_countries}"))
        }
    })
}

shinyApp(ui = ui, server = server)
