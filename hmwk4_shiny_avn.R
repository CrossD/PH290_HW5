### my first shiny app! ###

#load packages
library(shiny)
library(tidyverse)
library(gapminder)
library(stringr)
library(plotly)

#define analytical dataset (gapminder)
data <- gapminder::gapminder

#groupby choices can only be categorical
groupby_choices <- names(data[,sapply(data, class) == "factor"])

#viz choices can be any column
viz_choices <- names(data %>% select(-continent))

viz_choices_country <- names(data %>% select(-c(country, continent)))

#front-end

ui <- fluidPage(
  titlePanel("PH290: Homework 4 Shiny App"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("var_groupby", "Variable to group by?", choices = groupby_choices, selected = "continent"),
      selectInput("var_visualize", "Variable to summarize and visualize?", choices = viz_choices_country, selected = "lifeExp"),
      textInput("title", "Give a title for your plot!", value = "Type here!"),
    ),
    mainPanel(
      verbatimTextOutput("unique_text"),
      plotlyOutput("plot")
      )
  )
)

server <- function(input, output, session) {
  #reactive component for group by variable
  var_groupby <- reactive({
    get(input$var_groupby, data)
  })  
  
  #reactive component for variable to summarize/visualize
  var_visualize <- reactive({
    get(input$var_visualize, data)
  })
  
  #reactive component for name of column?!
  name_visualize <- reactive({
    as.character(input$var_visualize)
    })
  
  name_groupby <- reactive({
    as.character(input$var_groupby)
  })
  
  output$unique_text <- renderPrint({
    paste0("You are currently visualizing ", name_visualize(), " by ", name_groupby())
  })
  
  output$year_text <- renderPrint({
    print("Dots indicate that data were collected for that year and that geography.")
  })
  
  output$plot <- renderPlotly({
    if (name_visualize() == "year"){
      p <- ggplot(data, aes(y = var_groupby(), 
                            x = var_visualize(),
                            text = paste0(str_to_title(input$var_visualize), ": ", var_visualize(),
                                         "\n", str_to_title(input$var_groupby), ": ", var_groupby(),
                                         "\n", "Data were collected for this geography and year."))) +
        geom_point(color = "forestgreen") + 
        scale_y_discrete(limits = rev) +
        labs(title = input$title,
             x = str_to_title(input$var_visualize),
             y = str_to_title(input$var_groupby)) +
        guides(fill = "none")
    
      ggplotly(p, tooltip = "text", height = (200 + 50*data %>% 
                                                group_by(var_groupby()) 
                                              %>% summarise(n = n()) 
                                              %>% nrow())) %>%
        layout(showlegend = FALSE)
    }
    else{
      
      
      fig <- data %>%
        plot_ly(
          y = ~var_visualize(),
          x = ~var_groupby(),
          split = ~var_groupby(),
          type = 'violin',
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T)
        ) 
      
      fig <- fig %>%
        layout(
          yaxis = list(
            title = str_to_title(input$var_visualize),
            zeroline = F
          ),
          xaxis = list(title = str_to_title(input$var_groupby)), 
          title = input$title,
          showlegend = FALSE
        )
      
      fig
      
      
  
      
    }

  })

}

shinyApp(ui, server)
