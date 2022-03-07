#happiness factors & happiness score relationship change from 2015-2019
library(shiny)

complete_happiness$year <- as.numeric(complete_happiness$year)

ui <- dashboardPage(
  dashboardHeader(title = "Happiness"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = "Plot Options", width = 3,
          selectInput("factor", "Select Happiness Factors", choices = c("economy_gdp_per_capita", "family", "health_life_expectancy", "freedom", "trust_government_corruption"), 
                      selected = "economy_gdp_per_capita"),
          sliderInput("year",
                      "Year:",
                      min = 2015,
                      max = 2019,
                      value = 2015,
                      step = 1),
          
          helpText("Factors' values reflect the extent to which a factor contributes to the calculation of the Happiness Score")),
      
      box(title = "Plot of Happiness Factors Data", width = 7,
          plotOutput("plot", width = "600px", height = "500px")))))

server <- function(input, output, session) { 
  output$plot <- renderPlot({
    complete_happiness %>% 
      filter(year==input$year) %>% 
      ggplot(aes_string(x=input$factor, y="happiness_score"))+
      geom_point(size = 4, alpha = 0.5)+
      geom_smooth(formula = y ~ x, method=lm, se=T)+
      labs(y="Happiness Score")+
      theme_linedraw()+
      scale_color_brewer(palette = "Pastel1")+
      labs(x="factor value",
           y="happiness score")+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18))
    
    
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
