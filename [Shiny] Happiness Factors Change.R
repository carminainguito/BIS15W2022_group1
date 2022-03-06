#happiness factors change from 2015-2019
library(shiny)

complete_happiness$year <- as.character(complete_happiness$year)

ui <- dashboardPage(
  dashboardHeader(title = "Happiness"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = "Plot Options", width = 3,
          selectInput("factor", "Select Happiness Factors", choices = c("economy_gdp_per_capita", "family", "health_life_expectancy", "freedom", "trust_government_corruption"), 
                      selected = "economy_gdp_per_capita"),
          helpText("Factors' values reflect the extent to which a factor contributes to the calculation of the Happiness Score")),
      
      box(title = "Plot of Happiness Factors Data", width = 7,
          plotOutput("plot", width = "600px", height = "500px")))))

server <- function(input, output, session) { 
  output$plot <- renderPlot({
    complete_happiness %>%
      group_by(year) %>% 
      summarize(mean_factor_value = mean(get(input$factor), na.rm=TRUE)) %>% 
      ggplot(aes_string(x="year", y="mean_factor_value", fill="year"))+
      geom_col()+
      geom_text(aes(label = round(mean_factor_value, 2)),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                color = "black",
                size = 5)+
      theme_linedraw()+
      scale_fill_brewer(palette = "Pastel1")+
      labs(x=NULL,
           y="mean factor value")+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18))
    
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)

