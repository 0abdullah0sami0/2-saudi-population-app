shinyServer(function(input,output,session){
  
  plot <- reactive({
    
    Plot = Population %>% 
      mutate(year = as.Date(paste0("01-01-",year),format = "%d-%m-%Y")) %>%
      group_by(year,gender) %>% 
      summarise(Population = sum(population)) %>%
      
      ggplot(aes(x = year, y = Population , color = gender)) +
      geom_point(aes(text = format(year,"%Y"))) +
      geom_line() +
      xlab("Year") +
      ylab("Population (in Millions)") +
      scale_color_discrete(name = "Gender") +
      theme_classic() +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
      scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
      theme(plot.margin = margin(1.5,0.5,0.5,0.5,"cm"),plot.title = element_text(size = 12))
    
    ggplotly(Plot,tooltip = c("text", "y")) %>%
      layout(title = list(text = paste0("Evolution of Saudi Arabia's population by Gender \n between 2010 and 2022",
                                        '<br>',
                                        '<sup>',
                                        'Source: portal.saudicensus.sa','</sup>')))
  })
  
  output$plot1 <- renderPlotly({
    plot()
  })
  
  output$plot2 <- renderPlotly({
    plot()
  })
  
})
  