###########################
##### SERVER SIDE #########
###########################
server <- function(input, output) {
  output$value1 <- renderValueBox({ 
    valueBox(
      formatC(Totalsales, format="d", big.mark=',')
      ,paste('Total Sales')
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "light-blue")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(GrossProfit, format="d", big.mark=',')
      ,paste('Gross Profit')
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(GP_Percentage, format="d", big.mark=',')
      ,paste('Gross Profit as % of Sales')
      ,icon = icon("list",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value4 <- renderValueBox({ 
    valueBox(
      formatC(Total_Order, format="d", big.mark=',')
      ,paste('Number of goods ordered')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "purple")  
  })
  output$value5 <- renderValueBox({
    valueBox(
      formatC(paste(state_with_the_highest_sales$State, sep = ""), format="d", big.mark=',')
      ,paste(' State with highest sales value')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "purple")   
  })
  output$value6 <- renderValueBox({
    valueBox(
      formatC(paste(mgr_with_the_highest_sales$Manager, sep = ""), format="d", big.mark=',')
      ,paste('Manager with the highest sales value')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "purple")   
  })
  
  output$Trend <- renderPlotly({
    filtered <-
      full_data %>%
      filter(Chain == input$SupplyChain,
             State == input$States)
    
    plot_ly(data = filtered, x = ~FY.Qtr, y = ~Revenue, type = 'bar', color = I("grey"))%>%
      layout(xaxis = list(title = "Quarters"),
             yaxis = list(title = "Revenue"))
    
    
  })
  
  output$Category <- renderPlotly({
    filtered <-
      full_data %>%
      filter(Chain == input$SupplyChain,
             State == input$States)
    
    plot_ly(data = filtered, x = ~Category, y = ~Revenue,type = 'bar', color = I("grey"))%>%
      layout(xaxis = list(title = "Category"),
             yaxis = list(title = "Revenue"))
  })
  
  output$Managers <- renderPlot({
    Managers_sales_plot
  })
  
  output$Buyers <- renderPlot({
    Buyer_sales_plot
  })
  
}

shinyApp(ui, server)
