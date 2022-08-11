#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)

shinyServer(function(input, output) {
  
  groupColors <- colorFactor(c("violetred", "navyblue","turquoise1"),
                             domain = c("Entire home/apt", "Private room","Shared room"))
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      #addLegend(position = "bottomleft", pal = groupColors, values = manhat$room_type, opacity = 10, title = "Room Type") %>% 
      setView(lng = -73.971321, lat = 40.776676, zoom = 13)
     })
  
  mapdf <- reactive({
    print(input$neighbourhood)
    manhat %>%
      filter(neighbourhood %in% input$neighbourhood &
              #zipcode %in% input$zipcode &
               room_type %in% input$room_type & 
               price >= input$price[1] &
               price <= input$price[2] &
               number_of_reviews >= input$review[1] &
               number_of_reviews <= input$review[2]) 
  })
  
  
  observe({ 
    print(mapdf())
    proxy <- leafletProxy("map",data = mapdf()) %>%
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      addCircleMarkers(lng = mapdf()$longitude, lat = mapdf()$latitude, radius = 2, color = groupColors(manhat$room_type),
                       group = "CIRCLE", popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                                        'Listing ID:', id, '<br/>',
                                                        'Neighborhood:', neighbourhood, '<br/>',
                                                        'Zipcode:', zipcode, '<br/>',
                                                        'Room Type:', room_type,'<br/>',
                                                        'Price ($):', price,'<br/>',
                                                        'Number of Reviews:', number_of_reviews,'<br/>'))
      
  })
    
  
  output$table <- DT::renderDataTable({nypd})
  
  output$listings <- DT::renderDataTable({manhat})
  
  ########
  #analysis info connect
  #######
  
  output$stats <- renderPlot({
    tally%>%
      filter(zipcode %in% input$zipcode)%>%
      group_by(date)%>%
      summarise(freq = sum(n))%>%
      ggplot(aes(x=date, y = freq, group = 1), size = 1.2)+
      geom_line(color = "red")+
      geom_point()+
      labs(x = "2022", y = "Number of Reported Crimes") +
      theme_minimal()
  })
  
  
  output$worst_zip <- renderPlot({
    ggplot(data = worst_zip, aes(x = reorder(zipcode, -n) , y = n))+
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "",y = "Total Incidents (01/2022 - Present)")+
      ggtitle("Zipcodes with Highest Amount of Reported Crime")+
      theme(axis.text.x = element_text(size=10,vjust=0.4))
  })
  
  
  output$category = renderPlot({
  
    ggplot(cat, aes(x ="", y = n, fill=category))+
      geom_bar(width = 1, stat = "identity")+
      theme_minimal() +
      labs(x = "",y = "Total Reported Incidents")+
      ggtitle("Category Breakdown by Count")+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$exp_neighborhood = renderPlot({
    neighbourhood%>%
      arrange(desc(mean_price))%>%
      head(40)%>%
      ggplot(aes(x=mean_price,y=reorder(neighbourhood,mean_price))) +
      geom_col()+
      labs(
        x="Average Prices",
        y="Neighborhood Name",
        title="Most Expensive Neighborhoods")+
      theme_minimal()+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$pop_neighborhood = renderPlot({
    neighbourhood%>%
      arrange(desc(mean_popularity))%>%
      head(40)%>%
      ggplot(.,aes(x=mean_popularity,y=reorder(neighbourhood,mean_popularity))) +
      geom_col()+
      labs(
        x="Average Number of Reviews",
        y="Neighborhood Name",
        title="Most Popular Neighborhoods based on Reviews")+
      theme_minimal()+
      theme(axis.text.x = element_text(size=11,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$avg_roomtype = renderPlot({
    ggplot(avg_room,aes(x=room_type,y=AvgPrice,fill=(room_type))) +
      geom_bar(stat="identity",position="dodge") + 
      theme_minimal() + 
      labs(x="",y="Average Price in Dollars",title="Average Price by Room Type",
           fill="Room Type") + 
      theme(axis.text.x = element_text(size=15,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$median = renderPlot({
    ggplot(manhat, aes(x = neighbourhood, y = price))+
      geom_boxplot()+
      ggtitle("Price Distribution, by Neighborhood")+
      theme_minimal() +
      labs(x = "", y = "") +
      theme(axis.text.x = element_text(angle =90,size=10,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$rt_listings = renderPlot({
    ggplot(manhat, aes(x = neighbourhood, y = room_type))+
      geom_count()+
      ggtitle("Number of Room Types, by Neighborhood")+
      theme_minimal() +
      labs(x = "", y = "Room Type") +
      theme(axis.text.x = element_text(angle =90,size=10,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$reports = renderPlot({
    ggplot(new)+
      geom_bar(aes(x = reorder(offense,-freq), y = freq), stat = "identity")+
      ggtitle("Most Frequently Reported Offenses")+
      theme_minimal() +
      labs(x = "", y = "Number of Reports") +
      theme(axis.text.x = element_text(angle = 50,size=9,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$low_crime = renderPlot({
    ggplot(data=best_zip[best_zip$zipcode %in% c(10044,10282, 10069, 10280, 10281, 74),], 
           aes(x=factor(zipcode), y=n))+
      geom_bar(stat="identity", width = 0.5, group=1)+
      theme_minimal()+
      ggtitle("Zipcodes with Highest Amount of Reported Criminal Activity")+
      labs(x = "", y = "Total Incidents (01/2022 - Present)") +
      theme(axis.text.x = element_text(size=10,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$dates = renderPlot({
    ggplot(date_tally)+
      geom_point(aes(x = date, y = n))+
      theme_classic()+
      ggtitle("Number of reports, by Date")+
      labs(x = "",y = "Number of Reports")+
      theme(axis.text.x = element_text(size=13,vjust=0.4))+
      theme(panel.border = element_rect(color = "black",
                                        fill = NA,
                                        size = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
})




  
