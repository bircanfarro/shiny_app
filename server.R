shinyServer(function(input, output, session){
 
  
#Vizualization of refugees from Turkey within the year range
  
  output$from_turkey <- renderLeaflet({
    dt_from <- totalWithinYears(dt_ref_from_turkey, FALSE, min(dt_ref_from_turkey$Year), max(dt_ref_from_turkey$Year))
    dt_from_coords <- dt_from[dt_coords, on="Destination==name", nomatch=0]
    dt_from_coords$Radius <- calcRadius(dt_from_coords$Total)
    leaflet(dt_from_coords) %>%
      addCircleMarkers(radius = ~Radius,label = ~format(Total, big.mark=","), 
        stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#cc0000") %>%
          addProviderTiles(providers$CartoDB.Positron)
  })
  
  
#Vizualization of refugees live in Turkey within the year range
  
  output$to_turkey <- renderLeaflet({
    dt_to <- totalWithinYears(dt_ref_to_turkey, TRUE,  min(dt_ref_to_turkey$Year), max(dt_ref_to_turkey$Year))
    dt_to_coords <- dt_to[dt_coords, on="Origin==name", nomatch=0]
    dt_to_coords$Radius <- calcRadius(dt_to_coords$Total)
    leaflet(dt_to_coords) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~Radius, label = ~format(Total, big.mark=","), 
        stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#009900") %>%
           addProviderTiles(providers$CartoDB.Positron)
  })
  
  
#Vizualization of Turkish asylum seekers within the year range
  
  output$asylee <- renderLeaflet({
    dt_asylee_total <- totalAsyleesWithinYears(dt_asylee, min(dt_asylee$Year), max(dt_asylee$Year))
    dt_asylee_total_coords <- dt_asylee_total[dt_coords, on="Destination==name", nomatch=0]
    dt_asylee_total_coords$Radius <- calcRadius(dt_asylee_total_coords$Total)
    leaflet(dt_asylee_total_coords) %>%
      addCircleMarkers(radius = ~Radius,label = ~format(Total, big.mark=","), 
        stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#0000cc") %>%
          addProviderTiles(providers$CartoDB.Positron)
  })
  
  
  
  
  
 #Observation purpose was to prevent the recalibrating of the mapsize that was being interacted by the year slider.

  observe({
    dt_from <- totalWithinYears(dt_ref_from_turkey, FALSE, input$YearRangeFrom[1], input$YearRangeFrom[2])
    dt_from_coords <- dt_from[dt_coords, on="Destination==name", nomatch=0]
    dt_from_coords <- dt_from_coords[Total > 0]
    dt_from_coords$Radius <- calcRadius(dt_from_coords$Total)
    leafletProxy("from_turkey", data = dt_from_coords) %>%
     clearMarkers() %>%
      addCircleMarkers(radius = ~Radius, label = ~format(Total, big.mark=","), 
        stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#cc0000")
    
    output$table_from = DT::renderDataTable({
      dt_from
    })
    

    dt_to <- totalWithinYears(dt_ref_to_turkey, TRUE, input$YearRangeTo[1], input$YearRangeTo[2])
    dt_to_coords <- dt_to[dt_coords, on="Origin==name", nomatch=0]
    dt_to_coords <- dt_to_coords[Total > 0]
    dt_to_coords$Radius <- calcRadius(dt_to_coords$Total)
    leafletProxy("to_turkey", data = dt_to_coords) %>%
     clearMarkers() %>%
      addCircleMarkers(radius = ~Radius, label = ~format(Total, big.mark=","), 
       stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#009900")
    
    output$table_to = DT::renderDataTable({
      dt_to
    })
    
    dt_asylee_total <- totalAsyleesWithinYears(dt_asylee, input$YearRangeAsy[1], input$YearRangeAsy[2])
    dt_asylee_total_coords <- dt_asylee_total[dt_coords, on="Destination==name", nomatch=0]
    dt_asylee_total_coords <- dt_asylee_total_coords[Total > 0]
    dt_asylee_total_coords$Radius <- calcRadius(dt_asylee_total_coords$Total)
    leafletProxy("asylee", data = dt_asylee_total_coords) %>%
     clearMarkers() %>%
      addCircleMarkers(radius = ~Radius, label = ~format(Total, big.mark=","), 
       stroke = TRUE, weight = 8, opacity=0.15, fillOpacity = 0.65, color = "#0000cc")
    
    output$table_asylee = DT::renderDataTable({
      dt_asylee
    })
  })
  
  
  
  
 
 #Reactive purpose was to support recalculating the total value of each demographic was chosen based on the checkbox. 
     
  graph_data <- reactive({
    dt_demo_graph <- dt_demo[Destination == "Turkey"]
    dt_demo_graph[,Destination := NULL]
    dt_demo_graph[,Location := NULL]
    
    if (!any(input$fem_check_group == 1)) {
      dt_demo_graph[,Female.0.4 := NULL]
    }

    if (!any(input$fem_check_group == 2)) {
      dt_demo_graph[,Female.5.11 := NULL]
    }

    if (!any(input$fem_check_group == 3)) {
      dt_demo_graph[,Female.12.17 := NULL]
    }

    if (!any(input$fem_check_group == 4)) {
      dt_demo_graph[,Female.18.59 := NULL]
    }
    if (!any(input$fem_check_group == 5)) {
      dt_demo_graph[,Female.60. := NULL]
    }
    
    if (!any(input$male_check_group == 1)) {
      dt_demo_graph[,Male.0.4 := NULL]
    }

    if (!any(input$male_check_group == 2)) {
      dt_demo_graph[,Male.5.11 := NULL]
    }

    if (!any(input$male_check_group == 3)) {
      dt_demo_graph[,Male.12.17 := NULL]
    }

    if (!any(input$male_check_group == 4)) {
      dt_demo_graph[,Male.18.59 := NULL]
    }

    if (!any(input$male_check_group == 5)) {
      dt_demo_graph[,Male.60. := NULL]
    }
    return(dt_demo_graph)
  })
  
  
  output$demographics_graph <- renderDygraph({
    
    dygraph(graph_data(), main = "Demographics of Refugees in Turkey") %>%
      dyOptions(stackedGraph = TRUE)
  })
  
  
  
  output$demo_turkey = DT::renderDataTable({
    dt_demo
  })
  
  
  output$dt_asylee = DT::renderDataTable({
    dt_asylee
  })
  
})
  


