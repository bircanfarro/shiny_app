

  shinyUI(dashboardPage(
    skin = "green",
    
    
    dashboardHeader(
      title = "Refugees and Asylees of Turkey",
      titleWidth = 330
    ),
    
    
    dashboardSidebar(
      width = 330,
      sidebarUserPanel( "Bircan Buyukdag", "bircanbuyukdag@gmail.com", image = "IMG_0548H.jpg" ),
      sidebarMenu(
        menuItem("Maps", tabName = "map_dropdown", icon = icon("bars"), startExpanded=FALSE,
                 menuSubItem("Refugees From Turkey", tabName = "tab_from_turkey", icon = icon("map")),
                 menuSubItem("Refugees in Turkey", tabName = "tab_to_turkey", icon = icon("map")),
                 menuSubItem("Asylee From Turkey", tabName = "tab_asylee" , icon = icon("map"))
        ),
        
        menuItem("Demographics", tabName = "tab_demographics", icon = icon("bar-chart"))
      )
    ),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tab_from_turkey",
                sliderInput(inputId = "YearRangeFrom", "Year", sep = "",
                            min = min(dt_ref_from_turkey$Year), max = max(dt_ref_from_turkey$Year), 
                            value = c(min(dt_ref_from_turkey$Year), max(dt_ref_from_turkey$Year))),
                leafletOutput("from_turkey"),
                DT::dataTableOutput("table_from")
        ),
        
        tabItem(tabName = "tab_to_turkey", 
                sliderInput(inputId = "YearRangeTo", "Year", sep = "",
                            min = min(dt_ref_to_turkey$Year), max = max(dt_ref_to_turkey$Year), 
                            value = c(min(dt_ref_to_turkey$Year), max(dt_ref_to_turkey$Year))),
                leafletOutput("to_turkey"),
                DT::dataTableOutput("table_to")
        ),
        
        tabItem(tabName = "tab_asylee", 
                sliderInput(inputId = "YearRangeAsy", "Year", sep = "",
                            min = min(dt_asylee$Year), max = max(dt_asylee$Year), 
                            value = c(min(dt_asylee$Year), max(dt_asylee$Year))),
                leafletOutput("asylee"),
                DT::dataTableOutput("table_asylee")
        ),
      
        tabItem(tabName = "tab_demographics",
                checkboxGroupInput("fem_check_group", label = h3("Female Age Groups"), inline = TRUE,
                                    choices = list("Female.0.4" = 1,
                                                   "Female.5.11" = 2,
                                                   "Female.12.17" = 3,
                                                   "Female.18.59" = 4,
                                                   "Female.60." = 5), selected = 1),
                                     
                checkboxGroupInput("male_check_group", label = h3("Male Age Groups") , inline = TRUE,
                                    choices = list("Male.0.4" = 1, 
                                                   "Male.5.11" = 2, 
                                                   "Male.12.17" = 3, 
                                                   "Male.18.59" = 4, 
                                                   "Male.60." = 5), selected = 1),
                
                dygraphOutput("demographics_graph"),
                
                DT::dataTableOutput("demo_turkey")
          )
        )
      )
    )
  )
    
    

  
  
  







