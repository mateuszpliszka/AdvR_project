library(httr)
library(jsonlite)
library(tidyr)
library(shiny)
library(shinyjs)
library(R6)
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
options(scipen = 999)
library(RColorBrewer)
library(Rcpp)
library(curl)
library(shinybusy)


#To DO:
# defensive - kiedy nie ma polaczenia z internetem (to chyba wtedy shiny wogole nie bedzie dzialalo na stronie wiec chyba nie problem ale zostawiam na pozniej)
# problem page-size max - 100 - w trakcie rozwiazywania
#RCPP UZYC
#### Mateusz
# jako jeden z paramtetrów wejsciowych moze byc tez poziom

#srednia gmin w danym powiecie (np jeśli użytkownik wybierze dochod gmin policzyc srednia gmin dla kazdego
#powiatu/woj i wtedy wyrenderować)
# DODAC LADNE OPISY WYKRESOW

Sys.setenv(LANG = "en")


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .blocker {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: transparent;
          z-index: 9999;
        }
        "
      )
    )
  ),
  add_busy_spinner(spin = "fading-circle"), 
  useShinyjs(),  
  titlePanel("BDL Shiny App"),
  fluidRow(
    column(width = 3,
           selectInput(inputId = "Category", label = "Category", choices = NULL),
           selectInput(inputId = "Group", label = "Group", choices = NULL),
           selectInput(inputId = "Subgroup", label = "Subgroup", choices = NULL),
           uiOutput("additional_option_input"),
           selectInput(inputId = "Variable", label = "Variable", choices = NULL),
           selectInput(inputId = "Level", label = "Level", choices = NULL),
           selectInput(inputId = "Year", label = "Year", choices = NULL),
           actionButton(inputId = "Generate_map", label = "Generate map"),
           selectInput(inputId = "select_unit", label = "Select unit", choices = NULL),
           actionButton(inputId = "Generate_plot", label = "Generate plot")
    ),
    column(width = 9,
           div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 150vh; width: 100%;",
               plotOutput("mapPlot", width = "90%", height = "150%"),
               plotOutput("linePlot", width = "90%", height = "50%"),
               plotOutput("linePPlot", width = "90%", height = "50%")
           )
    )
  )
)

server <- function(input, output, session) {
  language= "pl"
  my_data_object <- Data$new()
  observe({
    data <- my_data_object$available_data()
    updateSelectInput(session, "Category", choices = setNames(data$id ,data$name))
  })
  
  observeEvent(input$Category, {
    category_id <- input$Category
    data <- my_data_object$available_data_group(category_id)
    updateSelectInput(session, "Group", choices = setNames(data$id ,data$name))
  })
  
  observeEvent(input$Group, {
    group_id <- input$Group
    data <- my_data_object$available_data_group(group_id)
    updateSelectInput(session, "Subgroup", choices = setNames(data$id ,data$name))
  })
  
  observeEvent(input$Subgroup, {
    if (input$Subgroup != "")
    {
      subGroup_id <- input$Subgroup
      shinyjs::runjs("$(document.body).append('<div class=\"blocker\"></div>');")
      data <- my_data_object$available_data_subgroup(subGroup_id)
      shinyjs::runjs("$('.blocker').remove();")  
      if( "n3" %in% colnames(data)) {
        updateSelectInput(session, "Variable", choices = setNames(data$n3 ,data$n3))
        output$additional_option_input <- renderUI({
          tagList(
            selectInput(inputId = "Additional_option_1", 
                        label = "Additional option",
                        choices = setNames(data$n1, data$n1)),
            selectInput(inputId = "Additional_option_2", 
                        label = "Additional option 2",
                        choices = setNames(data$n2, data$n2))
          )
        })
        updateSelectInput(session, "Additional_option_1", choices = setNames(data$n1 ,data$n1))
        updateSelectInput(session, "Additional_option_2", choices = setNames(data$n2, data$n2))
        
        
      }
      else if( "n2" %in% colnames(data)) {
        updateSelectInput(session, "Variable", choices = setNames(data$n2 ,data$n2))
        output$additional_option_input <- renderUI({
          selectInput(inputId = "Additional_option_1", 
                      label = "Additional option",
                      choices = setNames(data$n1, data$n1))
        })
        updateSelectInput(session, "Additional_option_1", choices = setNames(data$n1 ,data$n1))
        
      }
      else{
        updateSelectInput(session, "Variable", choices = setNames(data$n1 ,data$n1))
        output$additional_option_input <- renderUI({})
        
      }
      
    }
  })
  
  observeEvent(c(input$Variable, input$Additional_option_1, input$Additional_option_2), {
    if( "n3" %in% colnames(my_data_object$table)) {
      n1 <- input$Additional_option_1
      n2 <- input$Additional_option_2
      n3 <- input$Variable
      my_data_object$variableID <- my_data_object$table[(my_data_object$table$n1 == n1 & my_data_object$table$n2 == n2 & my_data_object$table$n3 == n3),"id"]
    }
    else if( "n2" %in% colnames(my_data_object$table)) {
      n1 <- input$Additional_option_1
      n2 <- input$Variable
      my_data_object$variableID <- my_data_object$table[(my_data_object$table$n1 == n1 & my_data_object$table$n2 == n2),"id"]
    }
    else{
      n1 <- input$Variable
      my_data_object$variableID <- my_data_object$table[my_data_object$table$n1 == n1,"id"]
    }
    
    level <- my_data_object$table[my_data_object$table$id == my_data_object$variableID,"level"]
    if (!is.null(level)) {
      values <- 0:level
      specific_values <- c(0, 2, 5, 6)
      values <- intersect(values, specific_values)
      
      specific_labels <- c("Polski", "Województw", "Powiatów", "Gmin")
      value_label_map <- setNames(specific_labels, specific_values)
      
      labeled_values <- sapply(values, function(x) {
        if (as.character(x) %in% names(value_label_map)) {
          value_label_map[as.character(x)]
        } else {
          as.character(x)
        }
      })
      
      names(labeled_values) <- NULL
      
      updateSelectInput(session, "Level", choices = setNames(values, labeled_values))
    }
    my_data_object$level <- 0
    if (!is.null(my_data_object$level)) {
      my_data_object$finalData_allYears <- my_data_object$get_data(my_data_object$variableID,my_data_object$level)
      updateSelectInput(session, "Year", choices = setNames(my_data_object$finalData_allYears$year ,my_data_object$finalData_allYears$year))
      updateSelectInput(session, "select_unit", choices = setNames(my_data_object$finalData_allYears$name ,my_data_object$finalData_allYears$name))
    }
    my_data_object$year <- input$Year
    if (!is.null(my_data_object$year)) {
      my_data_object$finalData_exactYear <- my_data_object$finalData_allYears[my_data_object$finalData_allYears$year == my_data_object$year, ]
    }
  })
  
  observeEvent(input$Level, {
    my_data_object$level <- input$Level
    if (!is.null(my_data_object$level)) {
      shinyjs::runjs("$(document.body).append('<div class=\"blocker\"></div>');") 
      my_data_object$finalData_allYears <- my_data_object$get_data(my_data_object$variableID,my_data_object$level)
      updateSelectInput(session, "Year", choices = setNames(my_data_object$finalData_allYears$year ,my_data_object$finalData_allYears$year))
      updateSelectInput(session, "select_unit", choices = setNames(my_data_object$finalData_allYears$name ,my_data_object$finalData_allYears$name))
      shinyjs::runjs("$('.blocker').remove();")  
    }
    my_data_object$year <- input$Year
    if (!is.null(my_data_object$year)) {
      my_data_object$finalData_exactYear <- my_data_object$finalData_allYears[my_data_object$finalData_allYears$year == my_data_object$year, ]
    }
  })
  
  observeEvent(input$Year, {
    my_data_object$year <- input$Year
    if (!is.null(my_data_object$year)) {
      my_data_object$finalData_exactYear <- my_data_object$finalData_allYears[my_data_object$finalData_allYears$year == my_data_object$year, ]
    }
  })
  
  observeEvent(input$Generate_map, {
    shinyjs::runjs("$(document.body).append('<div class=\"blocker\"></div>');")
    
    merged <- my_data_object$create_map()
    
    output$mapPlot <- renderPlot({
      ggplot(merged$geometry)+
        geom_sf(aes(fill=merged$val)) + 
        scale_fill_viridis_c()
    })
    shinyjs::runjs("$('.blocker').remove();")  
  })
  
  observeEvent(input$select_unit, {
    my_data_object$unit <- input$select_unit
  })
  
  observeEvent(input$Generate_plot, {
    plot_data <-my_data_object$create_plot()
    plot_data <- as.data.frame(plot_data)
    output$linePlot <- renderPlot({
      ggplot(plot_data, aes(x=year, group=1)) +
        geom_line(aes(y=val.x, colour='Average')) +
        geom_line(aes(y=val.y, colour='Chosen Unit'))
    })
    plot_p_data<-my_data_object$create_p_plot()
    output$linePPlot <- renderPlot({
      ggplot(plot_p_data, aes(x=year, group=1)) +
        geom_line(aes(y=avg_pct_change, colour='avg_pct_change')) +
        geom_line(aes(y=unit_pct_change, colour='unit_pct_change')) 
      
      
    })
    
    
    
  })
}
Data <- R6Class("Data",
                public = list(
                  category = NULL,
                  group = NULL,
                  subgroup = NULL,
                  additional = NULL,
                  variableID = NULL,
                  table = NULL,
                  level = NULL,
                  language = NULL,
                  finalData_allYears = NULL,
                  finalData_exactYear = NULL,
                  year = NULL,
                  unit = NULL,
                  # Constructor
                  initialize = function(category = "", group = "", subgroup = "", additional = "", variableID = "", level ="",language = "pl") {
                    self$category <- category
                    self$group <- group
                    self$subgroup <- subgroup
                    self$additional <- additional
                    self$variableID <- variableID
                    self.language <- language
                    self$table <- NULL
                    self$level <- level
                    self$finalData_allYears <- NULL
                    self$finalData_exactYear <- NULL
                    self$year <- NULL
                    self$unit <- NULL
                  },
                  
                  available_data = function() {
                    url <- "https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&format=json&page-size=100"
                    response <- httr::GET(url)
                    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                    return(data$results[c("id", "name")])
                  },
                  
                  available_data_group = function(parentId) {
                    self$category <- parentId
                    url = paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=", self$language, "&parent-id=", parentId, "&format=json&page-size=100")
                    response <- GET(url)
                    data = fromJSON(rawToChar(response$content))
                    return (data$results[c("id","name")])
                  },
                  
                  available_data_subgroup = function(parentId) {
                    self$group <- parentId
                    url = paste0("https://bdl.stat.gov.pl/api/v1/variables?lang=", self$language, "&subject-id=", parentId, "&format=json&page-size=100")
                    data <- self$multiplequeries(url,NULL)
                    if (is.element("n3", names(data))){
                      (data[c("id","n1","n2","n3","level")])}
                    else if (is.element("n2", names(data))){
                      (data[c("id","n1","n2","level")])
                    } else {
                      (data[c("id","n1","level")])
                    }
                    self$table <- data
                    #response <- GET(url)
                    #data = fromJSON(rawToChar(response$content))
                    #self$table <- data$results
                    # if (is.element("n3", names(data$results))){
                    #   (data$results[c("id","n1","n2","n3","level")])}
                    # else if (is.element("n2", names(data$results))){
                    #   (data$results[c("id","n1","n2","level")])
                    # } else {
                    #   (data$results[c("id","n1","level")])
                    # }
                  },
                  get_data = function(id,level, client_id = NULL) {
                    url <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", id ,"?unit-level=" , level, "&format=json&page-size=100")
                    data <- self$multiplequeries(url,NULL)
                    if (!is.null(data)){
                      unnested_data <- unnest(data, values)
                      return(unnested_data)
                    }
                    return(NULL)
                    
                  },
                  
                  multiplequeries = function(url, client_id = NULL) {
                    all_data <- data.frame()  # Initialize an empty data frame to store all responses
                    response <- GET(url, add_headers("X-ClientId" = client_id))
                    data <- fromJSON(rawToChar(response$content))
                    
                    if (!is.null(data$errors)) {
                      print(data$errors)
                      return (NULL)
                    }
                    all_data <- data$results
                    
                    while (!is.null(data$links$`next`)) {
                      url <- data$links$`next`
                      response <- GET(url, add_headers("X-ClientId" = client_id))
                      data <- fromJSON(rawToChar(response$content))
                      if (!is.null(data$errors)) {
                        print(data$errors)
                        return (all_data)
                      }
                      all_data <- rbind(all_data, data$results)  
                      url <- data$links$`next`
                      
                    }
                    return(all_data)
                  },
                  
                  create_map = function()
                  {
                    #wojewodztwa
                    if(self$level==2){
                      sf_woj<-st_read("wojewodztwa.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-substr(self$finalData_exactYear$id, 3, 4)
                      merged <- merge(x = sf_woj, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      return(merged)
                    }
                    #polska
                    if(self$level==0){
                      sf_pol<-st_read("polska.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-substr(self$finalData_exactYear$id, 1, 1)
                      merged <- merge(x = sf_pol, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      return(merged)
                    }
                    #powiaty
                    if(self$level==5){
                      
                      sf_pow<-st_read("powiaty.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-paste0(substr(self$finalData_exactYear$id, 3, 4), substr(self$finalData_exactYear$id, 8, 9))
                      merged <- merge(x = sf_pow, y = self$finalData_exactYear, by = "JPT_KOD_JE", all.x=TRUE)
                      return(merged)
                      
                    }
                    #gminy
                    if(self$level==6){
                      sf_gmi<-st_read("gminy.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-paste0(substr(self$finalData_exactYear$id, 3, 4), substr(self$finalData_exactYear$id, 8, 12))
                      merged <- merge(x = sf_gmi, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      return(merged)
                    }
                  },
                  create_plot = function(){
                    average_data <- self$finalData_allYears %>%
                      group_by(year) %>%
                      summarize(val = mean(val)) %>%
                      mutate(unit = "Average")
                    plot_data_one <- self$finalData_allYears %>%
                      filter(name==self$unit)
                    plot_data <- merge(x = average_data, y = plot_data_one, by = "year", all.x = TRUE)
                    return(plot_data)
                  },
                  create_p_plot = function(){
                    average_p_data <- self$finalData_allYears %>%
                      group_by(year) %>%
                      summarize(val = mean(val)) %>%
                      mutate(unit = "Average")
                    plot_p_data_one <- self$finalData_allYears %>%
                      filter(name==self$unit)
                    plot_p_data <- merge(x = average_p_data, y = plot_p_data_one, by = "year", all.x = TRUE)
                    cppFunction('
                    NumericVector percentageChange(NumericVector values) {
                      int n = values.size();
                      NumericVector pct_change(n);
                      
                      // Set the first year percentage to 0%
                      pct_change[0] = 0;
                      
                      for(int i = 1; i < n; i++) {
                        pct_change[i] = (values[i] / values[i-1]) -1 ;
                      }
                      
                      return pct_change;
                    }')
                    
                    plot_p_data <- plot_p_data %>%
                      mutate(
                        unit_pct_change = percentageChange(val.y),
                        avg_pct_change = percentageChange(val.x)
                      )
                    return(plot_p_data)
                  }
                  
                ),
)

shinyApp(ui, server)
