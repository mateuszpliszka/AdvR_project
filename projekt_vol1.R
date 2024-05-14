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
#To DO:
# page-size - jaki dobrac???
# przetestowac get_data
# defensive - kiedy nie ma polaczenia z internetem
# jesli n, n2 sie zmieni to odswiezyc 
# limit zapytan zapezpieczenie
# problem page-size max - 100 - w trakcie rozwiazywania 
#### Mateusz
# funkcja create_map parametry wejściowe data.frame z kolumnami id - kod jednostki; name- nazwa jednostki, year - wszystkie te same, val - wartosc dla jednostki wyplucic ggpolota
# defensive - sprawdzac czy wszyskie id z sa z tego samego poziomu terytorialnego 
# jako jeden z paramtetrów wejsciowych moze byc tez poziom



Sys.setenv(LANG = "en")

ui <- fluidPage(
  fluidRow(
    column(width = 3,
           textInput(inputId = "X-ClientId", label = "X-ClientId"),
           selectInput(inputId = "Category", label = "Category_1", choices = NULL),
           selectInput(inputId = "Group", label = "Group", choices = NULL),
           selectInput(inputId = "Subgroup", label = "Subgroup", choices = NULL),
           uiOutput("additional_option_input"),
           selectInput(inputId = "Variable", label = "Variable", choices = NULL),
           selectInput(inputId = "Level", label = "Level", choices = NULL),
           selectInput(inputId = "Year", label = "Year", choices = NULL),
           actionButton(inputId = "Generate_map", label = "Generate map")
    ),
    column(width = 9,
           div(style = "display: flex; align-items: center; justify-content: center; height: 90vh; width: 90%;",
               plotOutput("mapPlot", width = "90%", height = "90%")  # Adjust width and height here
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
    print("Observer 1")
  })
  
  observeEvent(input$Group, {
    group_id <- input$Group
    data <- my_data_object$available_data_group(group_id)
    updateSelectInput(session, "Subgroup", choices = setNames(data$id ,data$name))
    print("Observer 2")
  })
  
  observeEvent(input$Subgroup, {
    if (input$Subgroup != "")
    {
      subGroup_id <- input$Subgroup
      data <- my_data_object$available_data_subgroup(subGroup_id)
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
    print("Observer 3")
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
      values <- c(0:level)
      updateSelectInput(session, "Level", choices = setNames(values ,values))
    }
    print("Observer 4")
  })
  
  observeEvent(input$Level, {
    my_data_object$level <- input$Level
    if (!is.null(my_data_object$level)) {
      my_data_object$finalData_allYears <- my_data_object$get_data(my_data_object$variableID,my_data_object$level)
      updateSelectInput(session, "Year", choices = setNames(my_data_object$finalData_allYears$year ,my_data_object$finalData_allYears$year))
    }
    print(my_data_object$finalData_allYears)
    print("Observer 5")
  })
  
  observeEvent(input$Year, {
    my_data_object$year <- input$Year
    if (!is.null(my_data_object$year)) {
      my_data_object$finalData_exactYear <- my_data_object$finalData_allYears[my_data_object$finalData_allYears$year == my_data_object$year, ]
      print(my_data_object$finalData_exactYear)
    }
    print("Observer 6")
  })
  
  observeEvent(input$Generate_map, {
    merged <- my_data_object$create_map()
    
    output$mapPlot <- renderPlot({
      
      
      bb <-ggplot(merged$geometry)+
        geom_sf(aes(fill=merged$val)) + 
        scale_fill_brewer(type = "seq", palette=1, direction = 1, aesthetics = "colour")
      print(bb)
    
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
                  # Constructor
                  initialize = function(category = "", group = "", subgroup = "", additional = "", variableID = "", level ="", language = "pl") {
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
                  },
                  
                  available_data = function() {
                    url <- "https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&format=json&page-size=100"
                    response <- httr::GET(url)
                    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                    print(data)
                    # Extract content which we need
                    return(data$results[c("id", "name")])
                  },
                  
                  available_data_group = function(parentId) {
                    self$category <- parentId
                    url = paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=", self$language, "&parent-id=", parentId, "&format=json&page-size=100")
                    response <- GET(url)
                    data = fromJSON(rawToChar(response$content))
                    # Extraction content which we need
                    return (data$results[c("id","name")])
                  },
                  
                  available_data_subgroup = function(parentId) {
                    self$group <- parentId
                    url = paste0("https://bdl.stat.gov.pl/api/v1/variables?lang=", self$language, "&subject-id=", parentId, "&format=json&page-size=100")
                    response <- GET(url)
                    data = fromJSON(rawToChar(response$content))
                    self$table <- data$results
                    if (is.element("n3", names(data$results))){
                      (data$results[c("id","n1","n2","n3","level")])}
                    else if (is.element("n2", names(data$results))){
                      (data$results[c("id","n1","n2","level")])
                    } else {
                      (data$results[c("id","n1","level")])
                    }
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
                      print(data$results)
                      print(all_data)
                      all_data <- rbind(all_data, data$results)  
                      url <- data$links$`next`

                    }
                    return(all_data)
                  },
                  
                  create_map = function()
                  {
    
                    View(self$finalData_exactYear)
                    print(self$level)
                    #wojewodztwa
                    if(self$level==2){
                      sf_woj<-st_read("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\wojewodztwa.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-substr(self$finalData_exactYear$id, 3, 4)
                      merged <- merge(x = sf_woj, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      return(merged)
                    }
                    #polska
                    if(self$level==0){
                      sf_pol<-st_read("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\polska.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-substr(self$finalData_exactYear$id, 1, 1)
                      merged <- merge(x = sf_pol, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      return(merged)
                    }
                    #powiaty
                    if(self$level==5){
                      
                      sf_pow<-st_read("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\powiaty.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-paste0(substr(self$finalData_exactYear$id, 3, 4), substr(self$finalData_exactYear$id, 8, 9))
                      merged <- merge(x = sf_pow, y = self$finalData_exactYear, by = "JPT_KOD_JE", all.x=TRUE)
                      #merged$val <- cut(merged$val, breaks = 10)
                      # quantiles <- quantile(merged$val, probs = seq(0, 1, by = 1/10), na.rm = TRUE)
                      # 
                      # bins <- cut(merged$val, breaks = quantiles, labels = FALSE, dig.lab = 5)
                      # merged$val <- bins
                      num_bins <- 10
                      data_range <- range(merged$val, na.rm = TRUE)
                      bin_width <- diff(data_range) / num_bins
                      breaks <- seq(min(merged$val, na.rm = TRUE), max(merged$val, na.rm = TRUE) + bin_width, by = bin_width)
                      bins <- cut(merged$val, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                      merged$val <- bins
                      return(merged)
                      
                    }
                    #gminy
                    if(self$level==6){
                      sf_gmi<-st_read("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\gminy.shp")
                      self$finalData_exactYear$JPT_KOD_JE<-paste0(substr(self$finalData_exactYear$id, 3, 4), substr(self$finalData_exactYear$id, 8, 12))
                      merged <- merge(x = sf_gmi, y = self$finalData_exactYear, by = "JPT_KOD_JE",all.x = TRUE)
                      View(self$finalData_exactYear)
                      #merged$val <- cut(merged$val, breaks = 10)
                      # quantiles <- quantile(merged$val, probs = seq(0, 1, by = 1/10), na.rm = TRUE)
                      # 
                      # bins <- cut(merged$val, breaks = quantiles, labels = FALSE, dig.lab = 5)
                      # merged$val <- bins
                      num_bins <- 10
                      data_range <- range(merged$val, na.rm = TRUE)
                      bin_width <- diff(data_range) / num_bins
                      breaks <- seq(min(merged$val, na.rm = TRUE), max(merged$val, na.rm = TRUE) + bin_width, by = bin_width)
                      bins <- cut(merged$val, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                      merged$val <- bins
                      return(merged)
                    }
                  }
                ),
)

shinyApp(ui, server)


?cut



# Ogolna struktura
response <- GET("https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&format=json")
response
body <- content(response, "text")
body
data = fromJSON(rawToChar(response$content))
data$results
language = "pl"
available_data <- function() {
  url = paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&format=json&page-size=100")
  response <- GET(url, add_headers("X-ClientId" = NULL))
  data = fromJSON(rawToChar(response$content))
  # Extraction content which we need
  return (data$results[c("id","name")])
}

# Function to check subcategories of data,example available_data("K15") (only for category and group)
available_data_group <- function(parentId) {
  url = paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=",language,"&parent-id=", parentId, "&format=json&page-size=100")
  response <- GET(url)
  data = fromJSON(rawToChar(response$content))
  # Extraction content which we need
  return (data$results[c("id","name")])
}


available_data_subgroup <- function(parentId) {
  url = paste0("https://bdl.stat.gov.pl/api/v1/variables?lang=",language,"&subject-id=", parentId, "&format=json&page-size=100")
  response <- GET(url)
  data = fromJSON(rawToChar(response$content))
  if (is.element("n2", names(data$results))){
    (data$results[c("id","n1","n2","level")])
  } else {
    (data$results[c("id","n1","level")])
  }
}
client_id = "fceda050-86d0-4b96-0476-08dc73494bf9"
# Do testu
get_data <- function(id, level) {
  url <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", id ,"?unit-level=" , level, "&format=json&page-size=100")
  data <- multiplequeries(url,client_id)
  unnested_data <- unnest(data, values)
  return(unnested_data)
  
  # all_data <- data.frame()  # Initialize an empty data frame to store all responses
  # response <- GET(url, add_headers("X-ClientId" = client_id))
  # 
  # data <- fromJSON(rawToChar(response$content))
  # print(data)
  # print(data$links$self)
  # print(data$links$`next`)
  # 
  # while (!is.null(data$links$`next`)) {
  #   unnested_data <- unnest(data$results, values)
  #   all_data <- bind_rows(all_data, unnested_data)  
  #   url <- data$links$`next`
  #   response <- GET(url, add_headers("X-ClientId" = client_id))
  #   data <- fromJSON(rawToChar(response$content))
  #   print(data)
  # }
  # return(all_data)
}


# multiplequeries = function(url, client_id) {
#   print("gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg")
#   all_data <- data.frame()  # Initialize an empty data frame to store all responses
#   response <- GET(url, add_headers("X-ClientId" = client_id))
#   data <- fromJSON(rawToChar(response$content))
#   print(data)
#   while (!is.null(data$links$`next`)) {
#     all_data <- bind_rows(all_data, data$results)  
#     url <- data$links$`next`
#     response <- GET(url, add_headers("X-ClientId" = client_id))
#     data <- fromJSON(rawToChar(response$content))
#     print(data)
#   }
#   return(all_data)
# }

available_data()

available_data_group("K15")

z123 <- available_data_subgroup("P3183")
z123[z123$n1 == "1 kwartał", ]
xx2<-get_data("76447","6")

xx2






# Does not matter - tests

# First query about what data is available
response <- GET("https://bdl.stat.gov.pl/api/v1/subjects?lang=en&format=json")
response
data = fromJSON(rawToChar(response$content))
# Extraction content which we need
data$results
data$results[c("id","name")]


# Second query about what data is available in category
response <- GET("https://bdl.stat.gov.pl/api/v1/variables?subject-id=P3415")
response
data = fromJSON(rawToChar(response$content))
# Extraction content which we need
is.element("n2", names(data$results))
data$results[c("id","name")]

# Third query 


url = paste0("https://bdl.stat.gov.pl/api/v1/variables?subject-id=P3183&format=json")
response <- GET(url)
data = fromJSON(rawToChar(response$content))
data
data = fromJSON(rawToChar(response$content))
if (is.element("n2", names(data$results))){
  (data$results[c("id","n1","n2","level")])
} else {
  (data$results[c("id","n1","level")])
}


url = paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/3643?unit-level=2&format=json&page-size=100")
response <- GET(url)
data = fromJSON(rawToChar(response$content))
data
mysf<-read_sf("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\wojewodztwa.shp")
view(mysf)
