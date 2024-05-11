library(httr)
library(jsonlite)
library(tidyr)
library(shiny)
library(shinyjs)
library(R6)

#To DO:
# page-size - jaki dobrac???
# przetestowac get_data
# defensive - kiedy nie ma polaczenia z internetem

Sys.setenv(LANG = "en")

ui <- fluidPage(
  selectInput(inputId = "Category", 
              label = "Category",
              choices = NULL),
  selectInput(inputId = "Group", 
              label = "Group",
              choices = NULL),
  selectInput(inputId = "Subgroup", 
              label = "Subgroup",
              choices = NULL),
  uiOutput("additional_option_input"),
  selectInput(inputId = "Variable",
              label = "Variable",
              choices = NULL),
  selectInput(inputId = "Level",
              label = "Level",
              choices = NULL),
  
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
      data <- my_data_object$available_data_subgroup(subGroup_id)
      if( "n2" %in% colnames(data)) {
        updateSelectInput(session, "Variable", choices = setNames(data$n2 ,data$n2))
        output$additional_option_input <- renderUI({
          selectInput(inputId = "Additional_option", 
                      label = "Additional option",
                      choices = setNames(data$n1, data$n1))
        })
        updateSelectInput(session, "Additional_option", choices = setNames(data$n1 ,data$n1))
        
      }
      else{
        updateSelectInput(session, "Variable", choices = setNames(data$n1 ,data$n1))
        output$additional_option_input <- renderUI({})
        
      }
    }
  })
  
  observeEvent(input$Variable, {
    if( "n2" %in% colnames(my_data_object$table)) {
      n1 <- input$Additional_option
      n2 <- input$Variable
      my_data_object$variableID <- my_data_object$table[(my_data_object$table$n1 == n1 & my_data_object$table$n2 == n2),"id"]
      print(my_data_object$table[(my_data_object$table$n1 == n1 & my_data_object$table$n2 == n2),"id"])
    }
    else{
      n1 <- input$Variable
      my_data_object$variableID <- my_data_object$table[my_data_object$table$n1 == n1,"id"]
      print(my_data_object$table[my_data_object$table$n1 == n1,"id"])
    }
    
    level <- my_data_object$table[my_data_object$table$id == my_data_object$variableID,"level"]
    if (!is.null(level)) {
      print(class(level))
      values <- c(0:level)
      print(values)
      updateSelectInput(session, "Level", choices = setNames(values ,values))
    }

  })
  
}
shinyApp(ui, server)

Data <- R6Class("Data",
                public = list(
                  category = NULL,
                  group = NULL,
                  subgroup = NULL,
                  additional = NULL,
                  variableID = NULL,
                  table = NULL,
                  language = NULL,
                  # Constructor
                  initialize = function(category = "", group = "", subgroup = "", additional = "", variableID = "", language = "pl") {
                    self$category <- category
                    self$group <- group
                    self$subgroup <- subgroup
                    self$additional <- additional
                    self$variableID <- variableID
                    self.language <- language
                    self$table <- NULL
                  },
                  
                  available_data = function() {
                    url <- "https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&format=json&page-size=100"
                    response <- httr::GET(url)
                    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
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
                    if (is.element("n2", names(data$results))){
                      (data$results[c("id","n1","n2","level")])
                    } else {
                      (data$results[c("id","n1","level")])
                    }
                  },
                  
                  get_data = function(id,level) {
                    url = paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", id ,"?unit-level=" , level, "&format=json&page-size=100")
                    response <- GET(url)
                    data = fromJSON(rawToChar(response$content))
                    unnested_data <- unnest(data$results, values)
                    return(unnested_data)
                  }
                  
                  
                )
)




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
  response <- GET(url)
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

# Do testu
get_data <- function(id,level) {
  url = paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", id ,"?unit-level=" , level, "&format=json&page-size=100")
  response <- GET(url)
  data = fromJSON(rawToChar(response$content))
  unnested_data <- unnest(data$results, values)
  return(unnested_data)
}

available_data()

available_data_group("K15")

z123 <- available_data_subgroup("P3183")
z123[z123$n1 == "1 kwartał", ]
xx<-get_data("283959","2")

xx






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

