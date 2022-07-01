#******************************************************************************
#
# server.vars.R
# Variables used among reactive expressions and functions in server.R 
#
#******************************************************************************

g.df      <- reactiveValues(data = data.frame())              # contains the uploaded data
g.result  <- reactiveValues(tfdea = list(), lr = list(), dea = list(), mdea=list())      # contains TFDEA and LR results
g.error   <- NULL                                             # contains error message displayed to user
g.model   <- reactiveValues(value = data.frame())

# Functions to access and set variables from within server code

# Used in server.R, tables.R, io.R
get.model <- function() {
  return(g.model$value)
}

# Used in server.R
set.model <- function(model) {
  g.model$value <- model
}


get.df <- function() {
  return(g.df$data)
}

# Used in server.R
set.df <- function(df) {
  g.df$data <- df
}

# Used in tables.R, plot.R, io.R
get.result <- function() {
  return(g.result)
}

# Used in server.R
set.result.dea <- function(result) {
  g.result$dea <- result
}

# Used in server.R
set.result.mdea <- function(result) {
  g.result$mdea <- result
}

# Used in server.R
set.result.tfdea <- function(result) {
  g.result$tfdea <- result
}

# Used in server.R
set.result.lr <- function(result) {
  g.result$lr <- result
}

# Used in server.R
get.error <- function() {
  return(g.error)
}

# Used in functions.R...(TODO: identify errors from other functions)
set.error <- function(error) {
  g.error <- error
}

# Used in io.R
get.intro.date <- function() {
  return(input$intro.date) 
}

# Used in plot.R
get.front.date <- function() {
  return(input$front.date) 
}

# Used in plot.R
get.plot.label <- function() {
  return(input$plot.label.b) 
}

# Used in plot.R
get.plot.lr <- function() {
  return(input$plot.lr.b) 
}