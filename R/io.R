#******************************************************************************
#
# io.R
# Creates new inputs and download buttons using renderUI
#
#******************************************************************************

# Updates the Frontier Date numericInput depending on the year of introduction
# column selected by the user
output$frontier.date <- renderUI({
  
  # Get data and the current intro date
  df <- get.df()
  intro.date <- get.intro.date() 
  
  # If an introduction date column has been selected then set the lower and upper bound,
  # and the current value to 0.7 x the difference between min and max years
  if (intro.date != 'NONE'){
    intro.dates <- df[, intro.date]
    value <- floor((max(intro.dates) - min (intro.dates)) * 0.7) + min(intro.dates)
    
    numericInput('front.date', 'Select Frontier Year:', min = min(intro.dates), 
                 max = max(intro.dates), value = value)
  }
  else
    numericInput('front.date', 'Select Frontier Year:', 0, min = 0, max = 0)
})


# Shows download button after TFDEA analysis
output$btn.tfdea <- renderUI({
  if (length(get.result()$tfdea) != 0)
    downloadButton('btn.down.tfdea', 'Download Results')
})


# Saves TFDEA results to csv file when button pressed
output$btn.down.tfdea <- downloadHandler(
  filename = function() {
    paste0('tfdearesults-', Sys.Date(), '.xls')
  },
  content = function(file) {
    df <- get.result()$tfdea
    tryCatch(
      WriteXLS('df', file, row.names = TRUE, AdjWidth = TRUE, 
              BoldHeaderRow = TRUE, verbose = TRUE),
      error = function(e) { set.error(paste("Error downloading results to xls file:", e)) 
                            return(NULL) })
  },
  contentType = "application/vnd.ms-excel"
)


# Shows download button after LR analysis
output$btn.lr <- renderUI({
  if (length(get.result()$lr) != 0)
    downloadButton('btn.down.lr', 'Download Results')
})


# Saves LR results to csv file when button pressed
output$btn.down.lr <- downloadHandler(
  filename = function() {
    paste0('lrresults-', Sys.Date(), '.xls')
  },
  content = function(file) {
    df <- get.result()$lr
    tryCatch(
      WriteXLS('df', file, row.names = TRUE, AdjWidth = TRUE, 
              BoldHeaderRow = TRUE, verbose = TRUE),
      error = function(e) { set.error(paste("Error downloading results to xls file:", e)) 
                            return(NULL) })
  },
  contentType = "application/vnd.ms-excel"
)