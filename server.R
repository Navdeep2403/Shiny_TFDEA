#******************************************************************************
#
# server.R
# Contain the script that will run on the server when the user interacts 
# with the webpage interface. 
#
#******************************************************************************

shinyServer(function(input, output, session) {
  
  # Loads external R files
  source("R/server.vars.R", local=TRUE)$value # Variables used among reactive expressions and functions in server.R 
  source("R/tables.R", local=TRUE)$value      # Sortable datatables displaying data and results
  source("R/plots.R", local=TRUE)$value       # Plots displaying the results
  source("R/functions.R", local=TRUE)$value   # Functions used throughout the server code
  source("R/io.R", local=TRUE)$value          # Creates new inputs and download buttons using renderUI 
  
  # Observers monitor when the action buttons are pressed or a reactive value changes
  
  # Observe the Display Data button. When pressed, upload the data from the specified location, display 
  # the data in a table and update the appropriate selectInputs
  observe({
    # input$btn.display increases by 1 each time the button is pressed
    if (input$btn.display != 0){
      isolate({
        # Disable buttons to prevent user interaction
        # elem.disable("btn", TRUE, TRUE)
        
        # Upload data and show progress bar
#         withProgress(session, {
#           setProgress(message = 'Uploading Data',
#                       detail = 'This may take a while...')

          data.location <- switch(input$fsource,
                                  local = input$data.file,
                                  google = input$gs.url,
                                  dropbox = input$dropbox.url)
          
          # Load data from selected location and parameters (location: functions.R)
          df <- data.load(input$fsource, data.location, input$col.header, input$row.header, 
                          input$file.sep, input$file.quote)
          
          # Set global server df. This is required so the data can be accessed by renderUI, downHandler,
          # and renderDataTable functions. df cannot be sent as an argument to these functions.
          set.df(df)
          
          # Find if any error occurred, and if so display
          error <- get.error()
          if (!is.null(error))
            session$sendCustomMessage(type = "show_error", error)
          else {
            # Change the sidebarPanel tab to the setup model tab
            updateTabsetPanel(session, "ts.setup", selected = "ts.setup.model")
            # Populate tfdea inputs, outputs and intro date options with data 
            # from df under the Setup Model tab
            populate.options(df)
          }
          # Reset error after displayed
          set.error(NULL)
#         })
        
        # Re-enable buttons
        # elem.disable("btn", FALSE, TRUE)
      })
      
      # Change the results tabset to display the data
      updateTabsetPanel(session, "ts.result", selected = "ts.result.data")
    }
  }) # observe display
  
  # Observe the Run Analysis button. When pressed, run TFDEA and linear regression analysis and
  # display the results in the appropriate tables
  observe({
    # input$btn.analysis increases by 1 each time the button is pressed
    if (input$btn.analysis != 0){
      
        # Disable buttons to prevent user interaction
#         elem.disable("btn", TRUE, TRUE)
        
        # Calculate results and show progress bar
#         withProgress(session, {
#           setProgress(message = 'Calculation in Progress',
#                       detail = 'This may take a while...')
          
          isolate({
            # Get uploaded data saved in the global server df dataframe
            df <- get.df()
          
            # Run TFDEA analysis with selected parameters (location: function.R)
            tfdea <- tfdea.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date, 
                                    input$front.date, input$rts, input$orientation, input$secondary.obj,
                                    input$frontier.type, input$segroc)
            
            # Run linear regression analysis with selected parameters (location: function.R)
            lr <- lr.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date, input$front.date)
            
            # Set global server results. This is required so the results can be accessed by downHandler
            # and renderDataTable functions. results cannot be sent as an argument to these functions.
            set.result.tfdea(tfdea)
            set.result.lr(lr)
          })
#         }) 
        
        # Find if any error occurred, and if so display
        error <- get.error()
        if (!is.null(error))
          session$sendCustomMessage(type = "show_error", error)
        
        # Re-enable buttons
        elem.disable("btn", FALSE, TRUE)
      
      # Change the results tabset to display the plot
      updateTabsetPanel(session, "ts.result", selected = "ts.result.plot")
    }
  }) # observe analysis
  
  # Observe the change in results. If the results change, update plot accordingly
  observe({
    # Get TFDEA and linear regression results saved in the global server results list
    result <- get.result()
    # Plot results(location: plots.R)
    plot.result(result$lr, result$tfdea)
  })
  
  # Observe the change in the intro.date selectInput value. This ensures that when a 
  # column is selected for intro.date, the column will not be shown as a option for the 
  # inputs and outputs
  observe({
    if (input$intro.date != "NONE"){
      
      # Get uploaded data saved in the global server df dataframe
      df <- get.df()
      
      # Determine the names of the columns that are numeric
      col.numeric <- sapply(df, is.numeric)
      
      # None is shown under column names if no numeric columns exist
      col.names <- "NONE"
      if (sum(col.numeric) != 0)                 
        col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
      
      # Exclude column that is already selected for the introduction date
      col.names <- col.names[which(col.names != input$intro.date)]
      
      # Include all the remaining columns as input and output options
      col.names.in <- col.names
      col.names.out <- col.names
      
      # If the orientation is output, only allow Constant_1 as an input selection,
      # and vice-versa
      if (input$orientation == "out")
        col.names.in <- c("Constant_1", col.names)
      if (input$orientation == "in")
        col.names.out <- c("Constant_1", col.names)
      
      # Update the options for inputs and outputs
      updateSelectInput(session, 'tfdea.inputs', 'Select Input(s):', 
                        col.names.in)
      updateSelectInput(session, 'tfdea.outputs', 'Select Output(s):', 
                        col.names.out)
    }
    
  }) # observe intro.date
 
})