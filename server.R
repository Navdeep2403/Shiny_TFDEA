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
  # source("R/tables.R", local=TRUE)$value      # Sortable datatables displaying data and results
  source("R/plots.R", local=TRUE)$value       # Plots displaying the results
  source("R/functions.R", local=TRUE)$value   # Functions used throughout the server code
  # source("R/io.R", local=TRUE)$value          # Creates new inputs and download buttons using renderUI 
  

  # hide tabs in the side panel
  hideTab("ts.setup", "ts.setup.tfdea_selection", session)
  hideTab("ts.setup", "ts.setup.dea_selection", session)
  hideTab("ts.setup", "ts.setup.mdea_selection", session)

  # hide tabs in the result panel
  hideTab("ts.result", "ts.result.dea", session)
  hideTab("ts.result", "ts.result.tfdea", session)
  hideTab("ts.result", "ts.result.mdea", session)
  hideTab("ts.result", "ts.result.lr", session)
  
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
  
  
  
  output$dt.data <- renderDataTable({
    
    df <- get.df()
    if (nrow(df) == 0)
      return(NULL)
    
    table <- df # data.frame that will be displayed in the table
    
    # Arrange data that will be displayed in the table. renderDataTable does not show row 
    # names, so need to append row names
    row.nums <- seq(nrow(table))
    table <- cbind(row.nums, row.names(table), table)
    names(table) <- c("ROW", "DMU",  toupper(names(df)))
    return(table)
  }, options = list(searching=TRUE, ordering=TRUE, processing=0,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  
  # Display a summary of all results
  output$dt.summary <- renderDataTable({
    result <- get.result()
    if (length(result$tfdea) == 0)
      return(NULL)
    
    if (length(result$lr) == 0) 
      lr.mad <- NA
    else                            
      lr.mad <- result$lr$summary[1]
    
    table <- cbind(result$tfdea$summary[1], lr.mad)
    table <- format(table, nsmall = 3)
    names(table) <- c("MAD (TFDEA)", "MAD (LR)")
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    paging = FALSE, info = FALSE))
  
  
  # Display summary of TFDEA results
  output$dt.tfdea.summary <- renderDataTable({
    result <- get.result()
    if (length(result$tfdea) == 0)
      return(NULL)
    
    table <- result$tfdea$summary
    table <- format(table, nsmall = 3)
    names(table) <- toupper(colnames(table))
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    paging = FALSE, info = FALSE))
  

  # Display DEA efficiency results
  output$dt.dea.eff <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$eff) == 0)
      return(NULL)
    
    table <- result$dea$eff
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display DEA efficiency results
  output$dt.dea.lambda <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$lambda) == 0)
      return(NULL)
    
    table <- result$dea$lambda
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display DEA efficiency results
  output$dt.dea.objval <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$objval) == 0)
      return(NULL)
    
    table <- result$dea$objval
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display DEA efficiency results
  output$dt.dea.RTS <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$RTS) == 0)
      return(NULL)
    
    table <- result$dea$RTS
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display DEA efficiency results
  output$dt.dea.ORIENTATION <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$ORIENTATION) == 0)
      return(NULL)
    
    table <- result$dea$ORIENTATION
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display DEA efficiency results
  output$dt.dea.TRANSPOSE <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$dea$TRANSPOSE) == 0)
      return(NULL)
    
    table <- result$dea$TRANSPOSE
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$dea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))  
    
  # Display TFDEA forecast results
  output$dt.tfdea.forecast <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$tfdea) == 0)
      return(NULL)
    
    table <- result$tfdea$forecast
    table <- format(table, nsmall = 3)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    names(table) <- c("ROW", "DMU", toupper(names(result$tfdea$forecast))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  
  
  # Display summary of linear regression results 
  output$dt.lr.summary <- renderDataTable({
    result <- get.result()
    if (length(result$lr) == 0)
      return(NULL)
    
    table <- result$lr$summary
    table <- format(table, nsmall = 3)
    names(table) <- toupper(colnames(table))
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    paging = FALSE, info = FALSE))
  
  
  # Display linear regression coefficients
  output$dt.lr.coeff <- renderDataTable({
    result <- get.result()
    if (length(result$lr) == 0)
      return(NULL)
    
    table <- result$lr$coefficients
    table <- format(table, nsmall = 3)
    table <- cbind(row.names(table), table)
    names(table) <- c("COEFFICIENTS", toupper(names(result$lr$coefficients))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    paging = FALSE, info = FALSE))
  
  
  # Display linear regression multi-collinearity results
  output$dt.lr.mc <- renderDataTable({
    result <- get.result()
    if (length(result$lr$mc) == 0)
      return(NULL)
    
    table <- result$lr$mc
    table <- format(table, nsmall = 3)
    table <- cbind(row.names(table), table)
    names(table) <- c("COEFFICIENTS", toupper(names(result$lr$mc))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    paging = FALSE, info = FALSE))
  
  
  
  # Display linear regression forecast results
  output$dt.lr.forecast <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$lr) == 0)
      return(NULL)
    
    table <- result$lr$forecast
    table <- format(table, nsmall = 3)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$lr$forecast))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
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
  
  # Observe the Model Selection button. When pressed, go to next tab with de linear regression analysis and
  # display the results in the appropriate tables
  observe({
    # input$btn.analysis increases by 1 each time the button is pressed
    if (input$btn.next != 0){
      
      # Disable buttons to prevent user interaction
      #         elem.disable("btn", TRUE, TRUE)
      
      # Calculate results and show progress bar
      #         withProgress(session, {
      #           setProgress(message = 'Calculation in Progress',
      #                       detail = 'This may take a while...')
      
      isolate({
        data.model <- input$model
        

        # Find if any error occurred, and if so display
        error <- get.error()
        if (!is.null(error))
          session$sendCustomMessage(type = "show_error", error)
        
        # Re-enable buttons
        elem.disable("btn", FALSE, TRUE)
      })
      # print(data.model)

      if (data.model == "tfdea") {
        print("Inside TFDEA")
        # Change the results tabset to display the plot
        hideTab("ts.setup", "ts.setup.dea_selection", session)
        hideTab("ts.setup", "ts.setup.mdea_selection", session)
        showTab("ts.setup", "ts.setup.tfdea_selection", select = TRUE, session)
        set.model(data.model)

        # populate.options(df)
        # updateTabsetPanel(session, "ts.setup", selected = "ts.setup.tfdea_selection")
      }
      if (data.model == "dea") {
        print("Inside DEA")
        # Change the results tabset to display the plot
        hideTab("ts.setup", "ts.setup.tfdea_selection", session)
        hideTab("ts.setup", "ts.setup.mdea_selection", session)
        showTab("ts.setup", "ts.setup.dea_selection", select = TRUE, session)
        set.model(data.model)

        # populate.dea_options(df)
        # updateTabsetPanel(session, "ts.setup", selected = "ts.setup.dea_selection")
      }
      if (data.model == "mdea") {
        print("Inside MULTIPLIER_DEA")
        # Change the results tabset to display the plot
        hideTab("ts.setup", "ts.setup.dea_selection", session)
        hideTab("ts.setup", "ts.setup.tfdea_selection", session)
        showTab("ts.setup", "ts.setup.mdea_selection", select = TRUE, session)
        set.model(data.model)
        
        # populate.options(df)
        # updateTabsetPanel(session, "ts.setup", selected = "ts.setup.tfdea_selection")
      }
    }
  }) # observe analysis
  
  # Observe the Run Analysis button. When pressed, 
  observe({
    # input$btn.analysis increases by 1 each time the button is pressed
    #
    # check the model if DEA and run DEA Analysis and
    # display the results in the appropriate tables
    if (input$btn.deaanalysis != 0 && get.model() == "dea"){
      
          isolate({
            # Get uploaded data saved in the global server df dataframe
            df <- get.df()
            
            print("Inside DEA Analysis")
            
            dea <- dea.analysis(df, input$dea.inputs, input$dea.outputs, input$rts, input$orientation)
            
            set.result.dea(dea)
            set.result.tfdea(list())
            set.result.lr(list())

          })
#         }) 
        
        # Find if any error occurred, and if so display
        error <- get.error()
        if (!is.null(error))
          session$sendCustomMessage(type = "show_error", error)
        
        # Re-enable buttons
        elem.disable("btn", FALSE, TRUE)
      
      # hideTab("ts.result", "ts.result.plot", session)
      hideTab("ts.result", "ts.result.lr", session)
      hideTab("ts.result", "ts.result.tfdea", session)
      showTab("ts.result", "ts.result.dea", select = FALSE, session)
      updateTabsetPanel(session, "ts.result", selected = "ts.result.dea")
    }
    
    # check the model if TFDEA and run TFDEA and linear regression analysis and
    # display the results in the appropriate tables
    if (input$btn.tfdeaanalysis != 0 && get.model() == "tfdea"){
      
      # Disable buttons to prevent user interaction
      #         elem.disable("btn", TRUE, TRUE)
      
      # Calculate results and show progress bar
      #         withProgress(session, {
      #           setProgress(message = 'Calculation in Progress',
      #                       detail = 'This may take a while...')
      isolate({
        # Get uploaded data saved in the global server df dataframe
        df <- get.df()
        
        print("Inside TFDEA Analysis")
        
        # Run TFDEA analysis with selected parameters (location: function.R)
        # tfdea <- tfdea.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date, 
        #                         input$front.date, input$rts, input$orientation, input$secondary.obj,
        #                         input$frontier.type, input$segroc)
        tfdea <- tfdea.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date,
                                input$front.date, input$rts, input$orientation, input$secondary.obj,
                                input$frontier.type, input$segroc)
        
        # Run linear regression analysis with selected parameters (location: function.R)
        lr <- lr.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date, input$front.date)
        
        # Set global server results. This is required so the results can be accessed by downHandler
        # and renderDataTable functions. results cannot be sent as an argument to these functions.
        set.result.dea(list())
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

      hideTab("ts.result", "ts.result.dea", session)
      showTab("ts.result", "ts.result.tfdea", select = FALSE,  session)
      showTab("ts.result", "ts.result.lr", select = FALSE, session)
      updateTabsetPanel(session, "ts.result", selected = "ts.result.plot")

    }
  }) # observe analysis
  

  # Observe the change in results. If the results change, update plot accordingly
  observe({
    
    print("Inside Plot results section")
    # Get TFDEA and linear regression results saved in the global server results list
    result <- get.result()
    # Plot results(location: plots.R)
    plot.result(result$lr, result$tfdea, result$dea)
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