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
  source("R/plots.R", local=TRUE)$value       # Plots displaying the results
  source("R/functions.R", local=TRUE)$value   # Functions used throughout the server code
  
  # Un-comment after placing the code in files
  #
  # source("R/tables.R", local=TRUE)$value      # Sortable datatables displaying data and results
  # source("R/io.R", local=TRUE)$value          # Creates new inputs and download buttons using renderUI
  

  # hide tabs in the side panel
  hideTab("ts.setup", "ts.setup.model", session)
  hideTab("ts.setup", "ts.setup.tfdea_selection", session)
  hideTab("ts.setup", "ts.setup.dea_selection", session)
  hideTab("ts.setup", "ts.setup.mdea_selection", session)
  hideTab("ts.setup", "ts.setup.mdea_weights", session)

  # hide tabs in the result panel
  hideTab("ts.result", "ts.result.dea", session)
  hideTab("ts.result", "ts.result.tfdea", session)
  hideTab("ts.result", "ts.result.mdea", session)
  hideTab("ts.result", "ts.result.lr", session)
  
  mdea_cols_list_input = list()
  mdea_cols_list_output = list()
  
  # io.R - START
  
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
  
  # Shows download button after TFDEA analysis
  output$btn.dea <- renderUI({
    if (length(get.result()$dea) != 0)
      downloadButton('btn.down.dea', 'Download Results')
  })
  
  # Shows download button after TFDEA analysis
  output$btn.mdea <- renderUI({
    if (length(get.result()$mdea) != 0)
      downloadButton('btn.down.mdea', 'Download Results')
  })
  
  
  # Saves TFDEA results to csv file when button pressed
  output$btn.down.tfdea <- downloadHandler(
    filename = function() {
      paste0('tfdearesults-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      df <- get.result()$tfdea
      tryCatch(
        writexl::write_xlsx(df, file , col_names = TRUE, format_headers = TRUE),
        # WriteXLS(df, ExcelFileName = file, row.names = TRUE, AdjWidth = TRUE,
        #          BoldHeaderRow = TRUE, verbose = TRUE),
        error = function(e) { set.error(paste("Error downloading results to xls file:", e)) 
          return(NULL) })
    },
    contentType = "application/vnd.ms-excel"
  )
  
  # Saves DEA results to csv file when button pressed
  output$btn.down.dea <- downloadHandler(
    filename = function() {
      paste0('dearesults-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      df <- get.result()$dea$exportlist
      tryCatch(
        writexl::write_xlsx(df, file , col_names = TRUE, format_headers = TRUE),
        error = function(e) { print(e) 
          return(NULL) })
    },
    contentType = "application/vnd.ms-excel"
  )
  
  
  # Saves MultiplierDEA results to csv file when button pressed
  output$btn.down.mdea <- downloadHandler(
    filename = function() {
      paste0('multipier-dearesults-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      df <- get.result()$mdea$exportlist
      tryCatch(
        writexl::write_xlsx(df, file , col_names = TRUE, format_headers = TRUE),
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
      paste0('lrresults-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      df <- get.result()$lr
      tryCatch(
        writexl::write_xlsx(df, file , col_names = TRUE, format_headers = TRUE),
        error = function(e) { set.error(paste("Error downloading results to xls file:", e)) 
          return(NULL) })
    },
    contentType = "application/vnd.ms-excel"
  )
  # io.R - END
  
  # Tables.R - START
  
  # Display uploaded data
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
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), DMU = rownames(table), Efficiency = table)
    colnames(table)<-c("DMU", "Efficiency")
    
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
    colnames(table) <- c("DMU", "Objective Value") 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  
  # mDEA - Start
  
  # Display mDEA efficiency results
  output$dt.mdea.Efficiency <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$Efficiency) == 0)
      return(NULL)
    
    table <- result$mdea$Efficiency
    table <- format(table, nsmall = 2)
    
    print(table)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(seq(nrow(df)), rownames(table), table) 
    print(table)
    colnames(table) <- c("ROW", "DMU", "Efficiency") 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA Lambda results
  output$dt.mdea.Lambda <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$Lambda) == 0)
      return(NULL)
    
    table <- result$mdea$Lambda
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)), DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea$Lambda))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  
  # Display mDEA Free Weight results
  output$dt.mdea.Free_Weights <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$Free_Weights) == 0)
      return(NULL)
    
    table <- result$mdea$Free_Weights
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA Model Status
  output$dt.mdea.Model_Status <- renderDataTable({
    df <- get.df()
    result <- get.result()
    print(result$mdea$Model_Status)
    if (length(result$mdea$Model_Status) == 0)
      return(NULL)
    
    table <- result$mdea$Model_Status
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    print(table)
    print(names(result$mdea))
    names(table) <- c("ROW", "DMU", "CODE", "DESCRIPTION") 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA Input Values
  output$dt.mdea.InputValues <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$InputValues) == 0)
      return(NULL)
    
    table <- result$mdea$InputValues
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA Output Values
  output$dt.mdea.OutputValues <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$OutputValues) == 0)
      return(NULL)
    
    table <- result$mdea$OutputValues
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA HCU Input
  output$dt.mdea.HCU_Input <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$HCU_Input) == 0)
      return(NULL)
    
    table <- result$mdea$HCU_Input
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA HCU Output
  output$dt.mdea.HCU_Output <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$HCU_Output) == 0)
      return(NULL)
    
    table <- result$mdea$HCU_Output
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA vx
  output$dt.mdea.vx <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$vx) == 0)
      return(NULL)
    
    table <- result$mdea$vx
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # Display mDEA uy
  output$dt.mdea.uy <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$mdea$uy) == 0)
      return(NULL)
    
    table <- result$mdea$uy
    table <- format(table, nsmall = 2)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table)
    names(table) <- c("ROW", "DMU", toupper(names(result$mdea))) 
    return(table)
  }, options = list(searching=FALSE, ordering=FALSE, processing=FALSE,
                    lengthMenu = c(10, 20, 30), pageLength = 10))
  
  # mDEA - End
  
  
  
  # Display TFDEA forecast results
  output$dt.tfdea.forecast <- renderDataTable({
    df <- get.df()
    result <- get.result()
    if (length(result$tfdea) == 0)
      return(NULL)
    
    table <- result$tfdea$forecast
    table <- format(table, nsmall = 3)
    
    # renderDataTable does not show row names or numbers, so need to append both
    table <- cbind(ROW = seq(nrow(df)),DMU = rownames(table), table) 
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
  
  # Tables.R - END
  
  
  # Observers monitor when the action buttons are pressed or a reactive value changes
  
  # Observe the Display Data button. When pressed, upload the data from the specified location, display 
  # the data in a table and update the appropriate selectInputs
  observe({
    # input$btn.display increases by 1 each time the button is pressed
    if (input$btn.display != 0){
      isolate({

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
      showTab("ts.setup", "ts.setup.model", select = TRUE, session)
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
        hideTab("ts.setup", "ts.setup.mdea_weights", session)
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
        hideTab("ts.setup", "ts.setup.mdea_weights", session)
        showTab("ts.setup", "ts.setup.dea_selection", select = TRUE, session)
        set.model(data.model)

        # populate.dea_options(df)
        # updateTabsetPanel(session, "ts.setup", selected = "ts.setup.dea_selection")
      }
      if (data.model == "mdea") {
        print("Inside MULTIPLIER_DEA next")
        # Change the results tabset to display the plot
        hideTab("ts.setup", "ts.setup.dea_selection", session)
        hideTab("ts.setup", "ts.setup.tfdea_selection", session)
        hideTab("ts.setup", "ts.setup.mdea_weights", session)
        showTab("ts.setup", "ts.setup.mdea_selection", select = TRUE, session)
        set.model(data.model)
        
        # populate.options(df)
        # updateTabsetPanel(session, "ts.setup", selected = "ts.setup.tfdea_selection")
      }
    }
  }) # observe analysis
  
  
  # Observe the Next button from MDEA column selection 
  observe({
    # input$btn.analysis increases by 1 each time the button is pressed
    if (input$btn.mdeanext != 0){
      
      isolate({
        
        df <- get.df()
        data.model <- input$model
        
        # Extracting columns with non-zero values
        columns_list_in<-c()
        columns_list_out<-c()
        
        for (x in input$mdea.inputs) {
          if(!(0 %in% df[[x]])) {
            columns_list_in <- c(columns_list_in, x)
          }
        }
        
        for (x in input$mdea.outputs) {
          if(!(0 %in% df[[x]])) {
            columns_list_out <- c(columns_list_out, x)
          }
        }
        
        print("columns_list_in")
        print(columns_list_in)
        
        print("columns_list_out")
        print(columns_list_out)
        
        
        if (length(columns_list_in) > 1 || length(columns_list_out) > 1) {

          output$mdea.pairs.weights <- renderUI({
            slider_list = list()
            k=1
            
            slider_list[k] = list(HTML("<i><b>Note:</b> Columns having one or more values as zero(0) are skipped for weight restriction.</i><hr>"))
            
            if (length(columns_list_in) > 1) {

                k=k+1
                slider_list[k] = list(HTML("<h3>Inputs:</h3>"))
                
                k=k+1
                for(x in columns_list_in) {
                  for(y in columns_list_in) {
                    if(x!=y) {
                      min_lb = round(min(df[x])/max(df[y]) + 1, digits=2)
                      max_ub = round(max(df[x])/min(df[y]) + 1, digits=2)
    
                      default_lb = (min_lb + max_ub )/3
                      default_ub = default_lb * 2
      
                      # slider_list[k] = list(
                      #   sliderInput(
                      #     paste("mdea_weights", paste(x,y,sep="_"), sep = "__"), #InputID mdea_weights.column1_column2
                      #     paste(x,y,sep=" - "), # Input label on UI
                      #     min = min_lb,
                      #     max = max_ub,
                      #     value = c(default_lb,default_ub)
                      #     ))
                      
                      slider_list[k] = list(HTML(paste("<h4>", paste(x,y,sep=" - "), "</h4>")))
      
                      slider_list[k+1] = list(
                        textInput(
                          inputId = paste("lower_bound", paste(x,y,sep="_"), sep = "__"),
                          # value = 0,
                          label="Lower bound",
                          placeholder = "e.g. 0, 1, 2 etc."
                          ))
    
                      slider_list[k+2] = list(
                        textInput(
                          inputId = paste("upper_bound", paste(x,y,sep="_"), sep = "__"),
                          # value = Inf,
                          label="Upper bound",
                          placeholder = "e.g. 100, Inf, etc."
                          ))
      
                      # saving  InputID into a list for later access
                      mdea_cols_list_input <<- append(mdea_cols_list_input, paste(x,y,sep="_"))
      
                      k=k+3
                    }
                  }
                }
  
                print("mdea_cols_list_input")
                print(mdea_cols_list_input)
            }
            else {
              k=k+1
              slider_list[k] = list(HTML("In order to user Weight Distrbution, please choose atleast 2 'Non-Zero' Input columns, Otherwise Please ignore and Run the Analysis."))
            }
            
            if (length(columns_list_out) > 1) {
              
              k=k+1
              slider_list[k] = list(HTML("<h3>Outputs:</h3>"))
              
              k=k+1
              
              for(x in columns_list_out) {
                for(y in columns_list_out) {
                  if(x!=y) {
                    
                    min_lb = round(min(df[x])/max(df[y]) + 1, digits=2)
                    max_ub = round(max(df[x])/min(df[y]) + 1, digits=2)
                    
                    default_lb = (min_lb + max_ub)/3   # 1/3rd of the slider
                    default_ub = default_lb * 2        # 2/3rd of the slider
                    
                    # slider_list[k] = list(
                    #   sliderInput(
                    #     paste("mdea_weights", paste(x,y,sep="_"), sep = "__"), #InputID mdea_weights.column1_column2
                    #     paste(x,y,sep=" - "), # Input label on UI
                    #     min = min_lb,
                    #     max = max_ub,
                    #     value = c(default_lb,default_ub)
                    #   ))
                    
                    slider_list[k] = list(HTML(paste("<h4>", paste(x,y,sep=" - "), "</h4>")))
                    
                    slider_list[k+1] = list(
                      textInput(
                        inputId = paste("lower_bound", paste(x,y,sep="_"), sep = "__"),
                        # value = 0,
                        label="Lower bound",
                        placeholder = "e.g. 0, 1, 2 etc."
                      ))
                    
                    slider_list[k+2] = list(
                      textInput(
                        inputId = paste("upper_bound", paste(x,y,sep="_"), sep = "__"),
                        # value = Inf,
                        label="Upper bound",
                        placeholder = "e.g. 100, Inf, etc."
                      ))
                    
                    # saving  InputID into a list for later access
                    # mdea_cols_list_output <<- paste("mdea_weights", paste(x,y,sep="_"), sep = "__")
                    mdea_cols_list_output <<- append(mdea_cols_list_output, paste(x,y,sep="_"))
                    
                    k=k+3
                  }
                }
              }
              
              print("mdea_cols_list_output")
              print(mdea_cols_list_output)
              
            }
            else {
              k=k+1
              slider_list[k] = list(HTML("In order to user Weight Distrbution, please choose atleast 2 'Non-Zero' Output columns, Otherwise Please ignore and Run the Analysis."))
            }
  
            return(slider_list)
          })
        }

        # Find if any error occurred, and if so display
        error <- get.error()
        if (!is.null(error))
          session$sendCustomMessage(type = "show_error", error)

        # Re-enable buttons
        elem.disable("btn", FALSE, TRUE)
      })

      if (data.model == "mdea") {
        print("Inside MULTIPLIER_DEA")
        # Change the results tabset to display the plot
        hideTab("ts.setup", "ts.setup.dea_selection", session)
        hideTab("ts.setup", "ts.setup.tfdea_selection", session)
        showTab("ts.setup", "ts.setup.mdea_selection", select = TRUE, session)
        showTab("ts.setup", "ts.setup.mdea_weights", select = TRUE, session)
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
            set.result.mdea(list())
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
      hideTab("ts.result", "ts.result.mdea", session)
      showTab("ts.result", "ts.result.dea", select = FALSE, session)
      updateTabsetPanel(session, "ts.result", selected = "ts.result.plot")
    }
    
    
    # check the model if mDEA and run mDEA Analysis and
    # display the results in the appropriate tables
    if (input$btn.mdeaanalysis != 0 && get.model() == "mdea"){
      
      isolate({
        # Get uploaded data saved in the global server df dataframe
        df <- get.df()
        
        print("Inside MultiplierDEA Analysis")
        
        if (length(mdea_cols_list_input) > 1 && length(mdea_cols_list_output) > 1 ) {

            mdea_cols_list<-append(mdea_cols_list_input, mdea_cols_list_output)
            
            print("DEBUG: printing MDEA_WEIGHTS.COL1_COL2")
            print(mdea_cols_list)
            
            numerators <- c()
            denominators <- c()
            lower_bound <- c()
            upper_bound <- c()
            
            for (item in mdea_cols_list) {
              
              print(item)
    
              # split_item = strsplit(item,"__", fixed = TRUE)
              split_cols = strsplit(item, "_", fixed = TRUE)
              lower_bound_id = paste("lower_bound", item, sep="__")
              upper_bound_id = paste("upper_bound", item, sep="__")
              
              print("lower_bound_id")
              print(lower_bound_id)
              print("upper_bound_id")
              print(upper_bound_id)
              
              # if both upper and lower are empty then skip adding numerator, 
              # denominator, upper and lower bound altogether
              if (input[[upper_bound_id]] != "" && input[[lower_bound_id]] != "") {
                print(paste("Adding numerator", split_cols[[1]][1]))
                numerators <- c(numerators, split_cols[[1]][1])
                
                print(paste("Adding denominator", split_cols[[1]][2]))
                denominators <- c(denominators, split_cols[[1]][2])
                
                print(paste("Lower bound:", input[[lower_bound_id]]))
                # if no lower bound is provided but upper bound is there 
                # then assume 0 as lower bound
                if (input[[lower_bound_id]] == "") {
                  lower_bound <- c(lower_bound, 0)
                }
                else {
                  lower_bound <- c(lower_bound, as.numeric(input[[lower_bound_id]]))
                }
                
                print(paste("Upper bound:", input[[upper_bound_id]]))
                # if no upper bound is provided but lower bound is there 
                # then assume Inf as upper bound
                if (input[[upper_bound_id]] == "") {
                  upper_bound <- c(upper_bound, Inf)
                }
                else {
                  upper_bound <- c(upper_bound, as.numeric(input[[upper_bound_id]]))
                }
                
              }
            }
            
            print("Final Weight Restrcition: ")
            print("input$mdea.inputs")
            print(input$mdea.inputs)
            print("input$mdea.outputs")
            print(input$mdea.outputs)
            print("numerators")
            print(numerators)
            print("denominators")
            print(denominators)
            print("lower_bound")
            print(lower_bound)
            print("upper_bound")
            print(upper_bound)
            
            if (length(lower_bound) > 0 && length(upper_bound) == 0) {
              weightRestriction<-data.frame(lower = lower_bound,
                                            numerator = numerators,
                                            denominator = denominators)
            }
            else if (length(lower_bound) == 0 && length(upper_bound) > 0) {
              weightRestriction<-data.frame(numerator = numerators,
                                            denominator = denominators,
                                            upper = upper_bound)
            } 
            else {
              weightRestriction<-data.frame(lower = lower_bound,
                                            numerator = numerators,
                                            denominator = denominators,
                                            upper = upper_bound)
            }

            
            if (length(lower_bound) > 0 || length(upper_bound) > 0) {
              mdea <- mdea.analysis(session, df, input$mdea.inputs, input$mdea.outputs, input$rts, input$orientation, weightRestriction)
            }
            # if no weights are provided for column pairs, then run without weight restriction
            else {
              mdea <- mdea.analysis(session, df, input$mdea.inputs, input$mdea.outputs, input$rts, input$orientation)
            }
        }
        else {
            mdea <- mdea.analysis(session, df, input$mdea.inputs, input$mdea.outputs, input$rts, input$orientation)
        }
        # print("Inside MultiplierDEA Analysis")
        # print(mdea)

        set.result.mdea(mdea)
        set.result.dea(list())
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
      hideTab("ts.result", "ts.result.dea", session)
      showTab("ts.result", "ts.result.mdea", select = FALSE, session)
      updateTabsetPanel(session, "ts.result", selected = "ts.result.plot")
    }
    
    # check the model if TFDEA and run TFDEA and linear regression analysis and
    # display the results in the appropriate tables
    if (input$btn.tfdeaanalysis != 0 && get.model() == "tfdea"){
      

      isolate({
        # Get uploaded data saved in the global server df dataframe
        df <- get.df()
        
        print("Inside TFDEA Analysis")
        
        tfdea <- tfdea.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date,
                                input$front.date, input$rts, input$orientation, input$secondary.obj,
                                input$frontier.type, input$segroc)
        
        # Run linear regression analysis with selected parameters (location: function.R)
        lr <- lr.analysis(df, input$tfdea.inputs, input$tfdea.outputs, input$intro.date, input$front.date)
        
        # Set global server results. This is required so the results can be accessed by downHandler
        # and renderDataTable functions. results cannot be sent as an argument to these functions.
        set.result.dea(list())
        set.result.mdea(list())
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
      hideTab("ts.result", "ts.result.mdea", session)
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
    
    print("Inside Plot results section:2")
    plot.result(result$lr, result$tfdea, result$dea, result$mdea)
  })
  
  # Observe the change in the intro.date selectInput value. This ensures that when a 
  # column is selected for intro.date, the column will not be shown as a option for the 
  # inputs and outputs
  observe({
    if (input$intro.date != "NONE"){
      print("Inside Observe:1")
      
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
      
      print("col.names.in")
      print(col.names.in)
      
      print("col.names.out")
      print(col.names.out)
      
      
      # wr_num_cols <- col.names[which(col.names != input$mdea.wr_denom)]
      # wr_denom_cols <- col.names[which(col.names != input$mdea.wr_num)]
      
      # If the orientation is output, only allow Constant_1 as an input selection,
      # and vice-versa
      if (input$orientation == "out" || input$dea.orientation == "out" 
          || input$mdea.orientation == "out") {
        col.names.in <- c("Constant_1", col.names)
      }
      
      if (input$orientation == "in" || input$dea.orientation == "in" 
          || input$mdea.orientation == "in") {
        col.names.out <- c("Constant_1", col.names)
      }
      
      # Update the options for inputs and outputs for TFDEA
      updateSelectInput(session, 'tfdea.inputs', 'Select Input(s):', 
                        col.names.in)
      updateSelectInput(session, 'tfdea.outputs', 'Select Output(s):', 
                        col.names.out)
      
      # Update the options for inputs and outputs for DEA
      updateSelectInput(session, 'dea.inputs', 'Select Input(s):', 
                        col.names.in)
      updateSelectInput(session, 'dea.outputs', 'Select Output(s):', 
                        col.names.out)
      
      # Update the options for inputs and outputs for Multiplier DEA
      updateSelectInput(session, 'mdea.inputs', 'Select Input(s):', 
                        col.names.in)
      updateSelectInput(session, 'mdea.outputs', 'Select Output(s):', 
                        col.names.out)
      # 
      # print("Inside Observe:2")
      # # Update the options for Weight Restriction in Multiplier DEA
      # updateSelectInput(session, 'mdea.wr_num', 'Select Numerator Column:',
      #                   wr_num_cols)
      # updateSelectInput(session, 'mdea.wr_denom', 'Select Denominator Column:',
      #                   wr_denom_cols)
      # 
      
    }

  }) # observe intro.date
  

  ## Update TFDEA input and Output Select Inputs based on selection in each other
    
  observeEvent(input$tfdea.inputs, {

    # Get uploaded data saved in the global server df dataframe
    df <- get.df()

    # Determine the names of the columns that are numeric
    col.numeric <- sapply(df, is.numeric)

    col.names <- "NONE"
    if (sum(col.numeric) != 0)
      col.names <- colnames(df[, col.numeric], do.NULL = TRUE)

    cols.out <- c()

    for (col in col.names) {
      if (!(col %in% input$tfdea.inputs)) {
        cols.out <- c(col, cols.out)
      }
    }

    if (input$orientation == "in" || input$dea.orientation == "in"
        || input$mdea.orientation == "in") {
      cols.out <- c("Constant_1", cols.out)
    }

    updateSelectInput(session, 'tfdea.outputs', 'Select Output(s):',
                      cols.out, input$tfdea.outputs)

  })


  observeEvent(input$tfdea.outputs, {

    # Get uploaded data saved in the global server df dataframe
    df <- get.df()

    # Determine the names of the columns that are numeric
    col.numeric <- sapply(df, is.numeric)

    col.names <- "NONE"
    if (sum(col.numeric) != 0)
      col.names <- colnames(df[, col.numeric], do.NULL = TRUE)

    cols.in <- c()

    for (col in col.names) {
      if (!(col %in% input$tfdea.outputs)) {
        cols.in <- c(col, cols.in)
      }
    }

    if (input$orientation == "out" || input$dea.orientation == "out"
        || input$mdea.orientation == "out") {
      cols.in <- c("Constant_1", cols.in)
    }

    updateSelectInput(session, 'tfdea.inputs', 'Select Input(s):',
                      cols.in, input$tfdea.inputs)

  })

  
  ## Update DEA input and Output Select Inputs based on selection in each other
  observeEvent(input$dea.inputs, {

    
    # Get uploaded data saved in the global server df dataframe
    df <- get.df()
    
    if(nrow(df) != 0) {
      # Determine the names of the columns that are numeric
      col.numeric <- sapply(df, is.numeric)
      print("col.numeric")
      print(col.numeric)
    
      col.names <- "NONE"
      if (sum(col.numeric) != 0)
        col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
    
      cols.out <- c()
    
      for (col in col.names) {
        if (!(col == input$dea.inputs)) {
          cols.out <- c(col, cols.out)
        }
      }
    
      if (input$orientation == "in" || input$dea.orientation == "in"
          || input$mdea.orientation == "in") {
        cols.out <- c("Constant_1", cols.out)
      }
    
      updateSelectInput(session, 'dea.outputs', 'Select Output(s):',
                        cols.out, input$dea.outputs)
    }

  })


  observeEvent(input$dea.outputs, {

    # Get uploaded data saved in the global server df dataframe
    df <- get.df()
    
    if(nrow(df) != 0) {

      # Determine the names of the columns that are numeric
      col.numeric <- sapply(df, is.numeric)
  
      col.names <- "NONE"
      if (sum(col.numeric) != 0)
        col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
  
      cols.in <- c()
  
      for (col in col.names) {
        if (!(col == input$dea.outputs)) {
          cols.in <- c(col, cols.in)
        }
      }
  
      if (input$orientation == "out" || input$dea.orientation == "out"
          || input$mdea.orientation == "out") {
        cols.in <- c("Constant_1", cols.in)
      }
  
      updateSelectInput(session, 'dea.inputs', 'Select Input(s):',
                        cols.in, input$dea.inputs)
    }

  })


  ## Update MDEA input and Output Select Inputs based on selection in each other
  
  observeEvent(input$mdea.inputs, {
    
    # Get uploaded data saved in the global server df dataframe
    df <- get.df()
    
    # Determine the names of the columns that are numeric
    col.numeric <- sapply(df, is.numeric)
    
    col.names <- "NONE"
    if (sum(col.numeric) != 0)                 
      col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
    
    cols.out <- c()
    
    for (col in col.names) {
      if (!(col %in% input$mdea.inputs)) {
        cols.out <- c(col, cols.out)
      }
    }
    
    if (input$orientation == "in" || input$dea.orientation == "in" 
        || input$mdea.orientation == "in") {
      cols.out <- c("Constant_1", cols.out)
    }
    
    updateSelectInput(session, 'mdea.outputs', 'Select Output(s):', 
                      cols.out, input$mdea.outputs)
    
  })
  
  
  observeEvent(input$mdea.outputs, {
    
    # Get uploaded data saved in the global server df dataframe
    df <- get.df()
    
    # Determine the names of the columns that are numeric
    col.numeric <- sapply(df, is.numeric)
    
    col.names <- "NONE"
    if (sum(col.numeric) != 0)                 
      col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
    
    cols.in <- c()
    
    for (col in col.names) {
      if (!(col %in% input$mdea.outputs)) {
        cols.in <- c(col, cols.in)
      }
    }
    
    if (input$orientation == "out" || input$dea.orientation == "out" 
        || input$mdea.orientation == "out") {
      cols.in <- c("Constant_1", cols.in)
    }
    
    updateSelectInput(session, 'mdea.inputs', 'Select Input(s):', 
                      cols.in, input$mdea.inputs)
  })
  

})
