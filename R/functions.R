#******************************************************************************
#
# functions.R
# Functions used throughout the server code
#
#******************************************************************************

# Disable element(s) based on the id or class. In this case it is used to disable all buttons
# Parameters:
# id      -> identifier for the HTML element or the HTML class
# disable -> true to disable the element, false to (re)enable
# class   -> true if the id is a class identifier
elem.disable <- function(id, disable = FALSE, class = FALSE) {
  if (class) 
    msg <- paste0("$('.", id, "').prop('disabled',", tolower(disable), ")")
  else 
    msg <- paste0("$('#", id, "').prop('disabled',", tolower(disable), ")")
  
  session$sendCustomMessage(type="jsCode", list (code = msg))
}


# Retrieve data from three possible sources (local/google spreadsheet/dropbox)
# Parameters:
# fsource                 -> file source (local/google/dropbox)
# data.location           -> the URL or local file path where the file is located 
# col.header, row.header  -> true if the data contains a column/row header
# file.sep, file.quote    -> file seperator and file quote
# Return:
# df                      -> Uploaded data
data.load <- function(fsource, data.location = NULL, col.header = FALSE, row.header = FALSE, 
                      file.sep = ",", file.quote = ""){
  
  # Retrieved data from the specified file location
  df <- data.frame()
  switch(fsource,
         # Local file source selected by user
         local = {
           # Determine whether a file has been chosen by the user
           if (is.null(data.location)) {
             set.error("No File Chosen. Select a csv file using the 'Choose File' button") 
             return(data.frame())
           }
           
           # Retrieve the data from the specified file location
           tryCatch(
             df <- read.csv(data.location$datapath, sep = file.sep, quote = file.quote, header = col.header,
                            stringsAsFactors = FALSE, row.names = NULL),
             error = function(e) { set.error(paste("Error accessing local file:", e)) 
                                   return(NULL) })
         },
         # Google Spreadsheet file source selected by user
         google = {
           # sql query to select rows of data where column A is not empty
           # select.query <- "select * where A!=''"
           # # Split URL by key
           # url.split <- unlist(strsplit(data.location,"key="))
           # # If there is no key specified in the URL, return an error
           # if (length(url.split) < 2) {
           #   set.error("Google spreadsheet URL does not include a key (key=)")
           #   return(data.frame())
           # }
           # # Split URL by spreadsheet identifier (gid)
           # url.split <- unlist(strsplit(url.split[2],"#gid="))
           # # Google spreadsheet key, between 'key=' and '#gid=' in URL
           # file.key <- url.split[1]
           # # If the sheet number is not specified, then the first sheet will be selected
           # # Sheet number (starting at 0), follows '#gid=' in URL
           # file.gid <- ifelse (length(url.split) < 2, 0, url.split[2])
           # 
           # # Create the URL for the spreadsheet using the query, key and gid
           # file.url <- paste(sep="",'https://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=',
           #                   curlEscape(select.query), '&key=', file.key, '&gid=', file.gid)
           
           url <- toString(data.location)
           print(url)
           
           # Disable authentication for public Google Sheets
           gs4_deauth()
           # print(read_sheet(file.url))
           # print(file.url[1])

           # Retrieve the data from the specified spreadsheet URL
           tryCatch(
             # df <- read.csv(textConnection(getURL(file.url, ssl.verifypeer = FALSE)),
             #                header=col.header, stringsAsFactors = FALSE),
             df <- as.data.frame(read_sheet(url)),
             error = function(e) { set.error(paste("Error accessing Google spreadhsheet:", e)) 
                                   return(NULL) })
           print(df)
           
         },
         # Dropbox file source selected by user
         dropbox = {
           # Retrieve the data from the specified Dropbox URL
           tryCatch(
             df <- read.csv(textConnection(getURL(data.location, ssl.verifypeer = FALSE)), 
                            sep = file.sep, quote = file.quote, header = col.header, 
                            stringsAsFactors = FALSE, row.names = NULL),
             error = function(e) { set.error(paste("Error accessing Dropbox file:", e)) 
                                   return(NULL) })
         },
         {
           # Retrieve the data using default
           tryCatch(
             df <- read.csv(data.location, sep = file.sep, quote = file.quote, header = col.header,
                          stringsAsFactors = FALSE, row.names = NULL),
             error = function(e) { set.error(paste("Error accessing file:", e)) 
                                   return(NULL) })
         }
  )# End switch
  
# --> Only select rows with values for all columns (Should this be done?)
  df <- df[complete.cases(df),]
  
  # If the dataset has no rows then there is no data in the source file. Return error
  if (nrow(df) == 0) {
  # if (!isTruthy(df)) {
    set.error("Data cannot be found at specified location or chosen parameters not correct")
    return(data.frame()) 
  }

  # If row header = TRUE and there is more than one column, make the first column the row names 
  if (ncol(df) > 1 && row.header)
    if (anyDuplicated(df[, 1]) == 0) {
      rownames(df) <- df[, 1]
      df <- df[, -1]
    }
    else {
      set.error("Duplicated row names, deselect row header check box")
      return(data.frame()) 
    }

  # Return unique rows
  return (unique(df))
}


# Populate the SelectInput options under the Setup and Data Plot tab 
# Parameters:
# df -> uploaded data
populate.options <- function(df) {
  # Determine the names of the columns that are numeric
  print(df)
  col.numeric <- sapply(df, is.numeric)
  print(col.numeric)
  
  # If no numeric columns exist, return error
  if (sum(col.numeric) != 0)
    col.names <- colnames(df[, col.numeric], do.NULL = TRUE)
  else {
    set.error("There are no numeric columns. Ensure all entries in input/output columns
              of data are numeric")
    return()
  }
  
  print("Uniq")
  print(col.names)
  
  # Update selectInputs with names of numeric columns, with the addition of constant_1
  col.names <- c("Constant_1", col.names)
  #tfdea
  updateSelectInput(session, 'tfdea.inputs', 'Select Input(s):', 
                    col.names, selected = NULL)
  updateSelectInput(session, 'tfdea.outputs', 'Select Output(s):', 
                    col.names, selected = NULL)
  updateSelectInput(session, 'intro.date', 'Select Year of Introduction:', 
                    col.names[-1])
  #dea
  updateSelectInput(session, 'dea.inputs', 'Select Input:', 
                    col.names, selected = NULL)
  updateSelectInput(session, 'dea.outputs', 'Select Output:', 
                    col.names, selected = NULL)
  #mdea
  updateSelectInput(session, 'mdea.inputs', 'Select Input(s):', 
                    col.names, selected = NULL)
  updateSelectInput(session, 'mdea.outputs', 'Select Output(s):', 
                    col.names, selected = NULL)

  # mdea weight restriction
  updateSelectInput(session, 'mdea.wr_num', 'Weight Restriction (Numerator) Column:',
                    col.names, selected = NULL)
  updateSelectInput(session, 'mdea.wr_denom', 'Weight Restriction  (Denominator) Column:',
                    col.names, selected = NULL)

}


# Conduct DEA analysis:
# Parameters:
# df                          -> uploaded data
# inputs/outputs              -> DEA inputs and outputs
# rts/orientation             -> returns to scale and orientation of DEA model
dea.analysis <- function(df, inputs, outputs, rts = "vrs", orientation = "output") {
  
  print("Inside dea.analysis")
  # Check parameter values
  if (length(input$dea.inputs) == 0) {
    set.error("No input(s) selected. Select a minimum of 1 input") 
    return(NULL) 
  }
  if (length(input$dea.outputs) == 0) {
    set.error("No output(s) selected. Select a minimum of 1 output") 
    return(NULL) 
  }
  if (nrow(df) == 0) {
    set.error("No data exists in selected data file") 
    return(NULL) 
  }

  dmu.count <- nrow(df)
  dmu.names <- row.names(df)
  # Create vector of all 1's for constant
  constant <- rep(1, dmu.count)
  
  # Check if constant_1 was selected and if so append to inputs/outputs
  x.constant <- match("Constant_1", inputs)
  y.constant <- match("Constant_1", outputs)
  if (!is.na(x.constant))  
    x <- cbind(constant, df[inputs[-x.constant]])
  else                            
    x <- df[inputs]
  
  if (!is.na(y.constant))  
    y <- cbind(constant, df[outputs[-y.constant]])
  else                            
    y <- df[outputs]
  
  # Assign names to inputs and outputs
  colnames(x) <- toupper(paste('x', colnames(x), sep='_'))
  colnames(y) <- toupper(paste('y', colnames(y), sep='_'))
  
  orientation <- "out"

  
  DEA <- dea(X = x, Y = y, RTS = rts, ORIENTATION = orientation)
  
  DEA.x <- x
  DEA.y <- y
  DEA.rts <- rts
  DEA.orientation <- orientation
  
  list.result <- list(eff = DEA$eff, 
                      lambda = DEA$lambda,
                      objval = DEA$objval,
                      exportlist = list(eff = data.frame(DEA$eff),
                                        lambda = data.frame(DEA$lambda),
                                        objval = data.frame(DEA$objval)
                                        ))

  return(list.result) 
}

# Conduct Multiplier-DEA analysis with input orientation: 
# Parameters: 
# df                          -> uploaded data 
# inputs/outputs              -> TFDEA inputs and outputs 
# rts/orientation             -> returns to scale and orientation of TFDEA model 
# mdea.analysis <- function(df, inputs, outputs, rts = "vrs", orientation = "input") { 
   
#   print("Inside mdea.analysis") 
#   # Check parameter values 
#   if (length(input$mdea.inputs) == 0) { 
#     set.error("No input(s) selected. Select a minimum of 1 input")  
#     return(NULL)  
#   } 
#   if (length(input$mdea.outputs) == 0) { 
#     set.error("No output(s) selected. Select a minimum of 1 output")  
#     return(NULL)  
#   } 
#   if (nrow(df) == 0) { 
#     set.error("No data exists in selected data file")  
#     return(NULL)  
#   } 
   
#   dmu.count <- nrow(df) 
#   dmu.names <- row.names(df) 
#   # Create vector of all 1's for constant 
#   constant <- rep(1, dmu.count) 
   
#   # Check if constant_1 was selected and if so append to inputs/outputs 
#   x.constant <- match("Constant_1", inputs) 
#   y.constant <- match("Constant_1", outputs) 
#   if (!is.na(x.constant))   
#     x <- cbind(constant, df[inputs[-x.constant]]) 
#   else                             
#     x <- df[inputs] 
   
#   if (!is.na(y.constant))   
#     y <- cbind(constant, df[outputs[-y.constant]]) 
#   else                             
#     y <- df[outputs] 
#     y <- df[outputs] 
   
#   # Assign names to inputs and outputs 
#   colnames(x) <- toupper(paste('x', colnames(x), sep='_')) 
#   colnames(y) <- toupper(paste('y', colnames(y), sep='_')) 
   
#   orientation <- "output" 
   
#   print(x) 
#   print(y) 
#   print(rts) 
#   print(orientation) 
 

# Conduct Multiplier-DEA analysis:
# Parameters:
# df                          -> uploaded data
# inputs/outputs              -> TFDEA inputs and outputs
# rts/orientation             -> returns to scale and orientation of TFDEA model
mdea.analysis <- function(session, df, inputs, outputs, rts = "vrs", orientation = "output", weightRestriction) {
  
  print("Inside mdea.analysis")
  # Check parameter values
  if (length(input$mdea.inputs) == 0) {
    set.error("No input(s) selected. Select a minimum of 1 input") 
    return(NULL) 
  }
  if (length(input$mdea.outputs) == 0) {
    set.error("No output(s) selected. Select a minimum of 1 output") 
    return(NULL) 
  }
  if (nrow(df) == 0) {
    set.error("No data exists in selected data file") 
    return(NULL) 
  }

  # browser()
  print(input$mdea.inputs)
  print(df[input$mdea.inputs])
  
  for (col in input$mdea.inputs) {
    print(as.list(df[col]))
    for (val in as.list(df[col])){
      print(val)
      if (0 %in% val) {
        # set.error(paste("Input Column: ", col, " contains one or more values that are 0. This is invalid for Weight Restriction"))
        # return(NULL)
        # browser()
        # Find if any error occurred, and if so display
        shinyalert(paste("Application reloading soon... ERROR:", "In column", col, "data has DMU's with values that are zero, th is may cause numerical
      problems"),
                   type="error",
                   session = session,
                   showConfirmButton = FALSE,
                   callbackR = function(x) {session$reload();},
                   callbackJS = function(x) {location.reload();})
        Sys.sleep(3)
        session$reload();

      }  
    }
  }

  print(input$mdea.outputs)
  print(df[input$mdea.outputs])
  
  for (col in input$mdea.outputs) {
    print(as.list(df[col]))
    for (val in as.list(df[col])){
      print(val)
      if (0 %in% val) {
        # set.error(paste("Input Column: ", col, " contains one or more values that are 0. This is invalid for Weight Restriction"))
        # return(NULL)
        # browser()
        # Find if any error occurred, and if so display
        shinyalert(paste("Application reloading soon... ERROR:", "In column", col, "data has DMU's with values that are zero, th is may cause numerical
      problems"),
                   type="error",
                   session = session,
                   showConfirmButton = FALSE,
                   callbackR = function(x) {session$reload();},
                   callbackJS = function(x) {location.reload();})
        Sys.sleep(3)
        session$reload();
        
      }  
    }
  }
  
  dmu.count <- nrow(df)
  dmu.names <- row.names(df)
  # Create vector of all 1's for constant
  constant <- rep(1, dmu.count)
  
  # Check if constant_1 was selected and if so append to inputs/outputs
  x.constant <- match("Constant_1", inputs)
  y.constant <- match("Constant_1", outputs)
  if (!is.na(x.constant))  
    x <- cbind(constant, df[inputs[-x.constant]])
  else                            
    x <- df[inputs]
  
  if (!is.na(y.constant))  
    y <- cbind(constant, df[outputs[-y.constant]])
  else                            
    y <- df[outputs]
  
  
  
  
  # Assign names to inputs and outputs
  # colnames(x) <- toupper(paste('x', colnames(x), sep='_'))
  # colnames(y) <- toupper(paste('y', colnames(y), sep='_'))
  
  orientation <- "output"
  
  print("Inside mdea.analysis:2")

  print(x)
  print(y)
  print(rts)
  print(orientation)
  # print(weightRestriction)
  mDEA <- DeaMultiplierModel( x, y, rts, orientation, weightRestriction)
  # mDEA <- DeaMultiplierModel( x, y, rts, orientation)
  
  mDEA.x <- x
  mDEA.y <- y
  mDEA.rts <- rts
  mDEA.orientation <- orientation
  
  list.result <- list(Efficiency = mDEA$Efficiency, 
                      Lambda = mDEA$Lambda,
                      Model_Status  = mDEA$Model_Status,
                      vx = mDEA$vx,
                      uy = mDEA$uy,
                      Free_Weights = mDEA$Free_Weights,
                      InputValues = mDEA$InputValues,
                      OutputValues = mDEA$OutputValues,
                      HCU_Input = mDEA$HCU_Input,
                      HCU_Output = mDEA$HCU_Output,
                      exportlist = list(Efficiency = data.frame(mDEA$Efficiency),
                                        Lambda = data.frame(mDEA$Lambda),
                                        Model_Status  = data.frame(mDEA$Model_Status),
                                        vx = data.frame(mDEA$vx),
                                        uy = data.frame(mDEA$uy),
                                        Free_Weights = data.frame(mDEA$Free_Weights),
                                        InputValues = data.frame(mDEA$InputValues),
                                        OutputValues = data.frame(mDEA$OutputValues),
                                        HCU_Input = data.frame(mDEA$HCU_Input),
                                        HCU_Output = data.frame(mDEA$HCU_Output)
                                        )
                      )
  
  return(list.result)
}


# Conduct TFDEA analysis:
# Parameters:
# df                          -> uploaded data
# inputs/outputs              -> TFDEA inputs and outputs
# intro.date                  -> date of introduction column
# front.date                  -> frontier date
# rts/orientation             -> returns to scale and orientation of TFDEA model
# secondary.obj/frontier.type -> secondary objective and frontier type of TFDEA model
# segroc                      -> segmented rate of change
# Return:
# Forecast                    -> forecasted dates, ROCs, and efficiencies
# Model                       -> TFDEA parameters used for the analysis
# Summary                     -> MAD, Average ROC, ROC contributors, and other relevant information 
tfdea.analysis <- function(df, inputs, outputs, intro.date, front.date, rts = "vrs", orientation = "output", 
                           secondary.obj = "min", frontier.type = "static", segroc = FALSE){
  
  # Check parameter values
  if (length(input$tfdea.inputs) == 0) {
    set.error("No input(s) selected. Select a minimum of 1 input") 
    return(NULL) 
  }
  if (length(input$tfdea.outputs) == 0) {
    set.error("No output(s) selected. Select a minimum of 1 output") 
    return(NULL) 
  }
  if (nrow(df) == 0) {
    set.error("No data exists in selected data file") 
    return(NULL) 
  }
  if (!(intro.date %in% colnames(df))) {
    set.error("Introduction date column name not part of dataframe") 
    return(NULL) 
  }
  if (!is.numeric(front.date)) {
    set.error("Frontier date must be a numeric value") 
    return(NULL) 
  }
  
  dmu.count <- nrow(df)
  dmu.names <- row.names(df)
  # Create vector of all 1's for constant
  constant <- rep(1, dmu.count)

  # Check if constant_1 was selected and if so append to inputs/outputs
  x.constant <- match("Constant_1", inputs)
  y.constant <- match("Constant_1", outputs)
  if (!is.na(x.constant))  
    x <- cbind(constant, df[inputs[-x.constant]])
  else                            
    x <- df[inputs]
  
  if (!is.na(y.constant))  
    y <- cbind(constant, df[outputs[-y.constant]])
  else                            
    y <- df[outputs]
  
  # Assign names to inputs and outputs
  colnames(x) <- toupper(paste('x', colnames(x), sep='_'))
  colnames(y) <- toupper(paste('y', colnames(y), sep='_'))

  # Determine TFDEA results
  tryCatch(
    tfdea.output <- TFDEA(x = x, y = y, dmu_date_rel = df[, intro.date], 
                          date_forecast = front.date, rts = rts, orientation = orientation, 
                          second = secondary.obj, mode = frontier.type, segroc = segroc),
    error = function(e) { set.error(paste("Error with TFDEA analysis:", e)) 
                          return(NULL) })
  
  print(tfdea.output)
  # Calculate Mean Absolute Deviation (MAD)
  dev.dates <- tfdea.output$dmu_date_for - df[, intro.date]
  mad <- mean(abs(dev.dates), na.rm = TRUE)
  
# --> isEfficient no longer available in TFDEA package?
  #     soa.prod.r <- sum(sapply(tfdea.output$dmu_eff_rel, isEfficient, input$orientation))
  #     soa.prod.f <- sum(sapply(tfdea.output$dmu_eff_cur, isEfficient, input$orientation))
  
  # Determine ROC contributors
  roc.contr  <- sum(!is.na(tfdea.output$dmu_roc), na.rm = TRUE)
  # Determine number of early forecasts
  early.for  <- sum(tfdea.output$dmu_date_for < df[, intro.date], na.rm = TRUE)
  # Determine number of late forecasts
  late.for   <- sum(tfdea.output$dmu_date_for > df[, intro.date], na.rm = TRUE)

  # TFDEA results summary dataframe
  tfdea.summary <- data.frame(mad, tfdea.output$roc, roc.contr, early.for, late.for, stringsAsFactors = FALSE)
  names(tfdea.summary) <- c("mad", "avg.roc", "roc.contributors", "early.forecasts", "late.forecasts")
  
  # TFDEA model parameters dataframe
  tfdea.model <- data.frame(paste(colnames(x), collapse = "; "), paste(colnames(y), collapse = "; "), 
                            intro.date, front.date, rts, orientation, secondary.obj, frontier.type, 
                            segroc, stringsAsFactors = FALSE)
  names(tfdea.model) <- c("inputs", "outputs", "release.date", "frontier.date", "rts",
                          "orientation", "secondary.obj", "frontier.type", "segmented.roc") 
  
  # TFDEA forecast results
  tfdea.forecast <- data.frame(df[, intro.date], tfdea.output$dmu_eff_rel, tfdea.output$dmu_eff_cur, 
                               tfdea.output$dmu_eff_for, tfdea.output$dmu_roc, tfdea.output$dmu_sroc_cur,
                               tfdea.output$dmu_sroc_for, tfdea.output$dmu_date_for, stringsAsFactors = FALSE)
  names(tfdea.forecast) <- c("release.date", "efficiency.release", "efficiency.frontier", 
                             "efficiency.forecast", "roc", "sroc.frontier", "sroc.forecast", 
                             "forecasted.date")
  
  # Return results summary, model parameters, forecast results, and all lamba matrices as data.frames  
  list.result <- list(forecast = tfdea.forecast, model = tfdea.model, summary = tfdea.summary, 
                      lambda.rel = data.frame(tfdea.output$dmu_lambda_rel), 
                      lambda.cur = data.frame(tfdea.output$dmu_lambda_cur), 
                      lambda.for = data.frame(tfdea.output$dmu_lambda_for))

  return(list.result) 
}


# Conduct Linear Regression analysis and return:
# Parameters:
# df                          -> uploaded data
# inputs/outputs              -> inputs and outputs
# intro.date                  -> date of introduction column
# front.date                  -> frontier date
# Return:
# Forecast                    -> forecasted dates
# Model                       -> LR dependant and independant variables
# Summary                     -> MAD, R^2, adjusted R^2
# Coefficients                -> LR coefficients estimates, std error, p value 
lr.analysis <- function(df, inputs, outputs, intro.date, front.date){
  
  # Check parameter values
  if ((length(input$tfdea.inputs) + length(input$tfdea.outputs))  == 0) {
    set.error("No input(s)/output(s) selected. Select a minimum of 1 input/output") 
    return(NULL) 
  }
  if (nrow(df) == 0) {
    set.error("No data exists in selected data file") 
    return(NULL) 
  }
  if (!(intro.date %in% colnames(df))) {
    set.error("Introduction date column name not part of dataframe") 
    return(NULL) 
  }
  if (!is.numeric(front.date)) {
    set.error("Frontier date must be a numeric value") 
    return(NULL)
  }
  
  # Extract training and forecast data
  training.data <- subset(df, get(intro.date) <= front.date)
  forecast.data <- subset(df, get(intro.date) > front.date)
  
  # Combine inputs and outputs as independent variables and remove Constant_1
  indep.var <- c(inputs, outputs)
  indep.var <- indep.var[which(indep.var != "Constant_1")]

  # Create formula using the introduction date as the dependent variable and the inputs/outputs
  # as independent variables
  formula <- as.formula(paste(intro.date, ' ~ ', paste(indep.var, collapse = '+'))) 
  # Determine linear regression results using data including and before the frontier.date
  tryCatch(
    lr.output <- lm(formula, training.data),
    error = function(e) { set.error(paste("Error fitting linear model:", e)) 
                          return(NULL) })
  
  # Forecast results using linear regression model 
  tryCatch(
    forecast.result <- predict(lr.output, newdata = df),
    error = function(e) { set.error(paste("Error fitting linear model:", e)) 
                          return(NULL) })
  
  # Determine Mean Absolute Deviation (MAD) for forecasted results
  dev.dates <- forecast.result[which(df[, intro.date] > front.date)] - forecast.data[, intro.date]
  mad <- mean(abs(dev.dates), na.rm = TRUE)
  
  # Determine Multi-Collinearity values if there is more than one independent variable
  mc <- data.frame()
  if (length(indep.var) > 1) 
    mc <- vif(lr.output)
  
  # Linear regression results summary dataframe
  lr.summary <- data.frame(mad, summary(lr.output)$r.squared, 
                           summary(lr.output)$adj.r.squared, 
                           stringsAsFactors = FALSE)
  names(lr.summary) <- c("mad", "r2", "adjusted.r2") 
  
  # Linear regression coefficients dataframe
  lr.coefficients <- data.frame(summary(lr.output)$coefficients, 
                                stringsAsFactors = FALSE)
  
  # Multi-Collinearity dataframe
  lr.mc <- data.frame(mc, stringsAsFactors = FALSE)
  if (ncol(lr.mc) > 0) 
    names(lr.mc)[1] <- "GVIF"
  
  # Linear regression model parameters dataframe
  lr.model <- data.frame(intro.date, paste(indep.var, collapse = "; "), 
                         stringsAsFactors = FALSE)
  names(lr.model) <- c("dependent.var", "independent.var") 
  
  # Linear regression forecast results
  lr.forecast <- data.frame(df[, intro.date], forecast.result, stringsAsFactors = FALSE)
  names(lr.forecast) <- c("release.date", "forecasted.date")

  # Return results summary, model parameters, forecast results, coefficients, and Multi-Collinearity results
  list.result <- list(forecast = lr.forecast, model = lr.model, summary = lr.summary, 
                      coefficients = lr.coefficients, mc = lr.mc)
  
  return(list.result)
}
