#******************************************************************************
#
# tables.R
# Sortable datatables displaying data and results
#
#******************************************************************************

# Display uploaded data
# output$dt.data <- renderDataTable({
# 
#   df <- get.df()
#   if (nrow(df) == 0)
#     return(NULL)
#   
#   table <- df # data.frame that will be displayed in the table
#   
#   # Arrange data that will be displayed in the table. renderDataTable does not show row 
#   # names, so need to append row names
#   row.nums <- seq(nrow(table))
#   table <- cbind(row.nums, row.names(table), table)
#   names(table) <- c("ROW", "DMU",  toupper(names(df)))
#   return(table)
# }, options = list(bFilter=1, bSort=1, bProcessing=0,
#                   aLengthMenu = c(10, 20, 30), iDisplayLength = 10))
# 
# 
# # Display a summary of all results
# output$dt.summary <- renderDataTable({
#   result <- get.result()
#   if (length(result$tfdea) == 0)
#     return(NULL)
#   
#   if (length(result$lr) == 0) 
#     lr.mad <- NA
#   else                            
#     lr.mad <- result$lr$summary[1]
#   
#   table <- cbind(result$tfdea$summary[1], lr.mad)
#   table <- format(table, nsmall = 3)
#   names(table) <- c("MAD (TFDEA)", "MAD (LR)")
#   return(table)
# }, options = list(bFilter=0, bSort=0, bProcessing=0,
#                   bPaginate = 0, bInfo = 0))
# 
# 
# # Display summary of TFDEA results
# output$dt.tfdea.summary <- renderDataTable({
#   result <- get.result()
#   if (length(result$tfdea) == 0)
#     return(NULL)
#   
#   table <- result$tfdea$summary
#   table <- format(table, nsmall = 3)
#   names(table) <- toupper(colnames(table))
#   return(table)
# }, options = list(bFilter=0, bSort=0, bProcessing=0,
#                   bPaginate = 0, bInfo = 0))
# 
# 
# # Display TFDEA forecast results
# output$dt.tfdea.forecast <- renderDataTable({
#   df <- get.df()
#   result <- get.result()
#   if (length(result$tfdea) == 0)
#     return(NULL)
#   
#   table <- result$tfdea$forecast
#   table <- format(table, nsmall = 3)
#   
#   # renderDataTable does not show row names or numbers, so need to append both
#   table <- cbind(seq(nrow(df)), rownames(table), table) 
#   names(table) <- c("ROW", "DMU", toupper(names(result$tfdea$forecast))) 
#   return(table)
# }, options = list(bFilter=1, bSort=1, bProcessing=0,
#                   aLengthMenu = c(10, 20, 30), iDisplayLength = 10))
# 
# 
# 
# # Display summary of linear regression results 
# output$dt.lr.summary <- renderDataTable({
#   result <- get.result()
#   if (length(result$lr) == 0)
#     return(NULL)
#   
#   table <- result$lr$summary
#   table <- format(table, nsmall = 3)
#   names(table) <- toupper(colnames(table))
#   return(table)
# }, options = list(bFilter=0, bSort=0, bProcessing=0,
#                   bPaginate = 0, bInfo = 0))
# 
# 
# # Display linear regression coefficients
# output$dt.lr.coeff <- renderDataTable({
#   result <- get.result()
#   if (length(result$lr) == 0)
#     return(NULL)
#   
#   table <- result$lr$coefficients
#   table <- format(table, nsmall = 3)
#   table <- cbind(row.names(table), table)
#   names(table) <- c("COEFFICIENTS", toupper(names(result$lr$coefficients))) 
#   return(table)
# }, options = list(bFilter=0, bSort=0, bProcessing=0,
#                   bPaginate = 0, bInfo = 0))
# 
# 
# # Display linear regression multi-collinearity results
# output$dt.lr.mc <- renderDataTable({
#   result <- get.result()
#   if (length(result$lr$mc) == 0)
#     return(NULL)
#   
#   table <- result$lr$mc
#   table <- format(table, nsmall = 3)
#   table <- cbind(row.names(table), table)
#   names(table) <- c("COEFFICIENTS", toupper(names(result$lr$mc))) 
#   return(table)
# }, options = list(bFilter=0, bSort=0, bProcessing=0,
#                   bPaginate = 0, bInfo = 0))
# 
# 
# 
# # Display linear regression forecast results
# output$dt.lr.forecast <- renderDataTable({
#   df <- get.df()
#   result <- get.result()
#   if (length(result$lr) == 0)
#     return(NULL)
#   
#   table <- result$lr$forecast
#   table <- format(table, nsmall = 3)
#   
#   # renderDataTable does not show row names or numbers, so need to append both
#   table <- cbind(seq(nrow(df)), rownames(table), table)
#   names(table) <- c("ROW", "DMU", toupper(names(result$lr$forecast))) 
#   return(table)
# }, options = list(bFilter=1, bSort=1, bProcessing=0,
#                   aLengthMenu = c(10, 20, 30), iDisplayLength = 10))

