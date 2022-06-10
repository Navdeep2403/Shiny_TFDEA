#******************************************************************************
#
# ui.R
# Generates the webpage (html/javascript). The page is separated into two panels:
# 1. Left sidebar panel for entering setup information,
# 2. Right main panel for displaying the results.
#
#******************************************************************************

shinyUI(pageWithSidebar(
  
  # Top panel displaying header
  # headerPanel(HTML("<h3>Technology Forecasting using DEA or TFDEA</h3>"),
  headerPanel(HTML("<h3>Technology Forecasting using DEA (TFDEA)</h3>"), 
              windowTitle = "TFDEA"),
  
  # Sidebar Panel containing the tabs for the setup
  sidebarPanel(
    # Add custom CSS and javascript
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/tfdea.custom.css"),
      tags$script(src="js/tfdea.custom.js")
    ),
    # Required for showing progress bar (from shinyIncubator package)
    # progressInit(),
    
    # Setup tabs for the user to enter required parameters for analysis
    tabsetPanel(id="ts.setup",
                tabPanel("Upload File", value="ts.setup.upload",
                         #selectInput('fsource', 'File Source:', file.source.opts),
                         selectInput('fsource', 'File Source:',
                                     # list(Local='local', 'Google Spreadsheet'='google'), 'Local'),
                                     list(Local='local', 'Google Spreadsheet'='google', Dropbox='dropbox'), 'Local'),
                         
                         # This panel will only display when the local option has been selected for File Source
                         conditionalPanel(
                           condition = "input.fsource == 'local'",
                           fileInput('data.file', 'Choose CSV File:', accept= accepted.files)
                         ),                         
                         # This panel will only display when the google option has been selected for File Source
                         conditionalPanel(
                           condition = "input.fsource == 'google'",
                           HTML("<ol><li><a href = 'https://docs.google.com/spreadsheet' target = '_blank'>If required,
                                click here to create new Google Spreadsheet:</a></li><li>Share the Google Spreadsheet,
                                then copy the URL into the text area below:</li></ol>"),
                           tags$textarea(id='gs.url', rows=3, cols=40, class="textarea",
                                          placeholder=gs.default, required=TRUE, gs.default)
                         ),

                         # This panel will only display when the dropbox option has been selected  for File Source
                         conditionalPanel(
                           condition = "input.fsource == 'dropbox'",
                           HTML("Copy the Dropbox URL (including http(s)) into the textboxes below:"),
                           br(),
                           tags$textarea(id='dropbox.url', class="textarea", rows=2, cols=40,
                                         placeholder = dropbox.default, required=TRUE, dropbox.default)
                         ),

                         # This panel will only display when the dropbox or local option has been selected 
                         conditionalPanel(
                           condition = "input.fsource == 'local' || input.fsource == 'dropbox'",
                           selectInput('file.sep', 'Separator:', file.sep.opts),
                           selectInput('file.quote', 'Quote:', file.quote.opts)
                         ),

                         # Options to specify whether file contains row and/or column headers
                         checkboxInput('col.header', 'Column Header (Inputs/Outputs)', TRUE),
                         checkboxInput('row.header', 'Row Header (DMU names in first column?)', FALSE),

                         br(),
                         HTML("<b>Note 1:</b> The structure of the data should be rows for DMUs and columns 
                              for inputs and outputs"),
                         br(),
                         HTML("<b>Note 2:</b> Click Display Data, and if the dataset is correct select 
                              SETUP MODEL tab above to continue"),
                         br(), br(),
                         actionButton("btn.display","Display Data")
                ),
                tabPanel("Choose Model", value="ts.setup.model",
                         selectInput('model', 'Model:',
                                     # list(Local='local', 'Google Spreadsheet'='google'), 'Local'),
                                     list('TFDEA'='tfdea', 'DEA'='dea', 'DEA Multiplier'='dea_multiplier'), 'TFDEA'),
                         # This panel will only display when the local option has been selected for File Source
                         # conditionalPanel(
                         #   condition = "input.fsource == 'local'",
                         #   fileInput('data.file', 'Choose CSV File:', accept= accepted.files)
                         # ),                         
                         # # This panel will only display when the google option has been selected for File Source
                         # conditionalPanel(
                         #   condition = "input.fsource == 'google'",
                         #   HTML("<ol><li><a href = 'https://docs.google.com/spreadsheet' target = '_blank'>If required,
                         #        click here to create new Google Spreadsheet:</a></li><li>Share the Google Spreadsheet,
                         #        then copy the URL into the text area below:</li></ol>"),
                         #   tags$textarea(id='gs.url', rows=3, cols=40, class="textarea",
                         #                 placeholder=gs.default, required=TRUE, gs.default)
                         # ),
                         # 
                         # # This panel will only display when the dropbox option has been selected  for File Source
                         # conditionalPanel(
                         #   condition = "input.fsource == 'dropbox'",
                         #   HTML("Copy the Dropbox URL (including http(s)) into the textboxes below:"),
                         #   br(),
                         #   tags$textarea(id='dropbox.url', class="textarea", rows=2, cols=40,
                         #                 placeholder = dropbox.default, required=TRUE, dropbox.default)
                         # ),
                         actionButton("btn.next","Next")
                ),
                tabPanel("TFDEA Setup Model", value = "ts.setup.tfdea_selection",
                        # selectInput('intro.package', 'Select analysis package:', 'DEA','TFDEA', multiple = TRUE),
                         selectInput('intro.date', 'Select Year of Introduction Column:', 'NONE', multiple = FALSE),
                         # Creates numeric input for frontier date and updates to range of dates in introduction column
                         # selected above
                         uiOutput("frontier.date"),
                         selectInput('tfdea.inputs', 'Select Input(s):', 'NONE', multiple = TRUE),
                         selectInput('tfdea.outputs', 'Select Output(s):', 'NONE', multiple = TRUE),
                         selectInput('orientation', 'Orientation:', orientation.opts),
                         selectInput('rts', 'Return to Scale:', crs.opts),
                         selectInput('frontier.type', 'Frontier Type:', frontier.type.opts),
                         selectInput('secondary.obj', 'Secondary Objective:', secondary.obj.opts),
                         checkboxInput('segroc', 'Segemented ROC', FALSE),
                         br(),
                         actionButton("btn.tfdeaanalysis","Run TFDEA Analysis")
                ),
                tabPanel("DEA Setup Model", value = "ts.setup.dea_selection",
                         selectInput('dea.inputs', 'Select Input(s):', 'NONE', multiple = TRUE),
                         selectInput('dea.outputs', 'Select Output(s):', 'NONE', multiple = TRUE),
                         selectInput('dea.orientation', 'Orientation:', orientation.opts),
                         selectInput('dea.rts', 'Return to Scale:', crs.opts),
                         br(),
                         actionButton("btn.deaanalysis","Run DEA Analysis")
                )
    )
  ),
  
  # Main panel containing the tabs for the results
  mainPanel(
    # Results tabs for displaying the outcome of the analysis
    tabsetPanel(id="ts.result",
                tabPanel("Data Table", value="ts.result.data",
                         h5("DATA TABLE"),
                         dataTableOutput('dt.data')
                ),
                tabPanel("Result Plot", value="ts.result.plot",
                         h5("RESULT PLOT"),
                         dataTableOutput("dt.summary"),
                         div(class='row-fluid',
                             div(class="span3", checkboxInput('plot.label.b', 'Display labels for each DMU', TRUE)),
                             div(class="span3", checkboxInput('plot.lr.b', 'Include LR Results', TRUE))
                         ),
                         helpText("Notes: (1) Hover over DMUs to see details, (2) Download using button at top right of plot."),
#                       jqplotOutput("plot.result")
                        ggvisOutput("ggvis")
                ),
                tabPanel("Result TFDEA", value="ts.result.tfdea",
                         h5("TFDEA RESULTS"),
                         uiOutput("btn.tfdea"),
                         br(),
                         h6("SUMMARY"),
                         dataTableOutput('dt.tfdea.summary'),
                         h6("FORECAST RESULTS"),
                         dataTableOutput('dt.tfdea.forecast')
                ),
                tabPanel("Result DEA", value="ts.result.dea",
                         h5("DEA RESULTS"),
                         uiOutput("btn.dea"),
                         br(),
                         h6("EFFICIENCY"),
                         dataTableOutput('dt.dea.eff'),
                         h6("LAMBA VALUES"),
                         dataTableOutput('dt.dea.lambda'),
                         h6("OBJECTIVE VALUES"),
                         dataTableOutput('dt.dea.objval'),
                         h6("RETURN TO SCALE"),
                         dataTableOutput('dt.dea.RTS'),
                         h6("ORIENTATION"),
                         dataTableOutput('dt.dea.ORIENTATION'),
                         h6("TRANSPOSE"),
                         dataTableOutput('dt.dea.TRANSPOSE')
                ),
                tabPanel("Result LR", value="ts.result.lr",
                         h5("LINEAR REGRESSION RESULTS"),
                         uiOutput("btn.lr"),
                         br(),
                         h6("SUMMARY"),
                         dataTableOutput('dt.lr.summary'),
                         h6("COEFFICIENTS"),
                         dataTableOutput('dt.lr.coeff'),
                         h6("MULTI-COLLINEARITY"),
                         dataTableOutput('dt.lr.mc'),
                         h6("FORECAST RESULTS"),
                         dataTableOutput('dt.lr.forecast')
                )
    )
  )
))