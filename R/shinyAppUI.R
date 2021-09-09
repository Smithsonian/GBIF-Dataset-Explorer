#' shinyAppServer
#'
#' @importFrom graphics hist
#' @importFrom shinycssloaders withSpinner
#' @import shiny
#' @import leaflet
#' @import shinyWidgets
#

# create the shiny application user interface
# UI ----
shinyAppUI <- fluidPage(
  navbarPage(title = textOutput("app_title"),
             # Tab:Summary ----
             tabPanel("Summary", 
                      fluidRow(
                        column(width = 6,
                               br(),
                               uiOutput("pagetitle"),
                               h4("Fig. 1. Number of issues by record"),
                               shinycssloaders::withSpinner(plotOutput("summaryPlot3", height = 600)),
                               br(),
                               h4("Fig. 2. Pairwise image of issues common to the records"),
                               shinycssloaders::withSpinner(plotOutput("summaryPlot2", height = 600))
                        ),
                        column(width = 6,
                               br(),
                               h4("Table 1. Issues in the downloaded dataset and the number of records per issue"),
                               shinycssloaders::withSpinner(DT::dataTableOutput("summaryTable")),
                               br(),
                               hr(),
                               br(),
                               shinycssloaders::withSpinner(uiOutput("download_doi"))
                        )
                      )
             ),
             
             # Tab:Explore ----
             tabPanel("Explore Issues", 
                      fluidRow(
                        column(width = 4,
                               br(),
                               uiOutput("distinct_issues"),
                               uiOutput("downloadData"),
                               
                               HTML('<script type="text/javascript">
                           $(document).ready(function() {
                             $("#downloadData").click(function() {
                               $("#downloadData2").text("Loading data, please wait...").attr(\'disabled\',\'disabled\');
                               });
                             });
                           </script>
                           ')
                        ),
                        column(width = 4,
                               br(),
                               uiOutput("downloadOccFileInfo")
                        ),
                        column(width = 4,
                               br(),
                               uiOutput("downloadVerFileInfo")
                        )
                      ),
                      hr(), 
                      fluidRow(column(width=7,
                                      fluidRow(
                                        column(width=8,         
                                               uiOutput("issuename")
                                               # HTML("<dl><dt>"),
                                               # uiOutput("issuename"),
                                               # HTML("</dt><dd>"),
                                               # textOutput("issuedescript"),
                                               # HTML("</dd></dl>")
                                        ),
                                        column(width=4,
                                               uiOutput("clickdetails")
                                        )
                                      ),
                                      uiOutput("table_heading"),
                                      shinycssloaders::withSpinner(DT::dataTableOutput("table"))
                      ),
                      column(width=5, 
                             conditionalPanel("input.table_rows_selected != null && input.table_rows_selected != ''",
                                              shinyWidgets::panel(
                                                heading = "Record detail",
                                                status = "primary",
                                                uiOutput("recorddetail"),
                                                leaflet::leafletOutput("mymap")
                                              )
                             )
                      )
                      )
             ),
             # Tab:DataFields ----
             tabPanel("Explore Data Fields", 
                      br(),
                      fluidRow(
                        column(width = 6, 
                               uiOutput("explore_fields"),
                               shinycssloaders::withSpinner(DT::dataTableOutput("fields_table"))
                        ),
                        column(width = 6, 
                               shinycssloaders::withSpinner(uiOutput("fields_details_h")),
                               DT::dataTableOutput("fields_details"),
                               uiOutput("precision_note")
                        )
                      )
                      
             ),
             # Tab:Help ----
             tabPanel("Help", 
                      br(),
                      fluidRow(
                        column(width = 6, 
                               uiOutput("help1")
                        ),
                        column(width = 6, 
                               uiOutput("help2")
                        )
                      )
             )
  ),
  hr(),
  #footer ----
  uiOutput("footer")
)
