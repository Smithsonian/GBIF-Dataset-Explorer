#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' 
#' @import DBI
#' @import leaflet
#' @import ggplot2
#' @import shiny
#' @importFrom rgbif gbif_issues
#' @importFrom odbc odbc
#' @importFrom dplyr filter
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite prettify
#' @importFrom utils write.csv
#' @importFrom utils packageVersion
#'


# Define server logic of the app
# Based on https://github.com/MangoTheCat/shinyAppDemo/
shinyAppServer <- function(input, output, session) {

  #Import settings----
  source("gde_settings.R")
  
  output$app_title <- renderText({app_name})
  
  if (db_server == "postgresql"){
    if (!exists("pgdriver")){
      gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                               driver = "PostgreSQL",
                               database = pg_database,
                               uid = pg_user,
                               pwd = pg_password,
                               server = pg_host,
                               port = 5432))
      
      if (class(gbif_db) == "try-error"){
        gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                                 driver = "PostgreSQL Unicode(x64)",
                                 database = pg_database,
                                 uid = pg_user,
                                 pwd = pg_password,
                                 server = pg_host,
                                 port = 5432))
      }
      
      if (class(gbif_db) == "try-error"){
        gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                                 driver = "PostgreSQL Unicode",
                                 database = pg_database,
                                 uid = pg_user,
                                 pwd = pg_password,
                                 server = pg_host,
                                 port = 5432))
      }
    }else{
      gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                               driver = pgdriver,
                               database = pg_database,
                               uid = pg_user,
                               pwd = pg_password,
                               server = pg_host,
                               port = 5432))
    }
    
    
    if (class(gbif_db) == "try-error"){
      stop("Could not connect to PostgreSQL.")
    }
  }
  
  #gbif_issues ----
  gbifissues <- rgbif::gbif_issues()
  #issues dealing with coordinates 
  spatial_issues <- c("CONTINENT_COUNTRY_MISMATCH",
                      "CONTINENT_DERIVED_FROM_COORDINATES",
                      "CONTINENT_INVALID",
                      "COORDINATE_INVALID",
                      "COORDINATE_OUT_OF_RANGE",
                      "COORDINATE_PRECISION_INVALID",
                      "COORDINATE_REPROJECTED",
                      "COORDINATE_REPROJECTION_FAILED",
                      "COORDINATE_REPROJECTION_SUSPICIOUS",
                      "COORDINATE_ROUNDED",
                      "COORDINATE_UNCERTAINTY_METERS_INVALID",
                      "COUNTRY_COORDINATE_MISMATCH",
                      "COUNTRY_DERIVED_FROM_COORDINATES",
                      "COUNTRY_INVALID",
                      "GEODETIC_DATUM_ASSUMED_WGS84",
                      "GEODETIC_DATUM_INVALID",
                      "PRESUMED_NEGATED_LATITUDE",
                      "PRESUMED_NEGATED_LONGITUDE",
                      "PRESUMED_SWAPPED_COORDINATE",
                      "ZERO_COORDINATE")
  #issues dealing with depth 
  depth_issues <- c("DEPTH_MIN_MAX_SWAPPED",
                    "DEPTH_NON_NUMERIC",
                    "DEPTH_NOT_METRIC",
                    "DEPTH_UNLIKELY")
  #issues dealing with elevation 
  elev_issues <- c("ELEVATION_MIN_MAX_SWAPPED",
                   "ELEVATION_NON_NUMERIC",
                   "ELEVATION_NOT_METRIC",
                   "ELEVATION_UNLIKELY")
  #issues dealing with dates 
  date_issues <- c("RECORDED_DATE_INVALID",
                   "RECORDED_DATE_MISMATCH",
                   "RECORDED_DATE_UNLIKELY",
                   "IDENTIFIED_DATE_UNLIKELY")
  
  #issues dealing with taxonomy 
  taxo_issues <- c("TAXON_MATCH_FUZZY",
                   "TAXON_MATCH_HIGHERRANK",
                   "TAXON_MATCH_NONE")
  
  
  # Get rows with issues
  distinct_issues <- DBI::dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM bade_gbif_issues")
  distinct_issues <- unlist(distinct_issues, use.names = FALSE)
  
  #distinct_issues pulldown ----
  output$distinct_issues <- renderUI({
      selectInput(inputId = "i",
                label = "Select an issue:", 
                choices = distinct_issues,
                width = 360
      )
  })
  
  
  # Table of records with issue ----
  datarows <- reactive({
    
    req(input$i)
    cat(input$i)
    #Which cols to display by type of issue
    if (input$i %in% spatial_issues){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifid, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifid, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifid, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else if (input$i %in% taxo_issues){
      cols <- "gbifid, scientificName, kingdom, phylum, class, \"order\", family, genus, higherclassification"
    }else if (input$i == "BASIS_OF_RECORD_INVALID"){
      cols <- "gbifid, scientificName, basisOfRecord, recordedBy, locality, country"
    }else if (input$i == "REFERENCES_URI_INVALID"){
      cols <- "gbifid, scientificName, \"references\", recordedBy, locality, country"
    }else if (input$i == "INDIVIDUAL_COUNT_INVALID"){
      cols <- "gbifid, scientificName, individualcount, recordedBy, locality, country"
    }else if (input$i == "TYPE_STATUS_INVALID"){
      cols <- "gbifid, scientificName, typestatus, recordedBy, locality, country"
    }else{
      cols <- "gbifid, scientificName, recordedBy, locality, country"
    }
    
    
    query <- paste0("SELECT ", cols, " FROM bade_gbif_verbatim WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') and gbifid NOT IN (SELECT gbifid FROM bade_gbif_occ WHERE ignorerow = 't') ORDER BY gbifid")
    #print(query)
    datarows <- DBI::dbGetQuery(gbif_db, query)
    
    df <- data.frame(datarows, stringsAsFactors = FALSE)
    
    df
  })
  
  
  output$table_heading <- renderUI({
    req(input$i)
    h3("Rows in the 'verbatim' file with this issue:")
  })
  
  
  output$pagetitle <- renderUI({
    if (app_name != ""){
      app_name_title <- app_name
    }else{
      app_name_title <- "GBIF Dataset Explorer"
      }
  HTML(paste0('<script type="text/javascript">
                           top.document.title=\'', app_name_title, '\'
                           </script>'))
                           })
                           
  
  output$table <- DT::renderDataTable({
    req(input$i)
    DT::datatable(datarows(), 
                  escape = FALSE, 
                  options = list(
                      searching = TRUE, 
                      ordering = TRUE, 
                      pageLength = 10
                      ), 
                  rownames = FALSE, 
                  selection = 'single')
  })
  
  
  
  output$download_doi <- renderUI({
    metadata_string <- DBI::dbGetQuery(gbif_db, paste0("SELECT metadata_json::text FROM bade_gbif_metadata"))
    gbif_metadata <- jsonlite::fromJSON(metadata_string$metadata_json)
    gbif_metadata_json <- jsonlite::prettify(metadata_string$metadata_json)
    
    html_to_print <- paste0("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Occurrence Download Metadata</h3></div><div class=\"panel-body\"><div style = \"overflow-y: auto; overflow-x: auto;\"><dl>")
    
    for (i in 1:length(gbif_metadata)){
      html_to_print <- paste0(html_to_print, "<dt>", names(gbif_metadata[i]), "</dt>")
      if (names(gbif_metadata[i]) == "doi"){
        html_to_print <- paste0(html_to_print, "<dd><a href=\"https://doi.org/", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
      }else if(names(gbif_metadata[i]) == "downloadLink" || names(gbif_metadata[i]) == "license"){
        html_to_print <- paste0(html_to_print, "<dd><a href=\"", gbif_metadata[i], "\" target = _blank>", gbif_metadata[i], "</a></dd>")
      }else if(names(gbif_metadata[i]) == "size"){
        #from https://stackoverflow.com/questions/29787452/how-do-i-quickly-convert-the-size-element-of-file-info-from-bytes-to-kb-mb-g#comment48096201_29787527
        filesize <- structure(as.numeric(gbif_metadata[i]), class="object_size") 
        html_to_print <- paste0(html_to_print, "<dd>", format(filesize, units="auto"), "</dd>")
      }else if(names(gbif_metadata[i]) == "totalRecords" || names(gbif_metadata[i]) == "numberDatasets"){
        html_to_print <- paste0(html_to_print, "<dd>", prettyNum(gbif_metadata[i], big.mark = ",", scientific = FALSE), "</dd>")
      }else{
        html_to_print <- paste0(html_to_print, "<dd>", gbif_metadata[i], "</dd>")
      }
    }
    
    html_to_print <- paste0(html_to_print, "</dl>")
    
    
    html_to_print <- paste0(html_to_print, "<button type=\"button\" class=\"btn btn-info\" data-toggle=\"modal\" data-target=\"#myModal\">Metadata JSON</button>
    <!-- Modal -->
    <div class=\"modal fade\" id=\"myModal\" role=\"dialog\">
      <div class=\"modal-dialog\">
        <!-- Modal content-->
        <div class=\"modal-content\">
          <div class=\"modal-header\">
            <button type=\"button\" class=\"close\" data-dismiss=\"modal\">&times;</button>
              <h4 class=\"modal-title\">Archive Metadata</h4>
                </div>
                <div class=\"modal-body\">
                  ", pre(gbif_metadata_json), "
                  </div>
                <div class=\"modal-footer\">
                  <button type=\"button\" class=\"btn btn-default\" data-dismiss=\"modal\">Close</button>
                </div>
              </div>
          </div>
          </div>")
    
    html_to_print <- paste0(html_to_print, "</div></div></div>")
    
    tagList(
      HTML(html_to_print)#,
      # HTML(paste0("<script type=\"text/javascript\">
      #                      document.getElementsByClassName(\"navbar-brand\")[0].innerHTML = ", app_name, ";
      #               </script>"))
    )
  })
  
  
  
  
  # details of record ----
  output$recorddetail <- renderUI({
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- DBI::dbGetQuery(gbif_db, "SELECT gbifid FROM bade_gbif_occ WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM bade_gbif_issues) and ignorerow = 'f' ORDER BY gbifid")
    }else{
      this_summary_ids <- DBI::dbGetQuery(gbif_db, paste0("SELECT gbifid FROM bade_gbif_occ WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') and ignorerow = 'f' ORDER BY gbifid"))
    }
    
    this_record <- DBI::dbGetQuery(gbif_db, paste0("SELECT * FROM bade_gbif_occ WHERE gbifid = '", this_summary_ids[input$table_rows_selected,], "' ORDER BY gbifid"))
    this_record_dataset <- DBI::dbGetQuery(gbif_db, paste0("SELECT * FROM bade_gbif_datasets WHERE datasetKey in (SELECT datasetKey FROM bade_gbif_occ WHERE gbifid = '", this_record$gbifid, "')"))
    
    gbif_record_url <- paste0("https://www.gbif.org/occurrence/", this_record$gbifid)
    occurrenceID <- this_record$occurrenceid
    
    html_to_print <- paste0("<h4>Record gbifid: ", this_record$gbifid)
    
    html_to_print <- paste0(html_to_print, 
                            actionButton("delrecord", 
                                         label = "Hide record", 
                                         class = "btn btn-danger pull-right",
                                         icon = icon("remove", lib = "glyphicon")),
                            br()
    )
    
    html_to_print <- paste0(html_to_print, "</h4><dl class=\"dl-horizontal\"><dt>GBIF Record</dt><dd><a href=\"", gbif_record_url, "\" target = _blank>", gbif_record_url, "</a></dd>")
    if (!is.na(occurrenceID)){
      html_to_print <- paste0(html_to_print, "<dt>Occurrence ID</dt>")
      #check if it is a URL, just print otherwise
      if (substr(occurrenceID, 0, 4) == "http"){
        html_to_print <- paste0(html_to_print, "<dd><a href=\"", occurrenceID, "\" target = _blank>", occurrenceID, "</a></dd>")
      }else{
        html_to_print <- paste0(html_to_print, "<dd>", occurrenceID, "</dd>")
      }
    }
    
    if (this_record$catalognumber != ""){
      html_to_print <- paste0(html_to_print, "<dt>Catalog No.</dt><dd>", this_record$catalognumber, "</dd>")
    }
    
    if (input$i != "None"){
      html_to_print <- paste0(html_to_print, "<dt>Issues</dt>")
      issue_list <- base::strsplit(this_record$issue, ";")[[1]]
      
      for (i in 1:length(issue_list)){
        this_issue <- dplyr::filter(gbifissues, issue == issue_list[i])
        html_to_print <- paste0(html_to_print, "<dd><abbr title=\"", this_issue$description, "\">", this_issue$issue, "</abbr></dd>")
      }
    }
    
    #Images
    images <- DBI::dbGetQuery(gbif_db, paste0("SELECT * FROM bade_gbif_multimedia WHERE gbifid = '", this_record$gbifid, "' AND type = 'StillImage'"))
    if (dim(images)[1] > 0){
      
      html_to_print <- paste0(html_to_print, "<dt>Images</dt><dd>")
      
      no_images <- dim(images)[1]
      if (no_images > 3){
        no_images <- 3
      }
      
      for (j in 1:no_images){
        image_url <- images$identifier[j]
        image_title <- images$title[j]
        
        html_to_print <- paste0(html_to_print, "<a href=\"", image_url, "\" target = _blank><img src=\"", image_url, "\" width = \"140px\" alt = \"" , image_title, "\" style = \"padding: 5px;\"></a>")
      }
      html_to_print <- paste0(html_to_print, "</dd>")
    }
    
    
    #Dataset
    html_to_print <- paste0(html_to_print, "<dt>Dataset Title</dt><dd><a href=\"https://www.gbif.org/dataset/", this_record_dataset$datasetKey, "\" target = \"_blank\">", this_record_dataset$title, "</a></dd>")
    html_to_print <- paste0(html_to_print, "<dt>Dataset Institution</dt><dd>", this_record_dataset$institution, "</dd>")
    
    html_to_print <- paste0(html_to_print, "</dl>")
    
    verbatim_json <- paste0("http://api.gbif.org/v1/occurrence/", this_record$gbifid, "/verbatim")
    
    html_to_print <- paste0(html_to_print, "<a href=\"", verbatim_json, "\" target = _blank>Verbatim JSON</a><br>")
    
    HTML(html_to_print)
  })
  
  
  # Proxy for DT ----
  proxy = DT::dataTableProxy('table')
  # Delete row ----
  observeEvent(input$delrecord, {
    req(input$table_rows_selected)
    
    if (input$i == "None"){
      this_summary_ids <- DBI::dbGetQuery(gbif_db, "SELECT gbifid FROM bade_gbif_occ WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM bade_gbif_issues) AND ignorerow = 'f'")
    }else{
      this_summary_ids <- DBI::dbGetQuery(gbif_db, paste0("SELECT gbifid FROM bade_gbif_occ WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') AND ignorerow = 'f'"))
    }
    #cat(this_summary_ids)
    this_record <- dbExecute(gbif_db, paste0("UPDATE bade_gbif_occ SET ignorerow = 't' WHERE gbifid = '", this_summary_ids[input$table_rows_selected,], "'"))
    
    if (input$i == "None"){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% spatial_issues){
      cols <- "gbifid, scientificName, decimalLatitude, decimalLongitude, locality, country"
    }else if (input$i %in% depth_issues){
      cols <- "gbifid, scientificName, minimumDepthInMeters, maximumDepthInMeters, verbatimDepth"
    }else if (input$i %in% elev_issues){
      cols <- "gbifid, scientificName, minimumElevationInMeters, maximumElevationInMeters, verbatimElevation"
    }else if (input$i %in% date_issues){
      cols <- "gbifid, scientificName, eventDate, eventTime, startDayOfYear, endDayOfYear,year, month, day"
    }else{
      cols <- "gbifid, scientificName, recordedBy, locality, country"
    }
    
    if (input$i == "None"){
      datarows <- DBI::dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM bade_gbif_verbatim WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM bade_gbif_issues) and gbifid NOT IN (SELECT gbifid FROM bade_gbif_occ WHERE ignorerow = 't')"))
    }else{
      datarows <- DBI::dbGetQuery(gbif_db, paste0("SELECT ", cols, " FROM bade_gbif_verbatim WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') and gbifid NOT IN (SELECT gbifid FROM bade_gbif_occ WHERE ignorerow = 't')"))
    }
    
    datarows <- data.frame(datarows, stringsAsFactors = FALSE)
    DT::replaceData(proxy, datarows, rownames = FALSE)
  })
  
  
  
  # Record map ----
  output$mymap <- renderLeaflet({
    req(input$i)
    
    datarows <- DBI::dbGetQuery(gbif_db, paste0("SELECT gbifid, decimallatitude, decimallongitude FROM bade_gbif_occ WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') and ignorerow = 'f'"))
    points <- datarows[input$table_rows_selected,]
    
    #check if lat and lon exist
    req(points$decimallongitude)
    req(points$decimallatitude)
    
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(lng = as.numeric(points$decimallongitude), lat = as.numeric(points$decimallatitude)) %>%
      setView(as.numeric(points$decimallongitude), as.numeric(points$decimallatitude), zoom = 04)
  })
  
  
  
  # Name of issue ----
  output$issuename <- renderUI({
    req(input$i)
    this_issue <- dplyr::filter(gbifissues, issue == input$i)
    
    tagList(
      HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Definition of Issue</h3></div><div class=\"panel-body\">"),
      p(paste0("Issue: ", this_issue$issue)),
      p(paste0("Description: ", this_issue$description)),
      HTML("</div></div>")
    )
  })
  
  
  # Downloadable csv of selected issue ----
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$i, ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(dbGetQuery(gbif_db, paste0("SELECT * FROM bade_gbif_occ WHERE gbifid IN (SELECT gbifid from bade_gbif_issues WHERE issue = '", input$i, "') and ignorerow = 'f'")), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of rows with no issues ----
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("NO_ISSUES.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(DBI::dbGetQuery(gbif_db, "SELECT * FROM bade_gbif_occ WHERE gbifid NOT IN (SELECT DISTINCT gbifid FROM bade_gbif_issues) and ignorerow = 'f'"), file, row.names = FALSE)
    }
  )
  
  # Download button ----
  output$downloadData <- renderUI({
    req(input$i)
    
    if (input$i != "None"){
      downloadButton("downloadData1", "Download records with this issue", class = "btn-primary")  
    }else{
      downloadButton("downloadData2", "Download records with no issues", class = "btn-primary")  
    }
  })
  
  
  
  
  
  
  # Download Occ file ----
  output$downloadOcc <- downloadHandler(
    filename = function() {
      paste("occurrence.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(dbGetQuery(gbif_db, "SELECT * FROM bade_gbif_occ WHERE ignorerow = 'f'"), file, row.names = FALSE)
    }
  )
  
  output$downloadOccFile <- renderUI({
    req(input$i)
    downloadButton("downloadOcc", "Download Occurrence File", class = "btn-primary")
  })
  
  
  output$downloadOccFileInfo <- renderUI({
    req(input$i)
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Download Occurrence</h3></div><div class=\"panel-body\">
           <p>To download the occurrence file, in csv format, without the hidden rows:"),
      uiOutput("downloadOccFile"),
      HTML("</div></div>")
    )
  })
  
  
  
  
  # Download Verbatim file ----
  output$downloadOcc1 <- downloadHandler(
    filename = function() {
      paste("verbatim.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(DBI::dbGetQuery(gbif_db, "SELECT * FROM bade_gbif_verbatim WHERE gbifid IN (SELECT gbifid FROM bade_gbif_occ WHERE ignorerow = 'f')"), file, row.names = FALSE)
    }
  )
  
  output$downloadOccFile1 <- renderUI({
    req(input$i)
    downloadButton("downloadOcc1", "Download Verbatim File", class = "btn-primary")
  })
  
  
  output$downloadVerFileInfo <- renderUI({
    req(input$i)
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Download Verbatim</h3></div><div class=\"panel-body\">
           <p>To download the verbatim file, in csv format, without the hidden rows:"),
      uiOutput("downloadOccFile1"),
      HTML("</div></div>")
    )
  })
  
  
  
  # Help1 ----
  output$help1 <- renderUI({
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">How to use this tool</h3></div><div class=\"panel-body\">
         <p>Occurrence records in GBIF can be tagged with a number of issues that their system has detected. However, like the 
         <a href=\"https://www.gbif.org/article/5i3CQEZ6DuWiycgMaaakCo/gbif-infrastructure-data-processing\" target = _blank>
         processing information page</a> indicates:</p>
         <pre>Not all issues indicate bad data. Some are merley flagging the fact that GBIF has altered values during processing.</pre>
         <p>This tool allows collection and data managers, as well as researchers, to explore issues in GBIF Darwin Core Archive downloads in an easy web-based interface. Just enter a GBIF download key and the tool will download the zip file, create a local database, and display the issues in the data contained. Once provided with the GBIF key, this tool will:</p>
         <ul>
         <li>Download the zip archive</li>
         <li>Extract the files</li>
         <li>Create a local database</li>
         <li>Load the data from the occurrence, verbatim, multimedia, and dataset tables to the database</li>
         <li>Generate summary statistics of the issues</li>
         </ul>
        <p>As an alternative, you can copy the zip file to the `data` folder and run the `load_from_DwC_zip.R` script. It will run the same steps as above (skipping downloading the file) from the command line. </p>

        <p>Then, you can click the 'Explore Issues' tab to see how many records have been tagged with a particular issue.</p>

        <p>Once you select an issue, a table will display the rows that have been tagged with that issue. If you click on a row, more details of the occurrence record will be shown, including a map using Leaflet (if the record has coordinates). You can choose to delete the row from the local database.</p>

        <p>The 'Explore Data Fields' will show a summary and top data values in all fields of the `occurrence.txt` file (except for the gbifID field).</p>
         </div></div>")
  })
  
  # Help2 ----
  output$help2 <- renderUI({
    HTML("
         <div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">GBIF Download Key</h3></div><div class=\"panel-body\">
        <p>The key is obtained when requesting a download of occurrence data from GBIF. It can be requested via their API or on the website. If your download URL is:</p>
          <pre>https://www.gbif.org/occurrence/download/0001419-180824113759888</pre>
          <p>Then, the last part, '0001419-180824113759888' is the GBIF key you will need to provide this tool.</p>
          <p>This tool works only with Darwin Core Archives, not with CSV archives.</p>
         </div></div>
         
         <div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Deleting Records</h3></div><div class=\"panel-body\">
        <p>You can delete individual records from the local database by clicking on the 'Delete record' button.</p>
        <p>Deleted records will not show up in the table of results or when downloading the Occurrence or Verbatim files. This option can be used to remove records, for example, where:</p>
         <ul>
            <li>The issue is not a real problem and can be ignored</li>
            <li>The issue has been fixed in the collection database</li>
            <li>For researchers, the record can not be used in an analysis</li>
         </ul>
         </div></div>")
  })
  
  
  output$clickdetails <- renderUI({
    req(input$i)
    HTML("<p class=\"pull-right\">Click a record for details</p>")
  })
  
  # summaryTable ----
  output$summaryTable <- DT::renderDataTable({
    
    # Get rows with issues
    distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM bade_gbif_issues")
    distinct_issues <- unlist(distinct_issues, use.names = FALSE)
    
    #How many rows
    total_rows <- dbGetQuery(gbif_db, "SELECT count(*) FROM bade_gbif_verbatim")
    
    #summary 
    summary_vals <- data.frame(matrix(ncol = 4, nrow = 0, data = NA))
    
    for (i in 1:length(distinct_issues)){
      this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*)::integer as cnt FROM bade_gbif_issues WHERE issue = '", distinct_issues[i], "'"))
      
      issue_description <- gbifissues[gbifissues$issue == distinct_issues[i],]$description
      
      if (length(issue_description) == 0){
        issue_description <- "NA"
      }
      
      summary_vals <- rbind(summary_vals, cbind(distinct_issues[i], issue_description, this_issue[1]))
    }
    
    names(summary_vals) <- c("issue", "description", "no_records")

    #Remove underscores from issue names
    summary_vals$issue <- gsub("_", " ", summary_vals$issue)

    names(summary_vals) <- c("Issue", "Description", "No. records")
    
    DT::datatable(summary_vals, 
                  #caption = 'Table 1. Issues in the downloaded dataset and the number of records per issue', 
                  escape = FALSE, 
                  options = list(searching = FALSE, 
                                 ordering = TRUE, 
                                 pageLength = 10, 
                                 paging = TRUE,
                                 order = list(2, 'desc')
                  ), 
                  rownames = FALSE, 
                  selection = 'none') %>% 
      DT::formatCurrency('No. records', currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  
  # summaryPlot2 - Related issues ----
  output$summaryPlot2 <- renderPlot({
    
    issues_summ <- dbGetQuery(gbif_db, "select replace(a.issue, '_', ' ') as issue_a, replace(b.issue, '_', ' ') as issue_b, count(a.gbifid)::integer as no_records from (select gbifid, issue from bade_gbif_issues) a LEFT JOIN (select gbifid, issue from bade_gbif_issues) b ON (a.gbifid = b.gbifid AND a.issue != b.issue) WHERE b.issue IS NOT NULL GROUP BY a.issue, b.issue")
    
    ggplot(data = issues_summ, aes(issue_b, issue_a)) +
      geom_tile(aes(fill = no_records), color = "white") +
      theme(
        axis.title = element_blank(), 
        legend.position="right", 
        axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        #axis.text.y = element_text(size=10, angle = 45),
        axis.text.y = element_text(size=10),
        #axis.text = element_text(size = 10),
        plot.title = element_text(size = 18, face="bold")) +
      scale_x_discrete(limits = levels(issues_summ$issue_a)) + 
      scale_y_discrete(limits = levels(issues_summ$issue_b), position = "right") + 
      scale_fill_gradient(low = "yellow", high = "red") + 
      labs(
        fill = "No. of Records\nwith both\nIssues"#, 
        #title = "Fig. 2. Pairwise image of issues common to the records"
      )
  })
  
  
  
  # summaryPlot3 - Plot records with multiple issues ----
  output$summaryPlot3 <- renderPlot({
    issues_by_rec <- dbGetQuery(gbif_db, "select a.no_issues as no_issues, count(a.gbifid)::integer as no_records, round((count(a.gbifid)::numeric/(b.total_records)::numeric)*100, 2)::float as percent from (select gbifid, count(*)::integer as no_issues from bade_gbif_issues group by gbifid) a, (select count(gbifid)::integer as total_records from bade_gbif_occ) b group by a.no_issues, b.total_records")
    
    issues_by_rec_none <- dbGetQuery(gbif_db, "select 0 as no_issues, count(a.gbifid)::integer as no_records, round((count(a.gbifid)::numeric/(b.total_records)::numeric)*100, 2)::float as percent from (select gbifid from bade_gbif_occ WHERE gbifid NOT IN (select gbifid from bade_gbif_issues)) a, (select count(gbifid)::integer as total_records from bade_gbif_occ) b group by b.total_records")
    if (issues_by_rec_none$no_records > 0){
      #If there are records without issues, add them
      issues_by_rec <- rbind(issues_by_rec, issues_by_rec_none)
    }
    
    ggplot(data = issues_by_rec, aes(x = no_issues, y = no_records, label = percent, colour = no_issues, fill = no_issues)) +
      geom_col() +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      theme(
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        legend.position="none", 
        plot.title = element_text(size = 18, face="bold")
      ) + 
      geom_text(aes(label = paste(percent, "%"), size = 10), position=position_dodge(width = 0.9), vjust = -0.5) +
      labs(
        #title = "Fig. 1. Number of issues by record", 
        subtitle = "Percent is from total number of rows", 
        x = "No. of Issues/Record", 
        y = "No. of Records"
      )
  })
  
  
  
  # fields_table ----
  output$fields_table <- DT::renderDataTable({
    
    fields_summary <- dbGetQuery(gbif_db, "SELECT field_name, not_null_vals, no_rows_distinct FROM bade_gbif_issue_stats ORDER BY field_name")
    fields_summary[1] <- lapply(fields_summary[1], as.character)
    fields_summary[1] <- lapply(fields_summary[1], str_replace_all, pattern = '"', replacement = '')
    
    fields_summary[, 3] <- prettyNum(as.integer(fields_summary[, 3]), big.mark = ",", scientific = FALSE)
    
    names(fields_summary) <- c("Field", "Rows Not Empty and Not Null", "Distinct Values")
    
    session$userData$fields_summary <- fields_summary
    
    DT::datatable(fields_summary, 
                  escape = FALSE, 
                  options = list(searching = TRUE, ordering = TRUE, pageLength = 25), 
                  rownames = FALSE, 
                  selection = 'single')
  })
  
  
  
  # fields_table ----
  output$fields_details <- DT::renderDataTable({
    # details of record ----
    output$fields_details_h <- renderUI({
      h4("Select a field from the table on the left to see the data values.")
    })
    
    req(input$fields_table_rows_selected)
    
    #Get field
    fields_summary <- session$userData$fields_summary
    field_to_check <- as.character(fields_summary[input$fields_table_rows_selected, 1])
    
    output$fields_details_h <- renderUI({
      h4(paste0("Unique data values in data field: '", str_replace_all(field_to_check, '"', ''), "'."))
    })
    
    #no rows
    no_rows <- dbGetQuery(gbif_db, "SELECT COUNT(*) FROM bade_gbif_occ")
    
    # Get rows from field
    if (field_to_check %in% c("decimallatitude", "decimallongitude")){
      f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' WHEN char_length(split_part(", field_to_check, ", '.', 2)) > 6 THEN ", field_to_check, " || ' [SEE PRECISION NOTE BELOW]' ELSE CAST(", field_to_check, " AS text) END , count(*) as no_records FROM bade_gbif_occ GROUP BY ", field_to_check, " ORDER BY no_records DESC LIMIT 500")
    }else if (field_to_check == "issue"){
      f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' ELSE REPLACE(", field_to_check, ", ';', '; ') END , count(*) as no_records FROM bade_gbif_occ GROUP BY ", field_to_check, " ORDER BY no_records DESC LIMIT 500")
    }else{
      f_query <- paste0("SELECT CASE WHEN ", field_to_check, " IS NULL THEN '[NULL]' WHEN ", field_to_check, " = '' THEN '[EMPTY]' ELSE CAST(", field_to_check, " as TEXT) END , count(*) as no_records FROM bade_gbif_occ GROUP BY ", field_to_check, " ORDER BY no_records DESC LIMIT 500")
    }
    
    field_data <- dbGetQuery(gbif_db, f_query)
    
    for (i in seq(1, dim(field_data)[1])){
      field_data[i, 3] <- round((as.integer(field_data[i, 2]) / as.integer(no_rows$count)) * 100, 2)
    }
    
    names(field_data) <- c("Field Value", "No. of rows", "Percent of records")
    
    field_data$"No. of rows" <- prettyNum(as.integer(field_data$"No. of rows"), big.mark = ",", scientific = FALSE)
    
    DT::datatable(field_data,
                  escape = FALSE,
                  options = list(searching = TRUE, ordering = TRUE, pageLength = 25),
                  rownames = FALSE,
                  selection = 'none')
  })
  
  
  
  # details of record ----
  output$fields_details_h <- renderUI({
    h4("Select a field from the table on the left to see the data values.")
  })
  
  
  #Explore_fields----
  output$explore_fields <- renderUI({
    h4("Summary of each field on the occurrence.txt file")
  })
  
  
  # precision_note ----
  output$precision_note <- renderUI({
    
    req(input$fields_table_rows_selected)
    
    html_to_print <- "<br><br><p><strong>Note</strong>: Showing the top 500 results.</p>"
    
    #Get field
    fields_summary <- session$userData$fields_summary
    
    field_to_check <- as.character(fields_summary[input$fields_table_rows_selected, 1])
    
    if (field_to_check %in% c("decimallatitude", "decimallongitude")){
      html_to_print <- paste(html_to_print, "<br><p><strong>Precision Note</strong>: The precision in lat and lon values with more than 5 decimal places is usually excessive. Values with 6 decimal places measure differences of less than a meter. If the values were converted from another datum, they have to be rounded to the appropriate number of decimal places.")
    }
    
    HTML(html_to_print)
    
  })
  
  # footer ----
  output$footer <- renderUI({
    HTML(paste0("<br><br><br><div class=\"footer navbar-fixed-bottom\" style=\"background: #FFFFFF;\"><br><p>&nbsp;&nbsp;<a href=\"https://dpo.si.edu\" target = _blank>Digitization Program Office, OCIO</a>, Smithsonian | <a href=\"https://github.com/Smithsonian/GBIF-Dataset-Explorer\" target = _blank>gde</a> version ", packageVersion("gde"), "</p></div>"))
  })
}
