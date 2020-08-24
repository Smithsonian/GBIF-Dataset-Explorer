#' Open GBIF download from zip file
#' 
#' Function that loads the data in a zip file downloaded from GBIF
#'
#' @param zipfile Path to the zipfile
#' @param tmpdir Temporary path for extracted files
#' @param pgdriver ODBC driver for the Postgres database. If not set, the function will try some options.
#'
#' @export
#' @import shiny
#' @import dplyr
#' @import readr
#' @import odbc
#' @import DBI
#' @importFrom jsonlite fromJSON
#' @importFrom stringr fixed
#' @importFrom jsonlite prettify
#' @importFrom stringr str_replace str_replace_all
#' @importFrom XML xmlToList
#' @importFrom progress progress_bar
#' @importFrom R.utils countLines
#' @importFrom R.utils isDirectory
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom data.table fread
#' 
#' @examples
#' \dontrun{
#' data_loaded <- load_gbif_dwc(zipfile = "####.zip", tmpdir = "/tmp")
#' }


load_gbif_dwc <- function(zipfile = NA, tmpdir = NA, pgdriver = NA){
  
  if (is.na(zipfile) || file.exists(zipfile) == FALSE){
    stop("zipfile was not set")
  }
  
  if (file.exists(zipfile) == FALSE){
    stop("zipfile not found")
  }
  
  if (file.exists("gde_settings.R") == FALSE){
    cat(" ERROR: Could not find the file with the settings. Save \n a file \"gde_settings.R\" in the working directory with \nthis information:
            
##########
  # Database
  #  options are 'postgresql' (recommended) or 'sqlite'
  db_server <- \"postgresql\"
    
  #SQLite options
  database_file <- \"biodiversitydata.sqlite3\"
    
  #PostgreSQL options
  pg_host <- \"\"
  pg_user <- \"\"
  pg_database <- \"\"
  pg_password <- \"\"
  
  app_name <- \"\"
##########")
    stop("settings_file not found")
  }
  
  if (is.na(tmpdir)){
    stop("tmpdir not set")
  }
  
  if (isDirectory(tmpdir) == FALSE){
    stop("tmpdir does not exists")
  }
  
  #Import settings----
  source("gde_settings.R")
  
  if (db_server == "postgresql"){
    if (is.na(pgdriver)){
      gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                               driver = "PostgreSQL",
                               database = pg_database,
                               uid = pg_user,
                               pwd = pg_password,
                               server = pg_host,
                               port = 5432), silent = TRUE)
      
      if (class(gbif_db) == "try-error"){
        gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                                 driver = "PostgreSQL Unicode(x64)",
                                 database = pg_database,
                                 uid = pg_user,
                                 pwd = pg_password,
                                 server = pg_host,
                                 port = 5432), silent = TRUE)
      }
      
      if (class(gbif_db) == "try-error"){
        gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                                 driver = "PostgreSQL Unicode",
                                 database = pg_database,
                                 uid = pg_user,
                                 pwd = pg_password,
                                 server = pg_host,
                                 port = 5432), silent = TRUE)
      }
    }else{
      gbif_db <- try(DBI::dbConnect(odbc::odbc(),
                               driver = "PostgreSQL Unicode",
                               database = pg_database,
                               uid = pg_user,
                               pwd = pg_password,
                               server = pg_host,
                               port = 5432), silent = TRUE)
    }
    
    
    if (class(gbif_db) == "try-error"){
      stop("Could not connect to PostgreSQL.")
    }else{
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_occ;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_verbatim;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_multimedia;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_issues;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_datasets;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_issue_stats;")
      n <- DBI::dbExecute(gbif_db, "DROP TABLE IF EXISTS bade_gbif_metadata;")
    }
  }else if (db_server == "sqlite"){
    # if (file.exists(database_file)){
    #   try(unlink(database_file), silent = TRUE)
    # }
    # gbif_db <- DBI::dbConnect(RSQLite::SQLite(), database_file)
    stop("SQLite not yet implemented.")
  }else{
    stop("Incorrect db_server setting.")
  }
  
  occ_file <- paste0(tmpdir, "/occurrence.txt")
  ver_file <- paste0(tmpdir, "/verbatim.txt")
  multi_file <- paste0(tmpdir, "/multimedia.txt")
  dataset_xml_path <- paste0(tmpdir, "/dataset/")
  metadata_path <- paste0(tmpdir, "/metadata.xml")

  
  #Extract file in the command line
  cat("\n Extracting files from zip...\n  Please wait...\n")
  system2("unzip", args = c("-u", "-o", "-d", tmpdir, zipfile))
  
  
  #Occurrence table----
  occ_cols <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  occ_row1 <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1, skip = 10)
  gbif_cols_q <- paste0(occ_cols, collapse = " text, ")
  gbif_cols_q <- tolower(stringr::str_replace(gbif_cols_q, "gbifID text", "gbifid TEXT PRIMARY KEY"))
  gbif_cols_q <- paste0(gbif_cols_q, " text, ignorerow BOOLEAN DEFAULT 'f'")
  #Replace sql keywords
  gbif_cols_q <- stringr::str_replace(gbif_cols_q, ", group", ", \"group\"")
  gbif_cols_q <- stringr::str_replace(gbif_cols_q, "island\"group\"", "islandgroup")
  gbif_cols_q <- stringr::str_replace(gbif_cols_q, "order", "\"order\"")
  gbif_cols_q <- stringr::str_replace(gbif_cols_q, "references", "\"references\"")
  
  n <- DBI::dbExecute(gbif_db, paste0("CREATE TABLE bade_gbif_occ(", gbif_cols_q, ")"))
  
  #Verbatim table ----
  ver_cols <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  ver_row1 <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1, skip = 10)
  
  ver_cols_q <- paste0(ver_cols, collapse = " text , ")
  ver_cols_q <- tolower(stringr::str_replace(ver_cols_q, "gbifID text", "gbifid TEXT PRIMARY KEY"))
  #Replace sql keywords
  ver_cols_q <- stringr::str_replace(ver_cols_q, "references", "\"references\"")
  ver_cols_q <- stringr::str_replace(ver_cols_q, ", group", ", \"group\"")
  ver_cols_q <- stringr::str_replace(ver_cols_q, "island\"group\"", "islandgroup")
  ver_cols_q <- stringr::str_replace(ver_cols_q, "order", "\"order\"")
  
  n <- DBI::dbExecute(gbif_db, paste0("CREATE TABLE bade_gbif_verbatim(", ver_cols_q, " text)"))
  
  
  #multimedia table ----
  mm_cols <- data.table::fread(input = multi_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1)
  mm_row1 <- data.table::fread(input = multi_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = 1, skip = 10)
  
  mm_cols_q <- paste0(mm_cols, collapse = " text, ")
  mm_cols_q <- tolower(stringr::str_replace(mm_cols_q, "gbifID text", "gbifid TEXT PRIMARY KEY"))
  #Replace sql keywords
  mm_cols_q <- stringr::str_replace(mm_cols_q, "references", "\"references\"")
  
  n <- DBI::dbExecute(gbif_db, paste0("CREATE TABLE bade_gbif_multimedia(", mm_cols_q, " text)"))
  
  
  #Datasets table ----
  datasets_xml <- list.files(dataset_xml_path, pattern = "*.xml", full.names = TRUE)
  no_datasets <- length(datasets_xml)
  
  n <- DBI::dbExecute(gbif_db, 'CREATE TABLE bade_gbif_datasets(datasetKey text PRIMARY KEY, title text, institution text);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX ds_datasetKey ON bade_gbif_datasets(datasetKey);')
  
  pb <- progress::progress_bar$new(
    format = " Loading datasets [:bar] :percent in :elapsed",
    total = no_datasets, clear = FALSE, width= 100)
  
  for (i in 1:no_datasets){
    pb$tick()
    meta_file <- XML::xmlToList(datasets_xml[i])
    datasetKey <- stringr::str_replace(basename(datasets_xml[i]), ".xml", "")
    datasetTitle <- stringr::str_replace_all(meta_file$dataset$title, "'", "''")
    datasetInst <- stringr::str_replace_all(meta_file$dataset$creator$organizationName, "'", "''")
    insert_query <- paste0("INSERT INTO bade_gbif_datasets (datasetKey, title, institution) VALUES ('", datasetKey, "', '", datasetTitle, "', '", datasetInst, "');")
    n <- DBI::dbExecute(gbif_db, insert_query)
  }
  
  
  #how big?
  no_lines <- R.utils::countLines(occ_file)[1]
  
  #how many steps?
  no_rows <- 20000
  no_steps <- floor(no_lines/no_rows)
  
  if (no_steps ==0){
    no_steps <- 1
  }
  #occ_cols
  
  #Progress bar
  pb <- progress::progress_bar$new(
    format = " Loading occurrence and verbatim tables [:bar] :percent eta: :eta",
    total = no_steps, clear = FALSE, width= 100)
  
  for (i in 1:no_steps){
    pb$tick()
    #print(paste0("Loading data... (", i, " of ", no_steps, " steps)"))
    
    if (i == 1){
      gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1, colClasses = "character")
      verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1, colClasses = "character")
    }else{
      skip_rows <- i * no_rows
      gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows, colClasses = "character")
      verbatim_data <- data.table::fread(input = ver_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows, colClasses = "character")
    }
    
    
    while (dim(gbif_data)[2] != dim(occ_cols)[2]){
      gbif_data <- cbind(gbif_data, NA)
    }
    
    names(gbif_data) <- tolower(unlist(occ_cols))
    
    names(verbatim_data) <- tolower(unlist(ver_cols))
    
    #write rows
    DBI::dbWriteTable(gbif_db, "bade_gbif_occ", gbif_data, append = TRUE)
    DBI::dbWriteTable(gbif_db, "bade_gbif_verbatim", verbatim_data, append = TRUE)
  }
  
  rm(gbif_data)
  rm(verbatim_data)
  
  #Indices
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX gbifID ON bade_gbif_occ(gbifID);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX issue ON bade_gbif_occ(issue);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX gb_datasetKey ON bade_gbif_occ(datasetID);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX basisOfRecord ON bade_gbif_occ(basisOfRecord);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX scientificName ON bade_gbif_occ(scientificName);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX ignorerow ON bade_gbif_occ(ignorerow);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX verbatim_gbifID ON bade_gbif_verbatim(gbifID);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX multimedia_gbifID ON bade_gbif_multimedia(gbifID);')
  
  
  cat("\n Cataloging issues...")
  
  n <- DBI::dbGetQuery(gbif_db, "CREATE TABLE bade_gbif_issues(id serial PRIMARY KEY, gbifID text, issue text);")
  issue_list <- DBI::dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM bade_gbif_occ WHERE issue != ''")
  
  issues_list <- data.frame(matrix(ncol = 1, nrow = 0, data = NA))
  for (i in 1:dim(issue_list)[1]){
    a <- strsplit(issue_list[i,1], ";")
    for (j in 1:length(a[[1]])){
      issues_list <- c(issues_list, a[[1]][j])
    }
  }
  
  distinct_issues <- unique(unlist(issues_list))
  
  pb <- progress::progress_bar$new(
    format = " Generating list of issues... [:bar] :percent eta: :eta",
    total = length(distinct_issues), clear = FALSE, width = 100)
  
  for (i in 1:length(distinct_issues)) {
    pb$tick()
    DBI::dbExecute(gbif_db, paste0("INSERT INTO bade_gbif_issues (gbifID, issue) SELECT gbifid, '", distinct_issues[i], "' FROM bade_gbif_occ WHERE issue LIKE '%", distinct_issues[i], "%'"))
  }
  
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX issue_issue ON bade_gbif_issues(issue);')
  n <- DBI::dbExecute(gbif_db, 'CREATE INDEX gbifID_issue ON bade_gbif_issues(gbifid);')
  
  fields <- c(tolower(unlist(occ_cols)))
  
  #fields <- fields[-which(fields == "gbifid")]
  
  pb <- progress::progress_bar$new(
    format = " Generating field indices... [:bar] :percent eta: :eta",
    total = length(fields), clear = FALSE, width = 100)
  
  #Columns to skip
  skip_cols <- c("occurrenceremarks", "gbifid")
  
  for (f in seq(1, length(fields))){
    pb$tick()
    
    if (fields[f] %in% skip_cols){
      next
    }
    
    this_field <- stringr::str_replace(fields[f], fixed("group"), "\"group\"")
    this_field <- stringr::str_replace(this_field, "island\"group\"", "islandgroup")
    this_field <- stringr::str_replace(this_field, "order", "\"order\"")
    this_field <- stringr::str_replace(this_field, fixed("references"), "\"references\"")
    this_field <- stringr::str_replace(this_field, fixed("associated\"references\""), "associatedreferences")
    this_field <- stringr::str_replace(this_field, fixed("geo\"references\"ources"), "georeferencesources")
    this_field <- stringr::str_replace(this_field, fixed("identification\"references\""), "identificationreferences")
    this_field <- stringr::str_replace(this_field, fixed("\"order\"key"), "orderkey")
    
    n <- DBI::dbSendQuery(gbif_db, paste0("CREATE INDEX IF NOT EXISTS bade_gbif_occ_", fields[f], "_idx ON bade_gbif_occ(", this_field, ")"))
    dbClearResult(n)
  }
  
  
  
  #Field statistics----
  n <- DBI::dbSendQuery(gbif_db, "CREATE TABLE bade_gbif_issue_stats (field_name text, not_null_vals text, no_rows_distinct text)")
  dbClearResult(n)
  
  fields <- DBI::dbGetQuery(gbif_db, "SELECT COLUMN_NAME as name FROM information_schema.COLUMNS WHERE TABLE_NAME = 'bade_gbif_occ';")
  
  fields <- dplyr::filter(fields, name != 'ignorerow')
  fields <- dplyr::filter(fields, name != 'gbifid')
  
  no_rows_total <- DBI::dbGetQuery(gbif_db, "SELECT count(*)::integer from bade_gbif_occ")
  
  pb <- progress::progress_bar$new(
    format = " Calculating field statistics... [:bar] :percent eta: :eta",
    total = dim(fields)[1], clear = FALSE, width = 100)
  
  for (f in seq(1, dim(fields)[1])){
    pb$tick()
    this_field <- fields$name[f]
    
    #Replace sql keywords
    if (this_field %in% c("group", "order", "references")){
      this_field <- paste0('"', this_field, '"')
    }
    
    no_rows_null_q <- paste0("SELECT count(*)::integer as no_rows from bade_gbif_occ WHERE ", this_field, " IS NULL OR ", this_field, " = ''")
    no_rows_null <- DBI::dbGetQuery(gbif_db, no_rows_null_q)

    no_rows_notnull_pc1 <- round(((no_rows_total - no_rows_null)/no_rows_total) * 100, 2)
    no_rows_notnull_pc <- paste0(no_rows_notnull_pc1, " %")
    
    not_null <- paste0("<div class=\"progress\" style=\"background-color: #ffc107;\"><div class=\"progress-bar bg-success\" role=\"progressbar\" style=\"width: ", no_rows_notnull_pc1, "%; background-color: #28a745;\" aria-valuenow=\"", no_rows_notnull_pc1, "\" aria-valuemin=\"0\" aria-valuemax=\"100\" title=\"", no_rows_notnull_pc1, "\">", no_rows_notnull_pc1, "%</div></div>")
    
    no_rows_distinct <- DBI::dbGetQuery(gbif_db, paste0("SELECT count(DISTINCT ", this_field, ")::integer as distinct_vals from bade_gbif_occ"))
    
    n <- DBI::dbSendQuery(gbif_db, paste0("INSERT INTO bade_gbif_issue_stats (field_name, not_null_vals, no_rows_distinct) VALUES ('", this_field, "', '", not_null, "', '", no_rows_distinct, "')"))
    dbClearResult(n)
  }
  

  #Download metadata
  dl_meta_file <- XML::xmlToList(metadata_path)

  this_doi <- dl_meta_file$additionalMetadata$metadata$`gbif`$citation$.attrs
  gbif_key <- dl_meta_file$dataset$alternateIdentifier
  metadata_json <- paste0("http://api.gbif.org/v1/occurrence/download/", gbif_key)
  gbif_metadata <- httr::content(httr::GET(metadata_json), as="text", encoding = "UTF-8")
  
  n <- DBI::dbSendQuery(gbif_db, paste0("CREATE TABLE bade_gbif_metadata AS (SELECT '", gbif_metadata, "'::json AS metadata_json)"))
  dbClearResult(n)
  
  DBI::dbDisconnect(gbif_db)
  
  #delete files
  system2("rm", args = c(paste0(tmpdir, "/*.txt")))
  system2("rm", args = c(paste0(tmpdir, "/*.xml")))
  system2("rm", args = c("-r", paste0(tmpdir, "/dataset")))
  
  cat("\n Done! The database is ready.\n\n")
}