# GBIF-Dataset-Explorer (gde)

R Package to explore datasets from the biodivesity aggregator GBIF.

Once a DwC dataset is loaded, you can explore the issues and data values using a Shiny UI. 

## Load data from a DwC download

1. Install the package: 
   * Install devtools, if needed: `install.packages("devtools")`
   * Install from Github: `devtools::install_github('Smithsonian/GBIF-Dataset-Explorer')`
1. Download a DarwinCore zip from GBIF
1. Setup a database, currently only for Postgres
1. Copy the file `inst/shinyApp/gde_settings.R.template` to `gde_settings.R` in the working directory and edit the values
1. Run the function `gde::load_gbif_dwc(zipfile = "000000.zip", tmpdir = "/tmp/")`, editing the values as needed

## Run the app locally

After loading the data to the database:

1. Copy the file `inst/shinyApp/gde_settings.R.template` to `gde_settings.R` in the working directory and edit the values.
1. Run using: `gde::launchApp()`


## Serve the shiny app on a server

1. Install the package in the server: devtools::install_github('Smithsonian/GBIF-Dataset-Explorer')
1. Copy the files in `inst/shinyApp` to the server. 
1. Rename the file `gde_settings.R.template` to `gde_settings.R` and edit the values.
