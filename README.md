# GBIF-Dataset-Explorer (gde)

R Package to explore datasets from the biodivesity aggregator GBIF.

Once a DwC dataset is loaded, you can explore the issues and data values using a Shiny UI. 

![Screenshot of the Shiny app](inst/gde-screenshot-thumb.png?raw=true "Screenshot of the Shiny app")

## Load data from a DwC download

1. Install the package from CRAN: `install.packages("gde")`
1. Download a DarwinCore zip file from GBIF
1. Run the function `gde::load_gbif_dwc(zipfile = "000000.zip", tmpdir = "tmp")`, editing the arguments as needed

This step might take a while, depending on the size of the download and the specs of your computer.

## Run the app locally

After loading the data to the database, run the Shiny app using:

1. Run using: `gde::launchApp()`

