#' launches the GDE app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
# @example \dontrun {launchApp()}
#'
#' @import shiny
#'

#Based on https://github.com/MangoTheCat/shinyAppDemo/

# wrapper for shiny::shinyApp()
launchApp <- function() {
  
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}