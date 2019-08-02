#' launches the importsGui app
#' 
#' @export importsGui
#' 
#' @return shiny application object
#' 
#' @example \dontrun {importsGui()}
#' 
#' @import shiny

# wrapper for shiny::shinyApp()
importsGui <- function() {
  shinyApp(ui = ui, server = server)
}