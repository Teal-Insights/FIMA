
# start: ------------------------------------------------------------------
# loading necessary Rscripts
source(file = "./ui.R")
source(file = "./server.R")

# run the app
shiny::shinyApp(ui = ui, server = server)

# end: --------------------------------------------------------------------
