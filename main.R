source("data_gather.R")
source("./ui.R")
source("./server.R")


shinyApp(ui = ui, server=server)
