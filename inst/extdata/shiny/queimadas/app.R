library(shiny)
library(bslib)

ui <- bslib::page_fluid(
  bslib::layout_sidebar(
    title = "Title panel",
    sidebar = bslib::sidebar("Sidebar"),
    bslib::card(
      card_header("Card header"),
      "Card body"
    )
  )
)

server <- function(input, output) {}

shiny::shinyApp(ui = ui, server = server)
