library(shiny)

clicksUI <- function(id) {
  ns <- shiny::NS(id)
  div(id = "module_content",
    style = "background-color: #c9d8f0; width: 200px; padding: 5px",
    actionButton(ns('local_counter'), "I'm inside the module"),
    textOutput(ns("local_clicks"))
  )
}

clicksModule <- function(input, output, session, local_clicks) {

  session$userData$clicks_observer <- observeEvent(input$local_counter, {
    print(paste("Clicked", input$local_counter))
    local_clicks(input$local_counter)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$local_clicks <- renderText({
    ns <- session$ns
    paste("Clicks (local view):", input$local_counter)
  })
}

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  div(
    style = "background-color: #ffebf3; width: 200px; padding: 5px",
    actionButton('add_module', '', icon = icon('plus-circle')),
    actionButton('remove_module', '', icon = icon('trash'), class = "disabled"),
    textOutput("local_clicks_out") 
  ),
  tags$div(
    id = "container"
  )
)
server <- function(input, output, session) {
  
  local_clicks <- reactiveVal(NULL)
  
  output$local_clicks_out <- renderText({
    clicks <- 0
    module_clicks <- local_clicks()
    if (!is.null(module_clicks)) {
      clicks <- module_clicks
    }
    paste("Clicks (global view):", clicks)
  })
  
  observeEvent(input$add_module, {
    insertUI(
      selector = '#container',
      where = "beforeEnd",
      ui = clicksUI("my_module")
    )
    
    shinyjs::disable("add_module")
    shinyjs::enable("remove_module")
    callModule(clicksModule, "my_module", local_clicks)
  })
  
  observeEvent(input$remove_module, {
    removeUI(selector = "#module_content")
    shinyjs::disable("remove_module")
    shinyjs::enable("add_module")
    remove_shiny_inputs("my_module", input)
    local_clicks(input[["my_module-local_counter"]])
    session$userData$clicks_observer$destroy()
  })

}

shinyApp(ui = ui, server = server)


