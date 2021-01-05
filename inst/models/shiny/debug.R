library(shiny)

module_factory <- function(data_f = function(x) mtcars[, x],
                           model_f = function(x) mean(x),
                           output_shiny = shiny::plotOutput,
                           render_shiny = shiny::renderPlot,
                           render_shiny_input = function(x) plot(x),
                           ...){
  
  ui <- function(id, ...){
    ns <- NS(id)
    output_shiny(ns("ui_out"), ...)
  }
  
  server <- function(input, output, session, view_id){
    
    gadata <- shiny::reactive({
      
      data_f(view_id(), ...)
      
      })
    
    model <- shiny::reactive({
      shiny::validate(shiny::need(gadata(),
                                  message = "Waiting for data"))
      model_f(gadata(), ...)
    })
    
    output$ui_out <- render_shiny({
      shiny::validate(shiny::need(model(),
                                  message = "Waiting for model output"))
      render_shiny_input(gadata())
    })
    
    return(model)
  }
  
  list(
    module = list(
      ui = ui,
      server = server
    )
  )
}

made_module <- module_factory()

## ui.R
ui <- fluidPage(title = "module bug Shiny Demo",
                
                h1("Debugging"),
                selectInput("select", label = "Select", choices = c("mpg","cyl","disp")),
                textOutput("view_id"),
                made_module$module$ui("factory1"),
                br()
)

## server.R
server <- function(input, output, session){
  
  callModule(made_module$module$server, "factory1", view_id = reactive(input$select))
  output$view_id <- renderText(paste("Selected: ", input$select))
  
}

# run the app
shinyApp(ui, server)