library(shiny)
library(shinyWidgets)
library(shinybusy)

shinyApp(
    ui <- fluidPage(
        setBackgroundColor(
            color = c("#83afe0")
        ),
        titlePanel("Descarga de reportes de precipitación, niveles o caudales a partir de la red automática de Piragua"),
        radioButtons("radio", label = h3("Elija el tipo de reporte"),
                     choices = list("Reporte de precipitación" = 1, 
                                    "Reporte de niveles" = 2,
                                    "Reporte de caudales" = 3), 
                     selected = 1),
        checkboxGroupInput("checkGroup", label = h3("Escoja las territoriales"), 
                           choices = list("Aburrá Norte" = "Aburrá Norte",
                                          "Aburrá Sur" = "Aburrá Sur",
                                          "Cartama" = "Cartama",
                                          "Citará" = "Citará",
                                          "Hevéxicos" = "Hevéxicos",
                                          "Panzenú" = "Panzenú",
                                          "Tahamíes" = "Tahamíes",
                                          "Zenufaná" = "Zenufaná"),
                           selected = "Aburrá Norte"),
        dateRangeInput("dates", label = h3("Elección de fechas"), 
                       language = "es", start = Sys.Date() - 7,
                       end = Sys.Date()),
        downloadButton("report", "Generar reporte"),
        add_busy_bar(color = "#0396D8"),
        hr(),
        fluidRow(column(4, verbatimTextOutput("value")))
        ),
    server <-  function(input, output) {
        
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "reporte.pdf",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
                params <- list(fechas = input$dates, n = input$radio,
                               terri = input$checkGroup)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            }
        )
    }
)