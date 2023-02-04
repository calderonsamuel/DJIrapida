#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    
    add_spaces <- function(x) paste0("__", x, "__")
    
    inputs <- reactive({
        list(
            "Ciudad" = input$ciudad,
            "Fecha"= Sys.Date() |> as.character(),
            "Remitente"= input$remitente |> stringi::stri_trans_general("Latin-ASCII"),
            "DNI"= input$dni,
            "Folios"= input$folios |> as.character(),
            "Oportunidad"= input$oportunidad,
            "Email"= input$email,
            "Telefono"= input$telefono
        )
    })
    
    
    output$tabla <- DT::renderDT({
        nms <- names(inputs())
        values <- unlist(inputs())
        
        data <- data.frame(
            variable = nms,
            valores = values
        ) 
        
        data |> 
            DT::datatable(
                options = list(
                    dom = "t",
                    ordering = FALSE
                ),
                rownames = FALSE, 
                colnames = ""
                # colnames = rep("", ncol(data))
            )
    })
    
    output$generar <- downloadHandler(
        filename = \() {
            paste0("DJI-",input$dni,"-",inputs()$Fecha, ".docx")
        },
        content = \(file) {
            rmarkdown::render(
                input = "inst/rmarkdown/plantilla-carta.Rmd",
                output_file = file,
                params = inputs()
            )
        }
    )
    
    output$viewerDate <- renderText(input$ciudad |> add_spaces())
    
    output$viewerLetterNumber <- renderText({
        gsub("[^A-Z]","", input$remitente)
    })
    
    output$viewerAsuntoReferencia <- renderTable({
        asunto <- "Remisión de Declaración Jurada de Intereses – DJI"
        referencia <- "Cuarta Disposición Complementaria Transitoria del Reglamento que implementa la Ley N° 31227, respecto a la recepción, el ejercicio del control, fiscalización y sanción de la Declaración Jurada de Intereses de autoridades, funcionarios y servidores públicos del Estado, y candidatos a cargos públicos."
        
        tibble::tribble(
            ~variable, ~puntos, ~valor,
            "Asunto", ":", asunto,
            "Referencia", ":", referencia
        )
    }, 
    colnames = FALSE, bordered = FALSE)
    
    output$viewerPresentation <- renderText(input$remitente |> add_spaces())
    output$viewerDNI <- renderText(input$dni |> add_spaces())
    output$viewerFolios <- renderText(input$folios |> add_spaces())
    output$viewerOportunidad <- renderText(input$oportunidad |> add_spaces())
    
    output$viewerEmail <- renderText(input$email |> add_spaces())
    output$viewerPhone <- renderText(input$telefono |> add_spaces())
    
    output$viewerSignature <- renderText(paste0(rep("_", nchar(input$remitente) + 5), collapse = ""))
    output$viewerRemitente <- renderText(input$remitente |> add_spaces())
}
