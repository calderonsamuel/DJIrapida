#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    
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
    
    output$viewerDate <- renderText({
        paste0(input$ciudad, ", ", format(Sys.Date(), "%d de %B de %Y"))
    })
    
    output$viewerLetterNumber <- renderText({
        paste0("CARTA N° 01-", gsub("[^A-Z]","", input$remitente))
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
    colnames = FALSE, bordered = FALSE, )
    
    output$viewerLetterBody <- renderText({
        glue::glue("Yo, {input$remitente}, identificado con DNI {input$dni}, me dirijo a usted, en el marco de la normativa de la referencia, a fin de efectuar la remisión en original, en {input$folios} folios, y en sobre cerrado adjunto al presente, de mi DJI correspondiente al ejercicio presupuestal {format(Sys.Date(), '%Y')} y de oportunidad de presentación {input$oportunidad}.")
    })
    
    output$viewerPhoneEmail <- renderText({
        glue::glue("Cualquier coordinación al respecto, sírvase comunicarse al correo electrónico {input$email}  o al teléfono {input$telefono}.")
    })
    
    output$viewerSignature <- renderText(paste0(rep("_", nchar(input$remitente) + 5), collapse = ""))
    output$viewerRemitente <- renderText(input$remitente)
}
