#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        fluidPage(
            theme = bslib::bs_theme(version = 5),
            
            sidebarLayout(
            
                sidebarPanel(
                    class = "p-2",
                    
                    h2("DJI rápida", class = "px-3 py-2 mt-2"),
                    
                    textInputExtra(
                        "ciudad", "Ciudad de remisión", value = "Lima", badgeClass = "vi-ciudad"
                    ),
                    
                    textInputExtra(
                        "remitente", "Nombre de remitente", badgeClass = "vi-remitente"
                    ),
                    
                    textInputExtra(
                        "dni", "N° de DNI", placeholder = "Ejemplo: 07654321", badgeClass = "vi-dni"
                    ),
                    
                    numericInputExtra(
                        "folios", "N° de folios", min = 1, max = 99, value = 1, badgeClass = "vi-folios"
                    ),
                    
                    selectInputExtra(
                        inputId = "oportunidad",
                        label = "Oportunidad de presentación",
                        choices = c("al inicio", "periódica", "al cesar"),
                        selected = "al inicio",
                        badgeClass = "vi-oportunidad"
                    ),
                    
                    textInputExtra(
                        "email", "Email", badgeClass = "vi-email"
                    ),
                    
                    textInputExtra(
                        "telefono", "N° de teléfono", 
                        placeholder = "Ejemplo: 987654321", badgeClass = "vi-telefono"
                    )
                ),
                mainPanel(
                    div(
                        id = "letterWrapper",
                        class = "p-4",
                        div(
                            class="float-right",
                            tags$p(
                                textOutput("viewerDate", container = tags$code) |> tagAppendAttributes(class = "px-3 vi-ciudad"),
                                paste0(", ", format(Sys.Date(), "%d de %B de %Y"))
                            )
                        ),
                        div(
                            class = "mb-2 mt-2",
                            tags$p(
                                "CARTA N° 01-",
                                textOutput("viewerLetterNumber", container = tags$code)|> tagAppendAttributes(class = "px-3 vi-remitente")
                            )
                        ),
                        div(
                            tags$p("Señor", class = "mb-0"),
                            tags$strong("Subgerente de Gestión de Declaraciones Juradas", class = "mb-0"),
                            tags$p("Contraloría General de la República", class = "mb-0"),
                            tags$p("Jr. Camilo Carrillo 114", class = "mb-0"),
                            tags$strong("Jesús María /Lima /Lima", class = "mb-0")
                        ),
                        div(
                            class = "mt-3",
                            tableOutput("viewerAsuntoReferencia")
                        ),
                        div(
                            id = "viewerLetterBody",
                            tags$p(
                                "Yo, ",
                                textOutput("viewerPresentation", tags$code) |> tagAppendAttributes(class = "px-3 vi-remitente"),
                                ", identificado con DNI ",
                                textOutput("viewerDNI", tags$code) |> tagAppendAttributes(class = "px-3 vi-dni"), 
                                ", me dirijo a usted, en el marco de la normativa de la referencia, ", 
                                "a fin de efectuar la remisión en original, en ",
                                textOutput("viewerFolios", tags$code) |> tagAppendAttributes(class = "px-3 vi-folios"), 
                                "  folios, y en sobre cerrado adjunto al presente,",
                                "de mi DJI correspondiente al ejercicio presupuestal ",
                                format(Sys.Date(), '%Y'),
                                " y de oportunidad de presentación ", 
                                textOutput("viewerOportunidad", tags$code) |> tagAppendAttributes(class = "px-3 vi-oportunidad"),
                                "."
                            )
                        ),
                        div(
                            id = "viewerPhoneEmail",
                            class = "mt-2",
                            tags$p(
                                "Cualquier coordinación al respecto, sírvase comunicarse al correo electrónico ", 
                                textOutput("viewerEmail", tags$code) |> tagAppendAttributes(class = "px-3 vi-email"),
                                " o al teléfono ",
                                textOutput("viewerPhone", tags$code)|> tagAppendAttributes(class = "px-3 vi-telefono"),
                                "."
                                
                            )
                        ),
                        div(
                            class = "mt-2 mb-5",
                            tags$p("Atentamente,")
                        ),
                        div(
                            class = "mb-3",
                            textOutput("viewerSignature"),
                            textOutput("viewerRemitente")|> tagAppendAttributes(class = "px-3 vi-remitente"),
                        ),
                        downloadButton("generar", "Generar carta", class = "mt-3")
                    ),
                )
            )
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DJIrapida"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
