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
            
            titlePanel("DJI rápida"),
            
            sidebarLayout(
                sidebarPanel(
                    textInput("ciudad", "Ciudad de remisión", value = "Lima"),
                    textInput("remitente", "Nombre de remitente"),
                    textInput("dni", "N° de DNI", placeholder = "Ejemplo: 07654321"),
                    numericInput("folios", "N° de folios", value = 1),
                    selectInput(
                        inputId = "oportunidad",
                        label = "Oportunidad de presentación",
                        choices = c("al inicio", "periódica", "al cesar"),
                        selected = "al inicio"
                    ),
                    textInput("email", "Email"),
                    textInput("telefono", "N° de teléfono", placeholder = "Ejemplo: 987654321")
                ),
                mainPanel(
                    div(
                        id = "letterWrapper",
                        class = "p-4",
                        div(
                            textOutput("viewerDate") |> tagAppendAttributes(style = "text-align: right;")    
                        ),
                        div(
                            class = "mb-2 mt-2",
                            textOutput("viewerLetterNumber"),
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
                            textOutput("viewerLetterBody")
                        ),
                        div(
                            class = "mt-2",
                            textOutput("viewerPhoneEmail"),
                        ),
                        div(
                            class = "mt-2 mb-5",
                            tags$p("Atentamente,")
                        ),
                        div(
                            class = "mb-3",
                            textOutput("viewerSignature"),
                            textOutput("viewerRemitente"),
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
