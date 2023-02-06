textInputExtra <-
    function (inputId,
              label,
              value = "",
              width = NULL,
              badgeClass = NULL,
              placeholder = NULL)
    {
        value <- restoreInput(id = inputId, default = value)
        div(
            class = "form-group shiny-input-container",
            style = htmltools::css(width = validateCssUnit(width)),
            span(
                class = "badge px-2", 
                class = badgeClass,
                style = "height: 0.8rem;",
                " "
            ),
            shinyInputLabel(inputId, label),
                        tags$input(
                id = inputId,
                type = "text",
                class = "form-control",
                value = value,
                placeholder = placeholder
            )
        )
    }

shinyInputLabel <- function (inputId, label = NULL) {
    tags$label(
        label,
        class = "control-label",
        class = if (is.null(label))
            "shiny-label-null",
        id = paste0(inputId, "-label"),
        `for` = inputId
    )
}
