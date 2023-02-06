numericInputExtra <- function (inputId, label, value, min = NA, max = NA, step = NA, 
                               width = NULL, badgeClass = NULL) {
    value <- restoreInput(id = inputId, default = value)
    inputTag <- tags$input(id = inputId, type = "number", class = "form-control", 
                           value = formatNoSci(value))
    if (!is.na(min)) 
        inputTag$attribs$min = min
    if (!is.na(max)) 
        inputTag$attribs$max = max
    if (!is.na(step)) 
        inputTag$attribs$step = step
    div(class = "form-group shiny-input-container", style = htmltools::css(width = validateCssUnit(width)), 
        span(
            class = "badge px-2", 
            class = badgeClass,
            style = "height: 0.8rem;",
            " "
        ),
        shinyInputLabel(inputId, label), 
        inputTag)
}

formatNoSci <- function (x) {
    if (is.null(x)) 
        return(NULL)
    format(x, scientific = FALSE, digits = 15)
}
