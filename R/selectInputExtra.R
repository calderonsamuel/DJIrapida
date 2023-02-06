selectInputExtra <- function (inputId, label, choices, selected = NULL, multiple = FALSE, 
          selectize = TRUE, width = NULL, size = NULL, badgeClass = NULL) {
    input_tag <- shiny::selectInput(
        inputId = inputId,
        label = label,
        choices = choices,
        selected = selected,
        multiple = multiple,
        selectize = selectize,
        width = width,
        size = size
    )
    
    tq <- htmltools::tagQuery(input_tag)
    
    if (!is.null(badgeClass)) {
        tq$children("label")$
            before(
                span(
                    class = "badge px-2", 
                    class = badgeClass,
                    style = "height: 0.8rem;",
                    " "
                )
            )
    }
    
    tq$allTags()
}

