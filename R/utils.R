common_pop_names <- c("boundary", "bcell" ,"lymph", "live","tcell", "transitional"
                      , "debris", "nonDebris", "singlet"
                      , "cd3", "cd4", "cd8")
# myActionButton <- function (inputId, label, icon = NULL, ...) 
# {
#   tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", 
#               list(icon, label), ...)
# }


helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      "?"
    )
  )
}


mytextInput.typeahead <- function (id, label, local, valueKey, tokens, template, 
                                   limit = 20) 
{
  div(class = "form-group shiny-input-container",
      tags$label(label, `for` = id)
      , tags$br()
      , tagList(singleton(tags$head(tags$script(src = "shinysky/typeahead/typeahead.min.js"))), 
                singleton(tags$head(tags$script(src = "shinysky/typeahead/hogan.js"))), 
                singleton(tags$head(tags$script(src = "shinysky/typeahead/textInput-typeahead.js"))), 
                singleton(tags$head(tags$link(rel = "stylesheet", href = "shinysky/typeahead/typeahead.js-bootstrap.css"))), 
                HTML(sprintf("<input data-name=\"%s\" id=\"%s\" type=\"text\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" style=\"margin: 0 auto\"/> ", 
                             id, id)
                )
                , tags$script(HTML(sprintf("update_typeahead(\"%s\",%s,\"%s\",%s,\"%s\",%s)"
                                           ,id, toJSON(local), valueKey, toJSON(as.character(tokens))
                                           , HTML(template), limit)
                )
                )
      )
  )
}
