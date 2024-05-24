"%IN%" <- function(x,y){interaction(x) %in% interaction(y)}

set_shadow <- function (id = NULL, class = NULL) 
{
  cssShadow <- paste0(" box-shadow: 0 4px 6px 0 rgba(0,0,0,0.2);\n      transition: 0.3s;\n      border-radius: 5px;\n   ")
  cssShadow <- if (!is.null(id)) {
    if (!is.null(class)) {
      paste0("#", id, " .", class, " {", 
             cssShadow, "}")
    }
    else {
      paste0("#", id, " {", cssShadow, "}")
    }
  }
  else {
    if (!is.null(class)) {
      paste0(".", class, " {", cssShadow, "}")
    }
    else {
      NULL
    }
  }
  cssHover <- "box-shadow: 0 8px 12px 0 rgba(0,0,0,0.2);"
  cssHover <- if (!is.null(id)) {
    if (!is.null(class)) {
      paste0("#", id, ":hover .", class, ":hover {", 
             cssHover, "}")
    }
    else {
      paste0("#", id, ":hover", " {", 
             cssHover, "}")
    }
  }
  else {
    if (!is.null(class)) {
      paste0(".", class, ":hover", " {", 
             cssHover, "}")
    }
    else {
      NULL
    }
  }
  css <- paste(cssShadow, cssHover)
  htmltools::tags$head(htmltools::tags$style(css))
}