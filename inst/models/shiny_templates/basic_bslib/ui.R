library(bslib)

bg <- "{{ bg }}"
if(!nzchar(bg)) bg <- "#202123"

fg <- "{{ fg }}"
if(!nzchar(fg)) fg <- "#B8BCC2"

primary <- "{{ primary }}"
if(!nzchar(primary)) primary <- "#EA80FC"

secondary <- "{{ secondary }}"
success <- "{{ success }}"
info <- "{{ info }}"
warning <- "{{ warning }}"
danger <- "{{ danger }}"
base_font <- "{{ base_font }}"
code_font <- "{{ code_font }}"
heading_font <- "{{ heading_font }}"

if(!nzchar(secondary)) secondary <- NULL
if(!nzchar(success)) success <- NULL
if(!nzchar(info)) info <- NULL
if(!nzchar(warning)) warning <- NULL
if(!nzchar(danger)) danger <- NULL
if(!nzchar(code_font)) code_font <- NULL
if(!nzchar(heading_font)) heading_font <- NULL

font_google <- "{{ font_google }}"
if(!nzchar(font_google)) font_google <- "Grandstander"

bslib_theme <- bs_theme(
  bg = bg, fg = fg, primary = primary, 
  base_font = font_google(font_google),
  secondary = secondary,
  success = success,
  info = info,
  warning = warning,
  danger = danger,
  code_font = code_font,
  heading_font = heading_font,
  "font-size-base" = "1.1rem"
)

# a list of the model objects
models <- {{ model_list }}

# uses models (a list of models) to create the model UI 
shinytheme_tabPanel <- function(models){
  model_n <- paste0("model", seq_along(models)) 
  tabs <- mapply(
    function(x,y) shiny::tabPanel(y$model$description, y$ui(x)), 
    x=model_n, y=models, 
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  do.call(shiny::tabsetPanel, args = tabs)
}

## basic ui.R
shiny::navbarPage(
  title = "{{ shiny_title }}",
  theme = bslib_theme,
  shiny::tabPanel("Models",
                  shiny::sidebarPanel(
                    {{{ date_range }}}
                  ),
                  shiny::mainPanel(
                    shinytheme_tabPanel(models),
                    {{ auth_ui }}
                  )
  )
)
