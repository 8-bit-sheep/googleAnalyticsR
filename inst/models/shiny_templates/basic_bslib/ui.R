library(bslib)
my_theme <- bs_theme(
  bg = "#202123", fg = "#B8BCC2", primary = "#EA80FC", 
  base_font = font_google("Grandstander"),
  "font-size-base" = "1.1rem"
)

## basic ui.R
fluidPage(title = "{{ shiny_title }}",
          theme = my_theme,
                {{ auth_ui }},
                {{{ date_range }}},
                h2("Model Output"),
{{{ model_ui }}}
)
