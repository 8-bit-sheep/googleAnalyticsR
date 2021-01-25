## basic ui.R
fluidPage(title = "{{ shiny_title }}",
                {{ auth_ui }},
                {{{ date_range }}},
                h2("Model Output"),
{{{ model_ui }}}
)
