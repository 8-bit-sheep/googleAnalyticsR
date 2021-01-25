library(argonDash)
library(argonR)

logo <- "https://raw.githubusercontent.com/MarkEdmondson1234/googleAnalyticsR/master/inst/hexlogo/hex.png"

argonDashPage(
  title = "{{ shiny_title }}",
  description = "A ga_model_shiny() instance launched with argonDash",
  sidebar = argonDashSidebar(
    argonSidebarHeader(title = "{{ shiny_title }}"),
    argonBadge(text = "Re-authenticate",
               src = "/"),
    argonSidebarDivider(),
    {{ auth_ui }},
    argonSidebarDivider(),
    {{{ date_range }}},
    vertical = TRUE,
    size = "lg",
    side = "left",
    id="my_sidebar",
    brand_logo = logo,
    skin = "dark",
    brand_url = "https://code.markedmondson.me/googleAnalyticsR/reference/ga_model_shiny.html"
    ),
  header = argonDashHeader(),
  body = argonDashBody(
    argonCard(
      title = "Single Model Template",
      width = 12,
      {{{ model_ui }}}
    ),
    argonCard(
      width = 12,
      title = "argonDash",
      argonLead("A single model template using library(argonDash)"),
      p("Launched using ga_model_shiny() with template ga_model_shiny_template('argonDash')")
    )
  )

)
