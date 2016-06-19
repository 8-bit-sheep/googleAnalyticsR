#' authDropdown UI
#'
#' Shiny Module for use with \link{authDropdown}
#' 
#' @param id Shiny id
#'
#' @return Shiny UI
authDropdownUI <- function(id){

  ns <- shiny::NS(id)
  
  tagList(
    fluidRow(
      box(
        selectInput(ns("accounts"),
                    label="Accounts",
                    choices = NULL)
        , width = 4, title="Select Account", status="success", solidHeader=TRUE),
      box(
        selectInput(ns("web.prop"),
                    label="WebProperty",
                    choices = NULL)
        , width = 4, title="Select Web Property", status="success", solidHeader=TRUE),
      box(
        selectInput(ns("view"),
                    label="Select View",
                    choices = NULL)
        , width = 4, title="Pick View (ID)", status="success", solidHeader=TRUE)
    )
  )

}

#' authDropdown
#'
#' Shiny Module for use with \link{authDropdownUI}
#'
#' Call via \code{shiny::callModule(authDropdown, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param ga.table A table of GA tables
#'
#' @return Something
authDropdown <- function(input, output, session, ga.table){

  ns <- session$ns
  
  pList <- reactive({
    ga.table <- ga.table()
    
    ga.table[,c('accountName','webPropertyId','websiteUrl','viewName', 'viewId')]
    
  })
  
  observe({
    validate(
      need(pList(), "Need profiles")
    )
    pList  <- pList()
    
    choice <- unique(pList$accountName)
    
    updateSelectInput(session, 
                      "accounts",
                      label="Accounts",
                      choices = choice)
  })
  
  observe({
    validate(
      need(input$accounts, "Need accounts")
    )
    pList  <- pList()
    
    pList <- pList[input$accounts == pList$accountName,]

    choice <- pList$websiteUrl
    
    updateSelectInput(session, 
                      "web.prop", label="WebProperty",
                      choices = choice)
  })
  
  observe({
    validate(
      need(input$web.prop, "Need web")
    )
    pList <- pList()
    
    pList <- pList[input$web.prop == pList$websiteUrl,]
    
    choice <- pList$viewId 
    
    names(choice) <- paste(pList$viewName, pList$viewId)
    
    updateSelectInput(session, "view",
                      label="Views",
                      choices = choice)
  })
  
  chosen_view <- reactive({
    validate(
      need(input$view, "View")
    )
    pList <- pList()
    
    pList[input$view == pList$viewId,]
  })
  
  return(chosen_view)

}

#' metric_select UI
#'
#' Shiny Module for use with \link{metric_select}
#' 
#' @param id Shiny id
#'
#' @return Shiny UI
metric_selectUI <- function(id, label = "Metric"){

  ns <- shiny::NS(id)
  
  selectInput(ns("metric_select"),
              label=label,
              choices = c("Sessions" = "sessions",
                          "Users" = "users",
                          "New Users" = "newUsers",
                          "Page Views" = "pageviews",
                          "Revenue" = "transactionRevenue",
                          "Transactions" = "transactions",
                          "Goal 1 Completions" = "goal1Completions",
                          "Goal 2 Completions" = "goal2Completions",
                          "Goal 3 Completions" = "goal3Completions",
                          "Goal 4 Completions" = "goal4Completions",
                          "Goal 5 Completions" = "goal5Completions",
                          "Goal 6 Completions" = "goal6Completions",
                          "Goal 7 Completions" = "goal7Completions",
                          "Goal 8 Completions" = "goal8Completions",
                          "Goal 9 Completions" = "goal9Completions",
                          "Goal 10 Completions" = "goal10Completions",
                          "Goal 11 Completions" = "goal11Completions",
                          "Goal 12 Completions" = "goal12Completions",
                          "Goal 13 Completions" = "goal13Completions",
                          "Goal 14 Completions" = "goal14Completions",
                          "Goal 15 Completions" = "goal15Completions",
                          "Goal 16 Completions" = "goal16Completions",
                          "Goal 17 Completions" = "goal17Completions",
                          "Goal 18 Completions" = "goal18Completions",
                          "Goal 19 Completions" = "goal19Completions",
                          "Goal 20 Completions" = "goal20Completions",
                          "Total Events" = "totalEvents",
                          "Unique Events" = "uniqueEvents",
                          "Event Value" = "eventValue")
  )

}

#' metric_select
#'
#' Shiny Module for use with \link{metric_selectUI}
#'
#' Call via \code{shiny::callModule(metric_select, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return Something
metric_select <- function(input, output, session){

  ns <- session$ns

}