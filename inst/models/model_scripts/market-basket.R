# based on https://www.linkedin.com/pulse/market-basket-analysis-ga-r-jamarius-taylor/
library(plyr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(plotly)
library(googleAnalyticsR)


data_f <- function(view_id, 
                   date_range = c(Sys.Date() - 2, Sys.Date() - 1),
                   ...){
  
  google_analytics(view_id, 
                   date_range = date_range, 
                   metrics = c("itemQuantity", "itemRevenue"), 
                   dimensions = c("productName", "transactionId"),
                   order = order_type("itemQuantity", "DESCENDING"),
                   max = 9999) 
  
}

model_f <- function(df, ...){
  if(nrow(df) == 0){
    stop("No data found in this GA account")
  }
  
  temp <- tempfile(fileext = ".csv")
  df1 <- df %>%
    dplyr::mutate(
      itemCost = itemRevenue / itemQuantity
    ) %>%
    tidyr::uncount (weights = itemQuantity) %>%
    plyr::ddply(c('transactionId'),
                    function(tf1)paste(tf1$productName,
                                       collapse = ',')) %>%
    tidyr::separate('V1',
                  into = paste('item',1:70,sep = "_"),
                  sep = ',') %>%
    write.csv(temp)
  
  a_trans <- arules::read.transactions(temp, format = "basket", sep  = ",")
  
  rules <- apriori(a_trans, parameter = list(supp=0.01, conf=0.05)) 
  
  sort(rules, by='confidence', decreasing = TRUE)

}

output_f <- function(rules, 
                     method = c("graph", "scatterplot", "matrix"),
                     max_n = 100, ...){
  method <- match.arg(method)
  
  if(!require(arulesViz)){
    stop("Need library(arulesViz)")
  }
  
  plot(rules, method=method, engine = "default", max = max_n)

}

inputS <- shiny::tagList(
  shiny::numericInput("max_n","Maximum number of rules",100,5,300, step = 1),
  shiny::selectInput("method","Rule plot type", 
                     choices = c("graph", "scatterplot", "matrix"))
)

model <- ga_model_make(
  data_f = data_f, 
  required_columns = c("productName","transactionId","itemQuantity","itemRevenue"),
  model_f = model_f,
  output_f = output_f,
  required_packages = c("plyr","dplyr","tidyr","arules","arulesViz"),
  description = "Market Basket Analysis by Jamarius Taylor",
  outputShiny = shiny::plotOutput,
  renderShiny = shiny::renderPlot,
  inputShiny = inputS
)

ga_model_save(model, "inst/models/examples/market-basket.gamr")


