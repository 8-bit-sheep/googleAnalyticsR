#' New parse GA account summary
#' 
#' @param x The account summary items
#' @import tidyjson
parse_ga_account_summary <- function(x){
  
  json_accounts <- jsonlite::toJSON(x$items)
  class(json_accounts) <- c(class(json_accounts), "character")
  tidy_json <- json_accounts %>% tidyjson::as.tbl_json()
  
  tidy_json <- tidy_json %>% 
    gather_array() %>% 
    spread_values(accountId = jstring("id"), 
                  accountName = jstring("name")) %>%
    enter_object("webProperties") %>%
    gather_array() %>%
    spread_values(webPropertyId = jstring("id"), 
                  webPropertyName = jstring("name"),
                  internalWebPropertyId = jstring("internalWebPropertyId"),
                  level = jstring("level"),
                  websiteUrl = jstring("websiteUrl")) %>%
    enter_object("profiles") %>%
    gather_array() %>%
    spread_values(profileId = jstring("id"), 
                  profileName = jstring("name"),
                  type = jstring("type"),
                  starred = jstring("starred"))

  ## remove tidyjson artifacts
  out <- tidy_json[,setdiff(names(tidy_json), c("document.id","array.index"))]
  
  out
}
