# ga_model: Cumalitive visualisation of time-normalised traffic

library(plotly)
library(scales)
library(dplyr)
library(purrr)
library(ggplot2)

# fetch data
data_f <- function(view_id, 
                   date_range = c(Sys.Date() - 365, Sys.Date() - 1),
                   page_filter_regex = ".*", ...) {
    page_filter_object <- dim_filter("pagePath", 
                                     operator = "REGEXP", 
                                     expressions = page_filter_regex)
    
    page_filter <- filter_clause_ga4(list(page_filter_object), operator = "AND")
    
    google_analytics(
        viewId = view_id,
        date_range = date_range, 
        metrics = "uniquePageviews", 
        dimensions = c("date", "pagePath"), 
        dim_filters = page_filter, 
        max = 10000, 
        order = order_type("uniquePageviews", "DESCENDING"), 
        anti_sample = FALSE)
}

# model data
model_f <- function(
    ga_data, 
    first_day_pageviews_min = 1, 
    total_unique_pageviews_cutoff = 100, 
    days_live_range = 365, ...) {
    normalize_date_start <- function(page) {
        
    ga_data_single_page <- ga_data %>% 
        filter(pagePath == page)
    
    first_live_row <- min(which(ga_data_single_page$uniquePageviews > first_day_pageviews_min))
    
    ga_data_single_page <- ga_data_single_page[first_live_row:nrow(ga_data_single_page), ]
    
    normalized_results <- data.frame(
        date = seq.Date(from = min(ga_data_single_page$date),
                        to = max(ga_data_single_page$date), 
                        by = "day"), 
        days_live = seq(min(ga_data_single_page$date):max(ga_data_single_page$date)), page = page) %>% 
        left_join(ga_data_single_page, by = "date") %>% 
        mutate(uniquePageviews = ifelse(is.na(uniquePageviews), 
                                        0, 
                                        uniquePageviews)) %>% 
        mutate(cumulative_uniquePageviews = cumsum(uniquePageviews)) %>% 
        dplyr::select(page, days_live, uniquePageviews, cumulative_uniquePageviews)
    }
    
    pages_list <- ga_data %>% 
        group_by(pagePath) %>% 
        summarise(total_traffic = sum(uniquePageviews)) %>% 
        filter(total_traffic > total_unique_pageviews_cutoff)
    
    ga_data_normalized <- map_dfr(pages_list$pagePath, normalize_date_start)
    
    ga_data_normalized %>% filter(days_live <= days_live_range)
}

# output data
output_f <- function(ga_data_normalized, ...) {
    gg <- ggplot(ga_data_normalized, mapping = aes(x = days_live, y = cumulative_uniquePageviews, 
        color = page)) + geom_line() + scale_y_continuous(labels = comma) + labs(title = "Unique Pageviews by Day from Launch", 
        x = "# of Days Since Page Launched", y = "Cumulative Unique Pageviews") + 
        theme_light() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
        legend.position = "none", panel.grid.major.y = element_line(color = "gray80"), 
        axis.ticks = element_blank())
    ggplotly(gg)
}

# use via ga_model_make()
