# Pull data from specific category

# Libraries ------------------------------
library(httr)
library(readr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(purrr)
library(glue)

# Functions and setup ------------------------
get_category_posts <- function(
  cat_id = 5,
  from_day_ago = 0,
  duration_days = 8
){
  
  post_req <- httr::POST(
    url = "https://community.rstudio.com/admin/plugins/explorer/queries/21/run.csv",
    body = list(
      # 'params'='{"cat_id":"10","from_day_ago":"0","duration_days":"7"}'
      'params'=glue('{{"cat_id":"{cat_id}","from_day_ago":"{from_day_ago}","duration_days":"{duration_days}"}}')
    ),
    add_headers(
      .headers = c(
        "Content-Type" = "multipart/form-data;",
        "Api-Key" = Sys.getenv("COMM_API_KEY"),
        "Api-Username" = Sys.getenv("COMM_API_USR"))
    ),
    verbose()
  )
  stop_for_status(post_req)
  mydata <- rawToChar(post_req$content) %>% read_csv() %>% 
    mutate(
      topic_id = as.numeric(topic_id),
      category_id = as.numeric(category_id),
      views = as.numeric(views),
      posts_count = as.numeric(posts_count),
      reply_count = as.numeric(reply_count),
      like_count = as.numeric(like_count),
      incoming_link_count = as.numeric(incoming_link_count),
      score = as.numeric(score),
      word_count = as.numeric(word_count),
      sol_user_id = as.numeric(sol_user_id),
      sol_post_id = as.numeric(sol_post_id),
      image_upload_id = as.numeric(image_upload_id),
      upload_id = as.numeric(upload_id)
    )
  
  return(mydata)
}

category_df <- tribble(
  ~choice, ~cat_ids, ~category,   
  "R-Admin",  5, "R-Admin",
  "Tidyverse",  6, "Tidyverse",
  "Shiny (including shinyapps.io)",  8, "Shiny",
  "RStudio IDE", 9, "RStudio IDE",
  "R Markdown", 10, "R Markdown",
  "package-development", 11, "package-development",
  "Teaching", 13, "Teaching",
  "RStudio Cloud", 14,"RStudio Cloud",
  "ML (Tidymodels & MLverse)" , 15,"ML",
  "General", 17,"General",
  "R-Admin", 21,"R-Admin Package Management",
  "Shiny (including shinyapps.io)",24,"shinyapps.io",
  "R-Admin",27,"R-Admin Connect",
  "R-Admin Training", 12, "R-Admin Training"
)

# Pull Data ----------------------------
categorys <- category_df
#   filter(choice == params$categories)

text_summary = ""
result <- tibble()
for (i in categorys$cat_ids){
  
  section <- get_category_posts(
    cat_id = i,
    from_day_ago = 0,
    duration_days = 7
  )
  
  result <- bind_rows(
    result,
    section
  )
  
  text_summary <- paste(
    text_summary,
    
    glue(
      "- **{(category_df %>% filter(cat_ids == i))$category}** - {nrow(section)} new topics -",
      " {(section$posts_count == 0) %>% sum} with no response - ",
      "{is.na(section$sol_at) %>% sum} ({((is.na(section$sol_at) %>% sum)/(nrow(section))*100) %>% round(0)}%) unsolved<br>

")


  )
  
}
