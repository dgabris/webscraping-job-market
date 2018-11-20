library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(robotstxt)
library(mapsapi)
library(yaml)

maps_api_key <- read_yaml("maps_api_key.yml")

sample_portion <- 0.001

file_counter <- 1
result_filename <- paste0("sampling_industries_", file_counter, ".csv")

while(file.exists(result_filename)) {
  file_counter <- file_counter + 1
  result_filename <- paste0("sampling_industries_", file_counter, ".csv")
}

colnames_df <- data.frame(
  url = "url",
  date_created = "date_created",
  title = "title",
  employer = "employer",
  industry = "industry",
  job_category = "job_category",
  salary_avg = "salary_avg",
  location_job_post = "location_job_post",
  location_google = "location_google",
  location_google_lat = "location_google_lat",
  location_google_long = "location_google_long"
  
)

write_csv(colnames_df, result_filename)

base_url <- "https://www.profesia.sk/praca"
filter_url <- "/zoznam-pracovnych-oblasti"
page_url <- NULL
full_url <- paste0(base_url, filter_url, page_url)
webpage <- read_html(full_url)

industry_names <- trimws(str_extract(html_text(html_nodes(webpage, ".card .list-reset li")), "[^0-9]+"))
industry_counts <- str_extract(str_replace_all(html_text(html_nodes(webpage, ".card .list-reset li")), "\\s", ""), "[0-9]+")
industry_links <- html_attr(html_nodes(webpage, ".card .list-reset li a"), "href")

counter <- sum(pmax(as.integer(as.integer(industry_counts) * sample_portion), 1))
for(i in 1:length(industry_links)) {
  industry_name <- industry_names[i]
  industry_count <- industry_counts[i]
  industry_url <- str_replace(industry_links[i], "/praca", "")
  
  page_index <- 1
  url_letter <- "?"
  if(str_detect(filter_url, "\\?")) url_letter <- "&" 
  page_url <- paste0(url_letter, "page_num=", page_index)
  full_url <- paste0(base_url, industry_url, page_url)
  
  webpage <- read_html(full_url)
  job_counter <- html_nodes(webpage, "div[class = 'offer-counter text-right bold']")
  job_counter_text <- html_text(job_counter)
  job_count <- as.numeric(str_replace_all(str_replace(job_counter_text, "(.+) z (.+)", "\\2"), "\\s", ""))
  page_count <- (job_count %/% 20) + 1
  
  sample_size <- max(as.integer(job_count * sample_portion), 1)
  job_vector <- 1:job_count
  job_vector_sample <- sort(sample(job_vector, size = sample_size))
  page_vector_sample <- (job_vector_sample %/% 20) + 1
  job_vector_sample_mod <- job_vector_sample %% 20
  
  for(current_page in unique(page_vector_sample)) {
    page_url <- paste0(url_letter, "page_num=", current_page)
    full_url <- paste0(base_url, industry_url, page_url)
    webpage <- read_html(full_url)
    
    job_posts <- html_nodes(webpage, ".title")
    job_employers <- html_nodes(webpage, ".employer")
    job_locations <- html_nodes(webpage, ".job-location")
    job_posts_titles <- html_text(job_posts)
    job_posts_links <- html_attr(job_posts, "href")
    job_posts_employers <- html_text(job_employers)
    job_posts_locations <- html_text(job_locations)
    index_not_na <- !is.na(job_posts_links)
    
    jobs_sample_this_page <- job_vector_sample_mod[page_vector_sample == current_page]
    
    job_posts_titles <- job_posts_titles[index_not_na][jobs_sample_this_page]
    job_posts_links <- paste0(base_url, job_posts_links[index_not_na][jobs_sample_this_page])
    job_posts_employers <- job_posts_employers[jobs_sample_this_page]
    job_posts_locations <- job_posts_locations[jobs_sample_this_page]
    
    for(i in 1:length(job_posts_links)) {
      job_title <- job_posts_titles[i]
      job_employer <- job_posts_employers[i]
      job_location <- job_posts_locations[i]
      job_url <- job_posts_links[i]
      job_industry <- industry_name
      
      search_query <- paste(job_employer, job_location, sep = " ")
      google_location_doc <- mp_geocode(search_query, key = maps_api_key)
      google_location_point <- mp_get_points(google_location_doc)
      job_location_address <- google_location_point$address_google
      job_location_lat <- unname(unlist(google_location_point$pnt))[2]
      job_location_long <- unname(unlist(google_location_point$pnt))[1]
      
      job_webpage <- read_html(job_url)
      
      date_created_span <- html_nodes(job_webpage, ".overall-info, span[itemprop='datePosted']")[2]
      date_created <- html_text(date_created_span)
      
      job_category_html <- html_nodes(job_webpage, ".overall-info a")
      job_category <- paste(html_text(job_category_html[-length(job_category_html)]), collapse = "|")
      
      salary_avg_method1 <- NA_real_
      salary_avg_method2 <- NA_real_
      
      # Method 1 - job post header
      salary_node <- html_nodes(job_webpage, ".panel-body , span[itemprop='baseSalary']")
      salary_text <- html_text(salary_node)
      if(length(salary_text) >= 3) {
        salary_min <- as.numeric(html_text(salary_node)[[2]])
        salary_max <- as.numeric(html_text(salary_node)[[3]])
        salary_avg_method1 <- mean(c(salary_min, salary_max), na.rm = TRUE)
      }
      
      # Method 2 - job post footer
      overall_info_node <- html_nodes(job_webpage, ".overall-info")
      overall_info_text <- html_text(overall_info_node)
      overall_info_text_processed <- str_replace_all(str_to_lower(overall_info_text), "\\s{1,}", "")
      salary_avg_step1 <- str_extract(overall_info_text_processed, "[0-9\\-\\.,€]{0,}[0-9\\.,\\-]{1,}(€|eur)")
      salary_avg_step2 <- str_extract_all(salary_avg_step1, "[0-9,.]+", simplify = TRUE)
      salary_avg_step3 <- parse_number(salary_avg_step2, locale = locale(grouping_mark = ".", decimal_mark = ","))
      salary_avg_method2 <- mean(salary_avg_step3, na.rm = TRUE)
      
      salary_avg <- case_when(
        is.na(salary_avg_method1) ~ salary_avg_method2,
        is.na(salary_avg_method2) ~ salary_avg_method1,
        !is.na(salary_avg_method1) & salary_avg_method1 == 0 ~ salary_avg_method2,
        !is.na(salary_avg_method2) & salary_avg_method2 == 0 ~ salary_avg_method1,
        !is.na(salary_avg_method1) & salary_avg_method1 >= 10 & salary_avg_method1 <= 480 ~ salary_avg_method2,
        !is.na(salary_avg_method1) & !is.na(salary_avg_method2) & salary_avg_method1 == salary_avg_method2 ~ salary_avg_method1,
        !is.na(salary_avg_method1) & !is.na(salary_avg_method2) & salary_avg_method1 != salary_avg_method2 ~ salary_avg_method2
      )
      
      if(!is.na(salary_avg) & salary_avg <= 10) salary_avg <- salary_avg * 160
      
      print(industry_name)
      print(job_title)
      print(paste0("Method 1 salary estimate - ", salary_avg_method1))
      print(paste0("Method 2 salary estimate - ", salary_avg_method2))
      print(paste0("Final salary estimate - ", salary_avg))
      cat("\n")
      
      df <- data.frame(
        url = c(job_url, NA)[1],
        date_created = c(date_created, NA)[1],
        title = c(job_title, NA)[1],
        employer = c(job_employer, NA)[1],
        industry = c(job_industry, NA)[1],
        job_category = c(job_category, NA)[1],
        salary_avg = c(salary_avg, NA)[1],
        location_job_post = c(job_location, NA)[1],
        location_google = c(job_location_address, NA)[1],
        location_google_lat = c(job_location_lat, NA)[1],
        location_google_long = c(job_location_long, NA)[1]
      )
      
      write_csv(df, result_filename, append = TRUE)
      
      counter <- counter - 1
      console_text <- paste0("Working... ", counter, " job posts left...")
      print(console_text)
      
      Sys.sleep(3)
    }
  }
}

result_df <- read_csv(result_filename, col_names = TRUE)
result_df <- result_df[-1, ]
View(result_df)
