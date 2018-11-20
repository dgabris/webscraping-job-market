salary_extraction_fun <- function(job_url) {
  salary_min <- NA
  salary_max <- NA
  salary_avg <- NA
  
  job_webpage <- read_html(job_url)
  
  # Method 1 - job post header
  salary_node <- html_nodes(job_webpage, ".panel-body , span[itemprop='baseSalary']")
  print(salary_node)
  salary_text <- html_text(salary_node)
  print(salary_text)
  if(length(salary_text) >= 3) {
    salary_min <- as.numeric(html_text(salary_node)[[2]])
    salary_max <- as.numeric(html_text(salary_node)[[3]])
    salary_avg <- mean(c(salary_min, salary_max), na.rm = TRUE)
  }
  print(salary_avg)
  if(!is.na(salary_avg)) return(salary_avg)
  
  # Method 2 - job post footer
  overall_info_node <- html_nodes(job_webpage, ".overall-info")
  print(overall_info_node)
  overall_info_text <- html_text(overall_info_node)
  print(overall_info_text)
  overall_info_text_processed <- str_replace_all(str_to_lower(overall_info_text), "\\s{1,}", "")
  salary_avg_step1 <- str_extract(overall_info_text_processed, "[0-9\\-\\.,€]{0,}[0-9\\.,\\-]{1,}(€|eur)")
  salary_avg_step2 <- str_extract_all(salary_avg_step1, "[0-9,.]+", simplify = TRUE)
  salary_avg_step3 <- parse_number(salary_avg_step2, locale = locale(grouping_mark = ".", decimal_mark = ","))
  salary_avg <- mean(salary_avg_step3, na.rm = TRUE)
  print(salary_avg)
  if(!is.na(salary_avg)) return(salary_avg)
  
  # Method 3 - job post content
  # TODO
  return(salary_avg)
}