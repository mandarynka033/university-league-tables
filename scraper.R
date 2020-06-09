library(purrr)
library(progress)
library(readr)
library(rvest)
library(stringr)

# Collect list of courses
courses <- read_html('https://www.thecompleteuniversityguide.co.uk/courses') %>%
  html_nodes(xpath = '/html/body/section[2]/div[3]/div/a/p/span[1]') %>%
  html_text()

# Generate league table URLs
league_table_urls <- courses %>%
  str_remove_all(',') %>%
  str_replace_all(' ', '_') %>%
  str_replace_all('&', 'And') %>%
  gsub('(?<=_)([a-z])', '\\U\\1\\E', ., perl = TRUE) %>%
  str_c(
    'https://www.thecompleteuniversityguide.co.uk/league-table-files/',
    'all-years/Complete_University_Guide_',
    .,
    '.csv'
  )
names(league_table_urls) <- courses

# Create function to download CSV files
download_csv <- function(url) {
  league_table <- read_csv(url, skip = 4, col_types = cols(
    # This will miss a few columns which only appear in some tables (e.g.
    # `Research Assessment`) but these are of little use anyway
    'Year' = col_integer(),
    'Rank' = col_integer(),
    'Entry Standards' = col_integer(),
    'Student Satisfaction' = col_double(),
    'Research Quality' = col_double(),
    'Research Intensity' = col_double(),
    'Graduate Prospects' = col_double(),
    'Overall Score' = col_double(),
    .default = col_skip()
  ), na = c('NA', 'n/a'))
  pb$tick()
  return(league_table)
}

# Download and combine all datasets — be nice and don't use this too often
pb <- progress_bar$new(total = length(league_table_urls))
league_tables <- map_dfr(league_table_urls, download_csv, .id = 'Course')

# Clean up dataframe
league_tables$Course <- factor(league_tables$Course)
names(league_tables) <- names(league_tables) %>%
  str_replace_all(' ', '_') %>%
  tolower()

# Clean up environment
rm(list = c('courses', 'league_table_urls', 'download_csv', 'pb'))
