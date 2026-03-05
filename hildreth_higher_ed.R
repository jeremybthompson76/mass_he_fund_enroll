library(tidyverse)
library(scales)
library(rvest)
library(httr)
library(httr2)
library(pdftools)
library(bea.R)
library(tidyquant)
library(readxl)
library(rlang)
library(openxlsx)

setwd("C:/Users/Admin-Aspire/Documents/Hildreth")
options(scipen = 999)

# Source URLs
fall <- "https://educationtocareer.data.mass.gov/resource/gzpm-dvfd.csv"
annual <- "https://educationtocareer.data.mass.gov/resource/j7yp-crt6.csv"
tf <- "https://educationtocareer.data.mass.gov/resource/3bpk-k8k2.csv"
cthru <- "https://cthru.data.socrata.com/resource/kv7m-35wn.csv"

# My app token
app_token <- Sys.getenv("MA_API_TOKEN")

# Define function to import from API
api_get <- function(url, query, token) {
  resp <- GET(
    url,
    query = query,
    add_headers(`X-App-Token` = token)
    ) |>
    stop_for_status()
  
  read_csv(content(resp, "raw"), show_col_types = FALSE)
}

### Scrape UMass in-state undergraduate percentage from the student profile PDFs

# 1. Define a function to find the profile PDF on each page
find_profile_pdf <- function(page_url) {
  html <- read_html(page_url)
  
  hrefs <- html |>
    html_elements("a") |>
    html_attr("href") |>
    na.omit()
  
  links <- url_absolute(hrefs, page_url)
  
  pat <- "(?i)fall-\\d{4}-student-profile[^/]*\\.pdf$"
  pdfs <- links[str_detect(links, pat)]
  
  if (length(pdfs) == 0) stop("No matching PDF found on: ", page_url)
  
  # If multiple, pick the one with the highest year in the filename
  yrs <- str_extract(pdfs, "(?<=fall-)\\d{4}") |> as.integer()
  pdfs[[which.max(yrs)]]
}

# 2. Define a function to download the PDF
download_pdf <- function(pdf_url) {
  tmp <- tempfile(fileext = ".pdf")
  request(pdf_url) |>
    req_user_agent("Mozilla/5.0") |>
    req_perform(path = tmp)
  tmp
}

# 3. Define a function to extract in-state undergrad percentage from Table 8

extract_in_state_table8 <- function(pdf_path) {
  
  txt_pages <- pdf_text(pdf_path)
  
  # Find the page containing "Enrollment: Table 8"
  page_idx <- which(str_detect(txt_pages, regex("Enrollment:\\s*Table\\s*8", ignore_case = TRUE)))
  
  if (length(page_idx) == 0) {
    stop("Could not find 'Enrollment: Table 8' in PDF.")
  }
  
  txt <- txt_pages[page_idx[1]]
  
  # Split into lines
  lines <- str_split(txt, "\n")[[1]]
  lines <- str_squish(lines)
  
  # Find the line that starts with "In-State"
  row_line <- lines[str_detect(lines, regex("^In-State\\b", ignore_case = TRUE))]
  
  if (length(row_line) == 0) {
    stop("Could not find 'In-State' row in Table 8.")
  }
  
  row_line <- row_line[1]
  
  # Extract all numbers from the line
  nums <- str_extract_all(row_line, "\\d+(?:\\.\\d+)?%")[[1]]
  
  if (length(nums) == 0) {
    stop("Found 'In-State' row but no numeric values.")
  }
  
  # Take rightmost number
  val <- tail(nums, 1)
}

# 4. Define a function to run over a vector of URLs
extract_all <- function(profiles) {
  
  map_dfr(seq_len(nrow(profiles)), function(i) {
    
    u <- profiles$url[i]
    y <- profiles$year[i]
    
    tryCatch({
      
      pdf_url  <- find_profile_pdf(u)
      pdf_path <- download_pdf(pdf_url)
      
      value_raw <- extract_in_state_table8(pdf_path)
      
      tibble(
        year      = y,
        page_url = u,
        pdf_url  = pdf_url,
        value_raw = value_raw,
        value_num = readr::parse_number(value_raw)
      )
      
    }, error = function(e) {
      
      tibble(
        year = y,
        page_url = u,
        pdf_url = NA_character_,
        value_raw = NA_character_,
        value_num = NA_real_,
        error = conditionMessage(e)
      )
      
    })
  })
}

# 5. Scrape the vector of URLs from UMass Institutional Research page

# Start with archived profiles (before current year)
archive_url <- "https://www.umassp.edu/document-archive?terms=&type=176"

page  <- read_html(archive_url)
nodes <- html_elements(page, "a")

urls <- tibble(
  text = html_text2(nodes),
  href = html_attr(nodes, "href")
) |>
  filter(!is.na(href), href != "") |>
  filter(str_detect(text, "\\b\\d{4}\\b\\s+Fall\\s+Student\\s+Profile\\b")) |>
  mutate(
    year = as.integer(str_extract(text, "\\d{4}")),
    url = url_absolute(href, archive_url)
  ) |>
  distinct(year, url)
  
  # Now add the current profile
urls <- urls |>  bind_rows(
    urls |> arrange(year) |>
      summarise(
        year = max(year) + 1,
        url = "https://www.umassp.edu/reports-and-initiatives/institutional-research"
      )
  )

# Run it all
umass_in_state <- extract_all(urls) |>
  filter(!is.na(value_num)) |>
  mutate(
    fiscal_year = year + 1,
    segment = "University of Massachusetts",
    in_state_pct = value_num / 100
  ) |>
  select(fiscal_year, segment, in_state_pct)

###

### Now we append the UMass in-state percentages to the state university and community college in-state percentages, which we get by applying the API import function to ANNUAL enrollment (the fall data doesn't include a residency field) to get the in-state undergraduate percentage for state universities and community colleges.

residency <- bind_rows(
  umass_in_state |>
    filter(fiscal_year >= 2015),
  
  api_get(
    url = annual,
    query = list(
      `$where`  = "fiscal_year >= '2015' AND enrollment_level = 'Undergraduate'",
      `$select` = "fiscal_year, segment, (residency = 'In-State') AS in_state, sum(fte) AS fte",
      `$group` = "fiscal_year, segment, (residency = 'In-State')"
    ),
    token = app_token
  )|>
    pivot_wider(id_cols = c(fiscal_year, segment), names_from = in_state, names_prefix = "in_state_", values_from = fte) |>
    mutate(in_state_pct = in_state_TRUE / (in_state_TRUE + in_state_FALSE)) |>
    select(fiscal_year, segment, in_state_pct)
)

# Use 2025 in-state percentages for 2026
residency <- residency |>
  bind_rows(
    residency |>
    arrange(desc(fiscal_year)) |>
      slice_max(fiscal_year, n = 3) |>
      mutate(fiscal_year = 2026)
  )

# Now we apply the import function to FALL enrollment
enrollment <- api_get(
  url = fall,
  query = list(
    `$where`  = "enrollment_level = 'Undergraduate'",
    `$select` = "year_fall_term AS year, segment, sum(student_cnt) AS headcount, sum(fte) AS fte",
    `$group` = "year, segment"
  ),
  token = app_token
  ) |>
  mutate(
    fiscal_year = year + 1
  ) |>
  select(fiscal_year, segment, fte) |>
  
  # Join with in-state residency percentages
  left_join(residency) |>
  mutate(fte_in_state = round(fte * in_state_pct)) |>
  select(fiscal_year, segment, fte_in_state)
  
# Add rows for totals (not by segment)
enrollment <- enrollment |> bind_rows(
    enrollment |> group_by(fiscal_year) |>
      summarise(
        segment = "ALL",
        fte_in_state = sum(fte_in_state)
        )
  )

### Now we have our final in-state undergraduate FTE enrollment by segment and fiscal year to use as denominators for other measures.

# Before we get spending, we'll import Implicit Price Deflators from BEA using the bea.R package.


beaKey <- Sys.getenv("BEA_API_KEY")

# Define request
ipd <- beaGet(
  list(
    UserID = beaKey,
    Method = "GetData",
    datasetname = "NIPA", # National Income and Product Accounts
    TableName = "T10109", # Table 1.1.9
    Frequency = "Q", # Quarterly
    Year = "X", # All years
    ResultFormat = "json"
  )
) |>
  
  # Keep only state and local govt spending
  filter(LineNumber == 26) |>
  
  # Make long, assign column names based on regex
  pivot_longer(
    cols = matches("^DataValue_\\d{4}(Q[1-4])?$"),
    names_to = c("year", "quarter"),
    names_pattern = "DataValue_(\\d{4})(?:Q([1-4]))?",
    values_to = "ipd"
  ) |>
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter),
    
    # Make synthetic July 1-June 30 fiscal years
    fiscal_year = if_else(quarter >= 3, year + 1, year)
  ) |>
  group_by(fiscal_year) |>
  
  # Average IPD by fiscal year
  summarise(
    ipd_fy = mean(ipd),
    qtrs = n()) |>
  filter(
    !is.na(fiscal_year),
    qtrs == 4) |>
  select(!qtrs) |>
  arrange(desc(fiscal_year))

# Calculating fiscal year IPDs means we don't have the complete current fiscal year, so we can use CBO projections from https://www.cbo.gov/data/budget-economic-data. (Per a conversation with Nancy Wagman, MassBudget's former Budget Browser director.) Under the heading "Economic Projections" download the most recent projections -- an Excel file.

# Read the rows from the header row (quarters) through the last row of data 
cbo_proj <- read_excel(
  "51135-2025-09-Economic-Projections.xlsx",
  sheet = "1. Quarterly",
  range = "A7:AD55",
  col_names = TRUE
) |>
  
  # Rename the first column
  rename_with(~ "gdp_comp", 1) |>
  
  # De-select the column headed "Units"
  select(!Units) |>
  
  # Choose the state and local govt row
  filter(gdp_comp == "State and local") |>
  
  # Make a long file with rows for each year and quarter
  pivot_longer(
    cols = matches("^\\d{4}Q[1-4]$"),
    names_to = c("year", "quarter"),
    names_pattern = "(\\d{4})Q([1-4])",
    values_to = "gdp"
  ) |>
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter),
    
    # Make synthetic July 1-June 30 fiscal years
    fiscal_year = if_else(quarter >= 3, year + 1, year)
  ) |>
  group_by(fiscal_year) |>
  
  # Average state and local govt GDP by fiscal year
  summarise(
    gdp_fy = mean(gdp),
    qtrs = n()) |>
  filter(
    !is.na(fiscal_year),
    
    # Keep only complete fiscal years
    qtrs == 4) |>
  select(!qtrs) |>
  
  # Calculate annual growth rates
  mutate(
    gdp_start = first(gdp_fy),
    num_fys = fiscal_year - first(fiscal_year),
    cagr = (gdp_fy / gdp_start) ^ (1 / num_fys)
  ) |>
  filter(
    fiscal_year == first(ipd$fiscal_year) + 1
  )

#Add the current fiscal year to IPD table
ipd <- ipd |> bind_rows(
  ipd |> arrange(fiscal_year) |>
    summarise(
      fiscal_year = max(fiscal_year) + 1,
      ipd_fy = last(ipd_fy) * cbo_proj$cagr
    ) 
) |> 
  arrange(desc(fiscal_year))

# Adjustment factors

# Pull most recent IPD
ipd_current <- ipd |>
  slice_max(fiscal_year, n = 1) |>
  pull(ipd_fy)

# Divide most recent IPD by each FY's IPD 
ipd <- ipd |>
  mutate(
    ipd_adjust = ipd_current / ipd_fy
  )

### Now we have our adjustment factors to get all spending in current dollars.

# Apply import function to get spending from CTHRU
spending <- api_get(
  url = cthru,
  query = list(
    `$where`  = "fiscal_year >= '2015'",
    `$select` = "fiscal_year, fund_number, fund_name, department_name, department_code, substring(appropriation_account_number, 1, 4) || '-' || substring(appropriation_account_number, 5, 4) AS line_item_code, appropriation_account_name, total_available_for_spending, total_expenses, balance_forward_prior_appropriation_continued, unexpended_reverted",
    `$limit` = 50000
  ),
  token = app_token
  ) |>
  mutate(
    fund_name = case_match(
      fund_name,
      "Transitional Escrow Frund" ~ "Transitional Escrow Fund",
      "Massachsuetts Toursim Fund" ~ "Massachusetts Tourism Fund",
      .default = fund_name
    )
  )


# Import line items by segment (community college, state university, UMass). This has to be manually updated as new line items are added.
segments <- read.csv("segments.csv")

# Define function to get a list of all line items with codes, names, and links from a given Budget Browser subcategory

get_line_items <- function(subcat_url) {
  page <- read_html(subcat_url)
  
  tibble(
    line_item_code = page |>
      html_nodes("#line_item_table_all td.line_item_link a") |>
      html_text(trim = TRUE),
    
    line_item_name = page |>
      html_nodes("#line_item_table_all td.line_item_name") |>
      html_text(trim = TRUE),
    
    line_item_link = page |>
      html_nodes("#line_item_table_all td.line_item_link a") %>%
      html_attr("href") %>%
      { paste0("https://massbudget.org", ., "&inflation=nominal") },  # make full URL
    
    line_item_link_ipd = page |>
      html_nodes("#line_item_table_all td.line_item_link a") %>%
      html_attr("href") %>%
      { paste0("https://massbudget.org", ., "&inflation=ipd") }
  )
}

# Apply the function to the higher ed subcategory

line_items <- get_line_items("https://massbudget.org/budget-browser/subcategory/?id=Higher+Education")

# Define function to parse JavaScript chunk in line item web page

parse_js_chunk <- function(js_chunk) {
  
  # 1. fiscal years -> defines N
  fiscal_years <- str_match_all(
    js_chunk,
    'options\\.xAxis\\.categories\\.push\\("([^"]+)"\\)'
  )[[1]][,2]
  
  N <- length(fiscal_years)
  
  # 2. all series.name values
  series_names <- str_match_all(
    js_chunk,
    'series\\.name\\s*=\\s*"([^"]+)"'
  )[[1]][,2]
  
  S <- length(series_names)   # number of series
  
  # 3. ALL values, flattened
  values <- str_match_all(
    js_chunk,
    'series\\.data\\.push\\((?:parseFloat\\()?(-?[0-9.]+)\\)?\\)'
  )[[1]][,2] |> as.numeric()
  
  K <- length(values)
  
  # sanity check: K must equal S*N or S*N*multiples
  if (K %% N != 0) {
    stop("Push count not divisible by fiscal year count")
  }
  
  # 4. reshape into S blocks of N values
  chunks <- split(values, rep(seq_len(K / N), each = N))
  
  # 5. build output
  out <- list()
  
  for (i in seq_along(series_names)) {
    name <- series_names[i]
    parts <- str_split_fixed(name, ":", 2)
    code <- trimws(parts[,1])
    item <- trimws(parts[,2])
    vals <- chunks[[i]]
    
    out[[i]] <- tibble(
      line_item_code = code,
      line_item_name = item,
      fiscal_year = fiscal_years,
      value = vals
    )
  }
  
  bind_rows(out)
}

scrape_mb <- function(line_item_orig, url) {
  
  # pull down HTML as text
  html_text <- read_html(url) |> as.character()
  
  # extract JS chunk:
  # from first options.xAxis.categories.push
  # up to but not including the first history_chart
  js_chunk <- str_extract(
    html_text,
    "options\\.xAxis\\.categories\\.push[\\s\\S]*?(?=history_chart)"
  )
  
  if (is.na(js_chunk)) {
    message("No js_chunk found for: ", url)
    return(tibble())
  }
  
  df <- parse_js_chunk(js_chunk)
  
  df |>
    mutate(
      line_item_orig = line_item_orig,
      line_item_link = url
    )
}

safe_scrape <- safely(scrape_mb, otherwise = tibble())

all_bb_data <- line_items |>
  select(line_item_orig = line_item_code,
         url = line_item_link) |>
  pmap(safe_scrape) |>
  map("result") |>
  bind_rows()

all_bb_data_ipd <- line_items |>
  select(line_item_orig = line_item_code,
         url = line_item_link_ipd) |>
  pmap(safe_scrape) |>
  map("result") |>
  bind_rows()

# We get all historical line items (line_item_code) for all line item searches (on line_item_orig), which results in a data set of length ([# of fiscal years] x [# of line_item_orig-line_item_code combos]), which includes duplicate values. But we really just want the minimum number of rows, which is a data set of length ([# of fiscal years] x [# of distinct line_item_orig]). The distinct line_item_orig is whatever one has a nonzero value in the most recent year.

line_items_to_use <- all_bb_data |>
  filter(value!=0) |> # Non-zero values
  mutate(yr = max(fiscal_year), .by = line_item_orig) |> # Most recent year by line_item_orig
  mutate(line_item_to_use = as.integer((line_item_orig == line_item_code) & fiscal_year == yr)) |> # If the row fiscal year equals the most recent year by line_item_orig AND line_item_code = line_item_orig, flag as the line item code to use
  mutate(line_item_orig_name = if_else(line_item_code == line_item_orig, line_item_name,NA)) |> # include the name
  filter(!is.na(line_item_orig_name)) |> # drop NA names
  mutate(keep_line_item = sum(line_item_to_use), .by = line_item_orig) |> # assign all line items their line item to use
  distinct(line_item_orig, line_item_orig_name, keep_line_item) # create a distinct data set

# Join original data to the list of line items to use (and not use)
final_bb_data <- all_bb_data |>
  left_join(line_items_to_use, by = "line_item_orig") |>
  filter(keep_line_item == 1, !is.na(line_item_orig_name)) |> # keep the ones we want
  select(!c(line_item_link, keep_line_item)) |>
  mutate(
    line_item_name = case_match(
      line_item_name,
      "Massachusetts College of Art" ~ "Massachusetts College of Art and Design",
      "MA College of Liberal Arts" ~ "Massachusetts College of Liberal Arts",
      .default = line_item_name
      ),
    fin_aid = line_item_code %in% c(
      "1596-2414", "1596-2418", "1596-2423", "1596-2433", "1596-2501", "1596-2502", "1599-6087", "7066-0016", "7066-0021", "7070-0065", "7070-0066", "8700-1150"),
    fair_share = line_item_code %in% c(
      "1596-2414", "1596-2416", "1596-2417", "1596-2418", "1596-2419", "1596-2420", "1596-2421", "1596-2423", "1596-2425", "1596-2426", "1596-2432", "1596-2433", "1596-2439", "1596-2501", "1596-2502", "1596-2525", "1596-2601", "1596-2602")
    ) |>
  left_join(
    all_bb_data_ipd |>
      rename(value_ipd = value) |>
      select(
        fiscal_year,
        line_item_code,
        line_item_orig,
        value_ipd),
    by = c("line_item_code", "fiscal_year", "line_item_orig"))
            
# Up to this point we have exactly replicated Budget Browser numbers. We're actually going to use CTHRU's "Total Available for Spending" instead, but we're going to apply Budget Browser's harmonized line item codes.

# Pull a distinct list of Budget Browser line items, harmonized to line_item_orig, since FY 2015 with nonzero values.

he_spending <- final_bb_data |>
  filter(
    fiscal_year >= "FY15" & value != 0
    ) |>
  distinct(
    line_item_code,
    line_item_orig,
    line_item_orig_name,
    fin_aid,
    fair_share
    ) |>
  left_join(
    spending |>
      group_by(
        fiscal_year,
        fund_number,
        fund_name,
        line_item_code
        ) |>
      summarise(
        across(
          total_available_for_spending:unexpended_reverted,
          \(x) sum(x, na.rm = TRUE)
        ),
        .groups = "drop"
      )
    ) |>
  group_by(
    line_item_orig,
    line_item_orig_name,
    fin_aid,
    fair_share,
    fiscal_year,
    fund_number,
    fund_name
  ) |>
  summarise(
    across(
      total_available_for_spending:unexpended_reverted,
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>

  # Exclude capital line items
  filter(!line_item_orig %in% c("1596-2417", "1596-2525")) |>
  
  # Apply inflation adjustment factors
  left_join(
    ipd |> select(fiscal_year, ipd_adjust)) |>
  mutate(
    across(
      total_available_for_spending:unexpended_reverted,
      ~ round(.x * ipd_adjust),
      .names = "{.col}_infl"
    )
  ) |>

# Join with segments
  left_join(segments)

### Tuition and fees

# Because tuition and fees are a consumer (rather than a government) purchase, we adjust with CPI, which we download from FRED.

cpi <- tq_get(
  "CPIAUCSL",
  get = "economic.data",
  from = "2014-01-01") |>
  
  # September only (since it's when school years begin; ideally we'd average over fiscal years)
  filter(month(date) == 9) |>
  mutate(fiscal_year = year(date) + 1) |>
  rename(cpi = price)

# Adjustment factors

# Pull most recent CPI
cpi_current <- cpi |>
  slice_max(fiscal_year, n = 1) |>
  pull(cpi)

# Divide most recent CPI by each year's CPI
cpi <- cpi |>
  mutate(
    cpi_adjust = cpi_current / cpi
  ) |>
  select(fiscal_year, cpi_adjust)

# Now we get tuition and fees

# Download last 10 fiscal years in Excel
url  <- "https://www.mass.edu/datacenter/tuition/documents/AppendixTuitionFeesWeight7.xlsx"
dest <- "AppendixTuitionFeesWeight7.xlsx"

download.file(url, dest, mode = "wb")

tuition_fees <- read_excel(dest, skip = 3) |>
  rename_with(~ "segment", 1) |>
  rename_with(~ "institution", 2) |>
  filter(
    segment %in% c(
      "University of Massachusetts",
      "State Universities",
      "Community Colleges",
      "Weighted Average")
    ) |>
  mutate(
    institution = gsub("\\*", "", institution),
    segment = case_match(
      segment,
      "Weighted Average" ~ institution,
      .default = segment
      ),
    institution = case_when(
      segment == institution  ~ "Weighted Average",
      .default = institution
    )
  ) |>
  filter(segment != "State Universities excludes MCAD and MMA") |>
  select(!'% Chg') |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "fiscal_year",
    values_to = "tuition_fees"
  ) |>
  mutate(fiscal_year = as.integer(fiscal_year)) |>

  # Because DHE only publishes last 10 years of tuition and fees, we have to get FY 2015 using the API import function
  bind_rows(
    tf15 <- 
    api_get(
      url = tf,
      query = list(
        `$where`  = "fiscal_year = '2015'",
        `$select` = "institution, segment, fiscal_year, tuition_and_mand_fees AS tuition_fees"
      ),
      token = app_token
    ) |>
  mutate(
    institution = gsub(" Weighted Average", "", institution),
    segment = case_match(
      segment,
      "Weighted Average" ~ institution,
      .default = segment
    ),
    institution = case_when(
      segment == institution  ~ "Weighted Average",
      .default = institution
    )
  ) |>
    filter(segment != "State Universities - excludes MCAD and MMA")
  ) |>
  arrange(segment, fiscal_year) |>
  left_join(cpi) |>
  mutate(tuition_fees_infl = round(tuition_fees * cpi_adjust))

tf <- tuition_fees |> filter(fiscal_year == 2021) |>
  select(segment, institution, tuition_fees_infl) |>
  rename(tf_21 = tuition_fees_infl) |>
  left_join(tuition_fees |> filter(fiscal_year == 2025) |>
    select(segment, institution, tuition_fees_infl)) |>
    rename(tf_25 = tuition_fees_infl) |>
  mutate(ann_chg = (tf_25 / tf_21) ^ (1/4) - 1)

# Summary tables

# Campus spending

he_spending_summary <- he_spending |>
  filter(campus == TRUE) |>
  group_by(fiscal_year) |>
  summarise(
    across(
      total_available_for_spending_infl:unexpended_reverted_infl,
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  left_join(
    enrollment |> filter(segment == "ALL")
  ) |>
  mutate(
    pct_spent = total_expenses_infl / total_available_for_spending_infl,
    per_student = total_available_for_spending_infl / fte_in_state
  )

# Define function to summarize higher education CAMPUS spending -- in total or by segment
summarize_he_spending <- function(seg_var) {
  
  # Create vector map of segment column names and segment names
  seg_map <- c(
    commcoll = "Community Colleges",
    stateu = "State Universities",
    umass = "University of Massachusetts"
  )

  # Always filter for campus spending
  he_spending_summary <- he_spending |>
    filter(campus %in% TRUE)  
  
  # If by segment, filter by segment
  if(!missing(seg_var)) {
    seg_col <- rlang::as_name(rlang::enquo(seg_var))
    seg_name <- unname(seg_map[seg_col])
    
    he_spending_summary <- he_spending_summary |>
      filter(.data[[seg_col]] %in% TRUE)
  } else {
    
    # Otherwise just set segment name to "ALL" for purpose of matching with enrollment
    seg_name <- "ALL"
  }
  
  # Always join campus spending totals to enrollment, whether in total or by segment
  he_spending_summary <- he_spending_summary |>
    group_by(fiscal_year) |>
    summarise(
      across(
        total_available_for_spending_infl:unexpended_reverted_infl,
        \(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(segment = seg_name) |>
    left_join(
      enrollment |> filter(segment == seg_name)
    )

  # If by segment, join to tuition and fees and calculate student share of higher ed revenues
  if(!missing(seg_var)) {
    he_spending_summary <- he_spending_summary |>
    left_join(
      tuition_fees |> filter(segment == seg_name & institution == "Weighted Average") |> select(fiscal_year, tuition_fees_infl)
    ) |>
      mutate(
        student_share = (tuition_fees_infl * fte_in_state) / (tuition_fees_infl * fte_in_state + total_available_for_spending_infl)
      )
  }

  # Always calculate percent of appropriation spent and per-student spending
  he_spending_summary <- he_spending_summary |>
    mutate(
      pct_spent = total_expenses_infl / total_available_for_spending_infl,
      per_student = total_available_for_spending_infl / fte_in_state
    ) |>
    as.data.frame()
  
  return(he_spending_summary)
}

cc_spending_summary <- summarize_he_spending(commcoll)
su_spending_summary <- summarize_he_spending(stateu)
um_spending_summary <- summarize_he_spending(umass)
he_spending_summary <- summarize_he_spending()

# Fair share

fair_share_summary <- he_spending |>
  filter(fair_share == TRUE) |>
  group_by(fiscal_year, campus) |>
  summarise(
    across(
      total_available_for_spending_infl:unexpended_reverted_infl,
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  left_join(
    enrollment |> filter(segment == "ALL")
  ) |>
  mutate(
    pct_spent = total_expenses_infl / total_available_for_spending_infl,
    per_student = total_available_for_spending_infl / fte_in_state
  )

# Financial aid

fin_aid_summary <- he_spending |>
  filter(fin_aid == TRUE) |>
  group_by(fiscal_year, fund_number) |>
  summarise(
    across(
      total_available_for_spending_infl:unexpended_reverted_infl,
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  left_join(
    enrollment |> filter(segment == "ALL")
  ) |>
  mutate(
    pct_spent = total_expenses_infl / total_available_for_spending_infl,
    per_student = total_available_for_spending_infl / fte_in_state
  )

fin_aid_summary |> select(
  fiscal_year,
  fund_number,
  total_available_for_spending_infl) |>
  pivot_wider(values_from = total_available_for_spending_infl, id_cols = fund_number, names_from = fiscal_year) |>
  mutate(
    fund = case_match(
      fund_number,
      "0010" ~ "General fund",
      "2005" ~ "COVID relief",
      "2017" ~ "Fair Share"
    )
  ) |>
  arrange(desc(fund)) |>
  select(fund, '2015':'2026') |>
  write.csv("fin_aid_summary.csv")

fin_aid_summary |> select(
  fiscal_year,
  fund_number,
  per_student) |>
  pivot_wider(values_from = per_student, id_cols = fund_number, names_from = fiscal_year) |>
  mutate(
    fund = case_match(
      fund_number,
      "0010" ~ "General fund",
      "2005" ~ "COVID relief",
      "2017" ~ "Fair Share"
    )
  ) |>
  arrange(desc(fund)) |>
  select(fund, '2015':'2026') |>
  write.csv("fin_aid_per_student.csv")

he_spending_summary |> ggplot(aes(x = fiscal_year, y = total_available_for_spending_infl)) +
  geom_col(fill = "brown") +
  scale_x_continuous(breaks = sort(unique(he_spending_summary$fiscal_year))) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6)) +
  coord_cartesian(ylim = c(0, 2*10^9)) +
  labs(
    title = "Funding for campus operations has been increasing",
    subtitle = "But it dropped slightly in FY2026",
    x = "Fiscal Year",
    y = "FY26$s, millions"
  ) +
  theme_minimal(base_size = 12)

he_chart <- function(df, yaxis, ylab, ttl, sttl, clr, y_labels = label_dollar()) {
  
  yrs <- sort(unique(df$fiscal_year))

  y_col <- as_name(ensym(yaxis))
  
  U <- ceiling(max(df[[y_col]], na.rm = TRUE) / 5000) * 5000
  
  df |> ggplot(aes(x = fiscal_year, y = {{yaxis}})) +
    geom_col(fill = clr) +
    scale_x_continuous(breaks = yrs) +
#    scale_y_continuous(labels = y_labels) +
    scale_y_continuous(breaks = c(0, U), labels = y_labels) +
    coord_cartesian(ylim = c(0, U)) +
    labs(
      title = ttl,
      subtitle = sttl,
      x = "Fiscal Year",
      y = ylab
    ) +
    theme_minimal(base_size = 12)
}

# charts <- imap(
#   list(
#     "All MA public higher ed campuses" = list(he_spending_summary, "orange"),
#     "All MA community colleges"        = list(cc_spending_summary, "pink"),
#     "All MA state universities"        = list(su_spending_summary, "lightgreen"),
#     "All UMass campuses"              = list(um_spending_summary, "lightblue")
#   ),
#   \(x, sttl) he_chart(x[[1]], per_student, "FY26$s",
#                       "Operational spending per undergraduate student",
#                       sttl, x[[2]])
# )

ps_all <- he_chart(he_spending_summary, per_student, "FY26$s", "Operational spending per undergraduate student", "All MA public higher ed campuses", "orange")

ps_um <- he_chart(um_spending_summary, per_student, "FY26$s", "", "All UMass campuses", "lightblue")

ps_su <- he_chart(su_spending_summary, per_student, "FY26$s", "", "All MA state universities", "lightgreen")

ps_cc <- he_chart(cc_spending_summary, per_student, "FY26$s", "", "All MA community colleges", "pink")

  (ps_all + theme(axis.title.x = element_blank(),
               axis.text.x  = element_blank())) /
  (ps_um + theme(axis.title.x = element_blank(),
               axis.text.x  = element_blank())) /
  (ps_su + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank())) /
  ps_cc +
  plot_layout(guides = "collect")

# Tuition and fee charts

tf_um <- tuition_fees |> filter(segment == "University of Massachusetts") |>
  he_chart(tuition_fees_infl, "FY26$s", "In-state tuition and mandatory fees", "University of Massachusetts", "lightblue")

tf_su <- tuition_fees |> filter(segment == "State Universities") |>
  he_chart(tuition_fees_infl, "FY26$s", "", "MA state universities", "lightgreen")

tf_cc <- tuition_fees |> filter(segment == "Community Colleges") |>
  he_chart(tuition_fees_infl, "FY26$s", "", "MA community colleges", "pink")

(tf_um + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank())) /
  (tf_su + theme(axis.title.x = element_blank(),
                axis.text.x  = element_blank())) /
  tf_cc +
  plot_layout(guides = "collect")

enroll_um <- enrollment |> filter(segment == "University of Massachusetts") |>
  he_chart(fte_in_state, "", "Full-time equivalent enrollment among MA residents", "University of Massachusetts", "lightblue", y_labels = label_comma())

enroll_su <- enrollment |> filter(segment == "State Universities") |>
  he_chart(fte_in_state, "", "", "MA state universities", "lightgreen", y_labels = label_comma())

enroll_cc <- enrollment |> filter(segment == "Community Colleges") |>
  he_chart(fte_in_state, "", "", "MA community colleges", "pink", y_labels = label_comma())

(enroll_um + theme(axis.title.x = element_blank(),
               axis.text.x  = element_blank())) /
  (enroll_su + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank())) /
  enroll_cc +
  plot_layout(guides = "collect")


ggplot(aes(x = fiscal_year, y = fte_in_state)) +
  geom_col(fill = "pink") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(enrollment$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_comma()) +
  coord_cartesian(ylim = c(0, 60000)) +
  labs(
    title = "Community college enrollment is rebounding",
#    subtitle = "In-state tuition and mandatory fees, weighted average across campuses",
    x = "Fiscal Year",
    y = "MA resident FTE enrollment"
  ) +
  theme_minimal(base_size = 14)

su_spending_summary |> ggplot(aes(x = fiscal_year, y = per_student)) +
  geom_col(fill = "lightgreen") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(su_spending_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  coord_cartesian(ylim = c(0, 20000)) +
  labs(
    title = "Per student, state university funding has been rising steadily",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme_minimal(base_size = 14)

tuition_fees |> filter(segment == "State Universities") |>
  ggplot(aes(x = fiscal_year, y = tuition_fees_infl)) +
  geom_col(fill = "lightgreen") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(tuition_fees$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  coord_cartesian(ylim = c(0, 15000)) +
  labs(
    title = "State university tuition + fees are decreasing in real dollars",
    subtitle = "In-state tuition and mandatory fees, weighted average across campuses",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme_minimal(base_size = 14)

enrollment |> filter(segment == "State Universities") |>
  ggplot(aes(x = fiscal_year, y = fte_in_state)) +
  geom_col(fill = "lightgreen") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(enrollment$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_comma()) +
  coord_cartesian(ylim = c(0, 40000)) +
  labs(
    title = "State university enrollment is leveling off",
    x = "Fiscal Year",
    y = "MA resident FTE enrollment"
  ) +
  theme_minimal(base_size = 14)

um_spending_summary |> ggplot(aes(x = fiscal_year, y = per_student)) +
  geom_col(fill = "lightblue") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(um_spending_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Per student, UMass funding has been rising steadily",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme_minimal(base_size = 14)

tuition_fees |> filter(segment == "University of Massachusetts") |>
  ggplot(aes(x = fiscal_year, y = tuition_fees_infl)) +
  geom_col(fill = "lightblue") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(tuition_fees$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "UMass tuition + fees are decreasing in real dollars",
    subtitle = "In-state tuition and mandatory fees, weighted average across campuses",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme_minimal(base_size = 14)

enrollment |> filter(segment == "University of Massachusetts") |>
  ggplot(aes(x = fiscal_year, y = fte_in_state)) +
  geom_col(fill = "lightblue") +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(enrollment$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_comma()) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "UMass enrollment is decreasing slightly",
    x = "Fiscal Year",
    y = "MA resident FTE enrollment"
  ) +
  theme_minimal(base_size = 14)


p_total <-
  fin_aid_summary |>
  ggplot(aes(x = fiscal_year, y = total_available_for_spending_infl, fill = fund_number)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(fin_aid_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6)) +
  scale_fill_discrete(
    name = "Fund",
    breaks = c("0010", "2017", "2005"),
    labels = c("General fund", "Fair Share", "COVID response")
  ) +
  coord_cartesian(xlim = c(2015,2026), ylim = c(0, 5*10^8)) +
  labs(
    title = "Massachusetts state financial aid",
    x = "Fiscal Year",
    y = "FY26$s, millions"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank())

p_per_student <-
  fin_aid_summary |>
  ggplot(aes(x = fiscal_year, y = per_student, fill = fund_number)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(fin_aid_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_discrete(
    name = "Fund",
    breaks = c("0010", "2017", "2005"),
    labels = c("General fund", "Fair Share", "COVID response")
  ) +
  coord_cartesian(xlim = c(2015,2026), ylim = c(0, 5000)) +
  labs(
    title = "Massachusetts state financial aid per student",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank())

(p_total + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank())) /
  p_per_student +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fin_aid_summary |>
  ggplot(aes(x=fiscal_year, y=total_available_for_spending_infl, fill=fund_number)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(fin_aid_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6)) +
  scale_fill_discrete(
    name = "Fund",
    breaks = c("0010", "2005", "2017"),
    labels = c("General fund", "COVID response", "Fair Share")
  ) +
  coord_cartesian(xlim = c(2015,2026), ylim = c(0, 5*10^8)) +
  labs(
    title = "Massachusetts state financial aid",
    x = "Fiscal Year",
    y = "FY26$s, millions"
  ) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank())

fin_aid_summary |>
  ggplot(aes(x=fiscal_year, y=per_student, fill=fund_number)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(fin_aid_summary$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_discrete(
    name = "Fund",
    breaks = c("0010", "2005", "2017"),
    labels = c("General fund", "COVID response", "Fair Share")
  ) +
  coord_cartesian(xlim = c(2015,2026), ylim = c(0, 5000)) +
  labs(
    title = "Massachusetts state financial aid per student",
    x = "Fiscal Year",
    y = "FY26$s"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank())

student_share <- bind_rows(
  cc_spending_summary |> select(fiscal_year, segment, student_share),
  su_spending_summary |> select(fiscal_year, segment, student_share),
  um_spending_summary |> select(fiscal_year, segment, student_share)
) |>
  filter(!is.na(student_share))

tuition_fees

student_share |> ggplot(aes(
  x = fiscal_year,
  y = student_share,
  color = segment
  )) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = pretty_breaks(n = length(unique(student_share$fiscal_year)))
  ) +
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0.3, 0.6)) +
  labs(
    title = "Students' share of higher ed costs have decreased sharply",
    subtitle = "But it ticked up at community colleges in FY25",
    x = "Fiscal Year",
    y = "Tuition+fees ÷ higher ed revenue"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tuition_fees |>
  filter(segment == "Community Colleges") |>
  ggplot(aes(
  x = fiscal_year,
  y = tuition_fees_infl,
  color = segment,
  group = institution
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = sort(unique(tuition_fees$fiscal_year)),
    expand = expansion(mult = 0)
    ) +
  scale_y_continuous(labels = label_dollar()) +
  #coord_cartesian(ylim = c(0.3, 0.6)) +
  labs(
#    title = "Students' share of higher ed costs have decreased sharply",
#    subtitle = "But it ticked up at community colleges in FY25",
    x = "Fiscal Year",
    y = "Tuition + fees"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Make Excel file

tables <- list(
  fin_aid_summary,
  tuition_fees,
  enrollment,
  he_spending_summary,
  cc_spending_summary,
  su_spending_summary,
  um_spending_summary
)

tables <- mget(c(
  "fin_aid_summary",
  "tuition_fees",
  "enrollment",
  "he_spending_summary",
  "cc_spending_summary",
  "su_spending_summary",
  "um_spending_summary"
))

wb <- createWorkbook()

for (nm in names(tables)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, tables[[nm]])
}

saveWorkbook(wb, "jbt_tables.xlsx", overwrite = TRUE)