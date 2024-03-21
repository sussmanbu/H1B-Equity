library(readxl)
library(dplyr)

file_path <- "dataset/FYs97-22_NIVDetailTable.xlsx"

fy22 <- read_excel(file_path, sheet = 'FY22') %>%
  filter(!is.na(`Fiscal Year 2022`))

curr_continent <- fy22[1, 1]
fy22$Continent <- NA

for (i in 1:(nrow(fy22) - 1)) {
  curr_row <- fy22[i, 1]
  next_row <- fy22[i + 1, 1]
  if (curr_row != curr_continent & !grepl("^Totals for ", curr_row)) {
    fy22[i, 'Continent'] <- curr_continent
  } else if (grepl("^Totals for ", curr_row)) {
    curr_continent <- next_row
  }
}
processed_data <- fy22 %>%
  filter(!is.na(Continent)) %>%
  select(Country=1, Continent)

for (sheet_name in sheet_names) {
  sheet_data <- read_excel(file_path, sheet = sheet_name)
  if (sheet_name == 'FY22') {
    colnames(sheet_data)[30] <- "H-1B"
  }
  
  sheet_data <- sheet_data %>%
    select(Country = 1, `H-1B`) %>%
    group_by(Country) %>%
    summarise(`H-1B` = sum(`H-1B`, na.rm = TRUE)) %>%
    ungroup()
  
  colnames(sheet_data)[2] <- sheet_name
  
  if (is.null(processed_data)) {
    processed_data <- sheet_data
  } else {
    processed_data <- left_join(processed_data, sheet_data, by = "Country")
  }
}

saveRDS(processed_data, file = "dataset/cleaned_h1b_data.rds")