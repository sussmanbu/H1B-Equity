library(readxl)
library(dplyr)

file_path <- ""
sheet_names <- excel_sheets(file_path)

fy22 <- read_excel(file_path, sheet = 'FY22') %>%
  filter(!is.na(`Fiscal Year 2022`) & `Fiscal Year 2022` != "United Nations Laissez-Passer")

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


# #Load Perm Data
filepath <- 'dataset-ignore/PERM_FY2019.xlsx'
fy19 <- read_excel(filepath)
filepath <- 'dataset-ignore/PERM_Disclosure_Data_FY2020.xlsx'
fy20 <- read_excel(filepath)
filepath <- 'dataset-ignore/PERM_Disclosure_Data_FY2021.xlsx'
fy21 <- read_excel(filepath)
filepath <- 'dataset-ignore/PERM_Disclosure_Data_FY2022_Q4.xlsx'
fy22 <- read_excel(filepath)
filepath <- 'dataset-ignore/PERM_Disclosure_Data_FY2023_Q4.xlsx'
fy23 <- read_excel(filepath)

#Filter by H-1B
fy19 <- fy19 |> 
  filter(CLASS_OF_ADMISSION == 'H-1B')
fy20 <- fy20 |> 
  filter(CLASS_OF_ADMISSION == 'H-1B')
fy21 <- fy21 |> 
  filter(CLASS_OF_ADMISSION == 'H-1B')
fy22 <- fy22 |> 
  filter(CLASS_OF_ADMISSION == 'H-1B')
fy23 <- fy23 |> 
  filter(CLASS_OF_ADMISSION == 'H-1B')

#Only keep relevant columns(some years differ in column titles)
relevant_columns <- c("CASE_NUMBER", "CASE_STATUS", "DECISION_DATE", "EMPLOYER_NAME", "WAGE_OFFERED_FROM_9089", "COUNTRY_OF_CITIZENSHIP", "FOREIGN_WORKER_INFO_EDUCATION")
fy19_relevant <- fy19[, relevant_columns]
fy19_relevant <- fy19_relevant |> rename(WAGE_OFFER_FROM = WAGE_OFFERED_FROM_9089)
fy19_relevant <- fy19_relevant |> rename(FOREIGN_WORKER_EDUCATION = FOREIGN_WORKER_INFO_EDUCATION)
fy19_relevant$DECISION_DATE <- as.Date(fy19_relevant$DECISION_DATE)

relevant_columns <- c("CASE_NUMBER", "CASE_STATUS", "DECISION_DATE", "EMPLOYER_NAME", "WAGE_OFFER_FROM",  "COUNTRY_OF_CITIZENSHIP", "FOREIGN_WORKER_EDUCATION")
fy20_relevant <- fy20[, relevant_columns]
fy20_relevant$DECISION_DATE <- as.Date(fy20_relevant$DECISION_DATE)

relevant_columns <- c("CASE_NUMBER", "CASE_STATUS", "DECISION_DATE", "EMPLOYER_NAME", "WAGE_OFFER_FROM",  "COUNTRY_OF_CITIZENSHIP", "FOREIGN_WORKER_EDUCATION")
fy21_relevant <- fy21[, relevant_columns]
# fy21_relevant <- fy21_relevant |> rename(WAGE_OFFER_FROM = WAGE_OFFERED_FROM_9089)
fy21_relevant$DECISION_DATE <- as.Date(fy21_relevant$DECISION_DATE)

relevant_columns <- c("CASE_NUMBER", "CASE_STATUS", "DECISION_DATE", "EMPLOYER_NAME", "WAGE_OFFER_FROM", "COUNTRY_OF_CITIZENSHIP", "FOREIGN_WORKER_EDUCATION")
fy22_relevant <- fy22[, relevant_columns]
# fy21_relevant <- fy21_relevant |> rename(WAGE_OFFER_FROM = WAGE_OFFERED_FROM_9089)
fy22_relevant$DECISION_DATE <- as.Date(fy22_relevant$DECISION_DATE)

relevant_columns <- c("CASE_NUMBER", "CASE_STATUS", "DECISION_DATE", "EMPLOYER_NAME", "WAGE_OFFER_FROM",  "COUNTRY_OF_CITIZENSHIP", "FOREIGN_WORKER_EDUCATION")
fy23_relevant <- fy23[, relevant_columns]
# fy21_relevant <- fy21_relevant |> rename(WAGE_OFFER_FROM = WAGE_OFFERED_FROM_9089)
fy23_relevant$DECISION_DATE <- as.Date(fy23_relevant$DECISION_DATE)

#Extract year/month/etc
fy19_relevant$month <- month(fy19_relevant$DECISION_DATE)
fy19_relevant$year <- year(fy19_relevant$DECISION_DATE)
fy19_relevant$year_month <- paste(fy19_relevant$year, fy19_relevant$month, sep = "-")

fy20_relevant$month <- month(fy20_relevant$DECISION_DATE)
fy20_relevant$year <- year(fy20_relevant$DECISION_DATE)
fy20_relevant$year_month <- paste(fy20_relevant$year, fy20_relevant$month, sep = "-")

fy21_relevant$month <- month(fy21_relevant$DECISION_DATE)
fy21_relevant$year <- year(fy21_relevant$DECISION_DATE)
fy21_relevant$year_month <- paste(fy21_relevant$year, fy21_relevant$month, sep = "-")

fy22_relevant$month <- month(fy22_relevant$DECISION_DATE)
fy22_relevant$year <- year(fy22_relevant$DECISION_DATE)
fy22_relevant$year_month <- paste(fy22_relevant$year, fy22_relevant$month, sep = "-")

fy23_relevant$month <- month(fy23_relevant$DECISION_DATE)
fy23_relevant$year <- year(fy23_relevant$DECISION_DATE)
fy23_relevant$year_month <- paste(fy23_relevant$year, fy23_relevant$month, sep = "-")

#combine everything
combined_19_23 <- rbind(fy19_relevant, fy20_relevant)
combined_19_23 <- rbind(combined_19_23, fy21_relevant)
combined_19_23 <- rbind(combined_19_23, fy22_relevant)
combined_19_23 <- rbind(combined_19_23, fy23_relevant)

#write to csv
write.csv(combined_19_23, file = "dataset/combined_perm_19_23.csv", row.names = FALSE)

population_data <- read.csv("dataset/IMF_data/imf_population.csv", header=TRUE)
colnames(population_data) <- population_data[1, ]
population_data <- population_data[-1, ]

pattern <- "(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023)"

population_data <- population_data |>
  mutate(year = as.numeric(str_extract(Region, pattern))) |>
  filter(Year != '1996')

write.csv(population_data, file = "dataset/IMF_data/imf_population.csv", row.names = FALSE)
