# Read in and light munging on the Raleigh census data. 

library(data.table) 
library(magrittr)
library(readxl)
library(zoo)
library(tidyr)


#  Read in data. ----------------------------------------------------------
income_work <- read_excel("raleigh/data/census/wake_zips_income_and_work.xls", 
                          skip = 6) 

# Clean column names, remove NA rows. -------------------------------------

# Fill in column names that are blank. 
new_cols <- gsub("ZCTA5 ", "", colnames(income_work))
new_cols[nchar(new_cols) == 0] <- NA
new_cols <- na.locf(new_cols)  # impute zip values (package zoo)

# Update column names. 
new_cols <- paste0(new_cols, "___", gsub(" ", "_", tolower(income_work[1,])))
new_cols[1] <- "subject"  # reset first (it converted NA to "NA")
setnames(income_work, new_cols)
income_work <- data.table(income_work)

# Remove rows that are all NA (or mostly NA like it is a title). 
almost_all_NA_rows <- apply(income_work, 1, function(x) length(which(is.na(x))))
# Compute "almost all" as greater than 90% of the row is NA.
almost_all_NA_rows <- which(almost_all_NA_rows > (0.9 * ncol(income_work)))

# Remove the rows. 
income_work <- income_work[-almost_all_NA_rows]
income_work <- income_work[-1]  # the first row was incorporated into column name


# Reshape data. -----------------------------------------------------------
melt_income_work <- melt(income_work, id.vars = "subject")
long_income_work <- separate(melt_income_work, variable, into = c("zip", "measure"), sep = "___")
long_income_work <- subset(long_income_work, measure %in% c("estimate", "percent"))
long_income_work[ , length(which(is.na(value)))]  # no NAs, but lots of "(X)"
long_income_work[value == "(X)", value := NA]
long_income_work[ , subject := gsub("^[ ]+", "", subject)]  # remove leading whitespace

# Select columns. 
cols <- c("Median household income (dollars)", "Median family income (dollars)",
  "With Supplemental Security Income",
  "With Food Stamp/SNAP benefits in the past 12 months", 
  "Per capita income (dollars)", 
  "All families")  # All families is % families below poverty line
# Subset rows to only relevant variables ('subjects' in census terminology). 
long_income_work <- long_income_work[subject %in% cols, ]
long_income_work <- long_income_work[!is.na(value), ]
# Re-name certain columns. 
long_income_work[subject == "All families", subject := "Percent Families Below Poverty Line"]
long_income_work[grepl("^With", subject) & measure == "percent", 
                 subject := gsub("^With", "Percent", subject)]
long_income_work <- long_income_work[!grepl("^With", subject)]  # these are counts, not needed
long_income_work[ , subject := gsub("\\(|\\)", "", subject)]  # remove parens
long_income_work[ , subject := gsub(" ", "_", subject)]

# Convert numeric values in the `value` column. 
long_income_work[measure == "estimate" , value := gsub(",", "", value)]
long_income_work[measure == "percent", value := gsub("%", "", value)]

# Reshape the long data to wide format. 
cleaned_income_work <- dcast(long_income_work, zip ~ subject)
numeric_cols <- c("Median_family_income_dollars", 
                  "Median_household_income_dollars", 
                  "Per_capita_income_dollars", 
                  "Percent_Families_Below_Poverty_Line", 
                  "Percent_Food_Stamp/SNAP_benefits_in_the_past_12_months", 
                  "Percent_Supplemental_Security_Income")
cleaned_income_work[ , (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

# Correlation matrix. 
cor(subset(cleaned_income_work, select = numeric_cols))

# Write out data. 
write_csv(cleaned_income_work, path = "raleigh/data/census/census.csv")

