---
title: "Value of apartments sold in market transactions in Lesser Poland"
output:
  html_document:
    df_print: paged
  pdf_document: default
  github_document:
    html_preview: false
warnings: no
---
<b>Author:</b> Przemyslaw Niedziela (przemyslaw.niedziela98@gmail.com) <br> 
<b>Date:</b> Sept 2024 <br>

```{r setup, include=FALSE} 
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```


```{r}
library(dplyr)
library(tidyr)
library(data.table)
library(gplots)
library(car)
library(stringr)
library(lmtest)
library(plm)
library(panelAR)
```


Reading data 
```{r}
population_density <- read.csv("Population density per 1km2.csv", sep= ";")
nr_residential_units_sold <- read.csv("Number of residential units sold in market transactions.csv", sep = ";")
nr_apartments_put_into_use <- read.csv("Number of apartments put into use.csv", sep = ";")
area_of_appartments_sold <- read.csv("Usable area of apartments sold in market transactions.csv", sep = ";")
average_gross_monthly_wages <- read.csv("Average gross monthly wages.csv", sep = ";")
average_price_per_1m2_apartments_sold <- read.csv("Average price per 1 m2 of apartments sold in market transactions.csv", sep = ";")
value_of_apartments_sold <- read.csv("Value of apartments sold in market transactions.csv", sep = ";") 
```


Processing datasets
```{r}
transpose_df <- function(df) {
  transposed_df <- transpose(df)
  colnames(transposed_df) <- df$Nazwa
  rownames(transposed_df) <- colnames(df)
  transposed_df <- transposed_df[, !duplicated(as.list(transposed_df))]
  transposed_df <- transposed_df %>%
      filter(!grepl("kod|Kod|Nazwa", rownames(transposed_df)))
  return(transposed_df)
}

get_quarter <- function(df) {
  df$year <- sub(".*\\.(\\d{4}).*", "\\1", rownames(df))
  df$quarter <- sub("X(\\d).*", "Q\\1", rownames(df))
  df$time <- paste(df$year, df$quarter)
  df <- df[, !(names(df) %in% c("year", "quarter"))]
  rownames(df) <- NULL
  return(df)
}

filter_for_basic <- function(df) {
  df <- df %>% 
    filter(grepl("ogółem.ogółem", rownames(df)))
  return(df)
}

expand_yearly_to_quartely <- function(df) {
  df$year <- sub(".*(\\d{4}).*", "\\1", rownames(df))
  df_long <- df[rep(1:nrow(df), each = 4), ]
  df_long$quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), times = nrow(df))
  df_long$time <- paste(df_long$year, df_long$quarter)
  df_long <- df_long[, !(names(df_long) %in% c("year", "quarter"))]
  rownames(df_long) <- NULL
  return(df_long)
}

value_of_apartments_sold_processed <- get_quarter(filter_for_basic(transpose_df(value_of_apartments_sold)))
average_price_per_1m2_apartments_sold_processed <- get_quarter(filter_for_basic(transpose_df(average_price_per_1m2_apartments_sold)))
nr_residential_units_sold_processed <- get_quarter(filter_for_basic(transpose_df(nr_residential_units_sold)))
area_of_appartments_sold_processed <- get_quarter(filter_for_basic(transpose_df(area_of_appartments_sold)))

population_density_processed <- expand_yearly_to_quartely(transpose_df(population_density))
average_gross_monthly_wages_processed <- expand_yearly_to_quartely(transpose_df(average_gross_monthly_wages))

nr_apartments_put_into_use_processed <- transpose_df(nr_apartments_put_into_use)
nr_apartments_put_into_use_processed$time <- NA 
nr_apartments_put_into_use_processed$time <- ifelse(grepl("X1.kwartał", rownames(nr_apartments_put_into_use_processed)), 
                sub(".*(\\d{4}).*", "\\1 Q1", rownames(nr_apartments_put_into_use_processed)),
         ifelse(grepl("pierwsze.półrocze", rownames(nr_apartments_put_into_use_processed)), 
                sub(".*(\\d{4}).*", "\\1 Q2", rownames(nr_apartments_put_into_use_processed)),
         ifelse(grepl("X1.3.kwartały", rownames(nr_apartments_put_into_use_processed)), 
                sub(".*(\\d{4}).*", "\\1 Q3", rownames(nr_apartments_put_into_use_processed)),
         ifelse(grepl("rok.ogółem", rownames(nr_apartments_put_into_use_processed)), 
                sub(".*(\\d{4}).*", "\\1 Q4", rownames(nr_apartments_put_into_use_processed)), NA))))
nr_apartments_put_into_use_processed <- nr_apartments_put_into_use_processed[!is.na(nr_apartments_put_into_use_processed$time), ]

adjust_quarters <- function(df) {
  df$year <- as.numeric(sub("Q[1-4]", "", df$time))
  df$quarter <- sub(".*(Q[1-4])", "\\1", df$time)
  df[, 1:(ncol(df)-3)] <- lapply(df[, 1:(ncol(df)-3)], function(x) as.numeric(as.character(x)))
  
  unique_years <- unique(df$year)
  numeric_cols <- sapply(df, is.numeric)
  for (year in unique_years) {
    Q1_idx <- which(df$quarter == "Q1" & df$year == year)
    Q2_idx <- which(df$quarter == "Q2" & df$year == year)
    Q3_idx <- which(df$quarter == "Q3" & df$year == year)
    Q4_idx <- which(df$quarter == "Q4" & df$year == year)
    
    if (length(Q2_idx) > 0 && length(Q1_idx) > 0) {
      df[Q2_idx, numeric_cols] <- df[Q2_idx, numeric_cols] - df[Q1_idx, numeric_cols]
    }
    if (length(Q3_idx) > 0 && length(Q1_idx) > 0 && length(Q2_idx) > 0) {
      df[Q3_idx, numeric_cols] <- df[Q3_idx, numeric_cols] - df[Q2_idx, numeric_cols] - df[Q1_idx, numeric_cols]
    }
    if (length(Q4_idx) > 0 && length(Q1_idx) > 0 && length(Q2_idx) > 0 && length(Q3_idx) > 0) {
      df[Q4_idx, numeric_cols] <- df[Q4_idx, numeric_cols] - df[Q3_idx, numeric_cols] - df[Q2_idx, numeric_cols] - df[Q1_idx, numeric_cols]
    }
  }
  df <- df[, !names(df) %in% c("year", "quarter")]
  return(df)
}

nr_apartments_put_into_use_processed_totals <- adjust_quarters(nr_apartments_put_into_use_processed)
```


Merging everything together 
```{r}
list_of_dfs <- list(population_density = population_density_processed, 
                    nr_residential_units_sold = nr_residential_units_sold_processed, 
                    nr_apartments_put_into_use = nr_apartments_put_into_use_processed_totals,
                    area_of_appartments_sold = area_of_appartments_sold_processed,
                    average_gross_monthly_wages = average_gross_monthly_wages_processed,
                    average_price_per_1m2_apartments_sold = average_price_per_1m2_apartments_sold_processed, 
                    value_of_apartments_sold = value_of_apartments_sold_processed)

find_common_time_and_location <- function(list_of_dfs) {
  common_time <- Reduce(intersect, lapply(list_of_dfs, function(df) df$time))
  common_locations <- Reduce(intersect, lapply(list_of_dfs, function(df) colnames(df)[-1]))
  return(list(common_time = common_time, common_locations = common_locations))
}

filter_dfs_by_common_time_and_location <- function(list_of_dfs, common_time, common_locations) {
  filtered_list_of_dfs <- lapply(list_of_dfs, function(df) {
    df_filtered <- df[df$time %in% common_time, ]
    df_filtered <- df_filtered[, c("time", common_locations)]
    
    return(df_filtered)
  })
  
  return(filtered_list_of_dfs)
}

common_values <- find_common_time_and_location(list_of_dfs)
filtered_list_of_dfs <- filter_dfs_by_common_time_and_location(list_of_dfs, 
                                                               common_values$common_time, 
                                                               common_values$common_locations)
merged_df <- data.frame(time = character(),
                        location = character(),
                        population_density = numeric(), 
                        nr_residential_units_sold = numeric(), 
                        nr_apartments_put_into_use = numeric(),
                        area_of_appartments_sold = numeric(),
                        average_gross_monthly_wages = numeric(),
                        average_price_per_1m2_apartments_sold = numeric(), 
                        value_of_apartments_sold = numeric(),
                        stringsAsFactors = FALSE)

for (time in common_values$common_time) {
  for (location in common_values$common_locations) {
    values <- c()
    for (var_name in names(filtered_list_of_dfs)) {
      df <- filtered_list_of_dfs[[var_name]]
        loc_cols <- colnames(df)[-1] 
        value <- df[df$time == time, location]
        values <- c(values, value)
    }
    new_row <- c(time = time, location = location, values)
    merged_df <- rbind(merged_df, new_row)
  }
}
colnames(merged_df) <- c("time", 
                         "location", 
                         "population_density", 
                         "nr_residential_units_sold", 
                         "nr_apartments_put_into_use", 
                         "area_of_appartments_sold", 
                         "average_gross_monthly_wages", 
                         "average_price_per_1m2_apartments_sold",
                         "value_of_apartments_sold")
merged_df <- merged_df[merged_df$location != "time", ]

for (col in names(merged_df)[3:ncol(merged_df)]) {
  merged_df[[col]] <- as.numeric(gsub(",", ".", merged_df[[col]]))
}
merged_df <- na.omit(merged_df)
```

Time series visualisation for 6 biggest locations
```{r}
top_locations <- merged_df %>%
  group_by(location) %>%
  summarize(total_sold = sum(value_of_apartments_sold, na.rm = TRUE)) %>%
  top_n(6, total_sold) %>%
  pull(location)

visualisation_df <- merged_df %>%
  filter(location %in% top_locations) %>%
  mutate(year = as.numeric(sub(" Q[1-4]", "", time)),
         quarter = as.numeric(sub(".*Q", "", time)),
         year_numeric = year + 1/quarter - 0.25)

for (col in names(visualisation_df)[3:(ncol(visualisation_df)-3)]) {
  y <- visualisation_df[[col]]
  scatterplot(y~year_numeric|location, boxplots=F, smooth=T, regLine=FALSE, ylab = col, data=visualisation_df)
}
```

cross-section heterogeneity
```{r}
plotmeans(value_of_apartments_sold ~ location, main="cross-section heterogeneity", data=merged_df, bars=F)
```

Time effects 
```{r}
time_effects_df <- merged_df %>%
  mutate(year = as.numeric(sub(" Q[1-4]", "", time)),
         quarter = as.numeric(sub(".*Q", "", time)),
         year_numeric = year + 1/quarter - 0.25)

plotmeans(value_of_apartments_sold ~ year, main="Time effects", data=time_effects_df)
```

Simple OLS - all variables
```{r}
ols<- lm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    nr_apartments_put_into_use + 
                                    area_of_appartments_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold, 
         data=merged_df)

summary(ols)
```

Colinearity check 
```{r}
vif(ols)
```

Simple OLS - subset
```{r}
ols_subset<- lm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold, 
         data=merged_df)

summary(ols_subset)
```

Colinearity check 
```{r}
vif(ols_subset)
```

FE Model 
```{r}
fixed <-plm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold, 
         data=merged_df, index=c("location", "time"), model="within")
summary(fixed)
```

```{r}
fixef(fixed)
```

```{r}
summary(fixef(fixed, type = "dmean"))
```

```{r}
pFtest(fixed, ols_subset)
```

RE model 
```{r}
random <-plm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold, 
         data=merged_df, index=c("location", "time"), model="random")
summary(random)
```


Hausman Test
```{r}
phtest(fixed, random)
```

Two-way FE
```{r}
fixed.time <- plm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold + 
                                    factor(time), 
                  data=merged_df, index=c("location", "time"), model="within")

summary(fixed.time)
```

```{r}
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))
```

Pooling Model
```{r}
pool <-plm(value_of_apartments_sold ~ population_density + 
                                    nr_residential_units_sold + 
                                    average_gross_monthly_wages + 
                                    average_price_per_1m2_apartments_sold, 
           data=merged_df, index=c("location", "time"), model="pooling")
summary(pool)
```

```{r}
plmtest(pool, type=c("bp"))
```

Autocorrelation test 
```{r}
pwartest(fixed.time)
```
```{r}
bgtest(fixed.time)
```

```{r}
pbgtest(fixed.time, order=1)
```
Heteroskedasticity tests 
```{r}
plmtest(fixed.time, type = "bp")
```

```{r}
bptest(fixed.time)
```

```{r}
length(fitted(fixed.time) + I(fitted(fixed.time)^2)) 
```

Using panelAR correction to remove autocorrelation and heteroskedasticity 
```{r}
merged_df$year <- as.integer(str_sub(merged_df$time, 1, 4)) 
merged_df$quarter <- as.integer(str_sub(merged_df$time, 7, 7))  
merged_df$time_index <- (merged_df$year - min(merged_df$year)) * 4 + merged_df$quarter

corrected_model <- panelAR(value_of_apartments_sold ~ 
                                     nr_residential_units_sold + 
                                     average_gross_monthly_wages +  
                                     factor(time), 
                  data = merged_df, panelVar = "location", timeVar = "time_index", 
                  autoCorr = "psar1", panelCorrMethod = "pcse", rhotype = "theil")

summary(corrected_model)
```


Houseman test
```{r}
new_random <-plm(value_of_apartments_sold ~ nr_residential_units_sold + 
                                    average_gross_monthly_wages,
         data=merged_df, index=c("location", "time"), model="random")

common_vars <- intersect(names(corrected_model$coefficients), names(new_random$coefficients))

diff_wsp <- corrected_model$coefficients[common_vars] - new_random$coefficients[common_vars]
diff_V <- corrected_model$vcov[common_vars, common_vars] - new_random$vcov[common_vars, common_vars]

stat_haus <- abs(t(diff_wsp) %*% solve(diff_V) %*% diff_wsp)
stat_haus
```

```{r}
p_wart <- 1 - pchisq(abs(stat_haus), 2)
p_wart
```


