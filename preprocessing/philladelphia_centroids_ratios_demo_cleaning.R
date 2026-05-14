library(tidyverse)
library(tigris)
library(sf)

#shapefiles
tracts <- st_read("Census_Tracts_2010.geojson")

#ACS data
eda <- read.csv("EDA_data.csv")
#cleaned ACS data
acs_small <- read.csv("demographics_pa.csv")
#OPA
rtt <- read.csv("realestate_transfers_all.csv")
asmt <- read.csv("assessments.csv")

rtt2 <- rtt %>%
  select(opa_account_num, zip_code, document_type, assessed_value, total_consideration, display_date,shape)

#extract year of sale
rtt2$year <- as.integer(substr(rtt2$display_date, 1, 4))

rtt2_filtered <- rtt2 %>%
  filter(year >= 2013, year <= 2023)

#add market value from opa data
rtt2_filtered$year <- as.integer(rtt2_filtered$year)
asmt$year <- as.integer(asmt$year)

rtt2_joined <- rtt2_filtered %>%
  left_join(
    asmt %>% 
      select(parcel_number, year, market_value),
    by = c("opa_account_num" = "parcel_number", "year" = "year")
  )

#creating assessment ratio
rtt2_joined <- rtt2_joined %>%
  mutate(
    assessment_ratio = ifelse(
      total_consideration > 0,
      assessed_value / total_consideration,
      NA_real_
    )
  )

rtt2_joined <- rtt2_joined %>%
  mutate(
    mv_ratio = ifelse(
      total_consideration > 0,
      market_value / total_consideration,
      NA_real_
    )
  )

write.csv(rtt2_joined, "res_transfers_ratio.csv", row.names = FALSE)


#tract

tracts <- read.csv("ZIP_TRACT_032023.csv")

rtt2_joined <- rtt2_joined %>%
  mutate(zip5 = substr(zip_code, 1, 5))

tracts <- tracts %>%
  mutate(ZIP = sprintf("%05d", ZIP))

zip_best <- tracts %>%
  group_by(ZIP) %>%
  slice_max(RES_RATIO, n = 1) %>%
  ungroup()

rtt2_joined <- rtt2_joined %>%
  left_join(zip_best %>% select(ZIP, TRACT),
            by = c("zip5" = "ZIP")) %>%
  rename(tract = TRACT)
####
#Eda data

acs_small <- eda %>%
  transmute(
    GEOID,
    tract,
    year,
    
    # total population
    total_population = total_population_estimate_total,
    
    # race counts
    white = race_estimate_total_white_alone,
    black = race_estimate_total_black_or_african_american_alone,
    asian = race_estimate_total_asian_alone,
    
    # hispanic/latino count
    hispanic_latino = hispanic_or_latino_origin_estimate_total_hispanic_or_latino,
    
    # median income (replace with your actual column name)
    median_income = median_household_income_in_the_past_12_months_in_2022_inflation_adjusted_dollars_estimate_median_household_income_in_the_past_12_months_in_2022_inflation_adjusted_dollars,
    
    # educational attainment (example: percent with bachelor's or higher)
    edu_bachelors_plus =
      educational_attainment_for_the_population_25_years_and_over_estimate_total_bachelor_s_degree,
    #housing
    housing_units = housing_units_estimate_total,
    
    # compute percentages
    pct_white   = white   / total_population * 100,
    pct_black   = black   / total_population * 100,
    pct_asian   = asian   / total_population * 100,
    pct_hispanic = hispanic_latino / total_population * 100,
    pct_bachelors_plus = edu_bachelors_plus / total_population * 100
  )



acs_2020 <- acs_small %>% filter(year == 2020)

acs_small <- acs_small %>%
  mutate(
    GEOID = as.character(GEOID),
    GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")
  )



###master dataset no sale drops

rtt_sales <- rtt_ %>%
  filter(!is.na(market_value), !is.na(total_consideration), total_consideration > 0) %>%
  mutate(
    sale_price = total_consideration,
    assessed_value = market_value,
    ratio = assessed_value / sale_price
  )

tract_assessment <- rtt_sales %>%
  group_by(tract, year) %>%
  summarise(
    n_sales = n(),
    median_sale_price = median(sale_price, na.rm = TRUE),
    median_assessed_value = median(assessed_value, na.rm = TRUE),
    median_ratio = median(ratio, na.rm = TRUE),
    COD = median(abs(ratio - median_ratio), na.rm = TRUE) / median_ratio * 100,
    PRD = mean(ratio, na.rm = TRUE) / median_ratio,
    .groups = "drop"
  )

tract_assessment <- tract_assessment %>%
  mutate(
    tract = as.character(tract),
    tract = str_pad(tract, width = 11, side = "left", pad = "0")
  )
master_tract <- tract_assessment %>%
  left_join(acs_small, by = c("tract" = "GEOID", "year" = "year"))


joined <- tracts %>%
  left_join(master_tract, by = c("GEOID10" = "tract"))


tract_centroids <- tracts %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_as_sf() %>%
  st_transform(4326)

tract_centroids <- tract_centroids %>%
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )

write.csv(
  tract_centroids %>% 
    st_drop_geometry() %>% 
    select(GEOID10, NAME10, lon, lat),
  "philly_tract_centroids.csv",
  row.names = FALSE
)
