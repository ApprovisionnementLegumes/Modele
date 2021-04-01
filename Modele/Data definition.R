########################################
# Variable definitions
########################################

# 1 - Model variables
# these are the variables we would like to estimate with the model

production_locations <- NULL
production_total <- NULL
environmental_impact <- NULL
production_times <- NULL


# 2 - Data tables
# These are the tables loaded from the database
annual_production <- NULL
provinces <- NULL
products <- NULL
production_modes <- NULL
impacts <- NULL
production_impacts <- NULL

# 3 - Other variables
months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

########################################
# Load the database tables
########################################

#Pathway : copy & paste here the database pathway
pathway = "base de données brouillon projet 1230.xlsx"

annual_production <- read_xlsx(pathway, sheet="annual_production")
provinces <- read_xlsx(pathway, sheet="provinces")
products <- read_xlsx(pathway, sheet="products")
production_modes <- read_xlsx(pathway, sheet="production_modes")
prices <- read_xlsx(pathway, sheet="prices")
impacts <- read_xlsx(pathway, sheet="impacts")
production_impacts <- read_xlsx(pathway, sheet="production_impacts")

########################################
# Process the datatables
########################################

# Create a data table that is easier to use by merging the needed information
production_data <- merge(annual_production, products, by.x = "id_product", by.y = "id") %>% 
  mutate(name_product = name) %>% 
  select(-name)

# merge with the localities
production_data <- merge(production_data, provinces, by.x = "id_province", by.y = "id") %>% 
  mutate(name_province = name)%>% 
  select(-name)

# merge with the production modes
production_data <- merge(production_data, production_modes, by.x = "id_mode", by.y = "id") %>% 
  mutate(name_mode = name) %>% 
  select(-name)

# Get the monthly production (sous hypothese de production equivalent par mois)
production_data <- production_data %>% 
  mutate(monthly_quantity = quantity / (stop_harvest - start_harvest))





