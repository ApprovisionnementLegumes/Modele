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
surfaces <- NULL
provinces <- NULL
legumes <- NULL
production_modes <- NULL
transformation <- NULL
repartition_modes <- NULL
impacts <- NULL
production_impacts <- NULL

# 3 - Other variables
months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

########################################
# Load the database tables
########################################

#Pathway : copy & paste here the database pathway
pathway = "Database projet.xlsx"

surfaces <- read_xlsx(pathway, sheet="surfaces")
provinces <- read_xlsx(pathway, sheet="provinces")
legumes <- read_xlsx(pathway, sheet="legumes")
production_modes <- read_xlsx(pathway, sheet="production_modes")
transformation <- read_xlsx(pathway, sheet="transformation")
repartition_modes <- read_xlsx(pathway, sheet="%_modes")
prices <- read_xlsx(pathway, sheet="prices")
impacts <- read_xlsx(pathway, sheet="impacts")
production_impacts <- read_xlsx(pathway, sheet="production_impacts")

########################################
# Process the datatables
########################################




