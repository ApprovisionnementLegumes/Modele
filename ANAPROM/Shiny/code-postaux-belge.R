
library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)
library(data.table)
source("Data definition.R")
source("Get_Production.R")
source("Get_Province.R")
#source("Get_Production_Monthly.R")
#source("Inputs.R")
source("Get_Impacts.R")
source("Main.R")
library(kableExtra)
library(sigmoid)


library(formattable)


input_demand <- 800
input_legume <- "pea"
input_code <- 1348
input_time <- "july"
input_production_mode <- "conventionnal"
input_transformation <- "processed"

df = Main(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation)

tdf = df[[2]] 
tdf$normalized = sqrt(sqrt(tdf$impact_relatif/max(tdf$impact_relatif)))

tdf%>%
  
  mutate(
    value = cell_spec(value, color = spec_color(normalized, end = 0.9, option = "D", direction =-1),
                   bold = T))%>%
  mutate(
    impact_relatif = cell_spec(impact_relatif, color = spec_color(normalized, end = 0.9, option = "D", direction =-1),
                                bold = T))%>%
  
  select(-c("value_base", "means", "normalized")) %>%
  kable("html", escape = F) %>%
  kable_styling("hover", full_width = T) %>%
  row_spec(13, bold = TRUE, italic = FALSE, font_size = 18)%>%
  column_spec(1, bold = TRUE, color = "#565656")%>%
  column_spec(4, color = spec_color(tdf$incertitude, end = 0.9, option = "D", direction =-1))


tablebilan = df[[1]]%>%select(-c("status_id"))

tablebilan = tablebilan%>%mutate(
    status = color_tile_mean()(status))%>%
    kable("html", escape = F, align = c("l","c","c", "l","c")) %>%
    kable_styling("hover", full_width = F)%>%
    column_spec(1, bold = TRUE, color = "#565656")

df[[2]][13,]$value
