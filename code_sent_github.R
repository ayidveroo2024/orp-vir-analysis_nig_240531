

#Activate the packages
library(tidyverse)
library(zoo)
library(sf)


########## GET databases---------

lqas_dat <- read.csv("inputs_data_nig/lqas_nigeria_all_updt_may24.csv")
polio_dat <- read_rds("inputs_data_nig/polio_data.rds")
admin1_shpf <- polio_dat$global.prov |> filter(ADM0_NAME == "NIGERIA")
admin2_shpf <- polio_dat$global.dist |> filter(ADM0_NAME == "NIGERIA")
virus_list_orphan <- read.csv("inputs_data_nig/nigeria_orhphan_viruses_2023_may24.csv")

########## Counting the number of orphan viruses by districts---------

orph_countT <- virus_list_orphan |> 
  group_by(admin2_guid) |> 
  summarise(No.orp.virus = n()) |> 
  mutate(dist.type = "orphan") # this will add a new column adding the same values called orphan

#### Creating a map showing the districts reported the viruses and number of count. Use gradient color

#Join the shapefile with orph_count table

joinedtable_adm1 <- left_join(admin2_shpf, orph_countT, by = c("GUID" = "admin2_guid"))
joined_filtered <- joinedtable_adm1 |> filter(No.orp.virus > 0)

#Create a the first map to locate the districts reporting the orhan viruses


joined_filtered |> ggplot() +
  geom_sf(data = admin1_shpf, color = "black", linewidth = 0.6, fill = NA) + #we can insert filter inside to filter old shape files by saying filter(>= 2020. geom_sf(data = admin1_shpf |> filter(yr.end >= 2020), color = "black"
  geom_sf(aes(fill = No.orp.virus), color = NA) +
  #geom_sf(data = admin2_shpf, color = "black", linewidth = 0.6, fill = NA) +
  scale_fill_gradient2 (high = scales::muted("green")) +
  labs(title = "Districts reported orphan viruses from 2023 to May 2024, Nigeria") +
  theme_bw()