# This script formats the data for input into Power BI


#Reading new labels
new_labels <- read_excel(path = "inputs/class_scheme_MSA.xlsx",
                         sheet = "labels") %>%
  select(1:4) 

#load cached data results
load("cache/co_by_state.Rdata")
load("cache/co_by_msa.Rdata")


###########################
#Creating appropriate geo labels state and msa
us_co <- us_counties(resolution = "low") %>%
  select(geoid,state_name,name) %>%
  st_transform(5070)

us_st <- us_states(resolution = "low") %>%
  filter(!(stusps %in% c("HI","AK","PR")))  %>%
  st_transform(5070) %>% 
  select(state=name)


library(tigris)
us_msa <- core_based_statistical_areas(cb=T) %>%
  st_as_sf() %>%
  rename_all(str_to_lower) %>%
  st_transform(5070) %>%
  st_intersection(us_st) %>%
  select(geoid,geography=name,state) %>%
  st_set_geometry(NULL)


##############################
#Bind new labels to state
state_labeled <- inner_join(co_by_state_long,
                            new_labels,
                            by=c("co_type"="category")) %>%
  select(-co_type) %>%
  add_column(geography="All") %>%
  rename(state=description)
  
#Bind new labels to msa
msa_labeled <- inner_join(co_by_msa_long,
                            new_labels,
                            by=c("co_type"="category")) %>%
  select(-co_type,-description) %>%
  inner_join(us_msa,
            .,
            by="geoid")

#The state and msa dfs can now be bound since columns agree
out <- bind_rows(msa_labeled,
                 state_labeled %>% mutate(geoid=as.character(geoid)))

#write to outputs
write_csv(state_labeled %>% mutate(geoid=as.character(geoid)),path = "outputs/state_PBI.csv")
write_csv(msa_labeled,path = "outputs/msa_PBI.csv")
write_csv(out,path = "outputs/msa_state_PBI.csv")



###############################
#zip to cbsa crosswalk

# us_cbsa <- core_based_statistical_areas(cb=T) %>%
#   st_as_sf() %>%
#   rename_all(str_to_lower) %>%
#   st_transform(5070)
# 
# # write_sf(us_cbsa[us_st,],"outputs/cbsa.shp")
# 
# us_zips <- us_zipcodes() %>%
#   st_transform(5070)
# 
# crosswalk_zip_cbsa <- st_intersection(us_zips %>% select(zipcode),
#                                       us_cbsa %>% select(cbsa=geoid,name))
# 
# ggplot() +
#   geom_sf(data=us_cbsa[us_st,],fill="lightgray",color="red") +
#   geom_sf(data=crosswalk_zip_cbsa[us_st,],color="blue",alpha=.1) +
#   geom_sf(data=us_st,color="black",fill=NA) +
#   theme_void()
# 
# write_csv(crosswalk_zip_cbsa %>% st_set_geometry(NULL),path = "outputs/crosswalk_zip_cbsa.csv")
  