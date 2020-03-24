

#Extract occupation codes
get_cps_occ_codes <- function(){
  require(rvest)
  require(dplyr)
  cps.pointer <- read_html("https://cps.ipums.org/cps/codes/occ_2011_codes.shtml")
  
  occ_codes <- cbind(
    cps.pointer %>%
      html_nodes("dt") %>%
      html_text(),
    cps.pointer %>%
      html_nodes("dd") %>%
      html_text()
  ) %>%
    as_tibble(.name_repair = ~ c("codes", "description"))
  
  #############################
  #Defining health occupation subsets
  
  #Define health category
  occ_health_all <- occ_codes %>% 
    filter(codes %in% as.character(seq.int(3000,3655)))
  
  #Define specific health sector
  occ_health_subset <- occ_codes %>%
    filter(codes %in% c(
      "3210",
      "3060",
      "3256",
      "3110",
      "3220",
      "3255",
      "3258",
      "3260",
      "3300",
      "3320",
      "3400",
      "3420",
      "3500",
      "3510",
      "3540",
      "3600",
      "3645",
      "0350",
      "1650",
      "2025"
    ))
  
  
  
  
  save(occ_codes,occ_health_subset,occ_health_all,file = "cache/occ_codes.Rdata")
  
  return(NULL)
}


get_cps_metfips_codes <- function(){
  require(rvest)
  require(dplyr)
  cps.pointer <- read_html("https://cps.ipums.org/cps/codes/metfips_2014onward_codes.shtml")
  
  met_codes <- cbind(
    cps.pointer %>%
      html_nodes("dt") %>%
      html_text(),
    cps.pointer %>%
      html_nodes("dd") %>%
      html_text()
  ) %>%
    as_tibble(.name_repair = ~ c("codes", "description"))
  
  save(met_codes,file = "cache/met_codes.Rdata")
}


get_cps_ind_codes <- function(){
  #This function scrapes the CPS table and reads the google sheet defining occupation code subsets
  
  #Dependencies
  require(dplyr)
  require(rvest)
  ########################################
  cps.pointer <- read_html("https://cps.ipums.org/cps/codes/ind_2014_codes.shtml")
  
  ind_codes <- cbind(
    cps.pointer %>%
      html_nodes("dt") %>%
      html_text(),
    cps.pointer %>%
      html_nodes("dd") %>%
      html_text()
  ) %>%
    as_tibble(.name_repair = ~ c("codes", "description"))
  
  save(ind_codes,file="cache/ind_codes.Rdata")
  
}

##########################################
#Download google sheet and extract industry and occupation aggregations

build_selector <- function(occ=T,ind=T){
  #Args:
    #occ: logical, TRUE if pull occupation aggregates
    #ind: logical, TRUE if pull industry aggregates
  
  #Dependencies
  require(readxl)
  require(janitor)
  require(googledrive)
  ###############################################
  #Pull latest copy of the shared google sheet from 
  drive_download(file=as_id("1El3XUkgUbPRglRl3BCAl5ErDIkWJnqxtt_uXDs8QCMM"),
                 path = "inputs/reference_codes.xlsx",
                 overwrite = T)
  
  if(ind){
    #Read in data from ind_code
    selector_ind <- read_excel(path = "inputs/reference_codes.xlsx",
                           sheet = "ind_code") %>%
      clean_names("snake") %>%
      select(-description) %>%
      mutate(essential=ifelse(essential=="x",1,essential),
             ind_code=str_pad(ind_code,4,"left","0")) %>%
      rename_at(vars(-1),~str_c("ind_",.))
    
    save(selector_ind,file = "cache/selector_ind.Rdata")
  }
  
  if(occ){
    #Read in data from ind_code
    selector_occ <- read_excel(path = "inputs/reference_codes.xlsx",
                           sheet = "occ_code") %>%
      clean_names("snake") %>%
      select(-description) %>%
      mutate(essential=ifelse(essential=="x",1,essential),
             occ_code=str_pad(occ_code,4,"left","0")) %>%
      rename_at(vars(-1),~str_c("occ_",.))
    
    save(selector_occ,file = "cache/selector_occ.Rdata")
  }
  
 
}


##################################
#This basically replicates a function in ipumsr --> move to deprecate where used in code
get_labels <- function(df,var.name){
  df %>%
    select(!!var.name) %>%
    distinct() %>%
    transmute(code=!!as.name(var.name),
              description=to_factor(code)) %>%
    arrange(code)
}

#unit test
#get_labels(df=cps_data,var.name="relate")

  
