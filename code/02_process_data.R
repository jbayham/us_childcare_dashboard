
#Calculating indicators for school aged kids and potential caregivers in long
if(!file.exists("cache/child_needs.Rdata")){
  special.locations <- c(selector_ind %>% select(-1) %>% names())
                         #selector_occ %>% select(-1) %>% names())
  
  cps_recoded <- 
    bind_cols(
      cps_data %>%
        mutate(child_0_2=ifelse(dplyr::between(age,0,2),1,0),
               child_3_5=ifelse(dplyr::between(age,3,5),1,0),
               child_6_11=ifelse(dplyr::between(age,6,11),1,0),
               #child_12_17=ifelse(dplyr::between(age,12,17),1,0),
               #child_any=ifelse(dplyr::between(age,0,17),1,0),
               parents=as.numeric(relate %in% c(101,201,202,203,1113,1114,1116,1117)),
               nwa=ifelse(!(relate %in% c(301,303,901,1242,9200)) & !dplyr::between(empstat,1,19),1,0),
               sib=ifelse(dplyr::between(age,13,20) & !dplyr::between(empstat,1,19),1,0),
               ptw=ifelse(dplyr::between(wkstat,20,42) & age>=18,1,0)
    ),
    map_dfc(special.locations,   #for intersections of industry and occupation, make this a map2_dfc and require both x==1 and y==1
            function(x){
              cps_data %>%
                transmute(parent=as.numeric((relate %in% c(101,201,202,203,1113,1114,1116,1117)) & !!as.name(x)==1)) %>%
                rename_all(~str_c(.,"_",x))
            }) %>%
      mutate_all(~ifelse(is.na(.),0,.)))%>%
    group_by(hrhhid,hrhhid2,mish)
  
  
  #Aggregating the indicators to the household level
  cps_co_prelims <- bind_cols(
    cps_recoded %>%
      summarise_at(vars(contains("child"),contains("parent")),sum) %>%   #summing number of children and adults
      ungroup(),
    cps_recoded %>%
      summarise_at(vars(nwa,sib,ptw),max) %>%
      ungroup() %>%
      select(-hrhhid,-hrhhid2,-mish)
  )

  
  #Counting household children and parents based on criteria
  hh_co <- 
    bind_cols(
      cps_co_prelims %>% select(hrhhid,hrhhid2,mish),
      map_dfc(str_subset(names(cps_co_prelims),"^child"),
              function(x){
                temp <- cps_co_prelims %>%
                  mutate(co = ifelse(!!as.name(x)>0,!!as.name(x),0),
                         co_nwa=ifelse(co>0 & nwa==0,!!as.name(x),0),
                         co_sib=ifelse(co_nwa>0 & sib==0,!!as.name(x),0),
                         co_sing=ifelse(co_sib>0 & parents==1,!!as.name(x),0)
                  ) %>%
                  # mutate_at(vars(dplyr::starts_with("parent")),list(total=~ifelse(.>0 & co>0,.,0),
                  #                                                   sing=~ifelse(.==1 & nwa==0 & co>0,1,0))) %>%
                  # select(starts_with("co"),ends_with("total"),ends_with("sing"))
                  mutate_at(vars(starts_with("parent")),~ifelse(.>0 & co>0,.,0)) %>%
                  select(starts_with("co"),starts_with("parent"))  %>%
                   mutate_at(vars(starts_with("co")),~./parents) %>%
                   mutate_at(vars(starts_with("co")),~ifelse(is.nan(.) | is.infinite(.),0,.)) 
                
                out <- map_dfc(temp %>% select(starts_with("parent")) %>% names(),
                         function(z){
                           temp %>%
                             transmute_at(vars(starts_with("co")),~.*!!as.name(z)) %>%
                             #mutate_all(~ifelse(is.nan(.) | is.infinite(.),0,.)) %>%
                             rename_all(~str_c(z,"_",.))
                         }) %>%
                   rename_all(~str_c(x,"_",.)) 
                
                return(out)
              })
    ) 
    
    
  
  ################
  
  #Joining back to original data (full long)
  full_long <- inner_join(cps_recoded %>%
                            select(hrhhid,hrhhid2,mish,year,month,wtfinl,statecensus,metfips,metro,
                                   pernum,relate,age,sex,race,educ,
                                   ind,occ,empstat,wkstat,earnweek,one_of(special.locations)),
                          hh_co,
                          by=c("hrhhid","hrhhid2","mish")) %>%
    ungroup()
  
  
  
  #Joining back to household level data (long)
  hh_long <- inner_join(cps_recoded %>%
                          select(hrhhid,hrhhid2,mish,year,month,hwtfinl,statecensus,metfips,metro,hhincome,famsize) %>%
                          distinct(hrhhid,hrhhid2,mish,.keep_all=T),
                        hh_co,
                        by=c("hrhhid","hrhhid2","mish")) %>%
    ungroup()
  
  
  save(full_long,hh_long,file="cache/child_needs.Rdata")
}

