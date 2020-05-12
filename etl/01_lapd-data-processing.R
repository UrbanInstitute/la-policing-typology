# LAPD Data Processing
# Ashlin Oglesby-Neal
# 2020/04/19


### load packagaes

packages <- c("tidyverse", "skimr", "lubridate", "janitor", "here")

## INSTALL PACKAGES
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)}})


### load data sets

## downloaded from LA Open Data Portal
# arrests
arr <- read.csv(here("data", "Arrest_Data_from_2010_to_Present.csv"))
# stops
sto <- read.csv(here("data", "Vehicle_and_Pedestrian_Stop_Data_2010_to_Present.csv"))
# CFS are uploaded year by year, variable names change in 2018, so loading in 2010-2017 first
mydir <- "data/CFS"
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles <- myfiles[1:8]
cfs1017 <- plyr::ldply(myfiles, read.csv)
cfs18 <- read.csv(here("data/CFS", "LAPD_Calls_for_Service_2018.csv"))
cfs1017 <- clean_names(cfs1017)
cfs18 <- clean_names(cfs18)
cfs18 <- cfs18 %>%
  rename(reporting_district = rpt_dist, 
         call_type_description = call_type_text,
         area_occurred = area_occ)
cfs <- bind_rows(cfs1017,cfs18)
# crime
cri <- read.csv(here("data", "Crime_Data_from_2010_to_Present.csv"))
# reporting district info
rdi <- read.csv(here("data","LAPD_Reporting_Districts.csv"))

## ACS data from USC Neighborhood Data for Social Change project, cross-walked from census tracts to reporting districts
# acs
acs <- read.csv(here("data", "acs_vars_bydistrict.csv"))


### _____________________
### process each data set

##________
## arrests
# clean rep dist and year
arr2 <- arr %>%
  mutate(year = year(mdy(Arrest.Date))) %>%
  rename(repdist = Reporting.District)

# make demographic variables
arr2 <- arr2 %>%
  mutate(juv = ifelse(Age < 18,1,0),
         adu = ifelse(Age >= 18,1,0),
         reth = ifelse(Descent.Code=="H",1,
                       ifelse(Descent.Code=="B",2,
                              ifelse(Descent.Code=="W",3,
                                     ifelse(Descent.Code=="O" | Descent.Code=="I" | Descent.Code=="X",4,
                                            ifelse(Descent.Code == "A" | Descent.Code == "C" | Descent.Code == "D" | 
                                                     Descent.Code == "F" | Descent.Code == "G" | Descent.Code == "J" | 
                                                     Descent.Code == "K" | Descent.Code == "L" | Descent.Code == "P" | 
                                                     Descent.Code == "S" | Descent.Code == "U" | Descent.Code == "V" |                                                      Descent.Code == "Z",5,NA))))))

# arrest crime categories
p1va <- c(1,2,3,4) # part 1 violent
p1pa <- c(5,6,7) # part 1 property (not including arson)
p2a <- c(8:27) # part 2
pothera <- c(29,99,NA) # uncategorized

# make arrest type and arrests by race variables
arr2 <- arr2 %>%
  mutate(arr_all=1,
         arr_p1v = ifelse(Charge.Group.Code %in% p1va,1,0),
         arr_p1p = ifelse(Charge.Group.Code %in% p1pa,1,0),
         arr_p2 = ifelse(Charge.Group.Code %in% p2a,1,0),
         arr_other = ifelse(Charge.Group.Code %in% pothera,1,0),
         arr1 = ifelse(reth==1,1,0),
         arr2 = ifelse(reth==2,1,0),
         arr3 = ifelse(reth==3,1,0),
         arr4 = ifelse(reth==4,1,0),
         arr5 = ifelse(reth==5,1,0))

# aggregate to repdist-year
arr3 <- arr2 %>%
  group_by(repdist, year) %>%
  summarize(arr_total=sum(arr_all), p1v_total=sum(arr_p1v), p1p_total=sum(arr_p1p), 
            p2_total=sum(arr_p2), na_total=sum(arr_other),
            arr_total_adult=sum(adu), arr_total_juv=sum(juv),
            arr_total_h=sum(arr1), 
            arr_total_b=sum(arr2), 
            arr_total_w=sum(arr3), 
            arr_total_o=sum(arr4), 
            arr_total_a=sum(arr5))


#_______
## stops

# make year, stop type, and race variables
sto2 <- sto %>%
  mutate(year = year(mdy(Stop.Date)),
         year = ifelse(year==1900,NA,year),
         pedstop = ifelse(Stop.Type=="PED",1,0),
         vehstop = ifelse(Stop.Type=="VEH",1,0),
         anystop = 1,
         reth = ifelse(Descent.Code=="H",1,
                       ifelse(Descent.Code=="B",2,
                              ifelse(Descent.Code=="W",3,
                                     ifelse(Descent.Code=="O" | Descent.Code=="I" | Descent.Code=="M",4,
                                            ifelse(Descent.Code=="A",5,NA))))))

# create indicators by stop type and race
sto2 <- sto2 %>%
  mutate(anystop1 = ifelse(reth==1,1,0),
         vehstop1 = ifelse(reth==1 & vehstop==1,1,0),
         pedstop1 = ifelse(reth==1 & pedstop==1,1,0),
         anystop2 = ifelse(reth==2,1,0),
         vehstop2 = ifelse(reth==2 & vehstop==1,1,0),
         pedstop2 = ifelse(reth==2 & pedstop==1,1,0),
         anystop3 = ifelse(reth==3,1,0),
         vehstop3 = ifelse(reth==3 & vehstop==1,1,0),
         pedstop3 = ifelse(reth==3 & pedstop==1,1,0),
         anystop4 = ifelse(reth==4,1,0),
         vehstop4 = ifelse(reth==4 & vehstop==1,1,0),
         pedstop4 = ifelse(reth==4 & pedstop==1,1,0),
         anystop5 = ifelse(reth==5,1,0),
         vehstop5 = ifelse(reth==5 & vehstop==1,1,0),
         pedstop5 = ifelse(reth==5 & pedstop==1,1,0))

# fix reporting district - set to missing non-numeric values
sto2 <- sto2 %>%
  mutate(repdist = as.numeric(as.character(Reporting.District)),
         repdist = ifelse(repdist==9999,NA,repdist))

# aggregate to repdist-year
sto3 <- sto2 %>%
  group_by(repdist, year) %>%
  summarize(s_totnum=sum(anystop), s_numveh=sum(vehstop), s_numped=sum(pedstop),
            s_numall_h=sum(anystop1), s_numveh_h=sum(vehstop1), s_numped_h=sum(pedstop1),
            s_numall_b=sum(anystop2), s_numveh_b=sum(vehstop2), s_numped_b=sum(pedstop2),
            s_numall_w=sum(anystop3), s_numveh_w=sum(vehstop3), s_numped_w=sum(pedstop3),
            s_numall_o=sum(anystop4), s_numveh_o=sum(vehstop4), s_numped_o=sum(pedstop4),
            s_numall_a=sum(anystop5), s_numveh_a=sum(vehstop5), s_numped_a=sum(pedstop5))

##___________________
## calls for service

# clean up call type
cfs2 <- cfs %>%
  mutate(str_callgroup = str_sub(call_type_code, 1, 3),
         str_callgroup = ifelse(str_callgroup=="^00" | str_callgroup=="6","006",str_callgroup),
         callgroup = as.numeric(as.character(str_callgroup)))

# not resident-initiated calls according to LAPD
ri_la <- c(0, 10, 200, 720, 820, 990, 991, 999, 6, 902)

# Urban's definition also excludes non-resident alarm calls (all 906 except 906B1C) and jail calls (830)
ri_ui <- c(0, 10, 200, 720, 820, 990, 991, 999, 6, 902, 906, 830)

# make CFS type variables (trim is resident-initiated according to LAPD, ui is resident-initiated according to Urban)
cfs2 <- cfs2 %>%
  mutate(cfs_all=1,
         cfs_trim = ifelse(callgroup %in% ri_la ,0,1),
         cfs_ui = ifelse(callgroup %in% ri_ui,0,1),
         cfs_ui = ifelse(callgroup==906 & call_type_code == "906B1C",1,cfs_ui))

# distinguish more serious calls - those for calls about violent crimes
ser <- c(187, 207, 211, 242, 245, 246, 261, 287, 288)

cfs2 <- cfs2 %>%
  mutate(cfs_serious = ifelse(callgroup %in% ser,1,0),
         cfs_nonser = ifelse(cfs_serious==0 & cfs_ui==1,1,0))

# clean up year and rep dist
cfs2 <- cfs2 %>%
  mutate(year = year(mdy(dispatch_date)),
         repdist = reporting_district)

# aggregate to repdist-year
cfs3 <- cfs2 %>%
  group_by(repdist, year) %>%
  summarize(cfs_all=sum(cfs_all), cfs_trim=sum(cfs_trim), cfs_ui=sum(cfs_ui),
            cfs_serious=sum(cfs_serious), cfs_nonser=sum(cfs_nonser))


##_______
## crimes
# make year (using date occurred) and rep dist
cri2 <- cri %>%
  mutate(year = year(mdy(Date.Occurred))) %>%
  rename(repdist = Reporting.District)

# make crime types
# part 1 violent & part 1 property according to LAPD reporting to UCR
p1v <- c(110, 113, 121, 122, 815, 820, 821, 210, 220, 230, 231, 235, 236, 250, 251, 
         761, 926, 435, 436, 437, 622, 623, 624, 625, 626, 627, 647, 763, 928, 930)
p1p <- c(310, 320, 510, 520, 433, 330, 331, 410, 420, 421, 350, 351, 352, 353, 450,
         451, 452, 453, 341, 343, 345, 440, 441, 442, 443, 444, 445, 470, 471, 472, 
         473, 474, 475, 480, 485, 487, 491)

cri2 <- cri2 %>%
  mutate(crime_total=1,
         crime_p1v = ifelse(Crime.Code %in% p1v,1,0),
         crime_p1p = ifelse(Crime.Code %in% p1p,1,0),
         crime_p2 = ifelse(crime_p1v==0 & crime_p1p==0,1,0),
         gang_crime = str_detect(MO.Codes,"0906"))

# aggregate by reporting district and year
cri3 <- cri2 %>%
  group_by(repdist, year) %>%
  summarize(crime_total=sum(crime_total), crime_p1v=sum(crime_p1v), crime_p1p=sum(crime_p1p),
            crime_p2=sum(crime_p2), gang_crime=sum(gang_crime))  


##________________________
## reporting district info
# rename rep dist for merging
rdi <- rdi %>% rename(repdist = REPDIST, objectid = ï..OBJECTID)

##____
## ACS
# use 2017 data for 2018 because 2017 is most recent year available
acs18 <- acs %>%
  filter(year==2017) %>%
  mutate(year=2018)
acs2 <- bind_rows(acs, acs18)
# note that a Census error with tract 1370 caused data for 2010 and 2011 to be missing
# this affects reporting districts 2172 and 2173

#AS: update to make percent by race variables
# make variables for all adults, other races combined
acs2 <- acs2 %>%
  mutate(pop_age_18_older_count=pop_ages_18_24_count+pop_ages_25_34_count+pop_ages_35_44_count+
           pop_ages_45_54_count+pop_ages_55_64_count+pop_ages_65_older_count,
         pop_other_races_count=native_hawaiian_other_count+other_race_count+pop_two_or_more_count,
         a_pop_pct = asian_count / total_pop,
         b_pop_pct = black_count / total_pop,
         h_pop_pct = hispanic_count / total_pop,
         w_pop_pct = white_count / total_pop,
         o_pop_pct = pop_other_races_count / total_pop) %>%
  rename(a_pop_count = asian_count, b_pop_count = black_count,
         h_pop_count = hispanic_count, w_pop_count = white_count)

###_____________________________
### merge all data sets together

## create own base file of LAPD reporting districts and years
# this prevents the inclusion on non-LAPD reporting districts and years out of 2010-2018 range
base <- rdi %>%
  select(repdist) %>%
  mutate(year=2010, n=9)
repdist<-base$repdist
year<-base$year
nyears<-base$n
base_input<-data.frame(repdist,year,nyears)
base_data <- base_input[rep(rownames(base_input), base_input$nyears), ]
base_data$year <- base_data$year + (sequence(base_input$nyears)-1)
base_data <-  base_data %>% select(repdist,year)

## merge files

# start with base_data, add ACS
all_merge0 <- left_join(base_data, acs2, by=c("repdist", "year"))

## then add crime
all_merge1 <- left_join(all_merge0, cri3, by=c("repdist", "year"))

# then reporting district info
all_merge2 <- left_join(all_merge1, rdi, by="repdist")

# then stops
all_merge3 <- left_join(all_merge2, sto3, by=c("repdist", "year"))

# then cfs
all_merge4 <- left_join(all_merge3, cfs3, by=c("repdist", "year"))

# then arrest
all_merge5 <- left_join(all_merge4, arr3, by=c("repdist", "year"))

## evaluate data set with all merges
skim(all_merge5)


###_______________________________
### make rates needed for analysis

## rates by total LA population
# make population totals for all of LA for each year
yearly_populations <- all_merge5 %>%
  select(year, total_pop, pop_under_age_18_count, pop_age_18_older_count, a_pop_count, b_pop_count, 
         h_pop_count, pop_other_races_count, w_pop_count) %>%
  group_by(year) %>%
  summarize_all(sum, na.rm=TRUE) %>%
  rename(total_pop_LA = total_pop, pop_under_age_18_count_LA = pop_under_age_18_count, 
         pop_age_18_older_count_LA = pop_age_18_older_count, white_count_LA = w_pop_count,
         asian_count_LA = a_pop_count, black_count_LA = b_pop_count, 
         hispanic_count_LA = h_pop_count, pop_other_races_count_LA = pop_other_races_count)

# merge population totals to data set and make rates
all_merge6 <- all_merge5 %>%
  left_join(yearly_populations, by="year") %>%
  mutate(stop_rate_h_LA = s_numall_h  / hispanic_count_LA,
         veh_rate_h_LA = s_numveh_h  / hispanic_count_LA,
         ped_rate_h_LA = s_numped_h  / hispanic_count_LA,
         arr_rate_h_LA = arr_total_h  / hispanic_count_LA,
         stop_rate_b_LA = s_numall_b  / black_count_LA,
         veh_rate_b_LA = s_numveh_b  / black_count_LA,
         ped_rate_b_LA = s_numped_b  / black_count_LA,
         arr_rate_b_LA = arr_total_b  / black_count_LA,
         stop_rate_w_LA = s_numall_w  / white_count_LA,
         veh_rate_w_LA = s_numveh_w  / white_count_LA,
         ped_rate_w_LA = s_numped_w  / white_count_LA,
         arr_rate_w_LA = arr_total_w  / white_count_LA,
         stop_rate_a_LA = s_numall_a  / asian_count_LA,
         veh_rate_a_LA = s_numveh_a / asian_count_LA,
         ped_rate_a_LA = s_numped_a  / asian_count_LA,
         arr_rate_a_LA = arr_total_a  / asian_count_LA,
         stop_rate_o_LA = s_numall_o  / pop_other_races_count_LA,
         veh_rate_o_LA = s_numveh_o / pop_other_races_count_LA,
         ped_rate_o_LA = s_numped_o  / pop_other_races_count_LA,
         arr_rate_o_LA = arr_total_o  / pop_other_races_count_LA,
         juv_arr_rate_LA = arr_total_juv / pop_under_age_18_count_LA,
         adult_arr_rate_LA = arr_total_adult / pop_age_18_older_count_LA,
         arr_rate_LA = arr_total / total_pop_LA,
         stop_rate_LA = s_totnum / total_pop_LA,
         s_numveh_rate_LA = s_numveh / total_pop_LA, #AS: Update variable name to match 
         s_numped_rate_LA = s_numped / total_pop_LA, #AS: Update variable name to match 
         cfs_serious_rate_LA = cfs_serious / total_pop_LA, #AS: Update variable name to match 
         cfs_nonser_rate_LA = cfs_nonser / total_pop_LA, #AS: Update variable name to match 
         cfs_rate_LA = cfs_ui / total_pop_LA, #AS: Update variable name to match 
         crime_rate_LA = crime_total / total_pop_LA,
         crime_p1v_rate_LA = crime_p1v / total_pop_LA,
         crime_p1p_rate_LA = crime_p1p / total_pop_LA,
         crime_p2_rate_LA = crime_p2 / total_pop_LA)

# save final file
write.csv(all_merge6, file= here("data", "analysis_ready_by_district_final.csv"))