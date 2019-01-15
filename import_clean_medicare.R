
# load_libraries ----------------------------------------------------------

library(readr) #importing csv files
library(dplyr) #general analysis 
library(ggplot2) #making charts
library(lubridate) #date functions
library(reshape2) #use this for melt function to create one record for each team
library(tidyr)
library(janitor) #use this for doing crosstabs
library(scales) #needed for stacked bar chart axis labels
library(knitr) #needed for making tables in markdown page
#library(car)
library(aws.s3)
library(htmltools)#this is needed for Rstudio to display kable and other html code
library(rmarkdown)
library(readxl)
library(DT) #needed for making  searchable sortable data tble
library(kableExtra)
library(ggthemes)
#library(waffle)


#dir()

#Download the MA Landscape source file zip from here:
#https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovGenIn/index.html
#Inside there will be 2 csv files, splitting the alaphabet (A to M and N to W)
#These are the Medicare Advantage cost files (with premiums, deductible, out of pocket)
#Will need last year and new year 
#clean up the headers and put on same headers as last year



##Need to use the most current year's cost file as the starting point
#all Minnesota plans in that file need to be included in final data
#however, they might not have enrollment figures or prior year cost because some will be brand new



#Get most current Medicare Advantage enrollment by county data
#https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Monthly-Enrollment-by-Contract-Plan-State-County.html

#Also need state abbreviation conversion table

states <-  read_csv("state fips codes.csv") %>% rename(state=statename)




#enrollment file import

enroll <-  read_csv("CPSC_Enrollment_Info_2018_08.csv", col_types = cols(.default="c", `Plan ID`="i")) %>%
  rename(contractID=`Contract Number`,planID=`Plan ID`, ssa_code=`SSA State County Code`,
          fips=`FIPS State County Code`, state=`State`, county=`County`, enrollment=`Enrollment`) %>% 
  filter(state=='MN')




# IMPORT COST FILES -- UPDATE THIS CODE -----------------------------------




#import cost files for year 1
costyr1_A_M <-  read_csv("2018LandscapeSource file MA_AtoM 10142017.csv", col_types = cols(.default="c", planID="i"))
costyr1_N_W <- read_csv("2018LandscapeSource file MA_NtoW 10142017.csv", col_types = cols(.default="c", planID="i"))

#combine the year 1 cost files
costyr1 <-  rbind(costyr1_A_M, costyr1_N_W)


#import cost files for year 2
costyr2_A_M <-  read_csv("2019LandscapeSource file MA_AtoM 10012018.csv", col_types = cols(.default="c", planID="i"))
costyr2_N_W <-  read_csv("2019LandscapeSource file MA_NtoW 10012018.csv", col_types = cols(.default="c",planID="i"))

#Combine the year 2 cost files
costyr2 <-  rbind(costyr2_A_M, costyr2_N_W)


#remove uncessary files
rm(costyr1_A_M)
rm(costyr1_N_W)
rm(costyr2_A_M)
rm(costyr2_N_W)


#fix fields with dollar values by creating new fields as numeric

# replace $ with blank "" in the df$payment column.  and coerce that result to numeric
costyr1$monthly_premium2 = as.numeric(gsub("\\$", "", costyr1$monthly_premium))
costyr1$drug_deduct2 = as.numeric(gsub("\\$", "", costyr1$drug_deduct))
costyr1$moop2 = (gsub(",", "", costyr1$moop))  #first get rid of the comma
costyr1$moop2 = as.numeric(gsub("\\$", "", costyr1$moop2))  #then get rid of dollar sign (N/As will vanish)
costyr1$monthly_premium2[is.na(costyr1$monthly_premium2)] <- 0
costyr1$drug_deduct2[is.na(costyr1$drug_deduct2)] <- 0
costyr1$moop2[is.na(costyr1$moop2)] <- 0

costyr2$monthly_premium2 = as.numeric(gsub("\\$", "", costyr2$monthly_premium))
costyr2$drug_deduct2 = as.numeric(gsub("\\$", "", costyr2$drug_deduct))
costyr2$moop2 = as.numeric(gsub("\\$", "", costyr2$moop))
costyr2$moop2 = (gsub(",", "", costyr2$moop))  #first get rid of the comma
costyr2$moop2 = as.numeric(gsub("\\$", "", costyr2$moop2))  #then get rid of dollar sign (N/As will vanish)
costyr2$monthly_premium2[is.na(costyr2$monthly_premium2)] <- 0
costyr2$drug_deduct2[is.na(costyr2$drug_deduct2)] <- 0
costyr2$moop2[is.na(costyr2$moop2)] <- 0


#add state abbreviation field to costyr1 and costyr2

costyr1 <- inner_join(costyr1, states, by=c("state", "state")) %>% filter(state=='Minnesota')%>% mutate(datayear="year1")


costyr2 <- inner_join(costyr2, states, by=c("state", "state")) %>% filter(state=='Minnesota') %>% mutate(datayear="year2")



# IMPORT CONTRACT INFO FILE -----------------------------------------------


contract_info <- read_csv("CPSC_Contract_Info_2018_08.csv", col_types = cols(.default="c",`Plan ID`="i")) %>%
  rename(contractID=`Contract ID`, planID=`Plan ID`, organization_type=`Organization Type`,
         plan_type=`Plan Type`, offers_part_D=`Offers Part D`, SNP_plan=`SNP Plan`,
         eghp=EGHP, organization=`Organization Name`, org_marketing=`Organization Marketing Name`,
         plan_name=`Plan Name`, parent=`Parent Organization`, effective_date=`Contract Effective Date`)


enroll <-  left_join(enroll, contract_info, by=c("contractID"="contractID", "planID"="planID"))



#How many counties is each organization offering plans in?
#year 1
yr1_org_by_county <- costyr1 %>% group_by(county, organization) %>% summarise(count=n())
yr1_org_by_county_export <-  yr1_org_by_county %>% group_by(organization) %>% summarise(count=n())
write.csv(yr1_org_by_county_export, "yr1_org_by_county_export.csv")


#year 2
yr2_org_by_county <- costyr2 %>% group_by(county, organization) %>% summarise(count=n())
yr2_org_by_county_export <-  yr2_org_by_county %>% group_by(organization) %>% summarise(count=n())


write.csv(yr2_org_by_county_export, "./output/yr2_org_by_county_export.csv")



combine_years <- rbind(costyr1, costyr2)


#Fix organization names that were inconsistent from year to year
#Note: Allina had "medicare" as part of its name and was getting coded as Medica
combine_years <- combine_years %>% mutate(organization_rename= case_when(grepl("Humana", organization)~'Humana',
                                                                         grepl("Blue Cross", organization)~'Blue Cross',
                                                                         grepl("Essentia", organization)~'UCare',
                                                                         grepl("Allina", organization)~'Allina',
                                                                         grepl("Medica", organization)~'Medica',
                                                                         
                                                                         TRUE~organization))





#find the plans in year 1 that are not in year 2 
droppedplans <- anti_join(costyr1, costyr2, by=c("county"="county", "contractID"="contractID", "planID"="planID")) %>%
  select(county, contractID, planID) %>% mutate(dropflag="dropped")

#join with table to add "dropflag" field
combine_years <-  left_join(combine_years, droppedplans, by=c("county"="county", "contractID"="contractID", "planID"="planID"))


newplans <- anti_join(costyr2, costyr1, by=c("county"="county", "contractID"="contractID", "planID"="planID")) %>%
  select(county, contractID, planID) %>% mutate(newflag="new plan")

combine_years <-  left_join(combine_years, newplans, by=c("county"="county", "contractID"="contractID", "planID"="planID"))

combine_years_enroll <- left_join(combine_years, enroll %>% 
                      select(1,2,6,7,9,10,13,15),
                    by=c("contractID"="contractID", "planID"="planID", "county"="county"))


write.csv(combine_years_enroll,"./output/combine_years_medicare_advantage.csv", row.names=FALSE)







# OLD STUFF ---------------------------------------------------------------

#join to enrollment
droppedplans <- left_join(droppedplans, enroll %>% 
                            select(1,2,6,7),
                          by=c("contractID"="contractID", "planID"="planID", "county"="county"))  

#make a version with just the fields needed to export
droppedplans_export <-  droppedplans %>% select(contractID, planID, organization, plan_name, plan_type,  county,
                                                enrollment, monthly_premium2, drug_deduct2, moop2)



 
#Merge enrollment with most recent year of cost data (so we don't lose any plans)
#need to keep all the costyr2 records

merge1 <- left_join(costyr2, enroll %>% 
                       select(1,2,6,7,9,10,13,15),
                      by=c("contractID"="contractID", "planID"="planID", "county"="county")) %>% 
  rename(premiumCurrent=monthly_premium2, drugdeduct_current=drug_deduct2, moop_current=moop2)



#merge previous year of cost data
merge2 <- left_join(merge1, costyr1 %>% 
                      select(11,12,2,16,17,18),
                    by=c("contractID"="contractID", "planID"="planID", "county"="county")) %>% 
  rename(premium_lastyr=monthly_premium2,drugdeduct_lastyr=drug_deduct2, moop_lastyr=moop2 )



#add fields calculating differences and pct change in premiums, deductible and out of pocket
merge2 <-  merge2 %>% mutate(premium_diff=premiumCurrent-premium_lastyr, 
                             premium_pctchg=(premiumCurrent-premium_lastyr)/premium_lastyr,
                             drug_diff=drugdeduct_current-drugdeduct_lastyr,
                             drug_pctchg=(drugdeduct_current-drugdeduct_lastyr)/drugdeduct_lastyr,
                             moop_diff=moop_current-moop_lastyr,
                             moop_pctchg=(moop_current-moop_lastyr)/moop_lastyr)

#convert enrollment to numeric and get rid of asterisks that are used for zero enrollment
merge2$enroll2 = as.numeric(gsub("\\*", 0, merge2$enrollment))


#most popular in each county?

county_plan_enroll <- merge2 %>%
  arrange(county, enroll2) %>% 
  group_by(county) %>%
  mutate(rank=rank(enroll2))
  


 top_plan_by_county <-   county_plan_enroll %>% filter(rank==max(rank)) %>% select(contractID, planID, enroll2,rank) %>% 
    arrange(desc(enroll2))
  

 
 merge2 <-  left_join(merge2, top_plan_by_county %>% 
                        select(1,2,3,5),
                      by=c("county"="county", "contractID"="contractID", "planID"="planID"))
 
 top_plan_by_county_final <-  merge2 %>% filter(rank!="NA")
 

 #####################


 
 
 
 
 #######################
 
 #export for online
 

export_online <-  merge2 %>%
  select(contractID, planID, organization.x, plan_name.x, plan_type.x, plan_type.y, county,
          enroll2, premium_lastyr, drugdeduct_lastyr, moop_lastyr, premiumCurrent,
          drugdeduct_current, moop_current, premium_pctchg, drug_pctchg, moop_pctchg) %>% 
  mutate(eliminated=case_when(plan_type.y=='1876 Cost'~'yes', TRUE~'no'))



#write.csv(export_online, "export_snowbeck.csv", row.names = FALSE)

###export for Snowbeck

################################


#generate summary stats at county level for first year
counties_yr1 <-  costyr1 %>% group_by(county) %>% summarise(numplansyr1=n(), avg_premyr1=mean(monthly_premium2), median_premyr1=median(monthly_premium2),
                                           avg_drugyr1=mean(drug_deduct2), median_drugyr1=median(drug_deduct2), 
                                           avg_moopyr1=mean(moop2), median_moopyr1=median(moop2))
 
 #county level summary stats for second year
 counties_yr2 <-  costyr2 %>% group_by(county) %>% summarise(numplansyr2=n(), avg_premyr2=mean(monthly_premium2), median_premyr2=median(monthly_premium2),
                                                             avg_drugyr2=mean(drug_deduct2), median_drugyr2=median(drug_deduct2), 
                                                             avg_moopyr2=mean(moop2), median_moopyr2=median(moop2))
 
 #combine the county-level summary stats for the two years
 counties_compare <-  inner_join(counties_yr1, counties_yr2, by=c("county"="county"))

 #export county-level summary stats
 write.csv(counties_compare, "counties_compare.csv", row.names = FALSE)
 
 
 #statewide numbers year 1
state_yr1 <-  costyr1 %>%  summarise(numplansyr1=n(), avg_premyr1=mean(monthly_premium2), median_premyr1=median(monthly_premium2),
                                                             avg_drugyr1=mean(drug_deduct2), median_drugyr1=median(drug_deduct2), 
                                                             avg_moopyr1=mean(moop2), median_moopyr1=median(moop2))
 
 #statewide numbers year 2
 state_yr2 <- costyr2 %>% summarise(numplansyr2=n(), avg_premyr2=mean(monthly_premium2), median_premyr2=median(monthly_premium2),
                                    avg_drugyr2=mean(drug_deduct2), median_drugyr2=median(drug_deduct2), 
                                    avg_moopyr2=mean(moop2), median_moopyr2=median(moop2))
 write.csv(state_yr1, "state_summary_yr1.csv")
 
 write.csv(state_yr2, "state_summary_yr2.csv")
 
 



