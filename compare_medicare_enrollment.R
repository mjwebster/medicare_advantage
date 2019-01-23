#This is for comparing change in Medicare enrollment by parent company, state, or county between
#two different time periods
#Created January 2019



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



#Get most current Medicare Advantage enrollment by county data
#for this analysis, we are going to compare the newest file to the month prior
#https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Monthly-Enrollment-by-Contract-Plan-State-County.html

#Also need state abbreviation conversion table

states <-  read_csv("./data/state fips codes.csv") %>% rename(state=statename)

#enrollment and contract files import & merge

#current month from CPSC file
enroll_CPSC_current <-  read_csv("./data/CPSC_Enrollment_Info_2019_01.csv", col_types = cols(.default="c", `Plan ID`="i")) %>%
  rename(contractID=`Contract Number`,planID=`Plan ID`, ssa_code=`SSA State County Code`,
         fips=`FIPS State County Code`, state=`State`, county=`County`, enrollment=`Enrollment`) 

enroll_CPSC_current$enroll2 = as.numeric(gsub("\\*", 0, enroll_CPSC_current$enrollment))

contract_current <- read_csv("./data/CPSC_Contract_Info_2019_01.csv", col_types = cols(.default="c",`Plan ID`="i")) %>%
  rename(contractID=`Contract ID`, planID=`Plan ID`, organization_type=`Organization Type`,
         plan_type=`Plan Type`, offers_part_D=`Offers Part D`, SNP_plan=`SNP Plan`,
         eghp=EGHP, organization=`Organization Name`, org_marketing=`Organization Marketing Name`,
         plan_name=`Plan Name`, parent=`Parent Organization`, effective_date=`Contract Effective Date`)



enroll_CPSC_current <-  left_join(enroll_CPSC_current, contract_current %>% 
                               select(contractID, planID, organization, plan_name, parent, organization_type, plan_type), by=c("contractID"="contractID", "planID"="planID")) %>% 
  
  mutate(yrmonth='current')

  
#prevoius month
enroll_CPSC_previous <-  read_csv("./data/CPSC_Enrollment_Info_2018_12.csv", col_types = cols(.default="c", `Plan ID`="i")) %>%
rename(contractID=`Contract Number`,planID=`Plan ID`, ssa_code=`SSA State County Code`,
       fips=`FIPS State County Code`, state=`State`, county=`County`, enrollment=`Enrollment`) 

enroll_CPSC_previous$enroll2 = as.numeric(gsub("\\*", 0, enroll_CPSC_previous$enrollment))


contract_previous <- read_csv("./data/CPSC_Contract_Info_2018_12.csv", col_types = cols(.default="c",`Plan ID`="i")) %>%
  rename(contractID=`Contract ID`, planID=`Plan ID`, organization_type=`Organization Type`,
         plan_type=`Plan Type`, offers_part_D=`Offers Part D`, SNP_plan=`SNP Plan`,
         eghp=EGHP, organization=`Organization Name`, org_marketing=`Organization Marketing Name`,
         plan_name=`Plan Name`, parent=`Parent Organization`, effective_date=`Contract Effective Date`)



enroll_CPSC_previous <-  left_join(enroll_CPSC_previous, contract_previous %>% 
                               select(contractID, planID, organization, plan_name, parent, organization_type, plan_type), by=c("contractID"="contractID", "planID"="planID")) %>% 
  
  mutate(yrmonth='previous')





enroll_SCC_current <-  read_csv("./data/SCC_Enrollment_MA_2019_01.csv") %>% 
  rename(contractID=`Contract ID`, organization=`Organization Name`, orgtype=`Organization Type`, plantype=`Plan Type`, ssa_code=`SSA Code`,
         fips=`FIPS Code`, state=`State`, county=`County`, enrollment=`Enrolled`) 

enroll_SCC_current$enrollment[is.na(enroll_SCC_current$enrollment)] <- 0



enroll_SCC_previous<-  read_csv("./data/SCC_Enrollment_MA_2018_12.csv") %>% 
  rename(contractID=`Contract ID`, organization=`Organization Name`, orgtype=`Organization Type`, plantype=`Plan Type`, ssa_code=`SSA Code`,
         fips=`FIPS Code`, state=`State`, county=`County`, enrollment=`Enrolled`) 


enroll_SCC_previous$enrollment[is.na(enroll_SCC_previous$enrollment)] <- 0






# EXPORT FILES FOR SNOWBECK --------------------------------------------------------------------






mn_SCC <- enroll_SCC_current %>% filter(state=='MN', enrollment>0) 
#write.csv(mn_SCC, 'mn_SCC_jan2019.csv', row.names = FALSE)



mn_SCC_Dec2018 <- enroll_SCC_previous %>% filter(state=='MN', enrollment>0) 

#write.csv(mn_SCC_Dec2018, 'mn_SCC_Dec2018.csv', row.names = FALSE)




mn_CPSC_current <- enroll_CPSC_current %>% filter(state=='MN') 
#write.csv(mn_CPSC_current, 'mn_CPSC_current.csv', row.names = FALSE)


mn_CPSC_previous <- enroll_CPSC_previous %>% filter(state=='MN') 
#write.csv(mn_CPSC_previous, 'mn_CPSC_previous.csv', row.names = FALSE)



CPSC_current_Humana_United <-  enroll_CPSC_current %>% filter(parent=='UnitedHealth Group, Inc.' | 
                             parent == 'Humana Inc.')
#write.csv(CPSC_current_Humana_United, 'CPSC_current_Humana_United.csv', row.names = FALSE)

CPSC_previous_Humana_United <-  enroll_CPSC_previous %>% filter(parent=='UnitedHealth Group, Inc.' | 
                                                            parent == 'Humana Inc.')
#write.csv(CPSC_previous_Humana_United, 'CPSC_previous_Humana_United.csv', row.names = FALSE)


parent_orgs <- enroll_CPSC_current %>% group_by(parent) %>% summarise(count=n())


CPSC_current_Medica_Bright_HPUnity <-  enroll_CPSC_current %>% filter(parent=='Bright Health Management' | 
                                                                parent == 'HealthPartners UnityPoint Health, Inc.' |
                                                                  parent=='Medica Holding Company')
write.csv(CPSC_current_Medica_Bright_HPUnity, 'CPSC_current_Medica_Bright_HPUnity.csv', row.names = FALSE)

CPSC_previous_Medica_Bright_HPUnity <-  enroll_CPSC_previous %>% filter(parent=='Bright Health Management' | 
                                                                          parent == 'HealthPartners UnityPoint Health, Inc.' |
                                                                          parent=='Medica Holding Company')
write.csv(CPSC_previous_Medica_Bright_HPUnity, 'CPSC_previous_Medica_Bright_HPUnity.csv', row.names = FALSE)





# WORK ON LATER -----------------------------------------------------------



#append enroll_previous to enroll_current

enroll_compare <-  rbind(enroll_current, enroll_previous) %>%
  mutate(keep= case_when(plan_type=='Medicare Prescription Drug Plan' ~'no' , 
  
                        plan_type=='Medicare-Medicaid Plan HMO/HMOPOS' ~'yes'

plan_type=='National PACE'  ~'yes',
                        TRUE~'yes')) 






enroll_compare <-  enroll_compare %>% filter(keep=='yes' & planID<800)


