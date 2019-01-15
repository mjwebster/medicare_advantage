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

#current month
enroll_current <-  read_csv("./data/CPSC_Enrollment_Info_2019_01.csv", col_types = cols(.default="c", `Plan ID`="i")) %>%
  rename(contractID=`Contract Number`,planID=`Plan ID`, ssa_code=`SSA State County Code`,
         fips=`FIPS State County Code`, state=`State`, county=`County`, enrollment=`Enrollment`) 


contract_current <- read_csv("./data/CPSC_Contract_Info_2019_01.csv", col_types = cols(.default="c",`Plan ID`="i")) %>%
  rename(contractID=`Contract ID`, planID=`Plan ID`, organization_type=`Organization Type`,
         plan_type=`Plan Type`, offers_part_D=`Offers Part D`, SNP_plan=`SNP Plan`,
         eghp=EGHP, organization=`Organization Name`, org_marketing=`Organization Marketing Name`,
         plan_name=`Plan Name`, parent=`Parent Organization`, effective_date=`Contract Effective Date`)



enroll_current <-  left_join(enroll_current, contract_current %>% 
                               select(contractID, planID, organization, plan_name, parent, organization_type, plan_type), by=c("contractID"="contractID", "planID"="planID")) %>% 
  
  mutate(organization_rename= case_when(grepl("Humana", organization)~'Humana',
                                        grepl("Blue Cross", organization)~'Blue Cross',
                                        grepl("Essentia", organization)~'UCare',
                                        grepl("Allina", organization)~'Allina',
                                        grepl("Medica", organization)~'Medica',
                                        TRUE~organization),
         yrmonth='current')

  
#prevoius month
enroll_previous <-  read_csv("./data/CPSC_Enrollment_Info_2018_12.csv", col_types = cols(.default="c", `Plan ID`="i")) %>%
rename(contractID=`Contract Number`,planID=`Plan ID`, ssa_code=`SSA State County Code`,
       fips=`FIPS State County Code`, state=`State`, county=`County`, enrollment=`Enrollment`) 


contract_previous <- read_csv("./data/CPSC_Contract_Info_2018_12.csv", col_types = cols(.default="c",`Plan ID`="i")) %>%
  rename(contractID=`Contract ID`, planID=`Plan ID`, organization_type=`Organization Type`,
         plan_type=`Plan Type`, offers_part_D=`Offers Part D`, SNP_plan=`SNP Plan`,
         eghp=EGHP, organization=`Organization Name`, org_marketing=`Organization Marketing Name`,
         plan_name=`Plan Name`, parent=`Parent Organization`, effective_date=`Contract Effective Date`)



enroll_previous <-  left_join(enroll_previous, contract_previous %>% 
                               select(contractID, planID, organization, plan_name, parent, organization_type, plan_type), by=c("contractID"="contractID", "planID"="planID")) %>% 
  
  mutate(organization_rename= case_when(grepl("Humana", organization)~'Humana',
                                        grepl("Blue Cross", organization)~'Blue Cross',
                                        grepl("Essentia", organization)~'UCare',
                                        grepl("Allina", organization)~'Allina',
                                        grepl("Medica", organization)~'Medica',
                                        TRUE~organization),
         yrmonth='previous')


#append enroll_previous to enroll_current

enroll_compare <-  rbind(enroll_current, enroll_previous) %>%
  mutate(keep= case_when(plan_type=='Medicare Prescription Drug Plan' ~'no' , 
                           plan_type=='Medicare-Medicaid Plan HMO/HMOPOS' ~'yes' ,
                           plan_type=='National PACE'  ~'yes',
                         TRUE~'yes')) 


#convert enrollment to numeric and get rid of asterisks that are used for zero enrollment
enroll_compare$enroll2 = as.numeric(gsub("\\*", 0, enroll_compare$enrollment))


enroll_compare <-  enroll_compare %>% filter(keep=='yes' & planID<800)


