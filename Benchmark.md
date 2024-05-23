Benchmark Analysis
================
Mark Perkins and Sean Field
2024-04-30

# Load required libraries for data munging

``` r
library(ipeds)
library(tidyverse)
library(dplyr)
library(tidycensus)
library(knitr)
library(RODBC)
library(questionr)
```

# Download necessary data

``` r
### Include your Census Key:

census_api_key("API_Key Goes her")
```
### Get Access file path

IPEDSDatabase \<- odbcDriverConnect(“Driver={Microsoft Access Driver
(*.mdb, *.accdb)};DBQ=FILE PATH HERE”)

# Begin to build your dataset with IPEDS using the Access file from the site linked on GitHub

``` r
#Get Institution Information from HD Table and Reduce to Desired Variables
institutioninformation <-  sqlFetch(IPEDSDatabase, "HD2021")

instituioninformation<- subset(institutioninformation, CONTROL == 1)%>%
 select(UNITID, COUNTYCD, STABBR, INSTNM, IALIAS, F1SYSNAM, LONGITUD, 
         LATITUDE, LOCALE, C21SZSET)



institutioninformation <-  mutate(institutioninformation, locale = case_when(LOCALE == 11 ~ "City",
             LOCALE == 12 ~ "City",
             LOCALE == 13 ~ "City",
             LOCALE == 21 ~ "Suburb",
             LOCALE == 22 ~ "Suburb",
             LOCALE == 23 ~ "Suburb",
             LOCALE == 31 ~ "Town",
             LOCALE == 32 ~ "Town",
             LOCALE == 33 ~ "Town",
             LOCALE == 41 ~ "Rural",
             LOCALE == 42 ~ "Rural",
             LOCALE == 43 ~ "Rural",
             LOCALE == -3 ~ "Unknown"))

# Get graduate information
graduationinformation <-  sqlFetch(IPEDSDatabase, "GR200_21")
graduationinformation <-  graduationinformation %>%
 select(UNITID, L4GR100, L4GR150, L4GR200)

#Get gransfer information
transferinformation<- sqlFetch(IPEDSDatabase, "DRVGR2021")
transferinformation<- transferinformation %>%
  select(UNITID, TRRTTOT)

#Get retention information
retentioninformation <-  sqlFetch(IPEDSDatabase, "EF2021D")
retentioninformation <-  retentioninformation %>%
 select(UNITID, RET_PCF, RET_PCP)

#Get Institution Enrollment from EF Table and Keep all Variables
enrollmentinformationrace <-  sqlFetch(IPEDSDatabase, "EF2021A")
enrollmentinformationrace <-  subset(enrollmentinformationrace, EFALEVEL == 1)
enrollmentinformationrace <-  enrollmentinformationrace %>%
 select(UNITID, EFAIANT, EFASIAT, EFBKAAT, EFHISPT,
        EFNHPIT, EFNRALT, EFUNKNT, EF2MORT, EFWHITT)

#Get Institution Enrollment from EF Table and Keep all Variables
enrollmentinformationgender <-  sqlFetch(IPEDSDatabase, "EF2021")
enrollmentinformationgender <-  subset(enrollmentinformationgender, EFLEVEL == 10)


#Get Campus Cost Data fro GR Table and Reduce to Desired Variables
costinformation <-  sqlFetch(IPEDSDatabase, "DRVIC2021")
costinformation <-  costinformation %>%
 select(UNITID, CINSOFF, CINSON)
```

# A series of complex joins to link all the variables into one database for IPEDS

``` r
ipeds <- left_join(institutioninformation, enrollmentinformationgender, by = "UNITID") %>%
  left_join(enrollmentinformationrace, by = "UNITID") %>%
  left_join(graduationinformation, by = "UNITID") %>%
  left_join(retentioninformation, by = "UNITID") %>%
  left_join(costinformation, by = "UNITID") %>%
  left_join(transferinformation, by = "UNITID") %>%
  mutate(gradtransf = .01*(TRRTTOT + L4GR150)) %>%
  mutate(RET_PCF = .01*RET_PCF) %>%
  mutate(RET_PCP = .01*RET_PCP) %>%
  subset(C21SZSET %in% c(1, 2, 3, 4, 5)) %>%
  rename("Institution" = INSTNM) %>%
  rename("Alias" = IALIAS) %>%
  rename("Inst_System" = F1SYSNAM) %>%
  rename("Longitude" = LONGITUD) %>%
  rename("Lattitude" = LATITUDE) %>%
  rename("Locale" = LOCALE) %>%
  rename("Community_Type" = locale) %>%
  rename("Inst_Level" = EFLEVEL) %>%
  rename("Tot_Enrolled" = EFTOTAL) %>%
  rename("Tot_Men" = EFMEN) %>%
  rename("Tot_Women" = EFWOM) %>%
  rename("Tot_Full_Time" = EFFT) %>%
  rename("Tot_Full_Time_Men" = EFFTMEN) %>%
  rename("Tot_Full_Time_Women" = EFFTWOM) %>%
  rename("Tot_Part_Time" = EFPT) %>%
  rename("Tot_Part_Time_Men" = EFPTMEN) %>%
  rename("Tot_Part_Time_Women" = EFPTWOM) %>%
  rename("TwoYGradRate100" = L4GR100) %>%
  rename("TwoYGradRate150" = L4GR150) %>%
  rename("TwoYGradRate200" = L4GR200)%>%
  rename("FT_Retention" = RET_PCF) %>%
  rename("PT_Retention" = RET_PCP) %>%
  rename("Cost_Off_Campus" = CINSOFF) %>%
  rename("Cost_on_Campus" = CINSON) %>%
  rename("State" = STABBR) %>%
  rename("Am_Indian" = EFAIANT) %>%
  rename("Asian" = EFASIAT) %>%
  rename("Black" = EFBKAAT) %>%
  rename("Hispanic" = EFHISPT) %>%
  rename("Hawaii_PI" = EFNHPIT) %>%
  rename("Non_Resident" = EFNRALT) %>%
  rename("Unknown" = EFUNKNT) %>%
  rename("Two_or_More" = EF2MORT) %>%
  rename("White" = EFWHITT) %>%
  rename("transfer" = TRRTTOT)%>%
  mutate("Percent_Women" = (Tot_Women / Tot_Enrolled)*100) %>%
  mutate("Percent_FT" = (Tot_Full_Time / Tot_Enrolled)*100) %>%
  mutate("Percent_Am_Indian" = (Am_Indian / Tot_Enrolled)*100) %>%
  mutate("Percent_Asian" = (Asian / Tot_Enrolled)*100) %>%
  mutate("Percent_Black" = (Black / Tot_Enrolled)*100) %>%
  mutate("Percent_Hispanic" = (Hispanic / Tot_Enrolled)*100) %>%
  mutate("Percent_Hawaii_PI" = (Hawaii_PI / Tot_Enrolled)*100) %>%
  mutate("Percent_Non_Resident" = (Non_Resident / Tot_Enrolled)*100) %>%
  mutate("Percent_Unknown" = (Unknown / Tot_Enrolled)*100) %>%
  mutate("Percent_Two_or_More" = (Two_or_More / Tot_Enrolled)*100) %>%
  mutate("Percent_White" = (White / Tot_Enrolled)*100)



ipeds$COUNTYCODE  <-  sprintf("%05d", ipeds$COUNTYCD)
write.csv(ipeds, "ipeds.csv")
```

# Begin U.S. Census data extraction and analysis using tidycensus. This includes several metrics starting wtih change in population.

``` r
years<- lst(2015, 2020)

my_vars<- c(total_pop = "B01003_001")

population<-  map_dfr(years, ~ get_acs(geography = "county", variables = my_vars, year = .x, survey = "acs5", geometry = FALSE), .id = "year"  # when combining results, add id var (name of list item)
  ) %>%
  select(-moe) %>%  # shhhh
  arrange(variable, NAME)

population<- select(population, GEOID, year, estimate)

population<- population %>%
  spread(year, estimate)

population<- population %>%
  mutate("popchange" = population$'2020'-population$'2015')

population<- population %>%
  mutate("percentpopchange" = popchange/population$'2015')

population<- select(population, GEOID, popchange, percentpopchange)
```

## Get recent demographics from U.S. census.

``` r
#Capture the aligned cesus data and export to an excel file
key <- load_variables(2020, "acs5", cache = TRUE)

write.csv(key, "censuskey.csv")
```

``` r
#Estimated median household income in past 12 months, inflation adjusted 2020
income <- get_acs( geography = "county", variables = c(MedIncome="B19013_001"))
income<- rename.variable(income, "estimate","med_income")
income<- select(income, GEOID, med_income)


#Total in Population

HealthPro<-get_acs(geography = "county", variables = c(White="C27001A_005", Black="C27001B_005", Native="C27001C_005", Asian = "C27001D_005", Pacific="C27001E_005", Other= "C27001F_005", TwoMore="C27001G_005", WhiteNot="C27001H_005", Hispanic="C27001I_005"))
HealthPro<- HealthPro %>%
  select(GEOID, NAME, variable, estimate)
HealthPro<- HealthPro %>%
  spread(variable, estimate)
HealthPro<- HealthPro %>%
  replace_na(list(White= 0, Black=0, Native=0, Asian=0, Pacific=0, Other=0, TwoMore=0, WhiteNot=0, Hispanic=0))
HealthPro<- HealthPro %>%
  mutate("HPro" = White + Black + Native + Asian + Pacific + Other + TwoMore + WhiteNot + Hispanic)

#Total with health

HealthTot<-get_acs(geography = "county", variables = c(Whitet="C27001A_007", Blackt="C27001B_007", Nativet="C27001C_007", Asiant = "C27001D_007", Pacifict="C27001E_007", Othert= "C27001F_007", TwoMoret="C27001G_007", WhiteNott="C27001H_007", Hispanict="C27001I_007"))
HealthTot<- HealthTot %>%
  select(GEOID, NAME, variable, estimate)
HealthTot<- HealthTot %>%
  spread(variable, estimate)
HealthTot<- HealthTot %>%
  replace_na(list(Whitet= 0, Blackt=0, Nativet=0, Asiant=0, Pacifict=0, Othert=0, TwoMoret=0, WhiteNott=0, Hispanict=0))
HealthTot<- HealthTot %>%
  mutate("HTot" = Whitet + Blackt + Nativet + Asiant + Pacifict + Othert + TwoMoret + WhiteNott + Hispanict)


#Join the health variables
Health<-left_join(HealthTot, HealthPro, by = "GEOID")
Health<-rename.variable(Health, "NAME.x","County")
Health<- select(Health, GEOID, County, HPro, HTot)
Health<- Health%>%
  mutate("NoHealth"=HTot/HPro)

#Total in Population
MalePop<-get_acs(geography = "county", variables = c(WhMalePop="C23002A_004", BMalePop="C23002B_004", NMalePop="C23002C_004", AMalePop="C23002D_004", PIPop="C23002E_004", OMalePop="C23002F_004", TwoMalePop="C23002G_004", WAMalePop="C23002H_004", HMalePopl="C23002I_004"))
MalePop<- MalePop %>%
  select(GEOID, NAME, variable, estimate)
MalePop<- MalePop %>%
  spread(variable, estimate)
MalePop<- MalePop %>%
  replace_na(list(WhMalePop = 0, BMalePop = 0, NMalePop = 0, AMalePop= 0, PIPop = 0, OMalePop = 0, TwoMalePop = 0, WAMalePop = 0, HMalePopl= 0))
MalePop<- MalePop %>%
  mutate("MPop" = WhMalePop +  BMalePop +  NMalePop +  AMalePop + PIPop + OMalePop + TwoMalePop +  WAMalePop +  HMalePopl)
MalePop<- MalePop %>%
select(GEOID, NAME, MPop)


#Total Unemployed
MaleEmp<-get_acs(geography = "county", variables = c(WhMaleEmp="C23002A_008", BMaleEmp="C23002B_008", NMaleEmp="C23002C_008", AMaleEmp="C23002D_008", PIMaleEmpl="C23002E_008", OMaleEmp="C23002F_008", TwoMaleEmp="C23002G_008", WAMaleEmp="C23002H_008", HMaleEmpl="C23002I_008"))
MaleEmp<- MaleEmp %>%
  select(GEOID, NAME, variable, estimate)
MaleEmp<- MaleEmp %>%
  spread(variable, estimate)
MaleEmp<- MaleEmp %>%
  replace_na(list(WhMaleEmp = 0, BMaleEmp = 0, NMaleEmp = 0, AMaleEmp= 0, PMaleEmpl = 0, OMaleEmp = 0, TwoMaleEmp = 0, WAMaleEmp = 0, HMaleEmpl= 0))
MaleEmp<- MaleEmp %>%
  mutate("MUnEmp" = WhMaleEmp +  BMaleEmp +  NMaleEmp +  AMaleEmp + PIMaleEmpl + OMaleEmp + TwoMaleEmp +  WAMaleEmp +  HMaleEmpl)

MaleEmp<- MaleEmp %>%
select(GEOID,MUnEmp)

MaleEmp<- left_join(MalePop, MaleEmp, by = "GEOID")

MaleEmp<- MaleEmp%>%
mutate("MUnEmploy"= MUnEmp/MPop)

#Total in Population
FemalePop<-get_acs(geography = "county", variables = c(WhFemalePop="C23002A_016", BFemalePop="C23002B_016", NFemalePop="C23002C_016", AFemalePop="C23002D_016", PIPop="C23002E_016", OFemalePop="C23002F_016", TwoFemalePop="C23002G_016", WAFemalePop="C23002H_016", HFemalePopl="C23002I_016"))
FemalePop<- FemalePop %>%
  select(GEOID, NAME, variable, estimate)
FemalePop<- FemalePop %>%
  spread(variable, estimate)
FemalePop<- FemalePop %>%
  replace_na(list(WhFemalePop = 0, BFemalePop = 0, NFemalePop = 0, AFemalePop= 0, PIPop = 0, OFemalePop = 0, TwoFemalePop = 0, WAFemalePop = 0, HFemalePopl= 0))
FemalePop<- FemalePop %>%
  mutate("FPop" = WhFemalePop +  BFemalePop +  NFemalePop +  AFemalePop + PIPop + OFemalePop + TwoFemalePop +  WAFemalePop +  HFemalePopl)
FemalePop<- FemalePop %>% 
select(GEOID, NAME, FPop)

#Total Unemployed
FemaleEmp<-get_acs(geography = "county", variables = c(WhFemaleEmp="C23002A_021", BFemaleEmp="C23002B_021", NFemaleEmp="C23002C_021", AFemaleEmp="C23002D_021", PIFemaleEmpl="C23002E_021", OFemaleEmp="C23002F_021", TwoFemaleEmp="C23002G_021", WAFemaleEmp="C23002H_021", HFemaleEmpl="C23002I_021"))
FemaleEmp<- FemaleEmp %>%
  select(GEOID, NAME, variable, estimate)
FemaleEmp<- FemaleEmp %>%
  spread(variable, estimate)
FemaleEmp<- FemaleEmp %>%
  replace_na(list(WhFemaleEmp = 0, BFemaleEmp = 0, NFemaleEmp = 0, AFemaleEmp= 0, PFemaleEmpl = 0, OFemaleEmp = 0, TwoFemaleEmp = 0, WAFemaleEmp = 0, HFemaleEmpl= 0))
FemaleEmp<- FemaleEmp %>%
  mutate("FUnEmp" = WhFemaleEmp +  BFemaleEmp +  NFemaleEmp +  AFemaleEmp + PIFemaleEmpl + OFemaleEmp + TwoFemaleEmp +  WAFemaleEmp +  HFemaleEmpl)

FemaleEmp<- FemaleEmp %>%
select(GEOID,FUnEmp)

FemaleEmp<- left_join(FemalePop, FemaleEmp, by = "GEOID")

FemaleEmp<- FemaleEmp%>%
mutate("FUnEmploy"= FUnEmp/FPop)

employ<-left_join(MaleEmp, FemaleEmp, by="GEOID")
employ<- select(employ, GEOID, NAME.x, MPop, MUnEmp, FPop, FUnEmp)
employ<-rename.variable(employ, "NAME.x","County")
employ<- employ %>%
  mutate("total"= MPop+FPop)
employ<- employ %>%
  mutate("popemployed"= MUnEmp+FUnEmp)
employ<- employ %>%
  mutate ("Percent_Unemployed" = (popemployed/total))

#Total in Population
RacePop<-get_acs(geography = "county", variables = c(RacePop="B01001_001"))
RacePop<- rename.variable(RacePop, "estimate","RacePop")

#Total White
WhiteTot<-get_acs(geography = "county", variables = c(WhiteTot="B01001A_001"))
WhiteTot<-rename.variable(WhiteTot, "estimate","WhiteTot")

#Join the Race/Ethnicity
TotalWhite<-left_join(RacePop, WhiteTot, by = "GEOID")
TotalWhite<-rename.variable(TotalWhite, "NAME.x","County")
TotalWhite<- select(TotalWhite, GEOID, County, RacePop, WhiteTot)
TotalWhite<- TotalWhite%>%
mutate("PercentWhite"= WhiteTot/RacePop)

#Total Veterans
VetPop<-get_acs(geography = "county", variables = c(VetPop="B21001_002"))
VetPop<- rename.variable(VetPop, "estimate","VetPop")

#Total NonVeterans
VetNon<-get_acs(geography = "county", variables = c(VetNon="B21001_003"))
VetNon<-rename.variable(VetNon, "estimate","VetNon")

#Join the Veterans
Veteran<-left_join(VetPop, VetNon, by = "GEOID")
Veteran<-rename.variable(Veteran, "NAME.x","County")
Veteran<- select(Veteran, GEOID, County, VetNon, VetPop)
Veteran<- Veteran%>%
mutate("TotalPopV"= VetNon + VetPop)
Veteran<- Veteran%>%
mutate("PercentVet"= VetPop/TotalPopV)

#Total Pop
House<-get_acs(geography = "county", variables = c(House="B07401_001"))
House<- rename.variable(House, "estimate","House")
House<- select(House, GEOID, House)

#Total Same House
HouseSame<-get_acs(geography = "county", variables = c(HouseSame="B07401_017"))
HouseSame<-rename.variable(HouseSame, "estimate","HouseSame")
HouseSame<-  select(HouseSame, GEOID, HouseSame)

HousePercent<- left_join(HouseSame, House, by = "GEOID")

HousePercent<- 
  HousePercent%>%
  mutate("HousePercent" = HouseSame/House)
HousePercent<- select(HousePercent, GEOID, HousePercent)

#Get Married Statuses
Married<-get_acs(geography = "county", variables = c(MarriedPop="B07408_001",NeverMarried="B07408_002", Married="B07408_003", Divorced="B07408_004", Separated="B07408_005", Widowed="B07408_006"))
Married<- select(Married, GEOID, variable, estimate)

#Tranverse the data with spread
Married<- Married %>%
  spread(variable, estimate)

#Calculate Proportions and clean dataset
Married<- Married %>%
  mutate("percentnevermarried"=NeverMarried/MarriedPop)

Married<- Married %>%
  mutate("percentmarried"=Married/MarriedPop)

Married<- Married %>%
  mutate("percentdivorced"=Divorced/MarriedPop)

Married<- Married %>%
  mutate("percentseparated"=Separated/MarriedPop)

Married<- Married %>%
  mutate("percentwidowed"=Widowed/MarriedPop)

Married<- Married %>%
  mutate("percentsingle"=(1- percentmarried)+percentwidowed)

#Clean the dataset
Married<- select(Married, GEOID, percentnevermarried, percentmarried, percentdivorced, percentseparated, percentwidowed, percentsingle)
```

# Education level data from U.S. Census

``` r
Education<-get_acs(geography = "county", variables = c(EduPop="B07409_001",LessHS="B07409_002", HSorEquiv="B07409_003", SomeCollegeAss="B07409_004", Bach="B07409_005", GradorProf="B07409_006"))
Education<- select(Education, GEOID, variable, estimate)

#Tranverse the data with spread
Education<- Education %>%
  spread(variable, estimate)

#Calculate Proportions and clean dataset
Education<- Education %>%
  mutate("PercentLessthanHS"=LessHS/EduPop)

Education<- Education %>%
  mutate("PercentHS"=(HSorEquiv+SomeCollegeAss+Bach+GradorProf)/EduPop)

Education<- Education %>%
  mutate("PercentSomeorASS"=(SomeCollegeAss+Bach+GradorProf)/EduPop)

Education<- Education %>%
  mutate("PercentBach"=(Bach+GradorProf)/EduPop)

Education<- Education %>%
  mutate("PercentGradorPro"=GradorProf/EduPop)

Education<- select(Education, GEOID, PercentLessthanHS, PercentHS, PercentSomeorASS, PercentBach, PercentGradorPro)
```

# More demographic data from U.S. census. Note we calculate proportions given total population and age groups.

``` r
Tribes<-get_acs(geography = "county", variables = c(pop = "B01003_001", tribes = "B02014_002"))
Tribes<- select(Tribes, GEOID, variable, estimate)

# Transpose with spread
Tribes<- Tribes %>%
  spread(variable,estimate)

# Calculate the proportion
Tribes<- Tribes %>%
  mutate(percenttribe = tribes/pop)

Tribes<- select(Tribes, GEOID, percenttribe)
```

``` r
Parenting<-get_acs(geography = "county", variables = c(totkids = "B05009_001", undersixoneparent="B05009_013", sixtoseventeeenoneparent="B05009_031"))
Parenting<- select(Parenting, GEOID, variable, estimate)

#Tranverse the data with spread
Parenting<- Parenting %>%
  spread(variable, estimate)

#Calculate proportion
Parenting<- Parenting %>%
  mutate(SingleParPercent = (sixtoseventeeenoneparent + undersixoneparent)/totkids)
Parenting<- select(Parenting, GEOID, SingleParPercent)
```

``` r
Citizen<-get_acs(geography = "county", variables = c(totalpop= "B01003_001", totalmig = "B05011_001", notcitizen="B05011_002", naturalized="B05011_003"))
Citizen<- select(Citizen, GEOID, variable, estimate)

#Tranverse the data with spread
Citizen<- Citizen %>%
  spread(variable, estimate)

#Calculate proportion
Citizen<- Citizen %>%
  mutate(PercentNotCitizen = notcitizen/totalpop)

Citizen<- Citizen %>%
  mutate(PercentImmigrant    = totalmig/totalpop)

Citizen<- select(Citizen, GEOID, PercentNotCitizen, PercentImmigrant)
```

``` r
Renters<- get_acs(geography = "county", variables = c(totalh="B07013_001", renters = "B07013_003"))
Renters<- select(Renters, GEOID, variable, estimate)

#Transverse the data with spread
Renters<- Renters %>%
  spread(variable, estimate)

#Calculate proportions
Renters<- Renters %>%
  mutate(PercentRent = renters/totalh)

Renters<- select(Renters, GEOID, PercentRent)
```

# A series of joins to combine all the census data into one dataset.

``` r
census<- left_join(employ, Health, by = "GEOID")%>%
  left_join(income, by = "GEOID")%>%
  left_join(TotalWhite, by = "GEOID")%>%
  left_join(Veteran, by = "GEOID")%>%
  left_join(population, by = "GEOID")%>%
  left_join(HousePercent, by = "GEOID")%>%
  left_join(Married, by = "GEOID")%>%
  left_join(Education, by =  "GEOID")%>%
  left_join(Tribes, by = "GEOID")%>%
  left_join(Parenting, by= "GEOID")%>%
  left_join(Citizen, by = "GEOID")%>%
  left_join(Renters, by = "GEOID")%>%
  rename.variable("County.x","County")%>%
  mutate("WithHealth" = 1-NoHealth)

census<- select(census, GEOID, County,  Percent_Unemployed, HPro, HTot, NoHealth, WithHealth, med_income, RacePop, WhiteTot, PercentWhite, VetNon, VetPop, PercentVet, popchange, percentpopchange, HousePercent, percentnevermarried, percentmarried, percentdivorced, percentseparated, percentwidowed, percentsingle, PercentLessthanHS, PercentHS, PercentSomeorASS, PercentBach, PercentGradorPro, percenttribe, SingleParPercent, PercentNotCitizen, PercentImmigrant, PercentRent)

write.csv(census, "census.csv")
```

# Joining IPEDS and census data using county code as the linking column.

``` r
census<- read.csv("census.csv")

ipeds<- read.csv("ipeds.csv")
ipeds<- select(ipeds, COUNTYCD, UNITID, Institution, Inst_Level, Tot_Enrolled, Tot_Men, Tot_Women, Tot_Full_Time, Tot_Full_Time_Men, Tot_Full_Time_Women, Tot_Part_Time, Tot_Part_Time_Men, Tot_Part_Time_Women, Am_Indian, Asian, Black, Hispanic, Hawaii_PI, Non_Resident, Unknown, Two_or_More, White, TwoYGradRate100, TwoYGradRate150, TwoYGradRate200, FT_Retention, PT_Retention, Cost_Off_Campus, gradtransf, Percent_Women, Percent_FT, Percent_Am_Indian, Percent_Asian, Percent_Black, Percent_Hispanic, Percent_Hawaii_PI, Percent_Non_Resident, Percent_Unknown, Percent_Two_or_More, Percent_White, COUNTYCODE)

ipeds$COUNTYCD<- as.numeric(ipeds$COUNTYCD)


ipedsgraddata<-left_join(ipeds, census, by = c("COUNTYCD" = "GEOID"))

ipedsgraddata<- select(ipedsgraddata, COUNTYCODE, X, County, COUNTYCD, UNITID, Institution, Inst_Level, Tot_Enrolled, Tot_Men, Tot_Women, Tot_Full_Time, Tot_Full_Time_Men, Tot_Full_Time_Women, Tot_Part_Time, Tot_Part_Time_Men, Tot_Part_Time_Women, Am_Indian, Asian, Black, Hispanic, Hawaii_PI, Non_Resident, Unknown, Two_or_More, White, TwoYGradRate100, TwoYGradRate150, TwoYGradRate200, FT_Retention, PT_Retention, Cost_Off_Campus, gradtransf, Percent_Women, Percent_FT, Percent_Am_Indian, Percent_Asian, Percent_Black, Percent_Hispanic, Percent_Hawaii_PI, Percent_Non_Resident, Percent_Unknown, Percent_Two_or_More, Percent_White, Percent_Unemployed, HPro, HTot, NoHealth, WithHealth, med_income, RacePop, WhiteTot, PercentWhite, VetNon, VetPop, PercentVet, popchange, percentpopchange, HousePercent, percentnevermarried, percentmarried, percentdivorced, percentseparated, percentwidowed, percentsingle, PercentLessthanHS, PercentHS, PercentSomeorASS, PercentBach, PercentGradorPro, percenttribe, SingleParPercent, PercentNotCitizen, PercentImmigrant, PercentRent)


write.csv(ipedsgraddata, "ipedsgradmassive2.csv")
```

# Code for the Shiny dashboard. Be sure to use a Shiny application file to run this.

``` r
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(dplyr)
library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(corrr)
library(factoextra)
library(FactoMineR)


# Load data
df <- na.omit(read.csv("ipedsgradmassive2.csv"))
df <- select(df, UNITID, Institution, TwoYGradRate150,
             FT_Retention, PT_Retention, Cost_Off_Campus, gradtransf,
             Percent_Women, Percent_FT, Percent_Am_Indian, Percent_Asian, Percent_Black,
             Percent_Hispanic, Percent_Hawaii_PI, Percent_Non_Resident, Percent_Unknown,
             Percent_Two_or_More, Percent_White, Percent_Unemployed, WithHealth, med_income,
             PercentWhite, PercentVet, percentpopchange, HousePercent, percentnevermarried,
             percentmarried, percentdivorced, percentseparated, percentwidowed, percentsingle,
             PercentLessthanHS, PercentHS, PercentSomeorASS, PercentBach, PercentGradorPro,
             percenttribe, SingleParPercent, PercentNotCitizen, PercentImmigrant, PercentRent) %>%
  arrange(Institution)

corrdata <- df%>%
select(-UNITID)
corrdata<- correlate(corrdata)%>%
  shave() %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

formatted_df <- as.data.frame(corrdata)

formatted_df <- tibble::rownames_to_column(formatted_df, "Variable")%>%
  select(-Variable)

brks <- seq(-1, 1, .01)
clrs <- colorRampPalette(c("white", "#6baed6"))(length(brks) + 1)

dataCol_df <- ncol(formatted_df) - 1  # Exclude the first column which contains row names
dataColRng <- 1:dataCol_df  # Range of columns

datatable_obj <- datatable(
  formatted_df,
  escape = FALSE,
  options = list(
    paging = TRUE,
    searching = FALSE,
    info = FALSE,
    sort = TRUE,
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 2)  # Fix the first two left columns
  )
) %>%
  formatStyle(columns = dataColRng, backgroundColor = styleInterval(brks, clrs))


dictionary<- read.csv('datadictionary.csv')

# Define UI
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Collegium"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("user")), 
      menuItem("K Means", tabName = "kmean", icon = icon("user")), 
      menuItem("Data Dictionary", tabName = "dictionary", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              fluidPage(
                tags$style(HTML("
      .intro-background {
        background-image: url('grass.jpg');
        background-size: cover;
        background-position: center;
        height: 100vh;
        margin: 0;
        padding: 0;
        position: relative; /* Set position to relative */
      }
      .intro-text {
        position: absolute;
        bottom: 20%;
        right: 10%;
        color: white;
      }
    ")),
                div(
                  class = "intro-background",
                  div(
                    class = "intro-text",
                    h1("Collegium: A dashboard to find peer institutions for benchmarking")
                  )
                )
              )
      ),
      tabItem(tabName = 'kmean',
              fluidPage(
                titlePanel("K-Means Analysis to find Peer Community Colleges"),
                tags$style(HTML("
                                body {
                                background-color: #e4e5f0; /* Light gray background */
                                }
                                ")),
                fluidRow(
                  column(width = 12, 
                         h3("To use this dashboard, first examine the scatter plot 
                            to determine the vairables that correlate the highest.
                            Next, use the picker input control to select the variables
                            you want for your cluster analysis. Then select the number of 
                            clusters you want from the slidebar at level 1. 
                            Then search your institution in the search box on 
                            the first table. Next, select the cluster number 
                            for your institution. Next, scroll to the second 
                            row of plots and repeat. This should narrow your 
                            institution's peers down to a smaller sub-set of 
                            Community Colleges. You can download the first, 
                            second, or third level of peers. If your list is 
                            too large after three, you could randomly select 
                            from that list to get a reasonable set based on 
                            profile analytics. If you have too few, go up a level.")
                  )
                ),
                h1("Correlation Table"),
                fluidRow(
                  column(width = 12,
                          DT::dataTableOutput("correlation_table"),
                          downloadButton("cortbl", "Download Correlation Matrix"))),
                h1("Level 1"),
                fluidRow(
                  column(width = 12,
                         pickerInput("selected_vars", "Select Variables:", 
                                     choices = setdiff(colnames(df), c("UNITID", "Institution")),  # Exclude UNITID and Institution
                                     selected = setdiff(colnames(df), c("UNITID", "Institution")),  # Set all variables except UNITID and Institution as selected by default
                                     multiple = TRUE, 
                                     options = list(
                                       `actions-box` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `count-selected-text` = "Choose Variables")))
                ),
                fluidRow(
                  column(width = 12,
                         sliderInput("n_clusters", "Number of Clusters:", min = 1, max = 10, value = 5)
                  )
                ),
            
                fluidRow(
                  column(width = 3,
                         plotOutput("screeplot", height = "250px")
                  ),
                  column(width = 6,
                         plotOutput("cluster_plot", height = "250px")
                  ),
                  column(width = 3,
                         selectInput("selected_cluster", "Select Cluster:", 
                                     choices = NULL, 
                                     selected = NULL, multiple = TRUE),
                         downloadButton("dataset", "Download Institution Data")
                  )
                ),
                fluidRow(
                  column(width = 12,
                         DT::dataTableOutput("cluster_table")
                  )
                ),
                h1("Level 2"),
                fluidRow(
                  column(width = 12,
                         sliderInput("n_clusters2", "Number of Clusters:", min = 1, max = 10, value = 5)
                  )
                ),
                fluidRow(
                  column(width = 3,
                         plotOutput("screeplot2", height = "250px")
                  ),
                  column(width = 6,
                         plotOutput("cluster_plot2", height = "250px")
                  ),
                  column(width = 3,
                         selectInput("selected_cluster2", "Select Cluster:", 
                                     choices = NULL, 
                                     selected = NULL, multiple = TRUE),
                         downloadButton("dataset2", "Download Institution Data")
                  )
                ),
                fluidRow(
                  column(width = 12,
                         DT::dataTableOutput("cluster_table2")
                  )
                ),
                h1("Level 3"),
                fluidRow(
                  column(width = 12,
                         sliderInput("n_clusters3", "Number of Clusters:", min = 1, max = 10, value = 5)
                  )
                ),
                fluidRow(
                  column(width = 3,
                         plotOutput("screeplot3", height = "250px")
                  ),
                  column(width = 6,
                         plotOutput("cluster_plot3", height = "250px")
                  ),
                  column(width = 3,
                         selectInput("selected_cluster3", "Select Cluster:", 
                                     choices = NULL, 
                                     selected = NULL, multiple = TRUE),
                         downloadButton("dataset3", "Download Institution Data")
                  )
                ),
                fluidRow(
                  column(width = 12,
                         DT::dataTableOutput("cluster_table3")
                  )
                )
              )
      ),
      tabItem(tabName = 'dictionary',
              h1("Download the Dictionary"),
              column(width = 3,
                     downloadButton("datadictionary", "Download the Data Dictionary")),
              column(width = 12,
                     DT::dataTableOutput("dictionarytable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  ## Correlation Table
  output$correlation_table <- renderDT({
    datatable_obj
  })
  
  ## Download the correlation data
  ###Download handler for the data dictionary
  output$cortbl <- downloadHandler(
    filename = function() {
      paste("correlations", ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(corrdata, file)
    }
  )
  
  # Reactive expression for performing PCA
  pca_results <- reactive({
    selected_vars <- input$selected_vars
    if (is.null(selected_vars)) {
      selected_vars <- colnames(df)
    }
    PCA(select(df, all_of(selected_vars)), graph = FALSE)
  })
  
  # Reactive expression for performing K-means clustering
  kmeans_results <- reactive({
    set.seed(123)
    selected_vars <- input$selected_vars
    if (is.null(selected_vars)) {
      selected_vars <- colnames(df)
    }
    kmeans(scale(select(df, all_of(selected_vars))), input$n_clusters, nstart = 25)
  })
  
  # Reactive expression for filtering data based on selected cluster
  filtered_data <- reactive({
    if (!is.null(input$selected_cluster)) {
      df[kmeans_results()$cluster %in% input$selected_cluster, ]
    } else {
      df
    }
  })
  
  # Reactive expression for performing PCA on filtered data for second level clustering
  pca_results_df_with_clusters <- reactive({
    req(input$selected_vars)  # Ensure input is not NULL
    selected_vars <- input$selected_vars
    PCA(select(filtered_data(), all_of(selected_vars)), graph = FALSE)
  })
  
  # Reactive expression for performing K-means clustering on filtered data for second level clustering
  kmeans_results_df_with_clusters <- reactive({
    req(input$selected_vars)  # Ensure input is not NULL
    set.seed(123)
    selected_vars <- input$selected_vars
    kmeans(scale(select(filtered_data(), all_of(selected_vars))), input$n_clusters2, nstart = 25)
  })
  
  # Reactive expression for filtering data based on selected cluster for filtered data for second level clustering
  filtered_data_df_with_clusters <- reactive({
    if (!is.null(input$selected_cluster2)) {
      filtered_data()[kmeans_results_df_with_clusters()$cluster %in% input$selected_cluster2, ]
    } else {
      filtered_data()
    }
  })
  
  # Reactive expression for performing PCA on filtered data for third level clustering
  pca_results_df_with_clusters3 <- reactive({
    req(input$selected_vars)  # Ensure input is not NULL
    selected_vars <- input$selected_vars
    PCA(select(filtered_data_df_with_clusters(), all_of(selected_vars)), graph = FALSE)
  })
  
  # Reactive expression for performing K-means clustering on filtered data for third level clustering
  kmeans_results_df_with_clusters3 <- reactive({
    req(input$selected_vars)  # Ensure input is not NULL
    set.seed(123)
    selected_vars <- input$selected_vars
    kmeans(scale(select(filtered_data_df_with_clusters(), all_of(selected_vars))), input$n_clusters3, nstart = 25)
  })
  
  # Reactive expression for filtering data based on selected cluster for filtered data for third level clustering
  filtered_data_df_with_clusters3 <- reactive({
    if (!is.null(input$selected_cluster3)) {
      filtered_data_df_with_clusters()[kmeans_results_df_with_clusters3()$cluster %in% input$selected_cluster3, ]
    } else {
      filtered_data_df_with_clusters()
    }
  })
  
  # Update screeplot based on PCA results
  output$screeplot <- renderPlot({
    fviz_screeplot(pca_results(), addlabels = TRUE, ylim = c(0, 50))
  })
  
  # Update cluster_plot based on K-means clustering results
  output$cluster_plot <- renderPlot({
    fviz_cluster(
      kmeans_results(), 
      data = select(df, -UNITID, -Institution), 
      geom = "point", ellipse.type = "convex",
      ggtheme = theme_bw(),
      palette = "Set2"
    )
  })
  
  # Update screeplot based on PCA results for filtered dataset for second level clustering
  output$screeplot2 <- renderPlot({
    fviz_screeplot(pca_results_df_with_clusters(), addlabels = TRUE, barfill = "green", ylim = c(0, 50))
  })
  
  # Update cluster_plot2 based on K-means clustering results for filtered dataset for second level clustering
  output$cluster_plot2 <- renderPlot({
    fviz_cluster(
      kmeans_results_df_with_clusters(), 
      data = select(filtered_data(), -UNITID, -Institution), 
      geom = "point", 
      ellipse.type = "convex", 
      ggtheme = theme_bw(),
      palette = "Set1"  # Using the Set1 palette from RColorBrewer
    )
  })
  
  # Update screeplot based on PCA results for filtered dataset for third level clustering
  output$screeplot3 <- renderPlot({
    fviz_screeplot(pca_results_df_with_clusters3(), addlabels = TRUE, barfill = "red", ylim = c(0, 50))
  })
  
  # Update cluster_plot3 based on K-means clustering results for filtered dataset for third level clustering
  output$cluster_plot3 <- renderPlot({
    fviz_cluster(
      kmeans_results_df_with_clusters3(), 
      data = select(filtered_data_df_with_clusters(), -UNITID, -Institution), 
      geom = "point", 
      ellipse.type = "convex", 
      ggtheme = theme_bw(),
      palette = "Dark2"  # Using the Dark2 palette from RColorBrewer
    )
  })
  
  # Update table of institutions by cluster
  output$cluster_table <- DT::renderDataTable({
    df_with_clusters <- cbind(df, Cluster = factor(kmeans_results()$cluster))
    if (!is.null(input$selected_cluster)) {
      filtered_cluster_data <- df_with_clusters[df_with_clusters$Cluster %in% input$selected_cluster, ]
    } else {
      filtered_cluster_data <- df_with_clusters
    }
    sorted_cluster_data <- filtered_cluster_data[order(filtered_cluster_data$Institution), ]
    datatable(
      sorted_cluster_data[, c("UNITID", "Institution", "Cluster")],
      options = list(
        server = TRUE, 
        paging = TRUE,  
        pageLength = 10,  
        lengthMenu = c(10, 25, 50)
      )
    )
  })
  
  # Update table of institutions by cluster for filtered dataset for second level clustering
  output$cluster_table2 <- DT::renderDataTable({
    df_with_clusters2 <- cbind(filtered_data(), Cluster = factor(kmeans_results_df_with_clusters()$cluster))
    if (!is.null(input$selected_cluster2)) {
      filtered_cluster_data2 <- df_with_clusters2[df_with_clusters2$Cluster %in% input$selected_cluster2, ]
    } else {
      filtered_cluster_data2 <- df_with_clusters2
    }
    sorted_cluster_data <- filtered_cluster_data2[order(filtered_cluster_data2$Institution), ]
    datatable(
      sorted_cluster_data[, c("UNITID", "Institution", "Cluster")],
      options = list(
        server = TRUE,
        paging = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50)
      )
    )
  })
  
  # Update table of institutions by cluster for filtered dataset for third level clustering
  output$cluster_table3 <- DT::renderDataTable({
    df_with_clusters3 <- cbind(filtered_data_df_with_clusters(), Cluster = factor(kmeans_results_df_with_clusters3()$cluster))
    if (!is.null(input$selected_cluster3)) {
      filtered_cluster_data3 <- df_with_clusters3[df_with_clusters3$Cluster %in% input$selected_cluster3, ]
    } else {
      filtered_cluster_data3 <- df_with_clusters3
    }
    sorted_cluster_data <- filtered_cluster_data3[order(filtered_cluster_data3$Institution), ]
    datatable(
      sorted_cluster_data[, c("UNITID", "Institution", "Cluster")],
      options = list(
        server = TRUE,
        paging = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50)
      )
    )
  })
  
  # Update selectInput for selecting clusters
  observe({
    updateSelectInput(session, "selected_cluster", choices = as.character(1:input$n_clusters))
  })
  
  # Update selectInput for selecting clusters for filtered dataset for second level clustering
  observe({
    updateSelectInput(session, "selected_cluster2", choices = as.character(1:input$n_clusters2))
  })
  
  # Update selectInput for selecting clusters for filtered dataset for third level clustering
  observe({
    updateSelectInput(session, "selected_cluster3", choices = as.character(1:input$n_clusters3))
  })
  
  output$dataset <- downloadHandler(
    filename = function() {
      paste("dataset.csv", sep="")
    },
    content = function(file) {
      selected_vars <- input$selected_vars
      if (is.null(selected_vars)) {
        selected_vars <- colnames(df)
      }
      
      df_with_clusters <- cbind(df, Cluster = factor(kmeans_results()$cluster))
      
      if (!is.null(input$selected_cluster)) {
        filtered_cluster_data <- df_with_clusters[df_with_clusters$Cluster %in% input$selected_cluster, ]
      } else {
        filtered_cluster_data <- df_with_clusters
      }
      
      selected_vars <- c("UNITID", "Institution", selected_vars, "Cluster")
      filtered_selected_data <- filtered_cluster_data[, selected_vars, drop = FALSE]
      
      write.csv(filtered_selected_data, file, row.names = FALSE)
    }
  )
  
  output$dataset2 <- downloadHandler(
    filename = function() {
      paste("dataset2.csv", sep="")
    },
    content = function(file) {
      selected_vars <- input$selected_vars
      if (is.null(selected_vars)) {
        selected_vars <- colnames(df)
      }
      
      df_with_clusters <- cbind(filtered_data(), Cluster = factor(kmeans_results_df_with_clusters()$cluster))
      
      if (!is.null(input$selected_cluster2)) {
        filtered_cluster_data <- df_with_clusters[df_with_clusters$Cluster %in% input$selected_cluster2, ]
      } else {
        filtered_cluster_data <- df_with_clusters
      }
      
      selected_vars <- c("UNITID", "Institution", input$selected_vars, "Cluster")
      filtered_selected_data <- filtered_cluster_data[, selected_vars, drop = FALSE]
      
      write.csv(filtered_selected_data, file, row.names = FALSE)
    }
  )
  
  output$dataset3 <- downloadHandler(
    filename = function() {
      paste("dataset3.csv", sep="")
    },
    content = function(file) {
      selected_vars <- input$selected_vars
      if (is.null(selected_vars)) {
        selected_vars <- colnames(df)
      }
      
      df_with_clusters <- cbind(filtered_data_df_with_clusters(), Cluster = factor(kmeans_results_df_with_clusters3()$cluster))
      
      if (!is.null(input$selected_cluster3)) {
        filtered_cluster_data <- df_with_clusters[df_with_clusters$Cluster %in% input$selected_cluster3, ]
      } else {
        filtered_cluster_data <- df_with_clusters
      }
      
      selected_vars <- c("UNITID", "Institution", input$selected_vars, "Cluster")
      filtered_selected_data <- filtered_cluster_data[, selected_vars, drop = FALSE]
      
      write.csv(filtered_selected_data, file, row.names = FALSE)
    }
  )
  
  ##Data table for the data dictionary
  output$dictionarytable  <-  
    DT::renderDataTable(dictionary)
  
  
  ###Download handler for the data dictionary
  output$datadictionary <- downloadHandler(
    filename = function() {
      paste("dictionary", ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(dictionary, file)
    }
  )
}


shinyApp(ui = ui, server = server)
```

![](Benchmark_files/figure-gfm/cluster%20Shiny%20Code-1.png)<!-- -->
