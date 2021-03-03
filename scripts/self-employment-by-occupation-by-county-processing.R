library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Self-Employment by Occupation by County
# Created by Jenna Daly
# On 03/07/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2019,
    table = "S2406"
)

occupation <- data.table()
for (data in acsdata) {
    year <- data@endyear
      total <- acsSum(data, 1, "Total") 
      private_p <- acsSum(data, 2, "Private, Profit") 
      self_own <- acsSum(data, 3, "Self, Own") 
      private_np <- acsSum(data, 4, "Private, Not Profit") 
      govt <- acsSum(data, 5, "Government") 
      self_not_own <- acsSum(data, 6, "Self, Not Own") 
      total_m <- acsSum(data, 7, "Total Mgmt") 
      private_p_m <- acsSum(data, 8, "Private, Profit Mgmt") 
      self_own_m <- acsSum(data, 9, "Self, Own Mgmt") 
      private_np_m <- acsSum(data, 10, "Private, Not Profit Mgmt") 
      govt_m <- acsSum(data, 11, "Government Mgmt") 
      self_not_own_m <- acsSum(data, 12, "Self, Not Own Mgmt") 
      total_sv <- acsSum(data, 13, "Total Sv") 
      private_p_sv <- acsSum(data, 14, "Private, Profit Sv") 
      self_own_sv <- acsSum(data, 15, "Self, Own Sv") 
      private_np_sv <- acsSum(data, 16, "Private, Not Profit Sv") 
      govt_sv <- acsSum(data, 17, "Government Sv") 
      self_not_own_sv <- acsSum(data, 18, "Self, Not Own Sv") 
      total_sls <- acsSum(data, 19, "Total Sls") 
      private_p_sls <- acsSum(data, 20, "Private, Profit Sls") 
      self_own_sls <- acsSum(data, 21, "Self, Own Sls") 
      private_np_sls <- acsSum(data, 22, "Private, Not Profit Sls") 
      govt_sls <- acsSum(data, 23, "Government Sls") 
      self_not_own_sls <- acsSum(data, 24, "Self, Not Own Sls")
      total_nat <- acsSum(data, 25, "Total Nat") 
      private_p_nat <- acsSum(data, 26, "Private, Profit Nat") 
      self_own_nat <- acsSum(data, 27, "Self, Own Nat") 
      private_np_nat <- acsSum(data, 28, "Private, Not Profit Nat") 
      govt_nat <- acsSum(data, 29, "Government Nat") 
      self_not_own_nat <- acsSum(data, 30, "Self, Not Own Nat")
      total_prod <- acsSum(data, 31, "Total Prod") 
      private_p_prod <- acsSum(data, 32, "Private, Profit Prod") 
      self_own_prod <- acsSum(data, 33, "Self, Own Prod") 
      private_np_prod <- acsSum(data, 34, "Private, Not Profit Prod") 
      govt_prod <- acsSum(data, 35, "Government Prod") 
      self_not_own_prod <- acsSum(data, 36, "Self, Not Own Prod")
      
    datafips <- data.table(fips = getACSFips(data))
    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(total),
        estimate(private_p),
        estimate(self_own),
        estimate(private_np),
        estimate(govt),
        estimate(self_not_own), 
        estimate(total_m),
        estimate(private_p_m),
        estimate(self_own_m),
        estimate(private_np_m),
        estimate(govt_m),
        estimate(self_not_own_m), 
        estimate(total_sv), 
        estimate(private_p_sv), 
        estimate(self_own_sv), 
        estimate(private_np_sv), 
        estimate(govt_sv), 
        estimate(self_not_own_sv),
        estimate(total_sls),  
        estimate(private_p_sls), 
        estimate(self_own_sls), 
        estimate(private_np_sls),
        estimate(govt_sls), 
        estimate(self_not_own_sls), 
        estimate(total_nat), 
        estimate(private_p_nat), 
        estimate(self_own_nat), 
        estimate(private_np_nat),  
        estimate(govt_nat),  
        estimate(self_not_own_nat),
        estimate(total_prod),
        estimate(private_p_prod),  
        estimate(self_own_prod), 
        estimate(private_np_prod), 
        estimate(govt_prod),
        estimate(self_not_own_prod)
    )
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Total, All"                                                                    
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Private, Profit, All"                                             
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Self-Employed, Incorporated, All"                              
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Private, Not-for-profit, All"                                  
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Government, All"                                    
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Civilian employed population 16 years and over$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, All"
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Total, Management"  
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Private, Profit, Management"   
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Self-Employed, Incorporated, Management" 
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Private, Not-for-profit, Management"
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Government, Management"
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Management, business, science, and arts occupations$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, Management"
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Service occupations$", names(estimates) ) ] <- "Total, Service"                                                                                                                      
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Service occupations$", names(estimates) ) ] <- "Private, Profit, Service"                                                                                      
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Service occupations$", names(estimates) ) ] <- "Self-Employed, Incorporated, Service"                                                                       
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Service occupations$", names(estimates) ) ] <- "Private, Not-for-profit, Service"                                                                            
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Service occupations$", names(estimates) ) ] <- "Government, Service"                                                                             
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Service occupations$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, Service"                                         
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Sales and office occupations$", names(estimates) ) ] <- "Total, Sales"                                                                                                              
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Sales and office occupations$", names(estimates) ) ] <- "Private, Profit, Sales"                                                                             
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Sales and office occupations$", names(estimates) ) ] <- "Self-Employed, Incorporated, Sales"                                                              
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Sales and office occupations$", names(estimates) ) ] <- "Private, Not-for-profit, Sales"                                                                  
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Sales and office occupations$", names(estimates) ) ] <- "Government, Sales"                                                                    
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Sales and office occupations$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, Sales"                               
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Total, Natural"                                                                              
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Private, Profit, Natural"                                             
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Self-Employed, Incorporated, Natural"                              
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Private, Not-for-profit, Natural"                                  
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Government, Natural"                                    
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Natural resources, construction, and maintenance occupations$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, Natural"
    
    names(estimates)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Total, Production"                                                                               
    names(estimates)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Private, Profit, Production"                                              
    names(estimates)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Self-Employed, Incorporated, Production"                               
    names(estimates)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Private, Not-for-profit, Production"                                   
    names(estimates)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Government, Production"                                     
    names(estimates)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Production, transportation, and material moving occupations$", names(estimates) ) ] <- "Self-Employed, Not Incorporated, Production"
                     
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Business Type",
        variable.factor = F,
        value.name = "Percent",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(total) * 1.645,
        standard.error(private_p) * 1.645,
        standard.error(self_own) * 1.645,
        standard.error(private_np) * 1.645,
        standard.error(govt) * 1.645,
        standard.error(self_not_own) * 1.645, 
        standard.error(total_m) * 1.645,
        standard.error(private_p_m) * 1.645,
        standard.error(self_own_m) * 1.645,
        standard.error(private_np_m) * 1.645,
        standard.error(govt_m) * 1.645,
        standard.error(self_not_own_m) * 1.645,
        standard.error(total_sv) * 1.645, 
        standard.error(private_p_sv) * 1.645, 
        standard.error(self_own_sv) * 1.645, 
        standard.error(private_np_sv) * 1.645, 
        standard.error(govt_sv) * 1.645, 
        standard.error(self_not_own_sv) * 1.645,
        standard.error(total_sls) * 1.645,  
        standard.error(private_p_sls) * 1.645, 
        standard.error(self_own_sls) * 1.645, 
        standard.error(private_np_sls) * 1.645,
        standard.error(govt_sls) * 1.645, 
        standard.error(self_not_own_sls) * 1.645, 
        standard.error(total_nat) * 1.645, 
        standard.error(private_p_nat) * 1.645, 
        standard.error(self_own_nat) * 1.645, 
        standard.error(private_np_nat) * 1.645,  
        standard.error(govt_nat) * 1.645,  
        standard.error(self_not_own_nat) * 1.645,
        standard.error(total_prod) * 1.645,
        standard.error(private_p_prod) * 1.645,  
        standard.error(self_own_prod) * 1.645, 
        standard.error(private_np_prod) * 1.645, 
        standard.error(govt_prod) * 1.645,
        standard.error(self_not_own_prod) * 1.645
    )
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Total, All"                                                                    
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Private, Profit, All"                                             
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Self-Employed, Incorporated, All"                              
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Private, Not-for-profit, All"                                  
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Government, All"                                    
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Civilian employed population 16 years and over$", names(moes) ) ] <- "Self-Employed, Not Incorporated, All"
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Total, Management"  
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Private, Profit, Management"   
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Self-Employed, Incorporated, Management" 
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Private, Not-for-profit, Management"
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Government, Management"
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Management, business, science, and arts occupations$", names(moes) ) ] <- "Self-Employed, Not Incorporated, Management"
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Service occupations$", names(moes) ) ] <- "Total, Service"                                                                                                                      
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Service occupations$", names(moes) ) ] <- "Private, Profit, Service"                                                                                      
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Service occupations$", names(moes) ) ] <- "Self-Employed, Incorporated, Service"                                                                       
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Service occupations$", names(moes) ) ] <- "Private, Not-for-profit, Service"                                                                            
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Service occupations$", names(moes) ) ] <- "Government, Service"                                                                             
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Service occupations$", names(moes) ) ] <- "Self-Employed, Not Incorporated, Service"                                         
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Sales and office occupations$", names(moes) ) ] <- "Total, Sales"                                                                                                              
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Sales and office occupations$", names(moes) ) ] <- "Private, Profit, Sales"                                                                             
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Sales and office occupations$", names(moes) ) ] <- "Self-Employed, Incorporated, Sales"                                                              
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Sales and office occupations$", names(moes) ) ] <- "Private, Not-for-profit, Sales"                                                                  
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Sales and office occupations$", names(moes) ) ] <- "Government, Sales"                                                                    
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Sales and office occupations$", names(moes) ) ] <- "Self-Employed, Not Incorporated, Sales"                               
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Total, Natural"                                                                              
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Private, Profit, Natural"                                             
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Self-Employed, Incorporated, Natural"                              
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Private, Not-for-profit, Natural"                                  
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Government, Natural"                                    
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Natural resources, construction, and maintenance occupations$", names(moes) ) ] <- "Self-Employed, Not Incorporated, Natural"
    
    names(moes)[grep( "(.*Total.*Estimate|.*Estimate.*Total).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Total, Production"                                                                               
    names(moes)[grep( "(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Private, Profit, Production"                                              
    names(moes)[grep( "(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Self-Employed, Incorporated, Production"                               
    names(moes)[grep( "(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Private, Not-for-profit, Production"                                   
    names(moes)[grep( "(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Government, Production"                                     
    names(moes)[grep( "(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Production, transportation, and material moving occupations$", names(moes) ) ] <- "Self-Employed, Not Incorporated, Production"
     
    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Business Type",
        variable.factor = F,
        value.name = "Margins of Error",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Business Type`)
    setkey(moes, FIPS, Year, `Business Type`)

    occupation <- rbind(occupation, estimates[moes])
}

occupation <- occupation[occupation$FIPS != "0900100000",]

#Setup columns
occupation_long <- gather(occupation, `Measure Type`, Value, 4:5, factor_key = FALSE)
occupation_long$Variable[occupation_long$`Measure Type` == "Percent"] <- "Employment"
occupation_long$Variable[occupation_long$`Measure Type` == "Margins of Error"] <- "Margins of Error"
occupation_long$`Measure Type` <- "Percent"
occupation_long$`Measure Type`[grepl("Total", occupation_long$`Business Type`)] <- "Number"
occupation_long$Occupation <- "All"
occupation_long$Occupation[grepl("Management", occupation_long$`Business Type`)] <- "Management, business, science, and arts"
occupation_long$Occupation[grepl("Natural", occupation_long$`Business Type`)] <- "Natural resources, construction, and maintenance"
occupation_long$Occupation[grepl("Production", occupation_long$`Business Type`)] <- "Production, transportation, and material moving"
occupation_long$Occupation[grepl("Sales", occupation_long$`Business Type`)] <- "Sales and office"
occupation_long$Occupation[grepl("Service", occupation_long$`Business Type`)] <- "Service"

#Remove occupation from business column (removes everything after [inc] last comma in string)
occupation_long$`Business Type` <- sub("^(.*)[,].*", "\\1", occupation_long$`Business Type`)

#Merge in Counties by FIPS (filter out town data)
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
counties <- (county_fips_dp$data[[1]])

occupation_fips <- merge(occupation_long, counties, by = "FIPS")

occupation_fips$Year <- paste(occupation_fips$Year-4, occupation_fips$Year, sep="-")

occupation_fips$`Business Type` <- factor(occupation_fips$`Business Type`, 
                                             levels = c("Total",
                                                        "Self-Employed, Incorporated",
                                                        "Self-Employed, Not Incorporated",
                                                        "Government",
                                                        "Private, Profit",
                                                        "Private, Not-for-profit"))

occupation_fips$`Occupation` <- factor(occupation_fips$`Occupation`, 
                                             levels = c("All", 
                                                        "Management, business, science, and arts", 
                                                        "Service",
                                                        "Sales and office", 
                                                        "Natural resources, construction, and maintenance",
                                                        "Production, transportation, and material moving"))

occupation_final <- occupation_fips %>% 
  select(County, FIPS, Year, Occupation, `Business Type`, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, Occupation, `Business Type`, Variable)
    
write.table(
    occupation_final,
    file.path("data", "self-employment-occupation-county-2019.csv"),
    sep = ",",
    row.names = F,
    col.names = T,
    na = "-6666" 
)


