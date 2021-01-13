#############################################################################################
#############################################################################################
######################## Predicting House Values in Ohio (2012 Data) ########################
#############################################################################################
#############################################################################################

## Load in the packages we need
## If not loaded, install first using install.packages("package_name")
library(readr) # Read csv file
library(readxl) # Read excel file
library(dplyr) # Manipulate data
library(sqldf) # SQL Functions
library(ggplot2) # Graph output of data
library(ggtheme) # Theme of data
library(stargazer) # For tables
library(ggcorrplot) # For correlation tables
library(lmtest) # For het-sked test
library(sandwich) # For het-sked test
library(AER) # For fixed effects
library(plm) # For fixed effects

## Load in the 2012 housing data and 2011-12 school district grades
## House data
house2012 <- read_csv("~/Desktop/School/Graduate/Econometrics/Project/Data/house2012.csv")

## District data
districtGrades <- read_excel("~/Desktop/School/Graduate/Econometrics/Project/Data/1112_LRC_DISTRICT.xls")

## Use sql to select the needed variables from houses in 2012 and district grades
## Name this dataset houseValues
houseValues <- sqldf('SELECT house2012.location_type AS `location_type`,
                     house2012.estmtd_home_val_div_1000 AS `home_value`,
                     house2012.CBSAType AS `cbsa_type`,
                     house2012.city AS `city`,
                     house2012.state AS `state`,
                     house2012.zip AS `zip`,
                     house2012.bathroom_cnt AS `bathrooms`,
                     house2012.bedroom_cnt AS `bedrooms`,
                     house2012.room_cnt AS `rooms`,
                     house2012.construction_type_code AS `construction`,
                     house2012.built_year AS `built`,
                     house2012.building_area AS `building_area`,
                     house2012.ncesid AS `ncesid`,
                     house2012.squareMile AS `city_square_miles`,
                     house2012.pupilDensity AS `density`,
                     house2012.yearEndEnrollment AS `year_end_enrollment`,
                     house2012.popAsian AS `pop_asian`,
                     house2012.popPacificIsland AS `pop_pacific_island`,
                     house2012.popBlack AS `pop_black`,
                     house2012.popIndian AS `pop_indian`,
                     house2012.popHispanic AS `pop_hispanic`,
                     house2012.popWhite AS `pop_white`,
                     house2012.popMultiple AS `pop_multiple`,
                     house2012.popPoverty AS `pop_poverty`,
                     house2012.teacherAverageSalary AS `teacher_salary`,
                     house2012.districtMedianIncome AS `district_median_income`,
                     house2012.localTaxEffor AS `local_tax_effort`,
                     house2012.instructionalExpenditurePupil AS `instructional_expenditure_pupil`,
                     house2012.revenuePerPupil AS `revenue_per_pupil`,
                     house2012.lat AS `lat`,
                     house2012.lon AS `lon`,
                     districtGrades.County AS `county`,
                     districtGrades.Designation AS `designation`,
                     districtGrades.StandardsMetRate AS `standards_met_rate`,
                     districtGrades.GradRate4yr AS `grad_rate_4_yr`,
                     districtGrades.Performance_Index_Score AS `performance_index_score`,
                     districtGrades.Mean_Act AS `mean_Act`
                     FROM house2012
                     LEFT JOIN districtGrades
                     ON house2012.irn = districtGrades.districtIRN')

## Now filter out the values to just include not null
## Look at data with 0 bathrooms (important to know because homes with 0 bathrooms shouldn't be valued highly)
houseValues %>%
  filter(bathrooms == 0 & !is.na(home_value)) %>%
  summarize(median = median(home_value)) # Get median value of house with 0 bathrooms ($145,000)

## Look at plot of 0 bathrooms
houseValues %>% # Use our dataset 
  filter(bathrooms == 0 & !is.na(home_value)) %>% # Filter the conditions
  # We want a density plot of 
  ggplot(aes(x = home_value)) +
  geom_density(fill = "lightblue", color = "darkblue") +
  # Change the x axis to be in dollar format, clip off the right tail as that part of distribution is near 0
  scale_x_continuous(labels=scales::dollar_format(), limits=c(0, 750)) +
  # Add the median line
  geom_vline(aes(xintercept = median(home_value)), color = "darkblue", linetype = "dashed") +
  # Give labels
  labs(title = "Figure 6. --- Ohio Home Values", # This is actually figure 6 that will be used in the paper
       subtitle = "With 0 Bathrooms",
       x = "Home Value (In Thousands, $)",
       y = "Denstiy",
       caption = "David Slusser\n ECON 62054: Econometrics I") +
  theme_minimal() +
  theme(text = element_text(size = 15))


#############################################################################################
################################### Creating our dataset ####################################
#############################################################################################

## Use the houseValues dataset to make the changes
houseValues <- houseValues %>%
  # Filter out missing rooms and ACT
  filter(!is.na(bathrooms), !is.na(bedrooms), !is.na(rooms), !is.na(mean_Act)) %>%
  # The graduation rate is a character so if "--" means missing
  filter(grad_rate_4_yr != "--" & # grad_rate_4_yr is being read as character so filter out
           mean_Act != "NC") %>%
  # We only want to look at multifamily and single family dwelling units so filter by location type
  filter(location_type == "M" | location_type == "S") %>%
  mutate(grad_rate_4_yr = as.numeric(grad_rate_4_yr), # Make grad_rate_4_yr a numeric variable
         mean_Act = as.numeric(mean_Act)) %>% # Make mean ACT a numeric variable
  # Additionally we assume that there are at least 1 of every room type and the value of a house is greater than 0
  # But also there should be diminishing returns on number of rooms and/or measurement error
  filter(bathrooms > 0 & home_value > 0 & building_area > 0 & rooms <= 20 & rooms > 0 &
           bedrooms > 0 & !is.na(home_value))

## Change the CBSA type to a character variable (is helpful in the data viz section)
## In the model can use as.factor()
houseValues$cbsa_type <- ifelse(houseValues$cbsa_type == 1, "Micro",
                                ifelse(houseValues$cbsa_type == 2, "Metro",
                                       ifelse(is.na(houseValues$cbsa_type), "Other", "Other")))

houseValues$cbsa_type <- ifelse(!is.na(houseValues$cbsa_type), houseValues$cbsa_type, "Other")



#############################################################################################
#############################################################################################
################################ Understanding our dataset ##################################
#############################################################################################
#############################################################################################

## Get summary statisitics for house value, bathrooms, bedrooms, rooms, building area,
## city miles, population density, population stats, district stats

## Create dataset for summary statistics
houseValuesSumStats <- houseValues %>%
  select(home_value, bathrooms, bedrooms, rooms, building_area, city_square_miles, density,
         year_end_enrollment, pop_white, pop_black, pop_asian, pop_indian, pop_hispanic, pop_pacific_island,
         pop_multiple, pop_poverty, teacher_salary, district_median_income, local_tax_effort,
         instructional_expenditure_pupil, revenue_per_pupil, standards_met_rate, grad_rate_4_yr,
         performance_index_score, mean_Act)

## Use stargazer for data table
## We want to make the sumamry stats table with median
## This will be table 1
stargazer(as.data.frame(houseValuesSumStats), type = "text", title="Table 1. --- Summary Statistics: Full Data", 
          digits=2, out= "sumStats.html",
          covariate.labels=c("Home Value ($1,000 Dollars)", "Bathrooms", "Bedrooms",
                             "Rooms", "Building Area","City Square Miles", "Population Density",
                             "Year End Enrollment", "Population White (%)","Population Black (%)",
                             "Population Asian (%)", "Population Indian (%)", "Population Hispanic (%)",
                             "Population Pacific Island (%)", "Population Multiple (%)", "Population Poverty (%)",
                             "Teacher Salary (Dollars)", "District Median Income (Dollars)", "Local Tax Effort*",
                             "Instructional Expenditure Pure Pupil (Dollars)", "Revenue Per Pupil (Dollars)",
                             "Standards Met Rate (%)", "4 Yr Graduation Rate (%)", "Performance Index Score",
                             "Mean District ACT"),
          median = TRUE, # Include median
          font.size = "small", # Make the value small
          notes = "* Estimated by Ohio Taxation Department to Estimate Public Support for Schools") # Add footnote

## Calculate the correlations of the variables
## Create a correlation table plot using ggcorrplot
## Save correlations of houseValuesSumStats in data frame called corr
## Round to two decimal places
corr <- round(cor(houseValuesSumStats, use="complete.obs"), 2)

## Create a heatmap of the correaltion among the variables
## This is useful in constructing our model
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = theme_minimal(),
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "Figure 1. --- Correlation Matrix",
       subtitle = "House Value and District Qualities State of Ohio 2012",
       caption = "David Slusser\n ECON 62054: Econometrics I")

## We will now look at the most correlated variables to home value and variables we expect to matter
## Look at the home values, district income, building area, mean act distribution by CBSA
## We presume the CBSA has an impact on house values and other underlying effects
## Metropolitan areas (Cleveland, Columbus, etc) will differ from other areas in Ohio
## Especially those more rural areas

## Create a density plot of the home value
## Color and fill by the CBSA (Core-Based Statistical Area) type
ggplot(houseValues, aes(x = home_value, fill = cbsa_type, color = cbsa_type)) +
  geom_density(alpha = 0.25) +
  labs(title = "Figure 2. --- Ohio Home Values",
       subtitle = "By CBSA Type",
       x = "Home Value (In Thousands, $)",
       y = "Denstiy",
       caption = "David Slusser\n ECON 62054: Econometrics I") +
  # Label the fill and colors
  scale_fill_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  scale_color_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  # Make the x axis in dollar format
  scale_x_continuous(labels=scales::dollar_format(), limits=c(0, 750)) +
  theme_minimal() +
  theme(text = element_text(size = 15))

## Create a density plot of the district median income
## Color and fill by the CBSA (Core-Based Statistical Area) type
ggplot(houseValues, aes(x = district_median_income, fill = cbsa_type, color = cbsa_type)) +
  geom_density(alpha = 0.25) +
  labs(title = "Figure 3. --- Ohio School District Median Incomes",
       subtitle = "By CBSA Type",
       x = "District Median Incomes ($)",
       y = "Denstiy",
       caption = "David Slusser\n ECON 62054: Econometrics I") +
  # Label the fill and colors
  scale_fill_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  scale_color_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  # Make the x axis in dollar format
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(text = element_text(size = 15))

## Create a density plot of the building area 
## Color and fill by the CBSA (Core-Based Statistical Area) type
ggplot(houseValues, aes(x = building_area, fill = cbsa_type, color = cbsa_type)) +
  geom_density(alpha = 0.25) +
  labs(title = "Figure 4. --- Ohio House Building Area",
       subtitle = "By CBSA Type",
       x = "Building Area (Square Feet)",
       y = "Denstiy",
       caption = "David Slusser\n ECON 62054: Econometrics I") +
  # Label the fill and colors
  scale_fill_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  scale_color_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  scale_y_continuous() +
  # Make the x axis in dollar format
  scale_x_continuous(labels=scales::comma, limits = c(0, 10000)) +
  theme_minimal() +
  theme(text = element_text(size = 15))

## Create a density plot of the mean ACT by school district 
## Color and fill by the CBSA (Core-Based Statistical Area) type
ggplot(houseValues, aes(x = mean_Act, fill = cbsa_type, color = cbsa_type)) +
  geom_density(alpha = 0.25) +
  labs(title = "Figure 5. --- Ohio School District: Mean ACT",
       subtitle = "By CBSA Type",
       x = "Mean ACT",
       y = "Denstiy",
       caption = "David Slusser\n ECON 62054: Econometrics I") +
  # Label the fill and colors
  scale_fill_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  scale_color_discrete(name = "CBSA Type", labels = c("Metro", "Micro", "Other")) +
  theme_minimal() +
  theme(text = element_text(size = 15))


#############################################################################################
#############################################################################################
#################################### Regression Analysis ####################################
#############################################################################################
#############################################################################################

## We want to be able to repeat our estimates
## Set the seed
set.seed(12345)

## Create model 1
## Just use the house data
## This will be our simple model that will be used in comparison with model 2 (our fixed effects model)
model1 <- lm(home_value ~ bathrooms +
               # Square the bedrooms (test other non-linearities; inclue in full writeup)
               bedrooms + I(bedrooms^2) +
               rooms + 
               # Adjust the building area by 1000 to understand how the value of a home changes by increasing the
               # Building area by 1000 feet (easier interpretation)
               I(building_area / 1000) +
               built, # Inclue the year built, as a newer house we expect to increase home value
             data = houseValues)

summary(model1) # View the results of model 1
bptest(model1) # We need to test for het-sked, use breusch-pagan
coeftest(model1, vcov = vcovHC(model1, type="HC1")) # Get the robust standard errors

## Create model 2
## Fixed effects model of house data
## There are underlying location effects, so we will hold the school district constant
## It is the same as model 1 but with different intercepts for the school district
## Ideally, we'd be able to control for neighborhood effects
## We know houses on Lake Erie will be valued higher than houses in a rural area, all else equal
## Thus need for this fixed effects model
model2 <- plm(home_value ~ bathrooms +
                # Square the bedrooms (test other non-linearities; inclue in full writeup)
                bedrooms + I(bedrooms^2) +
                rooms +
                # Adjust the building area by 1000 to understand how the value of a home changes by increasing the
                # Building area by 1000 feet (easier interpretation)
                I(building_area / 1000) +
                built,
              data = houseValues,
              index = c("ncesid"), # Hold the school district constant
              model = "within")  # Fixed effects
summary(model2)  # View the results of model 2
bptest(model2) # Test for het-sked,  use breusch-pagan
coeftest(model2, vcov = vcovHC(model2, type="HC1")) # Get the robust standard errors

## Create model 3
## Build upon model 1
## Add demogographic data
## We will now add in information to the model that relates to the demographics
## This is to compare to the fixed effects model as we slowly add more information
## Include the explanation of variables in the write-up
model3 <- lm(home_value ~ bathrooms +
               # Square the bedrooms (test other non-linearities; inclue in full writeup)
               bedrooms + I(bedrooms^2) +
               rooms +
               # Adjust the building area by 1000 to understand how the value of a home changes by increasing the
               # Building area by 1000 feet (easier interpretation)
               I(building_area / 1000) +
               built + city_square_miles + 
               I(density / 100) + # Divide density by 100 to better understand a 1 unit change
               as.factor(cbsa_type) + 
               pop_poverty, # We expect an increase in poverty to decrease price of the house
             data = houseValues)
summary(model3) # View the results of model 3
bptest(model3) # Test for het-sked,  use breusch-pagan
coeftest(model3, vcov = vcovHC(model3, type="HC1")) # Get the robust standard errors

## Create model 4
## Build upon model 3
## Add school data
## We will now add in information to the model that relates to the school district itself
## This is to compare to the fixed effects model as we slowly add more information
## Include the explanation of variables in the write-up
model4 <- lm(home_value ~ bathrooms +
               # Square the bedrooms (test other non-linearities; inclue in full writeup)
               bedrooms + I(bedrooms^2) +
               rooms +
               # Adjust the building area by 1000 to understand how the value of a home changes by increasing the
               # Building area by 1000 feet (easier interpretation)
               I(building_area / 1000) +
               built + city_square_miles +
               # We removed density, put in the writeup why
               as.factor(cbsa_type) + 
               pop_poverty +
               # Divide salary by 1000 to better understand a 1 unit change (a change in $1,000)
               I(teacher_salary/1000) +
               local_tax_effort, # This variable illustrates the effort the community places on funding schools
             data = houseValues)
summary(model4) # View the results of model 4
bptest(model4) # Test for het-sked,  use breusch-pagan
coeftest(model4, vcov = vcovHC(model4, type="HC1")) # Get the robust standard errors


## Create model 5
## Build upon model 4
## Add school data on quality
## We will now add in information to the model that relates to the school district itself as in how good is the district?
## We assume there will be some premium on house values for a good school district
## This is to compare to the fixed effects model as we slowly add more information
## Include the explanation of variables in the write-up
model5 <- lm(home_value ~ bathrooms +
               # Square the bedrooms (test other non-linearities; inclue in full writeup)
               bedrooms + I(bedrooms^2) +
               rooms +
               # Adjust the building area by 1000 to understand how the value of a home changes by increasing the
               # Building area by 1000 feet (easier interpretation)
               I(building_area / 1000) +
               built + city_square_miles +
               as.factor(cbsa_type) +
               pop_poverty +
               # Divide salary by 1000 to better understand a 1 unit change (a change in $1,000)
               I(teacher_salary/1000) + +
               local_tax_effort +
               # Divide revenue by 1000 to better understand a 1 unit change (a change in $1,000)
               I(revenue_per_pupil / 1000) + # This variable illustrates the effort the community places on funding schools
               grad_rate_4_yr + mean_Act, # Grad rate and ACT illustrate best quality of school district
             data = houseValues)
summary(model5) # View the results of model 5
bptest(model5) # Test for het-sked,  use breusch-pagan
coeftest(model5, vcov = vcovHC(model5, type="HC1")) # Get the robust standard errors





