library(tidyverse)
# library(readr)
library(gtsummary)
library(survey)

# Variable lists
tbl1covariates = c("period_indic", "catag6", "irsex", "race_cat",
                   "age_cat", "marital_status", "educ_cat", "income_cat",
                   "health_insurance", "health_status", "employment_status",
                   "region", "urban_rural", "substance_use")


# Cannabis Decriminalization

data1 <- "state, abbreviation, st, decriminalization
Alabama, AL, 01, 0
Alaska, AK, 02, 1
Arizona, AZ, 04, 1
Arkansas, AR, 05, 0
California, CA, 06, 1
Colorado, CO, 08, 1
Connecticut, CT, 09, 1
Delaware, DE, 10, 1
District of Columbia, DC, 11, 1
Florida, FL, 12, 0
Georgia, GA, 13, 0
Hawaii, HI, 15, 1
Idaho, ID, 16, 0
Illinois, IL, 17, 1
Indiana, IN, 18, 0
Iowa, IA, 19, 0
Kansas, KS, 20, 0
Kentucky, KY, 21, 0
Louisiana, LA, 22, 0
Maine, ME, 23, 1
Maryland, MD, 24, 1
Massachusetts, MA, 25, 1
Michigan, MI, 26, 1
Minnesota, MN, 27, 1
Mississippi, MS, 28, 1
Missouri, MO, 29, 1
Montana, MT, 30, 1
Nebraska, NE, 31, 1
Nevada, NV, 32, 1
New Hampshire, NH, 33, 0
New Jersey, NJ, 34, 1
New Mexico, NM, 35, 1
New York, NY, 36, 1
North Carolina, NC, 37, 0
North Dakota, ND, 38, 1
Ohio, OH, 39, 1
Oklahoma, OK, 40, 0
Oregon, OR, 41, 1
Pennsylvania, PA, 42, 0
Rhode Island, RI, 44, 1
South Carolina, SC, 45, 0
South Dakota, SD, 46, 0
Tennessee, TN, 47, 0
Texas, TX, 48, 0
Utah, UT, 49, 0
Vermont, VT, 50, 1
Virginia, VA, 51, 1
Washington, WA, 53, 1
West Virginia, WV, 54, 0
Wisconsin, WI, 55, 0
Wyoming, WY, 56, 0"

state_cannabis <- read_csv(data1)

# Psilocbin Legislation

data2 <- "approve_date,effective_date,State,County,City,Action,fips_5
2021-10-01, 2021-10-01, California, Humbolt, Arcata, Deprioritized, 06023
2019-06-01, 2019-06-01, California, Alameda, Oakland, Deprioritized, 06001
2020-12-01, 2020-12-01, California, Alameda, Oakland, Decriminalized, 06001
2023-07-01, 2023-07-01, California, Alameda, Berkeley, Deprioritized, 06001
2023-10-01, 2023-10-01, California, Humbolt, Eureka, Decriminalized, 06023
2022-09-01, 2022-09-01, California, San Francisco, San Francisco, Deprioritized, 06075
2020-01-01, 2020-01-01, California, Santa Cruz, Santa Cruz, Deprioritized, 06087
2022-11-01, 2022-11-01, Colorado, Adams, All, Legalized, 08001
2022-11-01, 2022-11-01, Colorado, Arapahoe, All, Legalized, 08005
2022-11-01, 2022-11-01, Colorado, Boulder, All, Legalized, 08013
2022-11-01, 2022-11-01, Colorado, Denver, All, Legalized, 08031
2022-11-01, 2022-11-01, Colorado, Douglas, All, Legalized, 08035
2022-11-01, 2022-11-01, Colorado, El Paso, All, Legalized, 08041
2022-11-01, 2022-11-01, Colorado, Jefferson, All, Legalized, 08059
2022-11-01, 2022-11-01, Colorado, Larimer, All, Legalized, 08069
2022-11-01, 2022-11-01, Colorado, State-Wide, All, Legalized, 08000
2019-12-01, 2019-12-01, Colorado, Denver, Denver, Deprioritized, 08031
2021-03-01, 2021-03-01, District of Columbia, Washington, NA, Deprioritized, 11001
2021-05-01, 2021-05-01, Massachusetts, Middlesex, Cambridge, Deprioritized, 25017
2022-01-01, 2022-01-01, Massachusetts, Hampshire, Easthampton, Deprioritized, 25015
2021-06-01, 2021-06-01, Massachusetts, Hampshire, Northampton, Deprioritized, 25015
2021-04-01, 2021-04-01, Massachusetts, Middlesex, Somerville, Deprioritized, 25017
2020-12-01, 2020-12-01, Michigan, Washtenaw, Ann Arbor, Deprioritized, 26161
2022-01-01, 2022-01-01, Michigan, Wayne, Detroit, Decriminalized, 26163
2021-12-01, 2021-12-01, Michigan, Kent, Grand Rapids, Support decriminalization, 26081
2022-06-01, 2022-06-01, Michigan, Oakland, Hazel Park, Decriminalized, 26125
2021-04-01, 2021-04-01, Michigan, Washtenaw, Washtenaw County, Deprioritized, 26161
2023-05-01, 2023-05-01, Michigan, Oakland, Ferndale, Decriminalized, 26125
2024-04-01, 2024-04-01, Michigan, Washtenaw, Ypsilanti, Deprioritized, 26161
2021-02-01, 2021-02- 01, Oregon, State-Wide, State-Wide, Decriminalized, 41000
2023-01-01, 2023-01-01, Oregon, Clackamas, All, Legalized, 41005
2023-01-01, 2023-01-01, Oregon, Lane, All, Legalized, 41039
2023-01-01, 2023-01-01, Oregon, Marion, All, Legalized, 41047
2023-01-01, 2023-01-01, Oregon, Multnomah, All, Legalized, 41051
2023-01-01, 2023-01-01, Oregon, Washington, All, Legalized, 41067
2024-05-01, 2024-05-01, Oregon, Clackamas, All, Decriminalization, 41005
2024-05-01, 2024-05-01, Oregon, Lane, All, Decriminalization, 41039
2024-05-01, 2024-05-01, Oregon, Marion, All, Decriminalization, 41047
2024-05-01, 2024-05-01, Oregon, Multnomah, All, Decriminalization, 41051
2024-05-01, 2024-05-01, Oregon, Washington, All, Decriminalization, 41067
2024-05-01, 2024-05- 01, Oregon, State-Wide, State-Wide, Decriminalization, 41000
2024-09-01, 2024-09-01, Oregon, Clackamas, All, Possession reclassified as Misdemeanor, 41005
2024-09-01, 2024-09-01, Oregon, Lane, All, Possession reclassified as Misdemeanor, 41039
2024-09-01, 2024-09-01, Oregon, Marion, All, Possession reclassified as Misdemeanor, 41047
2024-09-01, 2024-09-01, Oregon, Multnomah, All, Possession reclassified as Misdemeanor, 41051
2024-09-01, 2024-09-01, Oregon, Washington, All, Possession reclassified as Misdemeanor, 41067
2024-09-01, 2024-09- 01, Oregon, State-Wide, State-Wide, Possession reclassified as Misdemeanor, 41000
2021-12-01, 2021-12-01, Washington, Jefferson, City of Port Townsend, Deprioritized, 53031
2021-07-01, 2021-07-01, Washington, Clark, All, Decriminalized, 53011
2021-07-01, 2021-07-01, Washington, King, All, Decriminalized, 53033
2021-07-01, 2021-07-01, Washington, Pierce, All, Decriminalized, 53053
2021-07-01, 2021-07-01, Washington, Snohomish, All, Decriminalized, 53061
2021-07-01, 2021-07-01, Washington, Spokane, All, Decriminalized, 53063
2021-07-01, 2021-07- 01, Washington, State-Wide, State-Wide, Decriminalized, 53000
2021-10-01, 2021-10-01, Washington, King, Seattle, Deprioritized, 53033
2023-07-01, 2023-07-01, Washington, Jefferson County, NA, Deprioritized, 53031
2024-09-01, 2024-09-01, Washington, Thurston County, Olympia, Deprioritized, 53067"

psilocybin_policy <- read_csv(data2)

# Functions

generate_plot <- function(data, outcome, title) {
  total <- data %>% filter(!!sym(outcome) == "Positive" | !!sym(outcome) == 1) %>% nrow()
  data %>%
    group_by(!!sym(outcome), year) %>%
    tally() %>%
    filter(!!sym(outcome) == "Positive" | !!sym(outcome) == 1 ) %>%
    droplevels() %>%
    ggplot(aes(y = n, x = year)) +
    geom_col(width = 0.5, fill = "steelblue") +
    theme_bw() +
    labs(y = "Number of Respondents", x = "Year", title = paste(title, "- Total:", total))
}

tbl1covariates = c("period_indic", "catag6", "irsex", "race_cat", 
                   "irmarit", "eduhighcat", "irwrkstat", "income", "rskyfqtes", 
                   "halluc_p",
                   "mj_ever", "trq_ever", "inh_ever", "ecstasy_ever", "pcp_ever","sed_ever","coc_ever", "meth_ever", 
                   "difobther", "difobtlsd")

tbl1_unweighted <- function(data, outcome) {
  data %>%
    tbl_summary(
      include = all_of(tbl1covariates),
      by = all_of(outcome),
      percent = "column",
      label = c( demo_var_list, outcome_list_label, substance_list_label) 
    ) %>% modify_header(label =  "Variable")
}

tbl1_weighted <- function(design, outcome) {
  design %>%
    tbl_svysummary(
      include = all_of(tbl1covariates),
      by = all_of(outcome),
      percent = "column",
      label = c(demo_var_list,  outcome_list_label, substance_list_label)
    ) %>% add_overall() %>% add_p() %>% modify_header(label =  "Variable")
}

tbl1_weighted_alt <- function(design, outcome) {
  design %>%
    tbl_svysummary(
      include = all_of(tbl1covariates[2:20]),
      by = all_of(outcome),
      percent = "column",
      label = c(demo_var_list,  outcome_list_label, substance_list_label)
    ) %>% add_overall() %>% add_p() %>% modify_header(label =  "Variable")
}

ud_regress <- function(substance, datsrc) {
  formula <- as.formula(paste("opioid_use ~", substance))
#Perform logistic regression using the glm function with binomial family
 glm(data = datsrc, formula, family = "binomial") %>% 
   tbl_regression(exponentiate = TRUE,
                  label = c(demo_var_list, substance_list_label))
}

ud_regress_a <- function(substance) {
  formula <- as.formula(paste("opioid_use ~", substance))
  # Perform Weighted logistic regression using the svyglm function with binomial family
  svyglm(design = desg_2, 
        formula = formula, family = "binomial")  %>% 
    tbl_regression(exponentiate = TRUE,
                  label = c(demo_var_list, substance_list_label))
}

ud_regress_b <- function(substance) {
  formula <- as.formula(paste("opioid_use ~", substance))
  # Perform Weighted logistic regression using the svyglm function with binomial family
  svyglm(design = desg_1, 
        formula = formula, family = "binomial")  %>% 
    tbl_regression(exponentiate = TRUE,
                  label = c(demo_var_list, substance_list_label))
}

# New Regression function that works for all substances
use_regress_a = function(dependent ,substance) {
  formula = as.formula(paste(dependent, " ~", substance))
  svyglm(design = desg_2, formula = formula, family = "binomial") %>% 
    tbl_regression(exponentiate = TRUE, label = c(demo_var_list, substance_list_label))
}

 use_regress_b = function(dependent ,substance) {
  formula = as.formula(paste(dependent, " ~", substance))
  svyglm(design = desg_1, formula = formula, family = "binomial") %>% 
    tbl_regression(exponentiate = TRUE, label = c(demo_var_list, substance_list_label))
}