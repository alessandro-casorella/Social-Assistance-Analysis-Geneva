library(haven)
library(dplyr)
library(purrr)
library(tidyr)
library(fastDummies)
library(forcats)
library(ggplot2)
library(fixest)
library(stringr)

# 1) DATA LOADING, PRELIMINARY EXPLORATION AND MERGING STEPS
path <- ".../shplong_p_user.dta"

# We start by uploading the variable library
metadata <- read_stata(path, n_max = 0) # given such a big file, I only want the metadata
# Extracting variables names and labels :
variable_library <- data.frame(
  variable_name = names(metadata),
  label = map_chr(metadata, ~ attr(.x, "label") %||% NA_character_),
  stringsAsFactors = FALSE
)
head(variable_library) # we can look here at the set of variables
##########
##########
# We now look at the number of both total person-year observations
# and unique individuals
individuals <- read_stata(path, col_select = "idpers")
total_obs <- nrow(individuals)
print(paste("Nombre total d'observations :", total_obs))
# We have 344678 observations.
unique_people <- length(unique(individuals$idpers))
print(paste("Nombre d'individus uniques :", unique_people))
# We have 49876 unique individuals in the sample.

# The ratio between overall observations and unique individuals 
# involved in the sample is 344,678 / 49,876 = 6.91. Why?

# Well, even though our panel spans 20 years or more, we have to account for 
# the fact that many individuals are interviewed for less than its total duration.

# Nevertheless, we have enough unique individuals to implement DML methods. 
# At the same time, we observe each individual for an average of 6.9 years, 
# which provides sufficient longitudinal depth for causal identification.
rm(individuals)
##########
##########
# Many useful variables are not available in the individual based longfile, 
# but can to be found in the houshold based longfile, uploaded here below.
# This includes :
# - the variable canton (our treatment)
# - useful controls/proxies for the forward-looking charachter of individuals : 
#   hh29 (owner or tenant?), hi20a (can you face unexpected expenses ?),
#   hldtyp (household type), hi22 (saving in 3rd pillar), hi30 (do you find
#   it's necessary to have 3rd pillar ?), hi20n (saving at least 400CHF/months)
#   and hhmove (did the persone moved since the last interview, the year before). 
# - useful controls : idispy (disposable household income), nbpers (number of 
#   persons in household).
# We will join all these variables, through idhous, from the h to the p longfile.
# First we upload the household longfile :
head(df_merged$iptotn, 20)
path_hous <- ".../shplong_h_user.dta"
metadata_hous <- read_stata(path_hous, n_max = 0)
# Extracting variables names and labels :
variable_library_hous <- data.frame(
  variable_name_hous = names(metadata_hous),
  label_hous = map_chr(metadata_hous, ~ attr(.x, "label") %||% NA_character_),
  stringsAsFactors = FALSE
)
head(variable_library_hous) # to see the variables
# Then we do the appropriate merge :
variables_to_extract <- c("idhous", "year", "canton", "hh29", "hi20a", "hldtyp", 
                          "hi22", "hi30", "hi20n", "hhmove", "idispy", "nbpers")
df_hous_subset <- read_stata(path_hous, col_select = all_of(variables_to_extract))
df_p <- read_stata(path) 
df_merged <- df_p %>%
  left_join(df_hous_subset, by = c("idhous", "year")) # leftjoin keeps all df_p observations
rm(df_hous_subset)
gc()
# Let's control if the join has worked well :
variable_library_updated <- data.frame(
  variable_name = names(df_merged),
  label = map_chr(df_merged, ~ attr(.x, "label") %||% NA_character_),
  stringsAsFactors = FALSE
)
head(variable_library_updated) # it worked ! ! ! 
saveRDS(df_merged, ".../df_merged_p_h.rds") 
# We save the file, in order to avoid redoing the heavy previous steps next time.
# We will only need to upload :
df_merged = readRDS(".../df_merged_p_h.rds")
##########
##########
# Let's now look at the number of observations we have, each year.
# We first do it for the overall dataframe, then for Geneva. 
# We could then confirm whether our sample observations are stable through time.
print(table(df_merged$year))
# The trend shows an overall increase in observations
# between 1999 and 2023, likely due to the refreshment sample.
# We also see some dips (2002–2003, for example, or 2017–2019),
# as well as a few spikes (2020). 
obs_geneve_by_year <- df_merged %>%
  filter(str_detect(as_factor(canton), "ge") & str_detect(as_factor(canton), "geneva")) %>%
  group_by(year) %>%
  tally()
print(obs_geneve_by_year, n = Inf)
# The Geneva observations are consistent with the overall data,
# and account for approximately 4–5% of the total. 

# The variability in the number of observations over time, whether
# at the Geneva or Swiss level, suggests a certain degree of heterogeneity 
# in the sample from one year to the next. 
# To ensure that our model properly compares the treatment and 
# control groups year by year, it would be prudent to add 
# years fixed effects to our diff-in-diff model. 


# 2) GENERAL PREPROCESSING 
# Preliminary note :
# We aim to prepare a cleaned, model-ready dataset. 
# Regarding the imputation of missing values, 
# we opted for a global imputation strategy 
# on the entire dataset rather than a "leakage-safe" 
# rolling window or cross-validation-based imputation. 
# This choice was made for computational efficiency 
# given the large scale of the SHP panel.

# We delete a small set of useless variables,
# before starting :
df_cleaned <- df_merged %>% 
  select(-idspou, -idint, -filter, -pmodes)
##########
##########
# Managing variable class and negative values :

# We see here  below that pdate is the only non numerical variable.
# We'll replace it by the corresponding year interview only,
# and encode it as numeric. 
non_numeric_vars <- names(df_cleaned)[!sapply(df_cleaned, is.numeric)]
print(non_numeric_vars) # pdate is the only non numeric variable
class(df_cleaned$pdate) # it has datetime class (not a problem, anyway)

# As all variables are recorded as numeric (or datetime, well...),
# we transform in factors the ones characterized 
# by explicit labels for the positive values.
# Why focusing on positive values ? 
# Because, to be precise, a lot of numerical have class
# 'haven_labelled', because they contain categorical labels
# for some specific negative values.

# Let's apply the factors transformation, where suitable :
df_cleaned <- df_cleaned %>%
  mutate(across(everything(), ~ {
    labs <- attr(., "labels")
    if (!is.null(labs) && any(labs >= 0)) {
      as_factor(.) 
    } else {
      . 
    }
  }))
##########
##########
# Many variables have labelled negative values,
# as we said.
# This does not represent a problem for factors,
# but for numerical variables, R will interpret
# these negative values as numbers, that's why
# we shall treat them. 
# Let's look at the meaning of negative values.
all_labels <- lapply(df_cleaned, function(x) attr(x, "labels"))
neg_labels <- unlist(all_labels)
unique_neg_labels <- neg_labels[neg_labels < 0]
unique_neg_labels <- unique_neg_labels[!duplicated(unique_neg_labels)]
print(sort(unique_neg_labels))
# For all non-factors, after additional exploration,
# we conclude that :
# -9, -3 and -4 must be set to 0
# -1, -2, -5, -6, -7, -8 can be encoded as NA.
# Let's do it :
df_cleaned <- df_cleaned %>%
  mutate(across(where(~ !is.factor(.)), as.numeric))
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.numeric), ~ {
    x <- .
    x[x %in% c(-9, -3, -4)] <- 0
    x[x %in% c(-1, -2, -6, -7, -8, -5)] <- NA
    return(x)
  }))
# The transformation doesn't work for pdate, because of its class structure.
# As this variable is quite redundant with year, we can delete it :
df_cleaned <- df_cleaned %>%
  select(-pdate)
#########
#########
# Warning : in subsequent steps, some functions could eliminate
# the labels and delete our factor transformation.
# For this reason, we will not apply transformations on df_cleaned 
# anymore. df_cleaned will remaine a "pure" dataset, through which 
# we can have information of variable classes. 

saveRDS(df_cleaned, ".../df_cleaned.rds") 
# We save the file, in order to avoid redoing the heavy previous steps next time.
# We will only need to upload :
df_cleaned = readRDS(".../df_cleaned.rds")

# We will now use df :
df <- df_cleaned
##########
##########
# Note : concerning the replacement of negative values for non-factors,
# here below is a more detailed code to justify our choices.
# We worked with the raw files from both persons and households, 
# as the nominology could differ.
meta_source <- read_stata(".../shplong_p_user.dta", n_max = 0)
dictionnaire_complet <- map_df(meta_source, ~ {
  lbls <- attr(.x, "labels")
  if (!is.null(lbls)) {
    data.frame(label = names(lbls), code = as.numeric(lbls))
  } else {
    NULL
  }
}) %>%
  distinct(code, label) %>% 
  filter(code < 0) %>%     
  arrange(code)
print(dictionary_negative_labels)
# We do the same for the household file,
# for which we also have some variables :
meta_source_h <- read_stata(".../shplong_h_user.dta", n_max = 0)
dictionary_negative_labels_h <- map_df(meta_source_h, ~ {
  lbls <- attr(.x, "labels")
  if (!is.null(lbls)) {
    data.frame(label = names(lbls), code = as.numeric(lbls))
  } else {
    NULL
  }
}) %>%
  distinct(code, label) %>% 
  filter(code < 0) %>%
  arrange(code)
print(dictionary_negative_labels_h)
# We also focus on all the values taken by our treatment, 
# to validate our choices :
# To prevent the lost of labels, let's see to which numeric index
# does Geneva corresponds :
labels_raw <- attr(meta_source_h$canton, "labels") 
dictionnaire_canton_h <- data.frame(
  label = names(labels_raw),
  code = as.numeric(labels_raw)
)
correspondance_cantons <- dictionnaire_canton_h %>%
  filter(code > 0) %>%
  mutate(
    Index_R = row_number() + sum(dictionnaire_canton_h$code < 0),
    Nom_Canton = label,
    Code_Stata = code
  ) %>%
  select(Index_R, Nom_Canton, Code_Stata)
correspondance_cantons <- dictionnaire_canton_h %>%
  filter(code > 0) %>%
  mutate(
    Index_R = row_number() + sum(dictionnaire_canton_h$code < 0),
    Nom_Canton = label,
    Code_Stata = code
  ) %>%
  select(Index_R, Nom_Canton, Code_Stata)
print(as.data.frame(correspondance_cantons))
# Geneva corresponds to variable number 13. 
# Similar code could be used in future in order to have knowledge 
# of other factor's modalities,
# while df_cleaned would help identifying factors. 
##########
##########
# Handling missing values :

# Concerning both treatment and outcomes, we will delete the missing values.
# But before working on them,
# Let's check for their missing values statistics.
na_summary <- df %>%
  select(canton, iwely, iuney, iempyg) %>%
  summarise(across(everything(), list(
    Manquants_NA = ~sum(is.na(.)),
    Présents_Valid = ~sum(!is.na(.))
  ))) %>%
  pivot_longer(everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)")
print(na_summary)
# Our treatment has 0% of missing values,
# Our outcomes have around 10% of NA, which is not a problem.
# We delete NA  rows for our treatment and outcomes
df <- df %>% filter(!is.na(iwely), !is.na(iempyg),!is.na(canton), !is.na(iuney))
nrow(df) # 298989 row are sufficient to do our analysis

sum(is.na(df)) # 160761461 NA remaning. We shall treat them.
# We impute the remaining numerical variables
# by the median, and the factors by the mode. 
# Warning : we eliminate the variables with more than half NA. 
# Let's start :
# We define the calculation of mode :
calc_mode_clean <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(NA) 
  ux <- unique(x_clean)
  ux[which.max(tabulate(match(x_clean, ux)))]
}
# We fix the elimination treshold and start imputation :
seuil_presence <- 0.5 
variables_valides <- df %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "prop") %>%
  filter(prop >= seuil_presence) %>%
  pull(var)

df <- df %>%
  select(all_of(variables_valides)) %>%
  mutate(across(everything(), ~ {
    if (is.numeric(.)) {
      val <- median(., na.rm = TRUE)
      if (is.na(val)) . else ifelse(is.na(.), val, .)
    } else {
      val <- calc_mode_clean(.)
      if (is.na(val)) . else ifelse(is.na(.), val, .)
    }
  }))
cat("Number of observations remaining :", nrow(df), "\n") # 298989
cat("Number of variables remaining :", ncol(df), "\n") # 380
cat("Number of NA remaining :", sum(is.na(df)), "\n") # 0
##########
##########
# We delete variables with no globale invariance, if there are,
# as they would present 0 standard error in our models :
vars_invariantes <- sapply(df, function(x) length(unique(x)) <= 1)
print(vars_invariantes)
# We don't have any ! No need to delete. 
# Warning : of course this is an imperfect criteria,
# as lot of factors would have no global invariance only due to
# negative values.
# However, these negatives can still be useful data. 
##########
##########
# Treatment definition :
# We set the Geneva canton as the treatment,
# and the rest of Switzerland as control.
# We also need a variable post for our diff in diff,
# that will be defined from the year 2007 (included).

# In the treatment definition, we must remind that 
# the ITT will be our parameter of interest.
# We focus on the treatment status in 2006, a year before
# the introduction of LIASI :
ids_geneve_2006 <- df %>%
  filter(
    as.numeric(as.character(year)) == 2006 & 
      as.numeric(canton) == 13
  ) %>%
  pull(idpers)
df <- df %>%
  mutate(
    treated = ifelse(idpers %in% ids_geneve_2006, 1, 0),
    post = ifelse(as.numeric(as.character(year)) >= 2007, 1, 0),
    did_interaction = treated * post
  )
df <- df %>%
  mutate(
    treated         = as.factor(treated),
    post            = as.factor(post),
    did_interaction = as.factor(did_interaction)
  )
##########
##########
# Outcomes definition :
# persistence_score_iwely, persistence_score_iuney and iempyg_outcome
df <- df %>%
  mutate(year_num = as.numeric(as.character(year)),
         period_pers = case_when(
           year_num %in% 2000:2006 ~ "PRE",
           year_num %in% 2009:2015 ~ "POST",
           TRUE ~ NA_character_
         ),
         period_inc = case_when(
           year_num %in% 2000:2006 ~ "PRE",
           year_num %in% 2010:2016 ~ "POST",
           TRUE ~ NA_character_
         ))

df_persistence <- df %>%
  filter(!is.na(period_pers)) %>% 
  group_by(idpers, period_pers) %>%
  summarise(
    persistence_score_iwely = sum(iwely > 0) / n(),
    persistence_score_iuney = sum(iuney > 0) / n(),
    .groups = "drop"
  )

df <- df %>%
  left_join(df_persistence, by = c("idpers", "period_pers"))

ihs <- function(x) {
  log(x + sqrt(x^2 + 1))
}

df_income <- df %>%
  filter(!is.na(period_inc)) %>%
  group_by(idpers, period_inc) %>%
  summarise(
    total_raw_emp = sum(iempyg),
    .groups = "drop"
  ) %>%
  mutate(iempyg_outcome = ihs(total_raw_emp))

df <- df %>%
  left_join(df_income %>% select(idpers, period_inc, iempyg_outcome), 
            by = c("idpers", "period_inc"))
df <- df %>%
  select(-year_num) 

rm(df_persistence, df_income)
gc()  

saveRDS(df, ".../df.rds") 
# We save the file, in order to avoid redoing the heavy previous steps next time.
# We will only need to upload :
df = readRDS(".../df.rds")


# 3) EXPLORATORY DATA ANALYSIS :
# Treatment analysis and sample size :

# In the first section, 
# we ensured having sufficiently large treatment and control samples. 
# However, given the reduction in our data during pre-processing as well as
# the structure of our outcomes, 
# it is now time to introduce some additional tests.

# We are also taking this opportunity to test
# potential spillover effects resulting from the LIASI 
# (from the treatment group to the control group). 

# First, let's see if we have sufficient  person-year observations
# in treatment and control groups, 
# both before and after 2007 :
table(df$post, df$treated, dnn = c("Post-2007", "Treatment"))
# Yes, we have enough observations (for the treated, 1696 before and 4214 after 2007). 
# This implies a strong prediction power of the covariates. 
# However, the modelling algorithms will only consider the year-person
# observations corresponding to a non-NA outcome.

# In addition to this, the statistical validity and inference strength of our models
# will be given by the outcomes size, which are cross-sectional. 
# For this reason, I will count the number of unique individuals,
# in both control and treatment groups and for the different periods :
# PRE-PERIOD :
summary_pre <- df %>%
  filter(as.numeric(as.character(year)) >= 2000 & as.numeric(as.character(year)) <= 2006) %>%
  group_by(treated) %>%
  summarise(
    n_unique = n_distinct(idpers)
  ) %>%
  mutate(Group = ifelse(treated == 1, "Treatment", "Control")) %>%
  select(Group, n_unique) %>%
  pivot_wider(names_from = Group, values_from = n_unique)
cat("Number of unique individuals, pre-period (2000-2006)\n")
print(as.data.frame(summary_pre))
# We have sufficient unique individuals.

# POST-PERIOD : 
summary_post_pers <- df %>%
  filter(as.numeric(as.character(year)) >= 2009 & as.numeric(as.character(year)) <= 2015) %>%
  group_by(treated) %>%
  summarise(n_unique = n_distinct(idpers)) %>%
  mutate(Group = ifelse(treated == 1, "Treatment", "Control")) %>%
  select(Group, n_unique) %>%
  pivot_wider(names_from = Group, values_from = n_unique)
cat("Number of unique individuals, POST-period (2009-2015) - Persistence\n")
print(as.data.frame(summary_post_pers))

summary_post_inc <- df %>%
  filter(as.numeric(as.character(year)) >= 2010 & as.numeric(as.character(year)) <= 2016) %>%
  group_by(treated) %>%
  summarise(n_unique = n_distinct(idpers)) %>%
  mutate(Group = ifelse(treated == 1, "Treatment", "Control")) %>%
  select(Group, n_unique) %>%
  pivot_wider(names_from = Group, values_from = n_unique)
cat("Number of unique individuals, POST-period (2010-2016) - Income\n")
print(as.data.frame(summary_post_inc))
# Here again, we have sufficient unique individuals. 

# 2000-2006 : 440 unique individuals in the treatment.
# 2010-2016 : 351 unique individuals (the most restrictive post-reform window.
# This indicates low attrition (the treatment doesn't modify the sample structure),
# as well as sufficient sample size.
# Fine, but concerning the last, how many people at the intersection ?
# How many people were observed in the first and in the second period ? 
# This is important, in panel-based models, if we want to have a balanced panel.
# However, in a DML approach, given the last of covariates used,
# it is more suitable to maximize the statistical power of the panel.
# We will not limit our analysis to the individuals observed before AND after.

# Now, let's investigate the spillover issue.
table(df$canton, df$treated)
# We have 5691 pure treated person-year observations.
# Pure treated observations refer to people who :
# 1) were in Geneva in 2006 (treated)
# 2) and were never recorded in another canton
# Although this number often include a same recorded across different years,
# 5691 is an appropriate number of observations for the treatment group.

# We also have 219 observations concenring people who,
# at one or many year of the panel, were recorded in other cantons :
# 13 observations concern Jura,
# 195 for Vaud,
# 11 for Valais.
# For these, we shall test for possible spillover effects 
# (people who moved out of Geneva because of the LIASI,
# and that could bias our results). 

# How would spillover effect manifest ?
# Well, if there is, we would have lot of people moving from Geneva to other cantons,
# around the 2007 reform. Is it the case ? 
# Let's look at the relocation dynamic across years :
spillover <- df %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(year_num >= 2000 & year_num <= 2010) %>%
  filter(canton %in% c(13, 28, 29)) %>%
  group_by(year_num, canton, treated) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = treated, values_from = n) %>%
  rename(Year = year_num, Canton = canton, Control = `0`, Treatment = `1`) %>%
  arrange(Canton, Year)
print(as.data.frame(spillover))
# There doesn't seem to be massive departure from canton 13 (Geneva),
# between 2006 and 2008. 
# We can exclude the spillover issue. 
##########
##########
# Outcomes distribution for treatment and control groups :
# 1. Social aid Persistence
options(repr.plot.width = 12, repr.plot.height = 8) 
ggplot(df, aes(x = persistence_score_iwely, fill = factor(treated))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution: Social Aid Persistence Score")

ggplot(df %>% filter(persistence_score_iwely > 0), 
       aes(x = persistence_score_iwely, fill = factor(treated))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution: Social Aid Persistence (Conditional on being a recipient)",
       subtitle = "Focusing on individuals with non-zero persistence")

table_stats_iwely <- df %>%
  group_by(treated) %>%
  summarise(
    `N` = n(),
    `Proportion of 0 (%)` = mean(persistence_score_iwely == 0, na.rm = TRUE) * 100,
    Moyenne = mean(persistence_score_iwely, na.rm = TRUE),
    Médiane = median(persistence_score_iwely, na.rm = TRUE),
    `Std. Error` = sd(persistence_score_iwely, na.rm = TRUE) / sqrt(n())
  )
print(table_stats_iwely)

# The social aid persistence score presents a zero-inflation issue, 
# as more than 95% of individuals never received social assistance. 
# Using non-linear learners within the DML framework 
# will help address this distributional challenge.

# Standard errors are highly precise due to the large sample size, 
# particularly in the control group. 
# Despite the mass of zeros, 
# the outcome varies across the full range between 0 and 1 in both groups, 
# providing sufficient variability for the analysis. 

# This also implies overlap in outcome distributions; 
# while less critical, it suggests that treated and control groups
# exhibit comparable outcome realizations, 
# although counterfactual validity depends on covariate overlap.

# Furthermore, individuals in the treatment group exhibit 
# a higher baseline dependency on social aid 
# (4.7%, for instance, compared to 1.3% in the control group). 
# While this could potentially indicate non-parallel trends, 
# it may also reflect a simple difference in levels. 
# This will be formally examined in the subsequent analysis.

# 2. Unemployment Persistence
options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(df, aes(x = persistence_score_iuney, fill = factor(treated))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution: Unemployment Persistence Score")

ggplot(df %>% filter(persistence_score_iuney > 0), 
       aes(x = persistence_score_iuney, fill = factor(treated))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution: Unemployment Persistence (Conditional on being a recipient)",
       subtitle = "Focusing on individuals with non-zero persistence")

table_stats_iuney <- df %>%
  group_by(treated) %>%
  summarise(
    `N` = n(),
    `0 proportion (%)` = mean(persistence_score_iuney == 0, na.rm = TRUE) * 100,
    Moyenne = mean(persistence_score_iuney, na.rm = TRUE),
    Médiane = median(persistence_score_iuney, na.rm = TRUE),
    `Std. Error` = sd(persistence_score_iuney, na.rm = TRUE) / sqrt(n())
  )
print(table_stats_iuney)

# The observations for unemployment persistence are similar 
# to those for social aid: 
# we find that 96.3% of the control group and 92.1% of the treatment group
# never experienced unemployment during the period. 
 # While the zero-inflation is slightly less pronounced 
# than for social assistance, 
# the overall distributional dynamics remain consistent.

# However, the second graphic indicates that shared support 
# appears more restricted in the upper tail of the distribution (scores > 0.75). 
# Geneva exhibits a distinct density of long-term unemployment outcomes
# that are infrequent, or nearly absent, in the control group.

# Although the primary dynamics remain comparable across groups, 
# this suggests that estimating the treatment effect 
# for the most persistent cases will rely heavily on the model's capacity 
# to extrapolate based on similar covariate profiles (X). 

# In this context, the flexibility of DML models is particularly valuable, 
# as they are better equipped to handle such distributional imbalances 
# than traditional linear frameworks.

# 3. Income IHS
options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(df, aes(x = iempyg_outcome, fill = factor(treated))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution: IHS Employment Income")

table_stats_income <- df %>%
  group_by(treated) %>%
  summarise(
    N = n(),
    `Proportion of 0 (%)` = mean(iempyg_outcome == 0, na.rm = TRUE) * 100,
    `Mean (IHS)` = mean(iempyg_outcome, na.rm = TRUE),
    `Mean (CHF) - approx.` = exp(mean(iempyg_outcome, na.rm = TRUE)), 
    `Q25 (CHF) - approx.` = exp(quantile(iempyg_outcome, 0.25, na.rm = TRUE)),
    `Median (CHF) - approx.` = exp(quantile(iempyg_outcome, 0.50, na.rm = TRUE)), # exp() detransformation is an approximation only working for high values
    `Q75 (CHF) - approx.` = exp(quantile(iempyg_outcome, 0.75, na.rm = TRUE)),
    `Q90 (CHF) - approx.` = exp(quantile(iempyg_outcome, 0.90, na.rm = TRUE)),
    `Std. Error (IHS)` = sd(iempyg_outcome, na.rm = TRUE) / sqrt(n())
  )
print(as.data.frame(table_stats_income))

# The distributions overlap is satisfied for employment income. 
# However, we observe an interesting trend: 
# while the treatment group exhibits higher dependency on social aid 
# and unemployment benefits, 
# it shows a lower proportion of zero working income. 

# This could suggest a higher prevalence of the "working poor" in Geneva
# compared to the control group. 
# Consequently, it will be good to control for individuals' professional status.

# The Inverse Hyperbolic Sine (IHS) transformation is advantageous.
# It effectively handles the substantial zero-income mass 
# (accounting for nearly 50% of the sample) 
# while dampening the influence of extreme high salaries, 
# as the right-skewness is highly visible in the detransformed values. 

# Given that IHS coefficients can be difficult to interpret directly, 
# we have provided detransformed descriptive statistics to facilitate 
# the economic discussion.
##########
##########
# Financial forward looking score 
# Lot of proxies for the forward looking character have been eliminated
# in the NA imputation process.

# We will thus build a very basic score, with 3 indicators :
# 1) hi22, which measures if the persons does invest in 3rd pillar (x3)
# 2) pi166, which gives the frequency of individual saving habits (x2)
# 3) hh29, which establish whether the person is owner/tenant (x1)
# Forward-looking score
df$score_fw <- with(df, {
  s_hh29  <- ifelse(hh29 == 7, 1, 
                    ifelse(hh29 %in% c(6, 8), 0, NA))
  s_hi22  <- ifelse(hi22 == 6, 1, 
                    ifelse(hi22 == 7, 0, NA))
  s_pi166 <- ifelse(pi166 == 6, 2, 
                    ifelse(pi166 == 7, 1, 
                           ifelse(pi166 == 8, 0, NA)))
  
  mat_weighted <- cbind(3 * s_hh29, 2 * s_pi166, 1 * s_hi22)
  
  raw_score <- rowSums(mat_weighted, na.rm = TRUE)
  
  n_valide <- rowSums(!is.na(cbind(s_hh29, s_pi166, s_hi22)))
  
  score <- ifelse(n_valide >= 2, raw_score / n_valide, NA)
  
  return(score)
})
df$score_fw <- as.numeric(scale(df$score_fw))

# Drawbacks : the score depends on the ponderation scheme and the NA number. 

# But we opt for a theory-driven weighting scheme rather than a data-driven PCA,
# in order to preserve economic interpretability of the index, 
# which is essential for heterogeneous treatment effect analysis.

# The goal is to have in interpretable measure.

# The index is used to define heterogeneous treatment groups
# We will use it both in the CSC and in the parallel trends analysis.
##########
##########
# Common Support Condition
# We focus both on pre-treatment main counfounders and on the score_fw.
# We want to see whether, for both treatment and control groups, 
# we have overlapping supports. 

# We focus on :
# our score_fw, in treatment vs control.
# age, which affects both the probability of being in Geneva and the outcome
# (as the demographic structure differs canton by canton)
# idispy, household income (same discussion)
# iwely, iuney and iempyg (the raw variables through which we constructed are outcomes)
# wstat, the working statuts
# isced, education level
# sex
# pd_167, as the permit system affects bot the probability of the treatment 
# and the profesional insertion.

# Of these variables, sex is the only non counfounder, 
# as it probably doesn't affect much the probability of the treatment
# (being in Geneva).
# However, it can explain lot of the outcomes variance and is thus an important covariate. 

# Instead of testing the support variable by variable,
# we estimate a global Propensity Score.
# That is, the conditional probability of being in Geneva (tratment group)
# given our set of 11 covariates.

# We then plot the density of these predicted probabilities for both groups,
# in order to verify the CSC assumption.
# If strong overlap is present, it means that for every individual in Geneva,
# we can find a valid conterfactual in the control (across all these covariates).
df_csc <- df %>%
  mutate(
    year_num    = as.numeric(as.character(year)),
    treated_num = as.numeric(as.character(treated)),
    sex   = as.factor(sex),
    isced = as.factor(isced),
    wstat = as.factor(wstat),
    pd167 = as.factor(pd167)
  )

df_csc <- df_csc %>% 
  filter(year_num < 2007) %>%
  select(idpers, treated_num, age, idispy, score_fw, 
         iempyg, iwely, iuney, sex, pd167, isced, wstat) %>%
  drop_na() %>%
  droplevels() 

ps_model <- glm(treated_num ~ age + idispy + score_fw + iempyg + 
                  iwely + iuney + sex + pd167 + isced + wstat, 
                data = df_csc, 
                family = binomial)

df_csc$pscore <- predict(ps_model, type = "response")

ggplot(df_csc, aes(x = pscore, fill = factor(treated_num))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Global Common Support Check (Propensity Score)",
       subtitle = paste0("Pre-2007 Sample (N = ", nrow(df_csc), ")"),
       x = "Probability of being in Geneva (Propensity Score)",
       fill = "Group (0=Control, 1=GVA)") +
  xlim(0, 1)

# We notice :
# 1) CSC is satisfied : any value of X observed in the treatment 
# has a counterfactual in the control group.
# 2) The control group, given our covariates, is clustered towards
# 0 probability of being treated, while the treatment distribution is
# is shifted to the right. 
# This confirms selection bias : 
# Geneva inhabitants have specific characteristic, 
# distinguishing them from the rest of Switzerland. 
##########
##########
# Parallel trends assumption, iwely :
# We don't use the variable persistence_score_iwely, as it is cross-sectional.
# We will test the parallel trends on the raw variable iwely.

# We start by considering the entire set of iwely values.
# Pre and post treatment period :
df_trends_iwely <- df
trends_full <- df_trends_iwely %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iwely, na.rm = TRUE), .groups = 'drop')
ggplot(trends_full, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Full Sample", x = "Year", y = "Social aid ammount", color = "Group")

# The control group remains stable and relatively pure throughout the panel; 
# however, it undergoes a level shift upwards around 2013, 
# which does not appear alarming. 

# Regarding the pre-treatment trends, 
# they are clearly not observed. 
# However, if we disregard the instability of the Genevan amounts 
# (explained by the smaller number of observations), 
# we can observe a certain stability of iwely in Geneva before 2007. 
# After 2007, on the other hand, 
# there is a clear drop in the aggregate amounts 
# (at least until 2013, when it rises again).

# Now, we exclude the zeros :
trends_active <- df_trends_iwely %>%
  filter(iwely > 0) %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iwely, na.rm = TRUE), .groups = 'drop')
ggplot(trends_active, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Active Users Only (Excl. 0)", x = "Year", y = "Social aid ammount", color = "Group")

# Levels harmonize between Geneva and the rest of Switzerland 
# (when excluding zeros), 
# proving that Geneva differentiates itself
# by the number of social assistance recipients rather 
# than by the amounts received by those individuals.

# It is clear that pure pre-treatment parallel trends are not observed, 
# suggesting that a simple OLS Difference-in-Differences (DiD) approach 
# would be inadequate. 
# The treatment sample is characterized by excessive volatility; 
# furthermore, while the second chart (excluding zeros) 
# demonstrates that beneficiaries' needs are comparable across Switzerland, 
# we face a potential risk of composition bias stemming 
# from the distinct structural and dynamic differences between Geneva 
# and the rest of the country. 

# A Double Machine Learning (DML) framework, 
# by introducing a high-dimensional set of covariates, 
# could well address these issues.

# As for the stability of the control group, 
# it stands firmly in our favor for the robustness of the identification.


# Parallel trends assumption, iuney.
# On the entire sample
df_trends_iuney <- df
trends_full_iuney <- df_trends_iuney %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iuney, na.rm = TRUE), .groups = 'drop')
ggplot(trends_full_iuney, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Full Sample (Unemployment Benefits)", 
       x = "Year", y = "Unemployment Benefits", color = "Group")

# The control group appears pure, here again.
# The trends, however, exhibit a dynamics similar to those 
# previously observed under iwely.

# While the control group remains a robust baseline, 
# the treatment group (Geneva) shows 
# significant volatility and a lack of synchronization prior to 2007. 

# 0 excluded (only for people involved) :
trends_active_iuney <- df_trends_iuney %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(iuney > 0) %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iuney, na.rm = TRUE), .groups = 'drop')
ggplot(trends_active_iuney, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Active Users Only (iuney > 0)", 
       x = "Year", y = "Unemployment benefits", color = "Group")

# The same discussion as for iwely (>0) applies, 
# except for the fact that from 2010 onwards, 
# the difference between Geneva and the rest of Switzerland 
# regarding unemployment income also appears in the intensive margin.

# We also observe an increase in the outcome level in Geneva after 2007, 
# which deviates from our initial hypothesis. 
# In fact, unemployment dynamics follow market forces that differ 
# from those of social assistance; 
# it is possible that the rise in unemployment amounts is driven by 
# higher pre-unemployment wages. 
# This further justifies the need for robust controls and, 
# consequently, the use of a Double Machine Learning (DML) framework


# Parallel trends : iempyg.
# On the entire sample :
df_trends_iempyg <- df
trends_full_iempyg <- df_trends_iempyg %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iempyg, na.rm = TRUE), .groups = 'drop')
ggplot(trends_full_iempyg, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Full Sample (Labor Income)", 
       x = "Year", y = "Labor income", color = "Group")

# We have divergent trends, and the control group is not even pure.
# The rise of Geneva incomes after 2012 could witness of a positive effect
# of our treatment ?
# It will be very difficult to conclude anything, 
# given the fore-mentionned dynamic. 

# Only active users (>0) :
trends_active_iempyg <- df_trends_iempyg %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(iempyg > 0) %>%
  group_by(year_num, treated) %>%
  summarise(mean_score = mean(iempyg, na.rm = TRUE), .groups = 'drop')
ggplot(trends_active_iempyg, aes(x = year_num, y = mean_score, color = factor(treated), group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Trends: Active Earners Only (iempyg > 0)", 
       x = "Year", y = "Labour income", color = "Group")
# Same discussion than for the entire sample. 

# Double Machine Learning (DML) can help address violations 
# of the parallel trends assumption when these arise from 
# compositional differences between treated and control units. 
# In such cases, methods relying on selection on observables, 
# such as DML or matching, can improve identification by flexibly 
# controlling for a rich set of covariates.

# However, if Geneva differs structurally from the rest of Switzerland, 
# (due to institutional features, labor market characteristics, 
# or differential exposure to macroeconomic shocks) 
# and if these differences are not captured by the available covariates, 
# then DML may not fully resolve the issue. 

# In such contexts, alternative designs such as synthetic control methods
# may be more appropriate. 
# A full event-study analysis would be particularly useful to further 
# assess the nature of the bias and to help distinguish whether a 
# flexible regression approach (such as DML) or a synthetic control framework 
# is more suitable, by better identifying the source of the divergence in trends. 
# However, we omit this step in the present analysis given the micro-level 
# nature of our panel data, for which a Double Machine Learning approach 
# appears more natural and better suited to handling
# high-dimensional covariates.
##########
##########
# Parallel trends assumptions, conditioning to score_fw
# (for future heterogeneity analysis)
# We will now do a shorter parallel trend analysis,
# but conditioning on the score_fw quantiles (first vs fourth quartile). 
# IWELY :
df_trends_iwely_fw <- df %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    fw_group = case_when(
      score_fw <= quantile(score_fw, 0.25, na.rm = TRUE) ~ "Low FW (Q1)",
      score_fw >= quantile(score_fw, 0.75, na.rm = TRUE) ~ "High FW (Q4)",
      TRUE ~ "Middle"
    )
  )
trends_full_iwely_fw <- df_trends_iwely_fw %>%
  group_by(year_num, treated, fw_group) %>%
  summarise(mean_score = mean(iwely, na.rm = TRUE), .groups = "drop")
ggplot(trends_full_iwely_fw,
       aes(x = year_num,
           y = mean_score,
           color = factor(treated),
           group = treated)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~fw_group) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Social aid amount trends by forward-looking heterogeneity",
    subtitle = "Low (Q1) vs High (Q4) forward-looking score",
    x = "Year",
    y = "Social aid amount",
    color = "Group"
  )
# The trends for social aid appear relatively stable 
# for the High and Middle FW groups, 
# but this is primarily a 'floor effect' 
# due to the very low number of observations; 
# these individuals naturally rely very little on social assistance. 
# Interestingly, a shift is observed starting in 2015, 
# where even these groups begin to show an emerging dependency. 

# For the Low FW (Q1) group, the most relevant for this analysis,
# we observe neither parallel trends nor a pure control group. 

# IUNEY :
df_trends_iuney_fw <- df %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    fw_group = case_when(
      score_fw <= quantile(score_fw, 0.25, na.rm = TRUE) ~ "Low FW (Q1)",
      score_fw >= quantile(score_fw, 0.75, na.rm = TRUE) ~ "High FW (Q4)",
      TRUE ~ "Middle"
    )
  )
trends_full_iuney_fw <- df_trends_iuney_fw %>%
  group_by(year_num, treated, fw_group) %>%
  summarise(mean_score = mean(iuney, na.rm = TRUE), .groups = "drop")
ggplot(trends_full_iuney_fw,
       aes(x = year_num,
           y = mean_score,
           color = factor(treated),
           group = treated)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~fw_group) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Unemployment benefits trends by forward-looking heterogeneity",
    subtitle = "Low (Q1) vs High (Q4) forward-looking score",
    x = "Year",
    y = "Unemployment benefits",
    color = "Group"
  )
# Segmentation among different groups made the treatment outcome even
# more volatile than befor (above all, for high fw individuals).

# We have a clear violation of parallel trends, associated to a 
# high sensitivity to outliers (huge pikes can be seen).

# We could condition on more covariates to assess whether the diverging trends
# are structural, or due to unobserved covariates 
# (in the second scenario, using DML would solve the issue);
# however, we would reduce even more the sample sizes, making graphs 
# even more volatile. 

# Endogeneous trends are problematic, as the heterogeneous effects
# we could find in subsequent models could come from divergent
# pre-treatment dynamics. 

# IEMPYG :
df_trends_iempyg_fw <- df %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    fw_group = case_when(
      score_fw <= quantile(score_fw, 0.25, na.rm = TRUE) ~ "Low FW (Q1)",
      score_fw >= quantile(score_fw, 0.75, na.rm = TRUE) ~ "High FW (Q4)",
      TRUE ~ "Middle"
    )
  )
trends_full_iempyg_fw <- df_trends_iempyg_fw %>%
  group_by(year_num, treated, fw_group) %>%
  summarise(mean_score = mean(iempyg, na.rm = TRUE), .groups = "drop")
ggplot(trends_full_iempyg_fw,
       aes(x = year_num,
           y = mean_score,
           color = factor(treated),
           group = treated)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~fw_group) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Labor income trends by forward-looking heterogeneity",
    subtitle = "Low (Q1) vs High (Q4) forward-looking score",
    x = "Year",
    y = "Labor income",
    color = "Group"
  )
# Diverging trends. 

# Parallel trends do not hold, not even conditionning on the fw score.
# As we said, conditionning on more covariates could help us assess 
# the origin of non-parallel trends. Does it come from the treatment/control
# composition ? Is it structural ? 
# However, we don't have sufficient statistical power to do this.

# To (partially) solve the issue of potential structural differences
# (that our micro data could not solve),
# we could introduce in our DML models some canton-specific linear trends
# (as canton-year interactions) ? 
# After then, we could also introduce some DML robustness checks/placebo tests
# to assess whether we have been able to solve potential structural issues.
# To have more details, please refer to the report.

# Note : to construct the quantiles, considering only the pre-treatment 
# values of the forward looking score would be safer, as it can be
# endogeneous to the treatment. 

# DiD BASELINE MODEL : 
# This is a naive model.
# We propose here a very simple OLS specification, 
# which accounts for neither the non-normality of error terms 
# nor the potential violation of the parallel trends assumption
df_baseline <- df
# We will work on the iwely related outcome. 

# Our outcome corresponds to an average for POST=0, and an average for POST=1;
# However, in our longfile, the first appears each year from 2000 to 2006, 
# and the second appears each year from 2009 to 2015.
# This can artificially inflate the statistical validity of inference,
# that's why we could change df_baseline in such a way that the persistence score
# only appears twice : once before 2007, and once after (for each individual). 
# The problem with this strategy, is that we lose the long-format, panel structure.
# For this reason, we'll not modify df_baseline.
# Instead, we will use idpers clustered standard errors in the inference.

# We specify factor covariates :
df_baseline <- df_baseline %>%
  mutate(
    sex   = as.factor(sex),
    isced = as.factor(isced),
    wstat = as.factor(wstat),
    pd167 = as.factor(pd167)
  )

# We define pre-treatment controls :
df_baseline <- df_baseline %>%
  group_by(idpers) %>%
  mutate(
    idispy_pre = first(na.omit(idispy[as.numeric(as.character(year)) <= 2006]))
  ) %>%
  ungroup() # no need to do it for the other controls, as they cannot be affected by the treatment

# We use an unbalanced DiD :
# we allow for people present between 2000 and 2006 to be included in the model,
# even if they were not present between 2009 and 2015 (and vice versa). 
# It makes sense in DML, as we already mentioned ,
# and we mantain this structure here in order to have comparable results. 

did_model_iwely <- feols(
  persistence_score_iwely ~ did_interaction + 
    age + i(year, idispy_pre, ref = "2006") + 
    sex + pd167 + isced + wstat | idpers + year, 
  data = df_baseline, 
  cluster = ~idpers
) # we excluded treated and post, to avoid multicollinearity
summary(did_model_iwely)

# No significant effect of did_interaction, 
# but this result has no causal meaning. 

# The model is highly saturated due to the inclusion 
# of individual fixed effects (idpers),
# year fixed effects, 
# and time-varying pre-treatment controls (idispy_pre:year).

# Let's apply the model to the other outcomes :
# IUNEY :
did_model_iuney <- feols(
  persistence_score_iuney ~ did_interaction + 
    age + i(year, idispy_pre, ref = "2006") + 
    sex + pd167 + isced + wstat | idpers + year, 
  data = df_baseline, 
  cluster = ~idpers
) 
summary(did_model_iuney)
# IEMPYG :
did_model_iempyg <- feols(
  iempyg_outcome ~ did_interaction + 
    age + i(year, idispy_pre, ref = "2006") + 
    sex + pd167 + isced + wstat | idpers + year, 
  data = df_baseline, 
  cluster = ~idpers
) 
summary(did_model_iempyg)

# NOTE : These baseline DiD models were estimated to understand implementation challenges
# and identify potential issues prior to the DML analysis.
# As the parallel trends assumption does not hold in this setting,
# we do not elaborate on these results in the proposal.
