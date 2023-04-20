set.seed(100)

library(tidyverse)
library(Hmisc)

all_data <- readRDS("data/preprocessed_scales.rds") %>% 
  as_tibble()

# Set classes
all_data$condition <- as.factor(all_data$condition)
class(all_data$age)
summary(all_data$age)
table(all_data$age)

all_data$gender <- as.factor(all_data$gender)

table(all_data$education)
all_data$education <- dplyr::recode(all_data$education, 
                                    "A-level (or equivalent)" = "A-level",
                                    "GCSE (or equivalent)" = "GCSE_or_below",
                                    "No formal education" = "GCSE_or_below",
                                    "Postgraduate degree (e.g., MA, MSc, PhD)" = "Postgrad",
                                    "Undergraduate degree (e.g., BA)" = "Undergrad")
all_data$education <- as.factor(all_data$education)
table(all_data$education)
all_data <- within(all_data, education <- relevel(education, ref = "A-level"))

table(all_data$ethnicity)
table(all_data$ethnicity_simple)
all_data <- within(all_data, ethnicity_simple <- relevel(factor(ethnicity_simple), ref = "White"))

class(all_data$income)
summary(all_data$income)
all_data$income_cat <- NA
all_data$income_cat <- ifelse(all_data$income<=3, 0, 1)
all_data$income_cat <- as.factor(all_data$income_cat)

table(all_data$partisanship)
all_data$partisanship <- dplyr::recode(all_data$partisanship, 
                                       "Brexit Party" = "Other",
                                       "Conservative Party" = "Cons",
                                       "Green Party" = "Green",
                                       "Liberal Democrats" = "Lib_Dem",
                                       "No party" = "No_party",
                                       "Other party" = "Other",
                                       "Plaid Cymru" = "Other",
                                       "SNP" = "Other",
                                       "UKIP" = "Other")
all_data$partisanship <- as.factor(all_data$partisanship)
table(all_data$partisanship)
all_data <- within(all_data, partisanship <- relevel(partisanship, ref = "Cons"))
all_data$partisan_binary <- ifelse(all_data$partisanship=="Cons", "Cons", "Other_party")
all_data$partisan_binary <- as.factor(all_data$partisan_binary)
all_data <- within(all_data, partisan_binary <- relevel(partisan_binary, ref = "Other_party"))

table(all_data$eu_ref)
all_data$eu_ref <- dplyr::recode(all_data$eu_ref, 
                                 "I voted Leave" = "Leave",
                                 "I voted Remain" = "Remain",
                                 "I was eligible to vote, but did not" = "No_vote",
                                 "I was not eligible to vote, but would have voted Leave" = "Leave",
                                 "I was not eligible to vote, but would have voted Remain" = "Remain",
                                 "I was not eligible to vote, but would not have voted" = "No_vote")
all_data$eu_ref <- as.factor(all_data$eu_ref)
all_data <- within(all_data, eu_ref <- relevel(eu_ref, ref = "Leave"))

# Descriptive stats
age <- round(tapply(all_data$age,all_data$condition,mean),0)
gender <- round(prop.table(table(all_data$gender,all_data$condition),2)*100,digits=1)
education <- round(prop.table(table(all_data$education,all_data$condition),2)*100,digits=1)
education <- education[c(2,1,4,3),]
ethnicity <- round(prop.table(table(all_data$ethnicity_simple,all_data$condition),2)*100,digits=1)
income <- round(prop.table(table(all_data$income_cat,all_data$condition),2)*100,digits=1)[2,]
party <- round(prop.table(table(all_data$partisanship,all_data$condition),2)*100,digits=1)
party <- party[c(1,3,4,2,6,5),]
referendum <- round(prop.table(table(all_data$eu_ref,all_data$condition),2)*100,digits=1)
referendum <- referendum[c(1,3,2),]
contact <- round(tapply(all_data$contact,all_data$condition,mean),2)
know <- round(tapply(all_data$know,all_data$condition,mean),2)
trust <- round(tapply(all_data$trust,all_data$condition,mean),2)
numeracy <- round(tapply(all_data$numeracy,all_data$condition,mean),2)

summary_df <- data.frame(rbind(age,gender,education,ethnicity,income,party,referendum,contact,know,trust,numeracy))
summary_df$sd <- NA
for(i in 1:length(summary_df$control)){
  summary_df$sd[i] <- round(sd(summary_df[i,1:3]),digits=2)
}
#summary_df[2:21,1:3] <- lapply(summary_df[2:21,1:3], function(x) paste(x,"%",sep=""))
cond <- table(all_data$condition)
summary_df <- rbind(summary_df,cond)
summary_df$sd[24] <- round(sd(summary_df[24,1:3]),digits=2)

row.names(summary_df) <- c("Age (mean):",
                           "Female (percent)",
                           "Male (percent)",
                           "Educ: GCSE or less (percent)",
                           "Educ: A-level (percent)",
                           "Educ: Undergraduate (percent)",
                           "Educ: Postgraduate (percent)",
                           "Ethnicity: White (percent)",
                           "Ethnicity: Other (percent)",
                           "Income: above mean (30k) (percent)",
                           "Party: Conservatives (percent)",
                           "Party: Labour (percent)",
                           "Party: Lib Dem (percent)",
                           "Party: Green Party (percent)",
                           "Party: Other party (percent)",
                           "Party: No party (percent)",
                           "EU vote: Leave (percent)",
                           "EU vote: Remain (percent)",
                           "EU vote: Didn't vote (percent)",
                           "Contact (mean)",
                           "Knowledge (mean)",
                           "Trust (mean)",
                           "Numeracy (mean)",
                           "N")

# BES benchmarks
bes_data <- read_sav("data/BES2019_W20_v23.0.sav")
# https://www.britishelectionstudy.com/data-object/wave-20-of-the-2014-2023-british-election-study-internet-panel/

bes_data_subset <- bes_data %>% 
  dplyr::select(age, 
                gender, 
                p_education, 
                p_ethnicity,
                p_gross_household,
                partyId,
                p_eurefvote,
                p_eurefturnout,
                wt)

bes_data_subset <- bes_data_subset %>% 
  rename(age = age,
         gender = gender,
         education = p_education,
         ethnicity = p_ethnicity,
         income = p_gross_household,
         party = partyId,
         referendum = p_eurefvote,
         referendum_turnout = p_eurefturnout,
         wt = wt)

summary(bes_data_subset)

bes_data_subset <- bes_data_subset %>% 
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female"),
         education = case_when(education == 1 ~ "GCSE_or_below",
                               education == 2 ~ "GCSE_or_below",
                               education == 3 ~ "GCSE_or_below",
                               education == 4 ~ "GCSE_or_below",
                               education == 5 ~ "A-level",
                               education == 6 ~ "A-level",
                               education == 7 ~ "A-level",
                               education == 8 ~ "GCSE_or_below",
                               education == 9 ~ "GCSE_or_below",
                               education == 10 ~ "GCSE_or_below",
                               education == 11 ~ "A-level",
                               education == 12 ~ "A-level",
                               education == 13 ~ "A-level",
                               education == 14 ~ "A-level",
                               education == 15 ~ "Undergrad",
                               education == 16 ~ "Undergrad",
                               education == 17 ~ "Postgrad",
                               education == 18 ~ "Undergrad",
                               education == 19 ~ NA_character_,
                               education == 20 ~ NA_character_,
                               TRUE ~ NA_character_),
         ethnicity = case_when(ethnicity == 1 ~ "White",
                               ethnicity == 2 ~ "White",
                               ethnicity > 2 & ethnicity < 16 ~ "Other",
                               TRUE ~ NA_character_),
         income = case_when(income < 7 ~ 0,
                            income > 6 & income < 16 ~ 1,
                            TRUE ~ NA_real_),
         party = case_when(party == 1 ~ "Cons",
                           party == 2 ~ "Labour",
                           party == 3 ~ "Lib_Dem",
                           party == 4 ~ "Other",
                           party == 5 ~ "Other",
                           party == 7 ~ "Green",
                           party == 9 ~ "Other",
                           party == 10 ~ "No_party",
                           party == 12 ~ "Other",
                           TRUE ~ NA_character_),
         referendum = case_when(referendum == 0 ~ "Remain",
                                referendum == 1 ~ "Leave",
                                referendum_turnout == 1 ~ "No_vote",
                                TRUE ~ NA_character_))

gender_props <- bes_data_subset %>% 
  drop_na(gender) %>%
  count(gender, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2))

education_props <- bes_data_subset %>% 
  drop_na(education) %>% 
  count(education, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2))

ethnicity_props <- bes_data_subset %>% 
  drop_na(ethnicity) %>% 
  count(ethnicity, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2)) 

income_props <- bes_data_subset %>% 
  drop_na(income) %>% 
  count(income, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2)) 

party_props <- bes_data_subset %>% 
  drop_na(party) %>% 
  count(party, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2)) 

referendum_props <- bes_data_subset %>% 
  drop_na(referendum) %>% 
  count(referendum, wt = wt) %>%
  mutate(prop = n/sum(n),
         prop = round(prop * 100,2))

summary_df$bes <- NA
summary_df$bes[1] <- round(weighted.mean(bes_data_subset$age, bes_data_subset$wt, na.rm = TRUE),2)
summary_df$bes[2] <- gender_props$prop[1]
summary_df$bes[3] <- gender_props$prop[2]
summary_df$bes[4] <- education_props$prop[2]
summary_df$bes[5] <- education_props$prop[1]
summary_df$bes[6] <- education_props$prop[4]
summary_df$bes[7] <- education_props$prop[3]
summary_df$bes[8] <- ethnicity_props$prop[2]
summary_df$bes[9] <- ethnicity_props$prop[1]
summary_df$bes[10] <- income_props$prop[2]
summary_df$bes[11] <- party_props$prop[1]
summary_df$bes[12] <- party_props$prop[3]
summary_df$bes[13] <- party_props$prop[4]
summary_df$bes[14] <- party_props$prop[2]
summary_df$bes[15] <- party_props$prop[6]
summary_df$bes[16] <- party_props$prop[5]
summary_df$bes[17] <- referendum_props$prop[1]
summary_df$bes[18] <- referendum_props$prop[3]
summary_df$bes[19] <- referendum_props$prop[2]

write.csv(summary_df,"tables/descr_stats_table.csv")

# recruitment targets
round(prop.table(table(all_data$ethnicity_simple,all_data$gender))*100,digits=1)

sample_achieved <- round(prop.table(table(all_data$ethnicity_simple,all_data$gender))*100,digits=1)
sample_achieved[1,1] <- paste(sample_achieved[1,1],"(44.0)",sep=" ")
sample_achieved[2,1] <- paste(sample_achieved[2,1],"(7.0)",sep=" ")
sample_achieved[1,2] <- paste(sample_achieved[1,2],"(42.0)",sep=" ")
sample_achieved[2,2] <- paste(sample_achieved[2,2],"(7.0)",sep=" ")

sample_achieved