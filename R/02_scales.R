set.seed(100)

library(tidyverse)

all_data <- readRDS("data/preprocessed.rds") %>% 
  as_tibble()

# knowledge items
all_data$k_scale_virus <- ifelse(is.na(all_data$k_scale_virus), 1, 0) #true (doesn't matter if false)
# https://www.euro.who.int/en/health-topics/health-emergencies/coronavirus-covid-19/novel-coronavirus-2019-ncov
# https://www.gov.uk/coronavirus
all_data$k_scale_bacteria <- ifelse(is.na(all_data$k_scale_bacteria), 1, 0) #false
all_data$k_scale_hot <- ifelse(is.na(all_data$k_scale_hot), 0, 1) #true
all_data$k_scale_vaccine <- ifelse(is.na(all_data$k_scale_vaccine), 0, 1) #true
all_data$k_scale_recover <- ifelse(is.na(all_data$k_scale_recover), 0, 1) #true
all_data$k_scale_mosquito <- ifelse(is.na(all_data$k_scale_mosquito), 1, 0) #false
all_data$k_scale_antibiotics <- ifelse(is.na(all_data$k_scale_antibiotics), 1, 0) #false
all_data$k_scale_none <- NULL # remove 'all are false' column

know_items <- data.frame(#all_data$k_scale_virus,
  #all_data$k_scale_bacteria,
  all_data$k_scale_hot, # 1
  all_data$k_scale_vaccine, # 2
  all_data$k_scale_recover) # 3
# all_data$k_scale_mosquito,
#all_data$k_scale_antibiotics)

library(psych)
psych::alpha(know_items, check.keys = TRUE)

library(mirt)
mirt_know_scale <- mirt(data=know_items,
                        model=1,
                        itemtype = "2PL")
summary(mirt_know_scale)
plot(mirt_know_scale, type="trace")
plot(mirt_know_scale, type="info")
coef(mirt_know_scale, IRTpars=T)
all_data$know <- fscores(mirt_know_scale)[,1] # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(know_items, cor="tet") # unidimensional

# Q3 for local independence
Q3resid <- data.frame(residuals(mirt_know_scale, type="Q3"))

# Model fit
itemfit(mirt_know_scale, empirical.plot = 1)
itemfit(mirt_know_scale, empirical.plot = 2)
itemfit(mirt_know_scale, empirical.plot = 3)

# contact items
all_data$contact_scale_tested <- ifelse(is.na(all_data$contact_scale_tested), 0, 1)
all_data$contact_scale_confident <- ifelse(is.na(all_data$contact_scale_confident), 0, 1)
all_data$contact_scale_know_tested <- ifelse(is.na(all_data$contact_scale_know_tested), 0, 1)
all_data$contact_scale_know_passed <- ifelse(is.na(all_data$contact_scale_know_passed), 0, 1)
all_data$contact_scale_treating <- ifelse(is.na(all_data$contact_scale_treating), 0, 1)
all_data$contact_scale_none <- NULL

contact_items <- data.frame(# all_data$contact_scale_tested,
  # all_data$contact_scale_confident,
  all_data$contact_scale_know_tested,
  all_data$contact_scale_know_passed,
  all_data$contact_scale_treating)

library(psych)
psych::alpha(contact_items, check.keys = TRUE)

library(mirt)
mirt_contact_scale <- mirt(data=contact_items,
                           model=1,
                           itemtype = "2PL")
summary(mirt_contact_scale)
plot(mirt_contact_scale, type="trace")
plot(mirt_contact_scale, type="info")
coef(mirt_contact_scale, IRTpars=T)
all_data$contact <- fscores(mirt_contact_scale)[,1] # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(contact_items, cor="tet") # unidimensional

# Q3 for local independence
Q3resid <- data.frame(residuals(mirt_contact_scale, type="Q3"))

# Model fit
itemfit(mirt_contact_scale, empirical.plot = 1)
itemfit(mirt_contact_scale, empirical.plot = 2)
itemfit(mirt_contact_scale, empirical.plot = 3)

# levels of concern
library(dplyr)
all_data$worry_scale_society <- as.character(all_data$worry_scale_society)
all_data$worry_scale_society <- dplyr::recode(all_data$worry_scale_society, 
                                              "Very worried" = 3,
                                              "Somewhat worried" = 2,
                                              "A little worred" = 1,
                                              "Not worried at all" = 0)
all_data$worry_scale_society <- as.numeric(all_data$worry_scale_society)

all_data$worry_scale_contracting <- as.character(all_data$worry_scale_contracting)
all_data$worry_scale_contracting <- dplyr::recode(all_data$worry_scale_contracting, 
                                                  "Very worried" = 3,
                                                  "Somewhat worried" = 2,
                                                  "A little worred" = 1,
                                                  "Not worried at all" = 0)
all_data$worry_scale_contracting <- as.numeric(all_data$worry_scale_contracting)

all_data$worry_scale_fr_fam <- as.character(all_data$worry_scale_fr_fam)
all_data$worry_scale_fr_fam <- dplyr::recode(all_data$worry_scale_fr_fam, 
                                             "Very worried" = 3,
                                             "Somewhat worried" = 2,
                                             "A little worred" = 1,
                                             "Not worried at all" = 0)
all_data$worry_scale_fr_fam <- as.numeric(all_data$worry_scale_fr_fam)

all_data$worry_scale_finances <- as.character(all_data$worry_scale_finances)
all_data$worry_scale_finances <- dplyr::recode(all_data$worry_scale_finances, 
                                               "Very worried" = 3,
                                               "Somewhat worried" = 2,
                                               "A little worred" = 1,
                                               "Not worried at all" = 0)
all_data$worry_scale_finances <- as.numeric(all_data$worry_scale_finances)

all_data$worry_scale_unwell <- as.character(all_data$worry_scale_unwell)
all_data$worry_scale_unwell <- dplyr::recode(all_data$worry_scale_unwell, 
                                             "Very worried" = 3,
                                             "Somewhat worried" = 2,
                                             "A little worred" = 1,
                                             "Not worried at all" = 0)
all_data$worry_scale_unwell <- as.numeric(all_data$worry_scale_unwell)

all_data$worry_scale_job <- as.character(all_data$worry_scale_job)
all_data$worry_scale_job <- dplyr::recode(all_data$worry_scale_job, 
                                          "Very worried" = 3,
                                          "Somewhat worried" = 2,
                                          "A little worred" = 1,
                                          "Not worried at all" = 0)
all_data$worry_scale_job <- as.numeric(all_data$worry_scale_job)

library(mirt)
worry_scale_items <- data.frame(all_data$worry_scale_contracting,
                                all_data$worry_scale_unwell,
                                all_data$worry_scale_fr_fam)
# all_data$worry_scale_finances,
# all_data$worry_scale_job,
# all_data$worry_scale_society) # these three don't load well
psych::alpha(worry_scale_items)

IRT_worry_scale <- mirt(data=worry_scale_items,
                        model=1,
                        itemtype = "gpcm")
summary(IRT_worry_scale)
plot(IRT_worry_scale, type="trace")
plot(IRT_worry_scale, type="info")
coef(IRT_worry_scale, IRTpars=T)
worry <- fscores(IRT_worry_scale)[,1] # each person's expected score
all_data$worry <- worry
hist(all_data$worry)

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(worry_scale_items, cor="poly") # unidimensional

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_worry_scale, type="Q3")) # Max: -0.723

# Model fit
itemfit(IRT_worry_scale, empirical.plot = 1)
itemfit(IRT_worry_scale, empirical.plot = 2)
itemfit(IRT_worry_scale, empirical.plot = 3) # cat 2 not so good

boxplot(all_data$worry ~ all_data$condition)
mean(all_data$worry[all_data$condition=="control"])
mean(all_data$worry[all_data$condition=="treat1"]) # more worried
mean(all_data$worry[all_data$condition=="treat2"]) # more worried
tapply(all_data$worry, all_data$condition, mean)

# government performance
table(all_data$govt_perform_response)
all_data$govt_perform_response <- as.character(all_data$govt_perform_response)
all_data$govt_perform_response <- dplyr::recode(all_data$govt_perform_response, 
                                                "Strongly agree" = 5,
                                                "Agree" = 4,
                                                "Neither agree nor disagree" = 3,
                                                "Disagree" = 2,
                                                "Strongly disagree" = 1)
all_data$govt_perform_response <- as.numeric(all_data$govt_perform_response)

all_data$govt_perform_adapt <- as.character(all_data$govt_perform_adapt)
all_data$govt_perform_adapt <- dplyr::recode(all_data$govt_perform_adapt, 
                                             "Strongly agree" = 5,
                                             "Agree" = 4,
                                             "Neither agree nor disagree" = 3,
                                             "Disagree" = 2,
                                             "Strongly disagree" = 1)
all_data$govt_perform_adapt <- as.numeric(all_data$govt_perform_adapt)

all_data$govt_perform_protecting <- as.character(all_data$govt_perform_protecting)
all_data$govt_perform_protecting <- dplyr::recode(all_data$govt_perform_protecting, 
                                                  "Strongly agree" = 5,
                                                  "Agree" = 4,
                                                  "Neither agree nor disagree" = 3,
                                                  "Disagree" = 2,
                                                  "Strongly disagree" = 1)
all_data$govt_perform_protecting <- as.numeric(all_data$govt_perform_protecting)

all_data$govt_perform_comm <- as.character(all_data$govt_perform_comm)
all_data$govt_perform_comm <- dplyr::recode(all_data$govt_perform_comm, 
                                            "Strongly agree" = 5,
                                            "Agree" = 4,
                                            "Neither agree nor disagree" = 3,
                                            "Disagree" = 2,
                                            "Strongly disagree" = 1)
all_data$govt_perform_comm <- as.numeric(all_data$govt_perform_comm)

all_data$govt_perform_advice <- as.character(all_data$govt_perform_advice)
all_data$govt_perform_advice <- dplyr::recode(all_data$govt_perform_advice, 
                                              "Strongly agree" = 5,
                                              "Agree" = 4,
                                              "Neither agree nor disagree" = 3,
                                              "Disagree" = 2,
                                              "Strongly disagree" = 1)
all_data$govt_perform_advice <- as.numeric(all_data$govt_perform_advice)

all_data$govt_perform_compared <- as.character(all_data$govt_perform_compared)
all_data$govt_perform_compared <- dplyr::recode(all_data$govt_perform_compared, 
                                                "Strongly agree" = 5,
                                                "Agree" = 4,
                                                "Neither agree nor disagree" = 3,
                                                "Disagree" = 2,
                                                "Strongly disagree" = 1)
all_data$govt_perform_compared <- as.numeric(all_data$govt_perform_compared)

gov_perform_items <- data.frame(# all_data$govt_perform_comm,
  # all_data$govt_perform_advice,
  # all_data$govt_perform_response, # removed for unidim
  all_data$govt_perform_adapt,
  all_data$govt_perform_compared,
  all_data$govt_perform_protecting)
psych::alpha(gov_perform_items)

IRT_govt_perf_scale <- mirt(data=gov_perform_items,
                            model=1,
                            itemtype = "gpcm")
summary(IRT_govt_perf_scale)
plot(IRT_govt_perf_scale, type="trace")
plot(IRT_govt_perf_scale, type="info")
coef(IRT_govt_perf_scale, IRTpars=T)
govt_perf <- fscores(IRT_govt_perf_scale)[,1] # each person's expected score
all_data$govt_perf <- govt_perf

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(gov_perform_items, cor="poly") # unidim

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_govt_perf_scale, type="Q3")) # -0.585

# Model fit
itemfit(IRT_govt_perf_scale, empirical.plot = 1)
itemfit(IRT_govt_perf_scale, empirical.plot = 2)
itemfit(IRT_govt_perf_scale, empirical.plot = 3)
# itemfit(IRT_govt_perf_scale, empirical.plot = 4)
# itemfit(IRT_govt_perf_scale, empirical.plot = 5)
# itemfit(IRT_govt_perf_scale, empirical.plot = 6)

boxplot(all_data$govt_perf ~ all_data$condition)
mean(all_data$govt_perf[all_data$condition=="control"])
mean(all_data$govt_perf[all_data$condition=="treat1"]) # higher performance
mean(all_data$govt_perf[all_data$condition=="treat2"]) # higher performance
tapply(all_data$worry, all_data$condition, mean)

# support for restrictions
all_data$restrictions_quarantine <- ifelse(is.na(all_data$restrictions_quarantine), 0, 1)
all_data$restrictions_schools <- ifelse(is.na(all_data$restrictions_schools), 0, 1)
all_data$restrictions_events <- ifelse(is.na(all_data$restrictions_events), 0, 1)
all_data$restrictions_hospital <- ifelse(is.na(all_data$restrictions_hospital), 0, 1)
all_data$restrictions_quar_flights <- ifelse(is.na(all_data$restrictions_quar_flights), 0, 1)
all_data$restrictions_soc_dist <- ifelse(is.na(all_data$restrictions_soc_dist), 0, 1)
all_data$restrictions_mask_transport <- ifelse(is.na(all_data$restrictions_mask_transport), 0, 1)
all_data$restrictions_mask_indoors <- ifelse(is.na(all_data$restrictions_mask_indoors), 0, 1)
all_data$restrictions_stop_flights <- ifelse(is.na(all_data$restrictions_stop_flights), 0, 1)
all_data$restrictions_none <- NULL

restrict_items <- data.frame(# all_data$restrictions_quarantine,
  # all_data$restrictions_schools,
  all_data$restrictions_events,
  # all_data$restrictions_hospital,
  # all_data$restrictions_stop_flights,
  # all_data$restrictions_quar_flights, # removed for unidim
  all_data$restrictions_soc_dist,
  # all_data$restrictions_mask_transport,
  all_data$restrictions_mask_indoors)

library(psych)
psych::alpha(restrict_items, check.keys = TRUE)

library(mirt)
mirt_restrict_scale <- mirt(data=restrict_items,
                            model=1,
                            itemtype = "2PL")
summary(mirt_restrict_scale)
plot(mirt_restrict_scale, type="trace")
plot(mirt_restrict_scale, type="info")
coef(mirt_restrict_scale, IRTpars=T)
all_data$restrict <- fscores(mirt_restrict_scale)[,1] # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(restrict_items, cor="tet") # unidimensional

# Q3 for local independence
Q3resid <- data.frame(residuals(mirt_restrict_scale, type="Q3"))

# Model fit
itemfit(mirt_restrict_scale, empirical.plot = 1)
itemfit(mirt_restrict_scale, empirical.plot = 2)
itemfit(mirt_restrict_scale, empirical.plot = 3)

boxplot(all_data$restrict ~ all_data$condition)
mean(all_data$restrict[all_data$condition=="control"])
mean(all_data$restrict[all_data$condition=="treat1"]) # more supportive
mean(all_data$restrict[all_data$condition=="treat2"]) # less supportive

# behavioral intentions
all_data$behavior_hairdresser <- ifelse(is.na(all_data$behavior_hairdresser), 0, 1)
all_data$behavior_museum <- ifelse(is.na(all_data$behavior_museum), 0, 1)
all_data$behavior_bars <- ifelse(is.na(all_data$behavior_bars), 0, 1)
all_data$behavior_public_toilet <- ifelse(is.na(all_data$behavior_public_toilet), 0, 1)
all_data$behavior_public_transport <- ifelse(is.na(all_data$behavior_public_transport), 0, 1)
all_data$behavior_cinemas <- ifelse(is.na(all_data$behavior_cinemas), 0, 1)
all_data$behavior_abroad <- ifelse(is.na(all_data$behavior_abroad), 0, 1)
all_data$behavior_worship <- ifelse(is.na(all_data$behavior_worship), 0, 1)
all_data$behavior_gym <- ifelse(is.na(all_data$behavior_gym), 0, 1)
all_data$behavior_public_gathering <- ifelse(is.na(all_data$behavior_public_gathering), 0, 1)
all_data$behavior_none <- NULL

behavior_items <- data.frame(# all_data$behavior_hairdresser,
  # all_data$behavior_museum,
  all_data$behavior_bars, # 1
  # all_data$behavior_public_toilet,
  # all_data$behavior_public_transport,
  all_data$behavior_cinemas, # 2
  # all_data$behavior_abroad,
  # all_data$behavior_worship,
  # all_data$behavior_gym,
  all_data$behavior_public_gathering) # 3

# install.packages("psych")
library(psych)
psych::alpha(behavior_items, check.keys = TRUE)

library(mirt)
mirt_behavior_scale <- mirt(data=behavior_items,
                            model=1,
                            itemtype = "2PL")
summary(mirt_behavior_scale)
plot(mirt_behavior_scale, type="trace")
plot(mirt_behavior_scale, type="info")
coef(mirt_behavior_scale, IRTpars=T)
all_data$behavior <- fscores(mirt_behavior_scale)[,1] # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(behavior_items, cor="tet") # unidimensional

# Q3 for local independence
Q3resid <- data.frame(residuals(mirt_behavior_scale, type="Q3"))

# Model fit
itemfit(mirt_behavior_scale, empirical.plot = 1)
itemfit(mirt_behavior_scale, empirical.plot = 2)
itemfit(mirt_behavior_scale, empirical.plot = 3)

boxplot(all_data$behavior ~ all_data$condition)
mean(all_data$behavior[all_data$condition=="control"])
mean(all_data$behavior[all_data$condition=="treat1"]) # less
mean(all_data$behavior[all_data$condition=="treat2"]) # less

# Numeracy scale
class(all_data$numeracy_discount)
all_data$numeracy <- rowMeans(all_data[c(22,23,24)], na.rm = TRUE)

# Trust scale
table(all_data$trust_understands)
all_data$trust_understands <- as.character(all_data$trust_understands)
all_data$trust_understands <- dplyr::recode(all_data$trust_understands, 
                                            "Strongly agree" = 5,
                                            "Agree" = 4,
                                            "Neither agree nor disagree" = 3,
                                            "Disagree" = 2,
                                            "Strongly disagree" = 1)
all_data$trust_understands <- as.numeric(all_data$trust_understands)

all_data$trust_intentions <- as.character(all_data$trust_intentions)
all_data$trust_intentions <- dplyr::recode(all_data$trust_intentions, 
                                           "Strongly agree" = 5,
                                           "Agree" = 4,
                                           "Neither agree nor disagree" = 3,
                                           "Disagree" = 2,
                                           "Strongly disagree" = 1)
all_data$trust_intentions <- as.numeric(all_data$trust_intentions)

all_data$trust_does_right <- as.character(all_data$trust_does_right)
all_data$trust_does_right <- dplyr::recode(all_data$trust_does_right, 
                                           "Strongly agree" = 5,
                                           "Agree" = 4,
                                           "Neither agree nor disagree" = 3,
                                           "Disagree" = 2,
                                           "Strongly disagree" = 1)
all_data$trust_does_right <- as.numeric(all_data$trust_does_right)

trust_items <- data.frame(all_data$trust_understands,
                          all_data$trust_intentions,
                          all_data$trust_does_right)
psych::alpha(trust_items)

IRT_trust_scale <- mirt(data=trust_items,
                        model=1,
                        itemtype = "gpcm")
summary(IRT_trust_scale)
plot(IRT_trust_scale, type="trace")
plot(IRT_trust_scale, type="info")
coef(IRT_trust_scale, IRTpars=T)
trust <- fscores(IRT_trust_scale)[,1] # each person's expected score
all_data$trust <- trust

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(trust_items, cor="poly") # unidim

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_trust_scale, type="Q3")) # -0.591

# Model fit
itemfit(IRT_trust_scale, empirical.plot = 1)
itemfit(IRT_trust_scale, empirical.plot = 2)
itemfit(IRT_trust_scale, empirical.plot = 3)

# Distrust scale
table(all_data$distrust_no_respect)
all_data$distrust_no_respect <- as.character(all_data$distrust_no_respect)
all_data$distrust_no_respect <- dplyr::recode(all_data$distrust_no_respect, 
                                              "Strongly agree" = 5,
                                              "Agree" = 4,
                                              "Neither agree nor disagree" = 3,
                                              "Disagree" = 2,
                                              "Strongly disagree" = 1)
all_data$distrust_no_respect <- as.numeric(all_data$distrust_no_respect)

all_data$distrust_ignore <- as.character(all_data$distrust_ignore)
all_data$distrust_ignore <- dplyr::recode(all_data$distrust_ignore, 
                                          "Strongly agree" = 5,
                                          "Agree" = 4,
                                          "Neither agree nor disagree" = 3,
                                          "Disagree" = 2,
                                          "Strongly disagree" = 1)
all_data$distrust_ignore <- as.numeric(all_data$distrust_ignore)

all_data$distrust_unfair <- as.character(all_data$distrust_unfair)
all_data$distrust_unfair <- dplyr::recode(all_data$distrust_unfair, 
                                          "Strongly agree" = 5,
                                          "Agree" = 4,
                                          "Neither agree nor disagree" = 3,
                                          "Disagree" = 2,
                                          "Strongly disagree" = 1)
all_data$distrust_unfair <- as.numeric(all_data$distrust_unfair)

distrust_items <- data.frame(all_data$distrust_no_respect,
                             all_data$distrust_ignore,
                             all_data$distrust_unfair)
psych::alpha(distrust_items)

IRT_distrust_scale <- mirt(data=distrust_items,
                           model=1,
                           itemtype = "gpcm")
summary(IRT_distrust_scale)
plot(IRT_distrust_scale, type="trace")
plot(IRT_distrust_scale, type="info")
coef(IRT_distrust_scale, IRTpars=T)
distrust <- fscores(IRT_distrust_scale)[,1] # each person's expected score
all_data$distrust <- distrust

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(distrust_items, cor="poly") # unidim

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_distrust_scale, type="Q3")) # -0.499

# Model fit
itemfit(IRT_distrust_scale, empirical.plot = 1)
itemfit(IRT_distrust_scale, empirical.plot = 2)
itemfit(IRT_distrust_scale, empirical.plot = 3)

# Randomization check
tapply(all_data$know, all_data$condition, mean)
tapply(all_data$contact, all_data$condition, mean)
tapply(all_data$numeracy, all_data$condition, mean)
tapply(all_data$trust, all_data$condition, mean)
tapply(all_data$distrust, all_data$condition, mean)

saveRDS(all_data, "data/preprocessed_scales.rds")
