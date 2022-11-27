set.seed(100)

library(tidyverse)

all_data <- readRDS("data/preprocessed_scales.rds") %>% 
  as_tibble()

# Set up dataframe for main effects coefficients
df <- data.frame("row" = 1:21, "coefs" = NA, 
                 "govt_perf_est" = NA, "govt_perf_se" = NA, "govt_perf_p" = NA,
                 "behavior_est" = NA, "behavior_se" = NA, "behavior_p" = NA,
                 "concern_est" = NA, "concern_se" = NA, "concern_p" = NA,
                 "restrict_est" = NA, "restrict_se" = NA, "restrict_p" = NA)

# and for interactions (including linear combinations)
df2 <- data.frame(DV=NA,Coef=NA, Est=NA, SE=NA, p=NA)

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

# max/min of scales
range(all_data$govt_perf)
range(all_data$restrict)
range(all_data$behavior)
range(all_data$worry)

range(all_data$contact)
range(all_data$know)

# Binary/factor coding of terms
all_data$contact_binary <- ifelse(all_data$contact<=0, 0, 1)
all_data$contact_binary <- as.factor(all_data$contact_binary)

all_data$know_binary <- ifelse(all_data$know<=0, 0, 1)
all_data$know_binary <- as.factor(all_data$know_binary)

all_data$worry_binary <- ifelse(all_data$worry<=0, 0, 1)
all_data$worry_binary <- as.factor(all_data$worry_binary)

all_data$govt_perf_binary <- ifelse(all_data$govt_perf<=0, 0, 1)
all_data$govt_perf_binary <- as.factor(all_data$govt_perf_binary)

all_data$restrict_binary <- ifelse(all_data$restrict<=0, 0, 1)
all_data$restrict_binary <- as.factor(all_data$restrict_binary)

all_data$behavior_binary <- ifelse(all_data$behavior<=0, 0, 1)
all_data$behavior_binary <- as.factor(all_data$behavior_binary)

all_data$trust_binary <- ifelse(all_data$trust<=0, 0, 1)
all_data$trust_binary <- as.factor(all_data$trust_binary)

all_data$numeracy_binary <- ifelse(all_data$numeracy<=0, 0, 1)
all_data$numeracy_binary <- as.factor(all_data$numeracy_binary)

# Descriptive stats
age <- round(tapply(all_data$age,all_data$condition,mean),0)
gender <- round(prop.table(table(all_data$gender,all_data$condition),2)*100,digits=1)
education <- round(prop.table(table(all_data$education,all_data$condition),2)*100,digits=1)
education <- education[c(2,1,4,3),]
ethnicity <- round(prop.table(table(all_data$ethnicity_simple,all_data$condition),2)*100,digits=1)
income <- round(prop.table(table(all_data$income_cat,all_data$condition),2)*100,digits=1)[2,]
party <- round(prop.table(table(all_data$partisanship,all_data$condition),2)*100,digits=1)
party <- party[c(1,4,5,3,2,6),]
referendum <- round(prop.table(table(all_data$eu_ref,all_data$condition),2)*100,digits=1)
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

write.csv(summary_df,"tables/descr_stats_table.csv")

# recruitment targets
round(prop.table(table(all_data$ethnicity_simple,all_data$gender))*100,digits=1)

sample_achieved <- round(prop.table(table(all_data$ethnicity_simple,all_data$gender))*100,digits=1)
sample_achieved[1,1] <- paste(sample_achieved[1,1],"(44.0)",sep=" ")
sample_achieved[2,1] <- paste(sample_achieved[2,1],"(7.0)",sep=" ")
sample_achieved[1,2] <- paste(sample_achieved[1,2],"(42.0)",sep=" ")
sample_achieved[2,2] <- paste(sample_achieved[2,2],"(7.0)",sep=" ")

sample_achieved

# Quick analysis
# H1-main. The presence of mortality data will cause participants in either of the two treatment groups to express higher levels of concern about COVID-19, compared to the control group.
tapply(all_data$worry, all_data$condition, mean) # yes

# H2-main. The presence of mortality data will cause participants in either of the two treatment groups to express worse perceptions of government performance, compared to the control group.
tapply(all_data$govt_perf, all_data$condition, mean) # no

# H3-main. The presence of mortality data will cause participants in either treatment group to report being willing to engage in fewer activities, compared to the control group.
tapply(all_data$behavior, all_data$condition, mean) # yes

# H4-main. The presence of mortality data will cause participants in either treatment group to express greater support for restrictions aimed at curbing the spread of the disease, compared to the control group.
tapply(all_data$restrict, all_data$condition, mean) # yes for treat1

# function for interaction effects
interact <- function(model, cov_matrix, ref_coef, int_coef){
  vec <- summary(model)$coefficients
  i1 <- grep(ref_coef,names(vec[,1]))[1] # grab first index
  i2 <- grep(int_coef,names(vec[,1]))[1] # grab index for interaction
  
  # calculate beta
  beta <- vec[i1,1] + vec[i2,1]
  
  # calculate se
  beta_se <- sqrt(cov_matrix[i1,i1] + cov_matrix[i2,i2] + 2*cov_matrix[i1,i2])
  
  # calculate p value
  p_value <- pnorm(-abs(beta)/beta_se) * 2
  
  out <- data.frame(DV=c(as.character(model$terms[[2]]),as.character(model$terms[[2]])),
                    Coef=c(ref_coef,int_coef), 
                    Est=c(vec[i1,1], beta), 
                    SE=c(vec[i1,2], beta_se), 
                    p=c(vec[i1,4],p_value))
  return(out)
}

# Level of concern
m_level_concern <- lm(worry ~ 
                        condition +
                        contact +
                        know +
                        numeracy +
                        trust +
                        age +
                        gender +
                        education +
                        ethnicity_simple +
                        income_cat +
                        partisanship +
                        eu_ref,
                      data = all_data)
summary(m_level_concern) # no effect

m_level_concern_0 <- lm(worry ~ 
                          condition,
                        data = all_data)

plot(m_level_concern, 1)
# Used to check the linear relationship assumptions. A horizontal line, without distinct patterns 
# is an indication for a linear relationship, what is good.
# If the residual plot indicates a non-linear relationship in the data, then a simple approach is 
# to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the 
# regression model.

plot(m_level_concern, 2)
# Used to examine whether the residuals are normally distributed. It’s good if residuals points 
# follow the straight dashed line.

plot(m_level_concern, 3)
# Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line 
# with equally spread points is a good indication of homoscedasticity.
# A possible solution to reduce the heteroscedasticity problem is to use a log or square root 
# transformation of the outcome variable (y).

plot(m_level_concern, 5)
# Residuals vs Leverage. Used to identify influential cases, that is extreme values that might 
# influence the regression results when included or excluded from the analysis. 
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers.
# A data point has high leverage, if it has extreme predictor x values. This can be detected by 
# examining the leverage statistic or the hat-value. A value of this statistic above 2(p + 1)/n 
# indicates an observation with high leverage (P. Bruce and Bruce 2017); where, p is the number of 
# predictors and n is the number of observations.

# robust standard errors
library(lmtest)
library(sandwich)
m_level_concern_vcov <- vcovHC(m_level_concern)
coeftest(m_level_concern, vcov = m_level_concern_vcov)
coefci(m_level_concern, vcov = m_level_concern_vcov)

# multiple comparison with robust standard errors
library(car)
library(multcomp)
concern.multcomp <- glht(m_level_concern, linfct = mcp(condition = "Tukey") , vcov = m_level_concern_vcov)
summary(concern.multcomp,test = adjusted("holm")) # "none" gives same as coeftest
# there is no significant difference between the effect from treat 1 and 2
par(mfrow = c(1, 1))
par(mar=c(4,8,4,2)) 
plot(confint(concern.multcomp))

df$coefs <- names(coeftest(m_level_concern, vcov = m_level_concern_vcov)[,1])
df$concern_est <- coeftest(m_level_concern, vcov = m_level_concern_vcov)[,1]
df$concern_se <- coeftest(m_level_concern, vcov = m_level_concern_vcov)[,2]
df$concern_p <- coeftest(m_level_concern, vcov = m_level_concern_vcov)[,4]

# Interact concern with contact
m_level_concern_int <- lm(worry ~ 
                            condition +
                            condition:contact +
                            contact +
                            know +
                            numeracy +
                            trust +
                            age +
                            gender +
                            education +
                            ethnicity_simple +
                            income_cat +
                            partisanship +
                            eu_ref,
                          data = all_data)
summary(m_level_concern_int)

par(mfrow = c(2, 2))
plot(m_level_concern_int)

# robust standard errors
library(lmtest)
library(sandwich)
m_level_concern_int_vcov <- vcovHC(m_level_concern_int)
coeftest(m_level_concern_int, vcov = m_level_concern_int_vcov)
coefci(m_level_concern_int, vcov = m_level_concern_int_vcov)

# linear combinations
library(multcomp)
summary(glht(m_level_concern_int, linfct = c("conditiontreat1 + conditiontreat1:contact = 0"), vcov. = m_level_concern_int_vcov))
summary(glht(m_level_concern_int, linfct = c("conditiontreat2 + conditiontreat2:contact = 0"), vcov. = m_level_concern_int_vcov))

df2 <- rbind(df2, interact(m_level_concern_int, m_level_concern_int_vcov, "conditiontreat1","conditiontreat1:contact"))
df2 <- rbind(df2, interact(m_level_concern_int, m_level_concern_int_vcov, "conditiontreat2","conditiontreat2:contact"))

df3 <- data.frame(cbind(summary(m_level_concern_int)$coefficients[c(22:23),],
                        coefci(m_level_concern_int)[c(22:23),],
                        data.frame(DV=rep("concern_contact",2))))

# Government performance
names(all_data)
m_govt_perf <- lm(govt_perf ~ 
                    condition +
                    contact +
                    know +
                    numeracy +
                    trust +
                    age +
                    gender +
                    education +
                    ethnicity_simple +
                    income_cat +
                    partisanship +
                    eu_ref,
                  data = all_data)
summary(m_govt_perf)

m_govt_perf_0 <- lm(govt_perf ~ 
                      condition,
                    data = all_data)

par(mfrow = c(2, 2))
plot(m_govt_perf)

# robust standard errors
library(lmtest)
library(sandwich)
m_govt_perf_vcov <- vcovHC(m_govt_perf)
coeftest(m_govt_perf, vcov = m_govt_perf_vcov)

# multiple comparison with robust standard errors
library(car)
library(multcomp)
govt_perf.multcomp <- glht(m_govt_perf, linfct = mcp(condition = "Tukey") , vcov = m_govt_perf_vcov)
summary(govt_perf.multcomp,test = adjusted("holm")) # "none" gives same as coeftest
par(mfrow = c(1, 1))
plot(confint(govt_perf.multcomp))

df$govt_perf_est <- coeftest(m_govt_perf, vcov = m_govt_perf_vcov)[,1]
df$govt_perf_se <- coeftest(m_govt_perf, vcov = m_govt_perf_vcov)[,2]
df$govt_perf_p <- coeftest(m_govt_perf, vcov = m_govt_perf_vcov)[,4]

# plot multiple comparison with ggplot
mult_comp_plot <- tibble(
  "Comparison" = c("UK-only - Control","Comparative - Control", "Comparative - UK-only"),
  "Estimate" = c(confint(govt_perf.multcomp)$confint[1,1],
                 confint(govt_perf.multcomp)$confint[2,1],
                 confint(govt_perf.multcomp)$confint[3,1]),
  "lwr" = c(confint(govt_perf.multcomp)$confint[1,2],
            confint(govt_perf.multcomp)$confint[2,2],
            confint(govt_perf.multcomp)$confint[3,2]),
  "upr" = c(confint(govt_perf.multcomp)$confint[1,3],
            confint(govt_perf.multcomp)$confint[2,3],
            confint(govt_perf.multcomp)$confint[3,3])
)

mult_comp_plot$Comparison <- factor(mult_comp_plot$Comparison, levels=c("Comparative - UK-only",
                                                                        "Comparative - Control",
                                                                        "UK-only - Control"))

png(file="plots/govperf_multcomp.png", width = 8, height = 6, units = 'in', res = 300)
mult_comp_plot %>% 
  ggplot() +
  aes(x = Comparison, y = Estimate, fill = Comparison) +
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", shape=21) +
  coord_flip() +
  ggtitle("Government Performance") +
  labs(subtitle = "95% CI with robust standard errors") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position="none") +
  xlab("") +
  ylab("Difference") +
  geom_hline(yintercept=0, linetype="dashed")
dev.off()

# untransformed effect
govt_perf_tbl <- all_data %>% 
  group_by(condition) %>% 
  summarise(govt_perform_adapt_mean = mean(govt_perform_adapt),
            govt_perform_adapt_sd = sd(govt_perform_adapt),
            govt_perform_adapt_n = length(govt_perform_adapt),
            govt_perform_compared_mean = mean(govt_perform_compared),
            govt_perform_compared_sd = sd(govt_perform_compared),
            govt_perform_compared_n = length(govt_perform_compared),
            govt_perform_protecting_mean = mean(govt_perform_protecting),
            govt_perform_protecting_sd = sd(govt_perform_protecting),
            govt_perform_protecting_n = length(govt_perform_protecting)) %>% 
  pivot_longer(cols = -condition,
               names_to = c('item','type'),
               names_pattern = '(govt_perform_adapt|govt_perform_compared|govt_perform_protecting)_(mean|sd|n)',
               values_to = 'value')
govt_perf_tbl

govt_perf_tbl %>% 
  filter(type == "mean") %>% 
  pivot_wider(names_from = condition,
              values_from = value) %>% 
  mutate(treat1_diff = treat1 - control,
         treat2_diff = treat2 - control) %>% 
  summarise(treat1 = mean(treat1),
            treat2 = mean(treat2),
            overall = mean(c(treat1, treat2))) # 0.431 (4.3%)

global_font_size = 14
axis_font_size = 13
govt_perf_untransformed_plot <- govt_perf_tbl %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  mutate(upr = mean + qt(0.975, df=n-1)*sd/sqrt(n),
         lwr = mean - qt(0.975, df=n-1)*sd/sqrt(n)) %>% 
  mutate(item = dplyr::recode(item,
                              'govt_perform_adapt' = 'Has adapted well to changes',
                              'govt_perform_compared' = 'Has responded comparatively well',
                              'govt_perform_protecting' = 'Has protected UK residents'),
         condition = dplyr::recode(condition,
                                   'treat1' = 'UK-only',
                                   'treat2' = 'Comparative',
                                   'control' = 'Control')) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ggplot() +
  aes(x = condition, y = mean, fill = item, label = mean) +
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", shape=21) +
  geom_text(vjust = -1.5) +
  xlab("Condition") +
  ylab("Mean response on a scale of 1-5") +
  ggtitle("Mean Untransformed Values for Government Performance Items") +
  labs(subtitle = "95% Confidence Intervals") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme(legend.position="none") +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  facet_wrap(~item)
png(file="plots/govperf_untrans.png", width = 12, height = 6, units = 'in', res = 300)
govt_perf_untransformed_plot
dev.off()

# interact with trust
m_govt_perf_int_trust <- lm(govt_perf ~ 
                              condition +
                              condition:trust +
                              contact +
                              know +
                              numeracy +
                              trust +
                              age +
                              gender +
                              education +
                              ethnicity_simple +
                              income_cat +
                              partisanship +
                              eu_ref,
                            data = all_data)
summary(m_govt_perf_int_trust)

par(mfrow = c(2, 2))
plot(m_govt_perf_int_trust)

anova(m_govt_perf, m_govt_perf_int_trust)
Anova(m_govt_perf_int_trust)

# robust standard errors
library(lmtest)
library(sandwich)
m_govt_perf_int_trust_vcov <- vcovHC(m_govt_perf_int_trust)
coeftest(m_govt_perf_int_trust, vcov = m_govt_perf_int_trust_vcov)

library(multcomp)
interact(m_govt_perf_int_trust, m_govt_perf_int_trust_vcov, "conditiontreat1","conditiontreat1:trust")
interact(m_govt_perf_int_trust, m_govt_perf_int_trust_vcov, "conditiontreat2","conditiontreat2:trust")

df2 <- rbind(df2, interact(m_govt_perf_int_trust, m_govt_perf_int_trust_vcov, "conditiontreat1","conditiontreat1:trust")) # 0.05856666 (p = 0.7008561) 
df2 <- rbind(df2, interact(m_govt_perf_int_trust, m_govt_perf_int_trust_vcov, "conditiontreat2","conditiontreat2:trust")) # -0.3309201 (p = 0.02104776)

# is the level of trust about the same across conditions?
tapply(all_data$trust, all_data$condition, mean)

df3 <- rbind(df3,data.frame(cbind(summary(m_govt_perf_int_trust)$coefficients[c(22:23),],
                                  coefci(m_govt_perf_int_trust)[c(22:23),],
                                  data.frame(DV=rep("govt_trust",2)))))

# plot linear combinations
mean(all_data$trust) # -0.0003451577
max(all_data$trust) # 2.680692
min(all_data$trust) # -1.956808

summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust*(-1) = 0"), vcov. = m_govt_perf_int_trust_vcov))
summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 = 0"), vcov. = m_govt_perf_int_trust_vcov))
summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust*(1) = 0"), vcov. = m_govt_perf_int_trust_vcov))

summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust*(-1) = 0"), vcov. = m_govt_perf_int_trust_vcov))
summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 = 0"), vcov. = m_govt_perf_int_trust_vcov))
summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust*(1) = 0"), vcov. = m_govt_perf_int_trust_vcov))

df_lincomb2 <- data.frame("Interaction" = c(rep("govt_permXtrust",6)),
                          "Condition" = c(rep("UK-Only",3),rep("Comparative",3)),
                          "Trust" = c(rep(c("0 (mean)","+1 SD","-1 SD"),2)),
                          "Effect" = c(summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]],
                                       summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]],
                                       summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust*(-1) = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]],
                                       summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]],
                                       summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]],
                                       summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust*(-1) = 0"), 
                                                    vcov. = m_govt_perf_int_trust_vcov))$test$coefficients[[1]]),
                          "SE" = c(summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]],
                                   summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]],
                                   summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat1 + conditiontreat1:trust*(-1) = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]],
                                   summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]],
                                   summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]],
                                   summary(glht(m_govt_perf_int_trust, linfct = c("conditiontreat2 + conditiontreat2:trust*(-1) = 0"), 
                                                vcov. = m_govt_perf_int_trust_vcov))$test$sigma[[1]]))
df_lincomb2$upr <- df_lincomb2$Effect + 1.96 * df_lincomb2$SE
df_lincomb2$lwr <- df_lincomb2$Effect - 1.96 * df_lincomb2$SE

df_lincomb2$Trust <- factor(df_lincomb2$Trust, levels = c("-1 SD","0 (mean)","+1 SD"))

png(file="plots/govperf_trust.png", width = 7, height = 6, units = 'in', res = 300)
ggplot(df_lincomb2, aes(x=Trust, y=Effect, group=Condition, color=Condition)) + 
  theme_minimal() +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(width = 0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Level of trust") +
  ylab("Treatment effect") +
  ggtitle("Government Performance: Condition x Trust") +
  labs(subtitle = "Estimated effect of condition compared to Control at different levels of trust") +
  theme(plot.title = element_text(face = "bold"))
dev.off()

# interact with partisanship
m_govt_perf_int_part <- lm(govt_perf ~ 
                             condition +
                             condition:partisanship +
                             contact +
                             know +
                             numeracy +
                             trust +
                             age +
                             gender +
                             education +
                             ethnicity_simple +
                             income_cat +
                             partisanship +
                             eu_ref,
                           data = all_data)
summary(m_govt_perf_int_part) # Conservatives less supportive in treat1, and Labour signicantly more

par(mfrow = c(2, 2))
plot(m_govt_perf_int_part)

library(lmtest)
library(sandwich)
m_govt_perf_int_part_vcov <- vcovHC(m_govt_perf_int_part)
coeftest(m_govt_perf_int_part, vcov = m_govt_perf_int_part_vcov)

df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat1","conditiontreat1:partisanshipLabour")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat1","conditiontreat1:partisanshipLib_Dem")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat1","conditiontreat1:partisanshipGreen")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat1","conditiontreat1:partisanshipOther")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat1","conditiontreat1:partisanshipNo_party")) 

df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat2","conditiontreat2:partisanshipLabour")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat2","conditiontreat2:partisanshipLib_Dem")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat2","conditiontreat2:partisanshipGreen")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat2","conditiontreat2:partisanshipOther")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part, m_govt_perf_int_part_vcov, "conditiontreat2","conditiontreat2:partisanshipNo_party")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_govt_perf_int_part)$coefficients[c(22:31),],
                                  coefci(m_govt_perf_int_part)[c(22:31),],
                                  data.frame(DV=rep("govt_partisan",2)))))

# interact with partisanship - binary version
all_data <- within(all_data, partisan_binary <- relevel(partisan_binary, ref = "Cons"))
m_govt_perf_int_part_bin <- lm(govt_perf ~ 
                             condition +
                             condition:partisan_binary +
                             contact +
                             know +
                             numeracy +
                             trust +
                             age +
                             gender +
                             education +
                             ethnicity_simple +
                             income_cat +
                             partisan_binary +
                             eu_ref,
                           data = all_data)
summary(m_govt_perf_int_part_bin) # Conservatives less supportive in treat1, and Labour signicantly more

par(mfrow = c(2, 2))
plot(m_govt_perf_int_part_bin)

library(lmtest)
library(sandwich)
m_govt_perf_int_part_bin_vcov <- vcovHC(m_govt_perf_int_part_bin)
coeftest(m_govt_perf_int_part_bin, vcov = m_govt_perf_int_part_bin_vcov)

df2 <- rbind(df2, interact(m_govt_perf_int_part_bin, m_govt_perf_int_part_bin_vcov, "conditiontreat1","conditiontreat1:partisan_binaryOther_party")) 
df2 <- rbind(df2, interact(m_govt_perf_int_part_bin, m_govt_perf_int_part_bin_vcov, "conditiontreat2","conditiontreat2:partisan_binaryOther_party")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_govt_perf_int_part_bin)$coefficients[c(18:19),],
                                  coefci(m_govt_perf_int_part_bin)[c(18:19),],
                                  data.frame(DV=rep("govt_partisan",2)))))

# interact with knowledge
m_govt_perf_int_know <- lm(govt_perf ~ 
                             condition +
                             condition:know +
                             contact +
                             know +
                             numeracy +
                             trust +
                             age +
                             gender +
                             education +
                             ethnicity_simple +
                             income_cat +
                             partisanship +
                             eu_ref,
                           data = all_data)
summary(m_govt_perf_int_know)

par(mfrow = c(2, 2))
plot(m_govt_perf_int_part)

library(lmtest)
library(sandwich)
m_govt_perf_int_know_vcov <- vcovHC(m_govt_perf_int_know)
coeftest(m_govt_perf_int_know, vcov = m_govt_perf_int_know_vcov)

df2 <- rbind(df2, interact(m_govt_perf_int_know, m_govt_perf_int_know_vcov, "conditiontreat1","conditiontreat1:know")) 
df2 <- rbind(df2, interact(m_govt_perf_int_know, m_govt_perf_int_know_vcov, "conditiontreat2","conditiontreat2:know")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_govt_perf_int_know)$coefficients[c(22:23),],
                                  coefci(m_govt_perf_int_know)[c(22:23),],
                                  data.frame(DV=rep("govt_know",2)))))

# Willingness to engage in activities
names(all_data)
m_behavior <- lm(behavior ~ 
                   condition +
                   contact +
                   know +
                   numeracy +
                   trust +
                   age +
                   gender +
                   education +
                   ethnicity_simple +
                   income_cat +
                   partisanship +
                   eu_ref,
                 data = all_data)
summary(m_behavior) # no effect

m_behavior_0 <- lm(behavior ~ 
                     condition,
                   data = all_data)

par(mfrow = c(2, 2))
plot(m_behavior)

# robust standard errors
library(lmtest)
library(sandwich)
m_behavior_vcov <- vcovHC(m_behavior)
coeftest(m_behavior, vcov = m_behavior_vcov)

# multiple comparison with robust standard errors
library(car)
library(multcomp)
m_behavior.multcomp <- glht(m_behavior, linfct = mcp(condition = "Tukey") , vcov = m_behavior_vcov)
summary(m_behavior.multcomp,test = adjusted("holm")) # "none" gives same as coeftest
par(mfrow = c(1, 1))
plot(confint(m_behavior.multcomp))

df$behavior_est <- coeftest(m_behavior, vcov = m_behavior_vcov)[,1]
df$behavior_se <- coeftest(m_behavior, vcov = m_behavior_vcov)[,2]
df$behavior_p <- coeftest(m_behavior, vcov = m_behavior_vcov)[,4]

# willingness interacted with knowledge
m_behavior_int <- lm(behavior ~ 
                       condition +
                       contact +
                       know +
                       condition:know +
                       numeracy +
                       trust +
                       age +
                       gender +
                       education +
                       ethnicity_simple +
                       income_cat +
                       partisanship +
                       eu_ref,
                     data = all_data)
summary(m_behavior_int)

par(mfrow = c(2, 2))
plot(m_behavior_int)

library(lmtest)
library(sandwich)
m_behavior_int_vcov <- vcovHC(m_behavior_int)
coeftest(m_behavior_int, vcov = m_behavior_int_vcov)

library(multcomp)
mean(all_data$know) + sd(all_data$know)
mean(all_data$know) - sd(all_data$know)
summary(glht(m_behavior_int, linfct = c("conditiontreat1 + conditiontreat1:know*(0.5037164) = 0"), vcov. = m_behavior_int_vcov))
summary(glht(m_behavior_int, linfct = c("conditiontreat2 + conditiontreat2:know*(0.5037164) = 0"), vcov. = m_behavior_int_vcov))
summary(glht(m_behavior_int, linfct = c("conditiontreat1 + conditiontreat1:know*(-0.6265092) = 0"), vcov. = m_behavior_int_vcov))
summary(glht(m_behavior_int, linfct = c("conditiontreat2 + conditiontreat2:know*(-0.6265092) = 0"), vcov. = m_behavior_int_vcov))

df2 <- rbind(df2, interact(m_behavior_int, m_behavior_int_vcov, "conditiontreat1","conditiontreat1:know")) 
df2 <- rbind(df2, interact(m_behavior_int, m_behavior_int_vcov, "conditiontreat2","conditiontreat2:know")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_behavior_int)$coefficients[c(22:23),],
                                  coefci(m_behavior_int)[c(22:23),],
                                  data.frame(DV=rep("behavior_know",2)))))

# willingess interacted with contact
m_behavior_int_cont <- lm(behavior ~ 
                            condition +
                            contact +
                            know +
                            condition:contact +
                            numeracy +
                            trust +
                            age +
                            gender +
                            education +
                            ethnicity_simple +
                            income_cat +
                            partisanship +
                            eu_ref,
                          data = all_data)
summary(m_behavior_int_cont)

par(mfrow = c(2, 2))
plot(m_behavior_int_cont)

library(lmtest)
library(sandwich)
m_behavior_int_cont_vcov <- vcovHC(m_behavior_int_cont)
coeftest(m_behavior_int_cont, vcov = m_behavior_int_cont_vcov)

library(multcomp)
mean(all_data$contact) + sd(all_data$contact)
mean(all_data$contact) - sd(all_data$contact)
summary(glht(m_behavior_int_cont, linfct = c("conditiontreat1 + conditiontreat1:contact*(0.6280946) = 0"), vcov. = m_behavior_int_cont_vcov))
summary(glht(m_behavior_int_cont, linfct = c("conditiontreat2 + conditiontreat2:contact*(0.6280946) = 0"), vcov. = m_behavior_int_cont_vcov))
summary(glht(m_behavior_int_cont, linfct = c("conditiontreat1 + conditiontreat1:contact*(-0.5084667) = 0"), vcov. = m_behavior_int_cont_vcov))
summary(glht(m_behavior_int_cont, linfct = c("conditiontreat2 + conditiontreat2:contact*(-0.5084667) = 0"), vcov. = m_behavior_int_cont_vcov))

df2 <- rbind(df2, interact(m_behavior_int_cont, m_behavior_int_cont_vcov, "conditiontreat1","conditiontreat1:contact")) 
df2 <- rbind(df2, interact(m_behavior_int_cont, m_behavior_int_cont_vcov, "conditiontreat2","conditiontreat2:contact")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_behavior_int_cont)$coefficients[c(22:23),],
                                  coefci(m_behavior_int_cont)[c(22:23),],
                                  data.frame(DV=rep("behavior_contact",2)))))

# Support for restrictions
m_restrict <- lm(restrict ~ 
                   condition +
                   contact +
                   know +
                   numeracy +
                   trust +
                   age +
                   gender +
                   education +
                   ethnicity_simple +
                   income_cat +
                   partisanship +
                   eu_ref,
                 data = all_data)
summary(m_restrict)

m_restrict_0 <- lm(restrict ~ 
                     condition,
                   data = all_data)

par(mfrow = c(2, 2))
plot(m_restrict)
# Used to check the linear relationship assumptions. A horizontal line, without distinct patterns 
# is an indication for a linear relationship, what is good.
# If the residual plot indicates a non-linear relationship in the data, then a simple approach is 
# to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the 
# regression model.

library(car)
# crPlots(m_restrict)

m_restrict_glm <- glm(restrict_binary ~ 
                        condition +
                        contact +
                        know +
                        numeracy +
                        trust +
                        age +
                        gender +
                        education +
                        ethnicity_simple +
                        income_cat +
                        partisanship +
                        eu_ref,
                      data = all_data, family = "binomial")
summary(m_restrict_glm)

# set model and dv
model <- m_restrict_glm
dv <- all_data$restrict_binary

library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)

# extreme values
par(mfrow = c(1, 1))
plot(model, which = 4, id.n = 3)
abline(h = 4/length(all_data))
# looks fine

# influential values
# extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% 
  filter(abs(.std.resid) > 3)
# none

# multicolinearity
car::vif(model)

# mcfadden
library(pscl)
pR2(model)[4]

# robust standard errors
library(lmtest)
library(sandwich)
m_restrict_vcov <- vcovHC(m_restrict)
coeftest(m_restrict, vcov = m_restrict_vcov)

# multiple comparison with robust standard errors
library(car)
library(multcomp)
m_restrict.multcomp <- glht(m_restrict, linfct = mcp(condition = "Tukey") , vcov = m_restrict_vcov)
summary(m_restrict.multcomp,test = adjusted("holm")) # "none" gives same as coeftest
par(mfrow = c(1, 1))
plot(confint(m_restrict.multcomp))

df$restrict_est <- coeftest(m_restrict, vcov = m_restrict_vcov)[,1]
df$restrict_se <- coeftest(m_restrict, vcov = m_restrict_vcov)[,2]
df$restrict_p <- coeftest(m_restrict, vcov = m_restrict_vcov)[,4]

df[3:14] <- round(df[3:14], digits=3)
df[1] <- NULL

# restrictions interacted with knowledge
m_restrict_int_know <- lm(restrict ~ 
                            condition +
                            contact +
                            know +
                            condition:know +
                            numeracy +
                            trust +
                            age +
                            gender +
                            education +
                            ethnicity_simple +
                            income_cat +
                            partisanship +
                            eu_ref,
                          data = all_data)
summary(m_restrict_int_know) # no effect

par(mfrow = c(2, 2))
plot(m_restrict_int_know)

library(lmtest)
library(sandwich)
m_restrict_int_know_vcov <- vcovHC(m_restrict_int_know)
coeftest(m_restrict_int_know, vcov = m_restrict_int_know_vcov)

library(multcomp)
mean(all_data$know) + sd(all_data$know)
mean(all_data$know) - sd(all_data$know)
summary(glht(m_restrict_int_know, linfct = c("conditiontreat1 + conditiontreat1:know*(0.5037164) = 0"), vcov. = m_restrict_int_know_vcov))
summary(glht(m_restrict_int_know, linfct = c("conditiontreat2 + conditiontreat2:know*(0.5037164) = 0"), vcov. = m_restrict_int_know_vcov))
summary(glht(m_restrict_int_know, linfct = c("conditiontreat1 + conditiontreat1:know*(-0.6265092) = 0"), vcov. = m_restrict_int_know_vcov))
summary(glht(m_restrict_int_know, linfct = c("conditiontreat2 + conditiontreat2:know*(-0.6265092) = 0"), vcov. = m_restrict_int_know_vcov))

df2 <- rbind(df2, interact(m_restrict_int_know, m_restrict_int_know_vcov, "conditiontreat1","conditiontreat1:know")) 
df2 <- rbind(df2, interact(m_restrict_int_know, m_restrict_int_know_vcov, "conditiontreat2","conditiontreat2:know")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_restrict_int_know)$coefficients[c(22:23),],
                                  coefci(m_restrict_int_know)[c(22:23),],
                                  data.frame(DV=rep("restrict_know",2)))))

m_restrict_int_know_glm <- glm(restrict_binary ~ 
                                 condition +
                                 contact +
                                 know +
                                 condition:know +
                                 numeracy +
                                 trust +
                                 age +
                                 gender +
                                 education +
                                 ethnicity_simple +
                                 income_cat +
                                 partisanship +
                                 eu_ref,
                               data = all_data, family = "binomial")
summary(m_restrict_int_know_glm)

# set model and dv
model <- m_restrict_int_know_glm
dv <- all_data$restrict_binary

library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)

# extreme values
par(mfrow = c(1, 1))
plot(model, which = 4, id.n = 3)
abline(h = 4/length(all_data))
# looks fine

# influential values
# extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% 
  filter(abs(.std.resid) > 3)
# none

# multicolinearity
car::vif(model)

# mcfadden
library(pscl)
pR2(model)[4]

# restrictions interacted with contact
m_restrict_int_contact <- lm(restrict ~ 
                               condition +
                               contact +
                               know +
                               condition:contact +
                               numeracy +
                               trust +
                               age +
                               gender +
                               education +
                               ethnicity_simple +
                               income_cat +
                               partisanship +
                               eu_ref,
                             data = all_data)
summary(m_restrict_int_contact)

par(mfrow = c(2, 2))
plot(m_restrict_int_contact)

library(lmtest)
library(sandwich)
m_restrict_int_contact_vcov <- vcovHC(m_restrict_int_contact)
coeftest(m_restrict_int_contact, vcov = m_restrict_int_contact_vcov)

library(multcomp)
mean(all_data$contact) + sd(all_data$contact)
mean(all_data$contact) - sd(all_data$contact)
summary(glht(m_restrict_int_contact, linfct = c("conditiontreat1 + conditiontreat1:contact*(0.6280946) = 0"), vcov. = m_restrict_int_contact_vcov))
summary(glht(m_restrict_int_contact, linfct = c("conditiontreat2 + conditiontreat2:contact*(0.6280946) = 0"), vcov. = m_restrict_int_contact_vcov))
summary(glht(m_restrict_int_contact, linfct = c("conditiontreat1 + conditiontreat1:contact*(-0.5084667) = 0"), vcov. = m_restrict_int_contact_vcov))
summary(glht(m_restrict_int_contact, linfct = c("conditiontreat2 + conditiontreat2:contact*(-0.5084667) = 0"), vcov. = m_restrict_int_contact_vcov))

df2 <- rbind(df2, interact(m_restrict_int_contact, m_restrict_int_contact_vcov, "conditiontreat1","conditiontreat1:contact")) 
df2 <- rbind(df2, interact(m_restrict_int_contact, m_restrict_int_contact_vcov, "conditiontreat2","conditiontreat2:contact")) 

df3 <- rbind(df3,data.frame(cbind(summary(m_restrict_int_contact)$coefficients[c(22:23),],
                                  coefci(m_restrict_int_contact)[c(22:23),],
                                  data.frame(DV=rep("restrict_contact",2)))))

m_restrict_int_contact_glm <- glm(restrict_binary ~ 
                                    condition +
                                    contact +
                                    know +
                                    condition:contact +
                                    numeracy +
                                    trust +
                                    age +
                                    gender +
                                    education +
                                    ethnicity_simple +
                                    income_cat +
                                    partisanship +
                                    eu_ref,
                                  data = all_data, family = "binomial")
summary(m_restrict_int_contact_glm)

# set model and dv
model <- m_restrict_int_contact_glm
dv <- all_data$restrict_binary

library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)

# extreme values
par(mfrow = c(1, 1))
plot(model, which = 4, id.n = 3)
abline(h = 4/length(all_data))
# looks fine

# influential values
# extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% 
  filter(abs(.std.resid) > 3)
# none

# multicolinearity
car::vif(model)

# mcfadden
library(pscl)
pR2(model)[4]

# write.csv(df2, "interaction_effects.csv")
# write.csv(df3, "interaction_effects_differences.csv")

# plot main effects
govt_perf_df <- data.frame(coeftest(m_govt_perf, vcov = m_govt_perf_vcov)[c(2:3),])
govt_perf_df$upr <- c(coefci(m_govt_perf, vcov = m_govt_perf_vcov)[2,2],
                      coefci(m_govt_perf, vcov = m_govt_perf_vcov)[3,2])
govt_perf_df$lwr <- c(coefci(m_govt_perf, vcov = m_govt_perf_vcov)[2,1],
                      coefci(m_govt_perf, vcov = m_govt_perf_vcov)[3,1])
govt_perf_df$condition <- c("UK-Only",
                            "Comparison")

govt_perf_graph <- ggplot(govt_perf_df, aes(x=condition, y=Estimate, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference") +
  ggtitle("Government Performance") +
  labs(subtitle = "Estimated effect compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
png(file="plots/govperf.png", width = 8, height = 6, units = 'in', res = 300)
govt_perf_graph
dev.off()

behavior_df <- data.frame(coeftest(m_behavior, vcov = m_behavior_vcov)[c(2:3),])
behavior_df$upr <- c(coefci(m_behavior, vcov = m_behavior_vcov)[2,2],
                     coefci(m_behavior, vcov = m_behavior_vcov)[3,2])
behavior_df$lwr <- c(coefci(m_behavior, vcov = m_behavior_vcov)[2,1],
                     coefci(m_behavior, vcov = m_behavior_vcov)[3,1])
behavior_df$condition <- c("UK-Only",
                           "Comparison")

behavior_graph <- ggplot(behavior_df, aes(x=condition, y=Estimate, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference") +
  ggtitle("Willingness to engage in activities") +
  labs(subtitle = "Estimated effect compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
png(file="plots/behavior.png", width = 8, height = 6, units = 'in', res = 300)
behavior_graph
dev.off()

concern_df <- data.frame(coeftest(m_level_concern, vcov = m_level_concern_vcov)[c(2:3),])
concern_df$upr <- c(coefci(m_level_concern, vcov = m_level_concern_vcov)[2,2],
                    coefci(m_level_concern, vcov = m_level_concern_vcov)[3,2])
concern_df$lwr <- c(coefci(m_level_concern, vcov = m_level_concern_vcov)[2,1],
                    coefci(m_level_concern, vcov = m_level_concern_vcov)[3,1])
concern_df$condition <- c("UK-Only",
                          "Comparison")

concern_graph <- ggplot(concern_df, aes(x=condition, y=Estimate, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference") +
  ggtitle("Level of concern") +
  labs(subtitle = "Estimated effect compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
png(file="plots/concern.png", width = 8, height = 6, units = 'in', res = 300)
concern_graph
dev.off()

restrict_df <- data.frame(coeftest(m_restrict, vcov = m_restrict_vcov)[c(2:3),])
restrict_df$upr <- c(coefci(m_restrict, vcov = m_restrict_vcov)[2,2],
                     coefci(m_restrict, vcov = m_restrict_vcov)[3,2])
restrict_df$lwr <- c(coefci(m_restrict, vcov = m_restrict_vcov)[2,1],
                     coefci(m_restrict, vcov = m_restrict_vcov)[3,1])
restrict_df$condition <- c("UK-Only",
                           "Comparison")

restrict_graph <- ggplot(restrict_df, aes(x=condition, y=Estimate, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference") +
  ggtitle("Support for restrictions") +
  labs(subtitle = "Estimated effect compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
png(file="plots/restrict.png", width = 8, height = 6, units = 'in', res = 300)
restrict_graph
dev.off()

library(ggpubr)
main_effects_graph <- ggarrange(govt_perf_graph, behavior_graph, concern_graph, restrict_graph,
                                ncol = 2, nrow = 2,
                                common.legend = FALSE, legend = NULL)
png(file="plots/main_effects.png", width = 12, height = 8, units = 'in', res = 300)
main_effects_graph
dev.off()

# plot interactions
df3 <- cbind(Interaction = rownames(df3), df3)
rownames(df3) <- 1:nrow(df3)

partisan_df <- subset(df3,DV=="govt_partisan")
rownames(partisan_df) <- 1:nrow(partisan_df)
partisan_df_treat1 <- partisan_df[c(1,3,5,7,9),]
partisan_df_treat1$Interaction <- c("Green","Labour","Lib Dem","No party","Other")
partisan_df_treat1$Interaction <- factor(partisan_df_treat1$Interaction,
                                         levels=c("No party","Other","Green","Lib Dem","Labour"),
                                         ordered=TRUE)
partisan_df_treat2 <- partisan_df[c(2,4,6,8,10),]
partisan_df_treat2$Interaction <- c("Green","Labour","Lib Dem","No party","Other")
partisan_df_treat2$Interaction <- factor(partisan_df_treat2$Interaction,
                                         levels=c("No party","Other","Green","Lib Dem","Labour"),
                                         ordered=TRUE)

partisan_df_binary <- partisan_df[c(11,12),]
partisan_df_binary$Condition <- c("UK-only","Comparative")

library(ggplot2)
library(RColorBrewer)
partisan_treat1_graph <- partisan_df_treat1 %>% 
  ggplot() +
  aes(x=Interaction, y=Estimate, group=Interaction) +
  geom_pointrange(aes(ymin=X2.5.., ymax=X97.5..), fill=c("#528D6B", "#E4003B", "#FAA61A", "grey", "grey"), shape=21) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  xlab("Comparison") +
  ylab("Difference on Government Performance Scale") +
  ggtitle("Condition x Partisanship: UK-Only") +
  labs(subtitle = "Difference in effect compared with Conservatives") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(values=c("grey", "grey", "#528D6B", "#FAA61A", "#E4003B"))
partisan_treat1_graph

partisan_treat2_graph <- partisan_df_treat2 %>% 
  ggplot() +
  aes(x=Interaction, y=Estimate, group=Interaction) +
  geom_pointrange(aes(ymin=X2.5.., ymax=X97.5..), fill=c("#528D6B", "#E4003B", "#FAA61A", "grey", "grey"), shape=21) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  xlab("") +
  ylab("Difference on Government Performance Scale") +
  ggtitle("Condition x Partisanship: Comparative") +
  labs(subtitle = "Difference in effect compared with Conservatives") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
partisan_treat2_graph

partisanship_graph <- ggarrange(partisan_treat1_graph, partisan_treat2_graph,
                                ncol = 2, nrow = 1,
                                common.legend = FALSE, legend = NULL)
png(file="plots/partisanship.png", width = 12, height = 6, units = 'in', res = 300)
partisanship_graph
dev.off()
# print 1600 x 400

partisan_binary_graph <- partisan_df_binary %>% 
  ggplot() +
  aes(x=Condition, y=Estimate, group=Condition, fill=Condition) +
  geom_pointrange(aes(ymin=X2.5.., ymax=X97.5..),color="black", shape=21) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  xlab("") +
  ylab("Difference on Government Performance Scale") +
  ggtitle("Condition x Partisanship") +
  labs(subtitle = "Difference in effect for non-Conservatives compared with Conservatives") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))
png(file="plots/partisanship_binary.png", width = 8, height = 6, units = 'in', res = 300)
partisan_binary_graph
dev.off()

govt_know_graph <- interact_plot(m_govt_perf_int_know, pred = know, modx = condition, 
                                 plot.points = FALSE, 
                                 robust = TRUE,
                                 interval = FALSE,
                                 linearity.check = FALSE,
                                 centered = "none")
govt_know_graph

behavior_know_graph <- interact_plot(m_behavior_int, pred = know, modx = condition, 
                                     plot.points = FALSE, 
                                     robust = TRUE,
                                     interval = FALSE,
                                     linearity.check = FALSE,
                                     centered = "none")
behavior_know_graph

restrict_know_graph <- interact_plot(m_restrict_int_know, pred = know, modx = condition, 
                                     plot.points = FALSE, 
                                     robust = TRUE,
                                     interval = FALSE,
                                     linearity.check = FALSE,
                                     centered = "none")
restrict_know_graph

knowledge_graph <- ggarrange(govt_know_graph, behavior_know_graph, restrict_know_graph,
                             ncol = 3, nrow = 1,
                             common.legend = TRUE, legend = NULL)
knowledge_graph

govt_trust_graph <- interact_plot(m_govt_perf_int_trust, pred = trust, modx = condition, 
                                  plot.points = FALSE, 
                                  robust = TRUE,
                                  interval = FALSE,
                                  linearity.check = FALSE,
                                  centered = "none")
govt_trust_graph

concern_contact_graph <- interact_plot(m_level_concern_int, pred = contact, modx = condition, 
                                       plot.points = FALSE, 
                                       robust = TRUE,
                                       interval = FALSE,
                                       linearity.check = FALSE,
                                       centered = "none")
concern_contact_graph

behavior_contact_graph <- interact_plot(m_behavior_int_cont, pred = contact, modx = condition, 
                                        plot.points = FALSE, 
                                        robust = TRUE,
                                        interval = FALSE,
                                        linearity.check = FALSE,
                                        centered = "none")
behavior_contact_graph

restrict_contact_graph <- interact_plot(m_restrict_int_contact, pred = contact, modx = condition, 
                                        plot.points = FALSE, 
                                        robust = TRUE,
                                        interval = FALSE,
                                        linearity.check = FALSE,
                                        centered = "none")
restrict_contact_graph

contact_graph <- ggarrange(concern_contact_graph, behavior_contact_graph, restrict_contact_graph,
                           ncol = 3, nrow = 1,
                           common.legend = FALSE, legend = NULL)
contact_graph

heterogeneity_graph <- ggarrange(concern_contact_graph, restrict_contact_graph,
                                 behavior_know_graph, restrict_know_graph,
                                 ncol = 2, nrow = 2,
                                 common.legend = FALSE, legend = NULL)
heterogeneity_graph
