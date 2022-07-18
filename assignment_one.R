
#Loading Required Packages

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(lmtest)
library(AER)
library(rmarkdown)

#Importing Data Sets

discounting <- read_excel("discounting.xlsx")
individual <- read_excel("individual.xlsx")

#Creating New Variables for Analysis

individual <- dplyr::mutate_all(individual, function(x) as.numeric(as.character(x)))
risk <- individual %>% dplyr::select(num_range("risk_", 1:10))
proportion_safe <- rowSums(risk)
individual <- individual %>% dplyr::mutate("proportion_safe" = proportion_safe/10)


altruism <- individual %>% dplyr::select(num_range("altr_", 1:4))
altruism_score <- rowSums(altruism)
individual <- individual %>% dplyr::mutate("altruism_score" = altruism_score/400)

#According to Wileyto et al.(2004):

#Reward Ratio = Immediate Reward/Delayed Reward. Then, the regresssor used is 1 - (1/R) 

discounting <- discounting %>% dplyr::mutate("reward_ratio" = today/later)
discounting <- discounting %>% dplyr::mutate("modified_R" = 1 - (1/reward_ratio))

#Logit Regression without intercept to find Individual Discount Factors

logit1 <- glm(data = discounting, ind_1 ~ modified_R + days - 1, family = "binomial")
logit2 <- glm(data = discounting, ind_2 ~ modified_R + days - 1, family = "binomial")

logit3 <- glm(data = discounting, ind_3 ~ modified_R + days - 1, family = "binomial")
logit4 <- glm(data = discounting, ind_4 ~ modified_R + days - 1, family = "binomial")
logit5 <- glm(data = discounting, ind_5 ~ modified_R + days - 1, family = "binomial")

logit6 <- glm(data = discounting, ind_6 ~ modified_R + days - 1, family = "binomial")
logit7 <- glm(data = discounting, ind_7 ~ modified_R + days - 1, family = "binomial")
logit8 <- glm(data = discounting, ind_8 ~ modified_R + days - 1, family = "binomial")
logit9 <- glm(data = discounting, ind_9 ~ modified_R + days - 1, family = "binomial")
logit10 <- glm(data = discounting, ind_10 ~ modified_R + days - 1, family = "binomial")

logit11 <- glm(data = discounting, ind_11 ~ modified_R + days - 1, family = "binomial")
logit12 <- glm(data = discounting, ind_12 ~ modified_R + days - 1, family = "binomial")
logit13 <- glm(data = discounting, ind_13 ~ modified_R + days - 1, family = "binomial")
logit14 <- glm(data = discounting, ind_14 ~ modified_R + days - 1, family = "binomial")

#Calculating Individual Daily Discount Factor = (coeff of t/coeff of modified R)

daily_discount_factors = c(-834.3228/-3345.098, -0.01926/-1.84741, -0.02243/-7.12329, -0.018182/-0.910106, -0.04325/-3.93866, -0.0114185/-0.970537, -0.024340/-0.1207219, -0.0895984/-15.02214104, -0.0075556/-0.4923875, -0.4418089/-263.6877093, -0.00863672/-5.7458937, -0.0127008/-2.58111805, -4.570806/-31.015681,-0.01406559/-0.86845615)
print(daily_discount_factors)



individual <- individual %>% tidyr::drop_na()
individual <- individual %>% dplyr::mutate("daily_discount" = daily_discount_factors)
acceptance_proposer_dummy <- c(0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0)
length(acceptance_proposer_dummy)
individual <- individual %>% dplyr::mutate("proposer" = acceptance_proposer_dummy)

#Calculating SPNE Outcomes (Corpus = 10 in each round of bargaining)


#Players 1 and 4 were paired against each other and player 4 was the proposer at acceptance

d1 <- daily_discount_factors[1]
d4 <- daily_discount_factors[4]

own_predicted_payoff_4 <- 10*((1-d1)/(1-(d1*d4)))
own_predicted_payoff_1 <- 10 - own_predicted_payoff_4

#Players 2 and 3 were paired against each other and player 3 was the proposer at acceptance

d2 <- daily_discount_factors[2]
d3 <- daily_discount_factors[3]

own_predicted_payoff_3 <- 10*((1-d2)/(1-(d2*d3)))
own_predicted_payoff_2 <- 10 - own_predicted_payoff_3

#Players 5 and 7 were paired against each other and player 5 was the proposer at acceptance

d5 <- daily_discount_factors[5]
d7 <- daily_discount_factors[7]

own_predicted_payoff_5 <- 10*((1-d7)/(1-(d5*d7)))
own_predicted_payoff_7 <- 10 - own_predicted_payoff_5

#Players 6 and 8 were paired against each other and player 8 was the proposer at acceptance

d6 <- daily_discount_factors[6]
d8 <- daily_discount_factors[8]

own_predicted_payoff_8 <- 10*((1-d6)/(1-(d6*d8)))
own_predicted_payoff_6 <- 10 -  own_predicted_payoff_8

#Players 9 and 12 were paired against each other and player 9 was the proposer at acceptance

d9 <- daily_discount_factors[9]
d12 <- daily_discount_factors[12]

own_predicted_payoff_9 <- 10*((1-d12)/(1-(d9*d12)))
own_predicted_payoff_12 <- 10 - own_predicted_payoff_9

#Players 10 and 14 were paired against each other and player 10 was the proposer at acceptance

d10 <- daily_discount_factors[10]
d14 <- daily_discount_factors[14]

own_predicted_payoff_10 <- 10*((1-d14)/(1-(d10*d14)))
own_predicted_payoff_14 <- 10 - own_predicted_payoff_10

#Players 11 and 13 were paired against each other and player 11 was the proposer at acceptance

d11 <- daily_discount_factors[11]
d13 <- daily_discount_factors[13]

own_predicted_payoff_11 <- 10*((1-d13)/(1-(d11*d13)))
own_predicted_payoff_13 <- 10 - own_predicted_payoff_11

predicted_own_shares = c(own_predicted_payoff_1, own_predicted_payoff_2, own_predicted_payoff_3, own_predicted_payoff_4, own_predicted_payoff_5, own_predicted_payoff_6, own_predicted_payoff_7, own_predicted_payoff_8, own_predicted_payoff_9, own_predicted_payoff_10, own_predicted_payoff_11, own_predicted_payoff_12, own_predicted_payoff_13, own_predicted_payoff_14)
individual <- individual %>% dplyr::mutate("predicted_own_shares" = predicted_own_shares)

#Econometric Analysis: focusing on theory because of a small sample size


#Difference in means of predicted and actual payoffs for proposers and responders: T-test (assuming Normality) and Wilcoxin Ranked Test (non-parametric)

proposer <- individual%>%dplyr::filter(proposer == 1)
responder <- individual%>%dplyr::filter(proposer == 0)

mean_proposer_predicted_payoff <- mean(proposer$predicted_own_shares)
mean_proposer_actual_payoff <- mean(proposer$payoff_at_acceptance)
mean_responder_predicted_payoff <- mean(responder$predicted_own_shares)
mean_responder_actual_payoff <- mean(responder$payoff_at_acceptance)

#For propers, avg predicted payoff > avg actual payoff while for responders, avg predicted payoff < avg actual payoff

t_proposer <- t.test(proposer$payoff_at_acceptance, proposer$predicted_own_shares, var.equal = TRUE)
t_proposer #p<0.01, 95% CI = (-4.6939, -2.3466), does not include 0

t_responder <- t.test(responder$payoff_at_acceptance, responder$predicted_own_shares, var.equal = TRUE)
t_responder #p<0.01, 95% CI = (1.0301, 4.5818)

wilcox.test(proposer$payoff_at_acceptance, proposer$predicted_own_shares, paired = TRUE, alternative = "two.sided") #p = 0.015 < 0.05
wilcox.test(responder$payoff_at_acceptance, responder$predicted_own_shares, paired = TRUE, alternative = "two.sided") #p = 0.031 < 0.05

individual <- individual%>%dplyr::mutate("diff" = predicted_own_shares - payoff_at_acceptance)

#The "diff" variable will be positive for proposers and negative for responders on an average

cor(individual) #No multicollinearity

#Adopting the use of heteroskedasticity corrected standard errors

diff_specification_one <- lm(data = individual, diff ~ gender + intuition + proportion_safe + altruism_score + proposer + altruism_score*proposer + proportion_safe*proposer + gender*proposer + intuition*proposer)
robust_one <- coeftest(diff_specification_one, vcov = vcovHC(diff_specification_one, type = "HC0"))
robust_one

diff_specification_two <- lm(data = individual, diff ~ proposer)
robust_two <- coeftest(diff_specification_two, vcov = vcovHC(diff_specification_two, type = "HC0"))
robust_two

diff_specification_three <- lm(data = individual, diff ~ rounds + rounds*proposer + gender + intuition + proportion_safe + altruism_score + proposer + altruism_score*proposer + proportion_safe*proposer + gender*proposer + intuition*proposer)
robust_three <- coeftest(diff_specification_three, vcov = vcovHC(diff_specification_three, type = 'HC0'))
robust_three

#Model Fit: Specification 3 has the lowest AIC

AIC(diff_specification_one)
AIC(diff_specification_two)
AIC(diff_specification_three)

#Visualising results using ggplot2

ggplot(individual) + geom_line(aes(diff, predicted_own_shares), color = "purple", size = 1)+ geom_line(aes(diff, payoff_at_acceptance), color = "black", size = 1, linetype = "dashed") + geom_vline(xintercept = 0, size = 1) + theme_classic() + ggtitle("Share Predictions and Actual Payoffs") + xlab("Predicted Payoff minus Actual Payoff") + ylab("Payoff")     
ggplot(individual, aes(as.factor(gender), altruism_score), fill = as.factor(gender)) + geom_boxplot(alpha = 0.05, width = 0.5, size = 0.75, fill = "purple") + theme_classic() + xlab("Women (1) vs Men (0)") + ylab("Altruism Score") + ggtitle("Altruism Score Varying Across Gender")
ggplot(individual, aes(as.factor(intuition), daily_discount*100)) + geom_boxplot(alpha = 0.05, width = 0.5, size = 0.75, fill = "purple") + theme_classic() + xlab("Reflective (1) vs Intuitive (0)") + ylab("Predicted Discount Factors in %") + ylim(0,3) + ggtitle("Discount Factors Varying Across Intuitive Ability")
ggplot(individual, aes(as.factor(intuition), proportion_safe)) + geom_boxplot(alpha = 0.05, width = 0.5, size = 0.75, fill = "purple") + theme_classic() + xlab("Reflective (1) vs Intuitive (0)") + ylab("Proportion of Safe Options Chosen") + ggtitle("Proportion of Safe Options Chosen According to Intuitive Ability")


