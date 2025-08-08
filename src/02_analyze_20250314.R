# !diagnostics off


rm(list=ls())

options(scipen=999)
options(java.parameters = "-Xmx4g")
# override table so that it always uses "NA"
table <- function(..., useNA = "always") {
  base::table(..., useNA = useNA)
}
select <- dplyr::select
vars <- dplyr::vars



library("ggplot2")
library("dplyr")
library("texreg")
library(tidyverse)
library(ggplot2)
library(effects)
library(patchwork)  # Or use gridExtra for an alternative layout




# FUNCTIONS ---------------------------------------------------------------
source("src/00_functions.R")


# LOAD DATA ---------------------------------------------------------------
load("data\\merge.RData")








# CREATE VARIABLES --------------------------------------------------------

# calculate access points as per Ehrlich, Standardize each variable 
merge <- merge %>%
  mutate(
    acc_party1 = 3-v2pscohesv_osp / sd(3-v2pscohesv_osp, na.rm = TRUE),    # party discipline
    acc_fed1 = v2x_feduni / sd(v2x_feduni, na.rm = TRUE),                      # federalism
    acc_pres1 = 1-br_pres / sd(br_pres, na.rm = TRUE),                      # presidentialism
    acc_dist1 = log1p(districts) / sd(log1p(districts), na.rm = TRUE),                  # electoral districts
    acc_enp1 = log1p(enp.seats) / sd(log1p(enp.seats), na.rm = TRUE),                  # effective number of parties 
    access1 = (acc_party1 + acc_fed1 + acc_pres1 + acc_dist1 + acc_enp1)/5
  )

merge <- merge %>%
  mutate(
    acc_party2 = (3-v2pscohesv_osp)/3,         # party discipline, reversed
    acc_fed2 = v2x_feduni,                      # federalism
    acc_pres2 = 1-br_pres,                      # presidentialism, reversed
    access2 = (acc_party2 + acc_fed2 + acc_pres2)/3
  )


aut <- merge[merge$v2x_regime2=="Autocracy",]



# ACCESS ------------------------------------------------------------------

# following ehrlich
# additive index of the following indicators, after standardization by the SD
# electoral districts   +    gol_dist   (0-550)       can't use it as it is only available for democracies
# party discipline     -      v2pscohesv_osp    (0-3)
# number of parties     +     gol_enep   (only for 38 countries)     can't use it as it is only available for democracies
# federalism         +          v2x_feduni
# presidentialism     -     br_pres


aut %>%
  filter(access1 == max(access1, na.rm = TRUE)) %>%
  select(ISOyr, access1)
aut %>%
  filter(access1 == min(access1, na.rm = TRUE)) %>%
  select(ISOyr, access1)

aut %>%
  filter(access2 == max(access2, na.rm = TRUE)) %>%
  select(ISOyr, access2)
aut %>%
  filter(access2 == min(access2, na.rm = TRUE)) %>%
  select(ISOyr, access2)




texreg::screenreg(m1a <- lm(access1 ~ gwf_regimetype4  + undp_hdi + logpop, data = aut))
texreg::screenreg(m1b <- lm(access1 ~ kailitz_regime   + undp_hdi + logpop, data = aut))
texreg::screenreg(m1c <- lm(access1 ~ wth_regime   + undp_hdi + logpop, data = aut))




# Generate effect plots for each model
effect_gwf <- as.data.frame(effect("gwf_regimetype4", m1a))
effect_kailitz <- as.data.frame(effect("kailitz_regime", m1b))
effect_wth <- as.data.frame(effect("wth_regime", m1c))

# Create individual plots
p1 <- ggplot(effect_gwf, aes(x = gwf_regimetype4, y = fit, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(title = "Effect of GWF Regime Type on Access", x = "GWF Regime Type", y = "Predicted Access") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(effect_kailitz, aes(x = kailitz_regime, y = fit, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(title = "Effect of Kailitz Regime on Access", x = "Kailitz Regime", y = "Predicted Access") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(effect_wth, aes(x = wth_regime, y = fit, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(title = "Effect of WTH Regime on Access", x = "WTH Regime", y = "Predicted Access") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all three plots into one figure
p1 + p2 + p3 + plot_layout(ncol = 1)  # Vertical layout





























# SOCIAL CONTROL ----------------------------------------------------------

# aut$cso_repress
# Does the government attempt to repress civil society organizations (CSOs)?
# 0: Severely. The government violently and actively pursues all real and even some imagined
# members of CSOs. They seek not only to deter the activity of such groups but to effectively
# liquidate them. Examples include Stalinist Russia, Nazi Germany, and Maoist China.
# 1: Substantially. In addition to the kinds of harassment outlined in responses 2 and 3 below,
# the government also arrests, tries, and imprisons leaders of and participants in oppositional
# CSOs who have acted lawfully. Other sanctions include disruption of public gatherings and
# violent sanctions of activists (beatings, threats to families, destruction of valuable property).
# Examples include Mugabe’s Zimbabwe, Poland under Martial Law, Serbia under Milosevic.
# 2: Moderately. In addition to material sanctions outlined in response 3 below, the
# government also engages in minor legal harassment (detentions, short-term incarceration) to
# dissuade CSOs from acting or expressing themselves. The government may also restrict the
# scope of their actions through measures that restrict association of civil society organizations
# with each other or political parties, bar civil society organizations from taking certain
# actions, or block international contacts. Examples include post-Martial Law Poland, Brazil in
# the early 1980s, the late Franco period in Spain.
# 3: Weakly. The government uses material sanctions (fines, firings, denial of social services) to
# deter oppositional CSOs from acting or expressing themselves. They may also use
# burdensome registration or incorporation procedures to slow the formation of new civil
# society organizations and sidetrack them from engagement. The government may also
# organize Government Organized Movements or NGOs (GONGOs) to crowd out independent
# organizations. One example would be Singapore in the post-Yew phase or Putin’s Russia.
# 4: No. Civil society organizations are free to organize, associate, strike, express themselves,
# and to criticize the government without fear of government sanctions or harassment.


aut$control <- aut$v2cseeorgs_osp + aut$v2csreprss_osp


texreg::screenreg(m2a <- lm(4-v2cseeorgs_osp ~ eci + undp_hdi + logpop + gwf_regimetype4,
                           data = aut)
)
jtools::effect_plot(m2a, pred = eci, interval = TRUE)

texreg::screenreg(m2b <- lm(4-v2csreprss_osp ~ eci + undp_hdi + logpop + gwf_regimetype4,
                            data = aut)
)
jtools::effect_plot(m2b, pred = eci, interval = TRUE)


# INFLUENCE ---------------------------------------------------------------

# aut$v2cscnsult_osp
# Are major civil society organizations (CSOs) routinely consulted by policymakers on policies relevant to their members?
# 0: No. There is a high degree of insulation of the government from CSO input. The government
# may sometimes enlist or mobilize CSOs after policies are adopted to sell them to the public at large. But it does not often consult with them in formulating policies.
# 1: To some degree. CSOs are but one set of voices that policymakers sometimes take into
# account.
# 2: Yes. Important CSOs are recognized as stakeholders in important policy areas and given
# voice on such issues. This can be accomplished through formal corporatist arrangements or
# through less formal arrangements.


# aut$bti_IGs
# To what extent is there a network of cooperative associations or interest groups to mediate between society and the political system?
# 10  There is a broad range of interest groups that reflect competing social interests, tend to balance one another and are cooperative.
# 7 There is an average range of interest groups, which reflect most social interests. However, a few strong interests dominate, producing a latent risk of pooling conflicts.
# 4 There is a narrow range of interest groups, in which important social interests are underrepresented. Only a few players dominate, and there is a risk of polarization.
# 1 Interest groups are present only in isolated social segments, are on the whole poorly balanced and cooperate little. A large number of social interests remain unrepresented.



texreg::screenreg(m3a <- lm(v2cscnsult_osp ~ eci*access1 + control + undp_hdi + logpop,
                             data = aut)
)
jtools::effect_plot(m3a, pred = eci, interval = TRUE)

interactions::interact_plot(m3a, interval = TRUE)



texreg::screenreg(m3b <- lm(bti_IGs ~ eci + access1 +  undp_hdi + logpop,
                            data = aut)
)
jtools::effect_plot(m3b, pred = eci, interval = TRUE)





jtools::effect_plot(m_mediation, pred = control, interval = TRUE)





library(mediation)


texreg::screenreg(m_total <- lm(v2cscnsult_osp ~ eci + access1 + undp_hdi + logpop, data = aut))  # Step 1: Total Effect (Direct effect of eci on v2cscnsult_osp)
texreg::screenreg(m_mediator <- lm(control ~ eci + access1 + undp_hdi + logpop, data = aut)) # Step 2: Mediator Model (Does eci influence control?)
texreg::screenreg(m_mediation <- lm(v2cscnsult_osp ~ eci + control + access1 + undp_hdi + logpop, data = aut)) # Step 3: Mediation Model (Does control mediate the effect?)
summary(med_results <- mediate(m_mediator, m_mediation, treat = "eci", mediator = "control", boot = TRUE, sims = 1000)) # Run mediation analysis

# Summary of mediation results
summary(med_results)




