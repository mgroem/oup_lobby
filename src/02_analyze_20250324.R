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
library(patchwork)  
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyr)


# FUNCTIONS ---------------------------------------------------------------
source("src/00_functions.R")

# LOAD DATA ---------------------------------------------------------------
load("data\\merge.RData")

# CREATE VARIABLES --------------------------------------------------------
# calculate access points as per Ehrlich, Standardize each variable 
# additive index of the following indicators, after standardization by the SD
# electoral districts   +    gol_dist   (0-550)       can't use it as it is only available for democracies
# party discipline     -      v2pscohesv_osp    (0-3)
# number of parties     +     gol_enep   (only for 38 countries)     can't use it as it is only available for democracies
# federalism         +          v2x_feduni
# presidentialism     -     br_pres

summary(merge$districts)  # 4007 NAs
summary(merge$enp.seats)  # 3388 NAs

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
summary(merge$access1)  # 5286 NAs
summary(merge$access2)  # 716  NAs



# SUBSET AND CHECK MISSING ------------------------------------------------------------------
aut <- merge[merge$v2x_regime2=="Autocracy",]

aut <- aut[aut$year>=1990,]

summary(aut$access1)      # 2147 NAs
summary(aut$access2)      # 295  NAs
summary(aut$undp_hdi)     # 520  NAs
summary(aut$bti_IGs)      # 2410 NAs
summary(aut$eci)          # 1630 NAs
summary(aut$genepy)       # 1271 NAs



missing_by_year <- aut %>%
  mutate(across(everything(), as.character)) %>%  # make all columns character
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  group_by(year, variable) %>%
  summarise(
    missing_count = sum(is.na(value) | value == ""),  # treat empty strings as missing
    total = n(),
    missing_prop = missing_count / total,
    .groups = "drop"
  )


missing_summary_year <- aut %>%
  group_by(year) %>%
  summarise(
    total_obs = n(),
    total_vars = ncol(cur_data()) - 1,  # exclude 'year'
    total_cells = total_obs * total_vars,
    total_missing = sum(is.na(across(everything()))),
    missing_prop = total_missing / total_cells,
    .groups = "drop"
  )


ggplot(missing_summary_year, aes(x = year, y = missing_prop)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "darkred") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Missing Values by Year",
    x = "Year",
    y = "Missing Data (%)"
  ) +
  theme_minimal()







# MAP ------------------------------------------------------------------

# Filter your dataset for year 2020 and autocracies
merge_2022_autocracies <- aut %>%
  filter(year == 2022, v2x_regime2 == "Autocracy")
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$continent != "Antarctica", ]
world_autocracies <- left_join(world, merge_2022_autocracies, by = c("iso_a3" = "ISO"))


# Plot the map
m1 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "grey90", size = 0.1) +    # Draw all countries in white with light grey borders
  geom_sf(data = world_autocracies %>% filter(!is.na(bti_IGs)),     # Overlay autocracies with greyscale fill
          aes(fill = bti_IGs), color = "grey50", size = 0.1) +
  scale_fill_gradient(name = "",
                      low = "gray90", high = "gray10", na.value = "white") +
  coord_sf(crs = "+proj=robin") +  # Apply the Robinson projection 
  theme_classic() +
  labs(title = "(a) Interest group institutionalization",caption = "Source: BTI") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA))

m2 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "grey90", size = 0.1) +    # Draw all countries in white with light grey borders
  geom_sf(data = world_autocracies %>% filter(!is.na(v2cscnsult_osp)),     # Overlay autocracies with greyscale fill
          aes(fill = v2cscnsult_osp), color = "grey50", size = 0.1) +
  scale_fill_gradient(name = "",
                      low = "gray90", high = "gray10", na.value = "white") +
  coord_sf(crs = "+proj=robin") +  # Apply the Robinson projection 
  theme_classic() +
  labs(title = "(b) CSO consultations", caption = "Source: VDem") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA))

combined_plot <- m1 + m2 + plot_layout(ncol = 1)



ggsave("results/Fig18.1.jpg", plot = combined_plot, width = 5, height = 8, dpi = 600)
ggsave("results/Fig18.1.pdf", plot = combined_plot, width = 5, height = 8)
ggsave("results/Fig18.1.eps", plot = combined_plot, width = 5, height = 8, device = "eps")


# mean scores
merge %>%
  filter(year == 2022) %>%
  summarise(
    mean_bti_IGs = mean(bti_IGs, na.rm = TRUE),
    sd_bti_IGs   = sd(bti_IGs, na.rm = TRUE),
    mean_v2cscnsult_osp = mean(v2cscnsult_osp, na.rm = TRUE),
    sd_v2cscnsult_osp   = sd(v2cscnsult_osp, na.rm = TRUE)
  )


# means by regime type
merge %>%
  filter(year == 2022) %>%
  group_by(v2x_regime2) %>%
  summarise(
    mean_bti_IGs = mean(bti_IGs, na.rm = TRUE),
    sd_bti_IGs   = sd(bti_IGs, na.rm = TRUE),
    mean_v2cscnsult_osp = mean(v2cscnsult_osp, na.rm = TRUE),
    sd_v2cscnsult_osp   = sd(v2cscnsult_osp, na.rm = TRUE),
    .groups = "drop"
  )


# t-test
ttest_bti <- merge %>%  # BTI IGs
  filter(year == 2022, !is.na(bti_IGs)) %>%
  t.test(bti_IGs ~ v2x_regime2, data = .)
ttest_consult <- merge %>%  # CSO consultation
  filter(year == 2022, !is.na(v2cscnsult_osp)) %>%
  t.test(v2cscnsult_osp ~ v2x_regime2, data = .)
print(ttest_bti)
print(ttest_consult)

# highest and lowest

merge %>%
  filter(year == 2022, v2x_regime2 == "Autocracy", !is.na(bti_IGs)) %>%
  arrange(desc(bti_IGs)) %>%
  summarise(
    highest = first(ISO),
    highest_value = first(bti_IGs),
    lowest = last(ISO),
    lowest_value = last(bti_IGs)
  )

merge %>%
  filter(year == 2022, v2x_regime2 == "Autocracy", !is.na(v2cscnsult_osp)) %>%
  arrange(desc(v2cscnsult_osp)) %>%
  summarise(
    highest = first(ISO),
    highest_value = first(v2cscnsult_osp),
    lowest = last(ISO),
    lowest_value = last(v2cscnsult_osp)
  )


# density plots

merge %>%
  filter(year == 2022, !is.na(bti_IGs)) %>%
  ggplot(aes(x = bti_IGs, fill = v2x_regime2)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Interest Group Institutionalization (BTI)",
    subtitle = "By Regime Type, 2022",
    x = "BTI Interest Group Score",
    y = "Density",
    fill = "Regime Type"
  ) +
  theme_minimal()



merge %>%
  filter(year == 2022, !is.na(v2cscnsult_osp)) %>%
  ggplot(aes(x = v2cscnsult_osp, fill = v2x_regime2)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of CSO Consultation (V-Dem)",
    subtitle = "By Regime Type, 2022",
    x = "CSO Consultation Score",
    y = "Density",
    fill = "Regime Type"
  ) +
  theme_minimal()





# ACCESS ------------------------------------------------------------------
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


merge$gwf_regimetype4[merge$v2x_regime2=="Democracy"] <- "Democracy"
merge$kailitz_regime[merge$v2x_regime2=="Democracy"] <- "Democracy"
merge$wth_regime[merge$v2x_regime2=="Democracy"] <- "Democracy"

vars <- c("access1", "gwf_regimetype4", "kailitz_regime", "wth_regime",
                 "undp_hdi", "logpop", "e_regionpol_7C")

# Drop rows with missing values in any of these variables
mod <- merge[complete.cases(merge[, vars]), ]

# Now run the models on the cleaned dataset
texreg::screenreg(m1a <- lm(access1 ~ gwf_regimetype4 + undp_hdi + logpop + e_regionpol_7C, data = mod))
texreg::screenreg(m1b <- lm(access1 ~ kailitz_regime + undp_hdi + logpop + e_regionpol_7C, data = mod))
texreg::screenreg(m1c <- lm(access1 ~ wth_regime + undp_hdi + logpop + e_regionpol_7C, data = mod))


# Extract effects and add model label
effect_gwf <- as.data.frame(effect("gwf_regimetype4", m1a)) %>%
  mutate(model = "GWF", regime = gwf_regimetype4)

effect_kailitz <- as.data.frame(effect("kailitz_regime", m1b)) %>%
  mutate(model = "Kailitz", regime = kailitz_regime)

effect_wth <- as.data.frame(effect("wth_regime", m1c)) %>%
  mutate(model = "WTH", regime = wth_regime)

# Combine into one data frame
effects_all <- bind_rows(effect_gwf, effect_kailitz, effect_wth)

# Get Democracy benchmark for each model
democracy_lines <- effects_all %>%
  filter(regime == "Democracy") %>%
  select(model, democracy_fit = fit)

# Plot
ggplot(effects_all, aes(x = regime, y = fit, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  facet_wrap(~ model, scales = "free_x") +
  geom_hline(
    data = democracy_lines,
    aes(yintercept = democracy_fit),
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "Effect of Regime Type on Predicted Access",
    x = "Regime Type",
    y = "Predicted Access to Policymaking"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )







ggsave("results/Fig18.2.jpg", plot = combined_plot, width = 5, height = 8, dpi = 600)
ggsave("results/Fig18.2.pdf", plot = combined_plot, width = 5, height = 8)
ggsave("results/Fig18.2.eps", plot = combined_plot, width = 5, height = 8, device = "eps")






















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


aut$control <- 8-(aut$v2cseeorgs_osp + aut$v2csreprss_osp)


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


texreg::screenreg(m3a1 <- lm(v2cscnsult_osp ~ eci + access1 + control + undp_hdi + logpop,data = aut))
texreg::screenreg(m3a2 <- lm(v2cscnsult_osp ~ eci*access1 + control + undp_hdi + logpop,data = aut))
texreg::screenreg(m3a3 <- lm(v2cscnsult_osp ~ eci*access1*control + undp_hdi + logpop,data = aut))


interactions::interact_plot(m3a2, pred = eci, modx = access1, modx.values=c(0.5, 2), 
                            interval = T, rug = T, rug.sides = "bl", colors = "Greys") +
  theme_classic()
interactions::interact_plot(m3a3, pred = eci, 
                            modx = access1, modx.values=c(0.5, 2), modx.labels = c("Low", "High"),
                            mod2 = control, mod2.values=c(2, 6),  mod2.labels = c("Low", "High"), 
                            interval = T, rug = T, rug.sides = "bl", colors = "Greys",
                            x.label = "Economic complecity", y.label = "Consultations") +
  theme_classic()



texreg::screenreg(m3b1 <- lm(bti_IGs ~ eci + access1 + control + undp_hdi + logpop,data = aut))
texreg::screenreg(m3b2 <- lm(bti_IGs ~ eci*access1 + control + undp_hdi + logpop,data = aut))
texreg::screenreg(m3b3 <- lm(bti_IGs ~ eci*access1*control + undp_hdi + logpop,data = aut))

interactions::interact_plot(m3b2, pred = eci, modx = access1, modx.values=c(0.5, 2), 
                            interval = T, rug = T, rug.sides = "bl")
interactions::interact_plot(m3b3, pred = eci, modx = access1, modx.values=c(0.5, 2), 
                            mod2 = control, mod2.values=c(2, 6), 
                            interval = T, rug = T, rug.sides = "bl")










library(mediation)


texreg::screenreg(m_total <- lm(v2cscnsult_osp ~ eci + access1 + undp_hdi + logpop, data = aut))  # Step 1: Total Effect (Direct effect of eci on v2cscnsult_osp)
texreg::screenreg(m_mediator <- lm(control ~ eci + access1 + undp_hdi + logpop, data = aut)) # Step 2: Mediator Model (Does eci influence control?)
texreg::screenreg(m_mediation <- lm(v2cscnsult_osp ~ eci + control + access1 + undp_hdi + logpop, data = aut)) # Step 3: Mediation Model (Does control mediate the effect?)
summary(med_results <- mediate(m_mediator, m_mediation, treat = "eci", mediator = "control", boot = TRUE, sims = 1000)) # Run mediation analysis

# Summary of mediation results
summary(med_results)




