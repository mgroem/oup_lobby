# !diagnostics off


rm(list=ls())

options(scipen=999)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_271') # for 64-bit version
pacman::p_load(rJava)


options(java.parameters = "-Xmx4g")


pacman::p_load(lme4)
pacman::p_load("ggplot2")
pacman::p_load("glmmTMB")
pacman::p_load("stargazer")
pacman::p_load("dplyr")
pacman::p_load("httr")
pacman::p_load("ggeffects")
pacman::p_load("effects")
pacman::p_load("interplot")
pacman::p_load("sjPlot")
pacman::p_load("texreg")
pacman::p_load("bbmle")
pacman::p_load(ggrepel)
library(tidyverse)
library(plotly)
library(htmlwidgets)


# FUNCTIONS ---------------------------------------------------------------
source("src/00_functions.R")


# LOAD DATA ---------------------------------------------------------------
load("data\\merge.RData")





# CREATE VARIABLES --------------------------------------------------------

#

merge$v2elsrgel[is.na(merge$v2elsrgel)] <- 0
merge$v2ellocelc[is.na(merge$v2ellocelc)] <- 0


merge$infodemands <- merge$eci + merge$v2xel_frefair 
# + (merge$v2elsrgel)/5 + (merge$v2ellocelc)/5


merge$csos <- merge$v2cscnsult_osp/4 + merge$v2cseeorgs_osp/8

#merge$v2csprtcpt_osp + merge$v2csreprss_osp


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


merge$yr <- paste("'",substrRight(as.character(merge$year),2), sep = "")

merge$ISOyr <- paste(merge$ISO, merge$yr, sep="")






# DESCRIPTIVES ------------------------------------------------------------

#merge <- merge[merge$v2x_regime<2,]
#merge <- merge[merge$year>=2015,]
#merge <- merge[merge$e_regionpol_6C==6,]


demands <- merge[c("ISO", "year", "infodemands")]
demands <- demands[order(demands$infodemands),]

access <- merge[c("ISO", "year", "v2cscnsult_osp")]
access <- access[order(access$v2cscnsult_osp),]



merge$logpop <- log(merge$e_wb_pop)



merge <- merge %>%
  dplyr::group_by(ISO) %>%
  tidyr::fill(logpop) %>%
  dplyr::ungroup()  %>%
  as.data.frame()



merge <- 
  merge %>% group_by(ISO) %>% 
  mutate(csos_lag1 = dplyr::lag(csos, n = 1L, order_by = year))  %>% 
  as.data.frame()

merge <- 
  merge %>% group_by(ISO) %>% 
  mutate(csos_lag2 = dplyr::lag(csos, n = 2L, order_by = year))  %>% 
  as.data.frame()

merge <- 
  merge %>% group_by(ISO) %>% 
  mutate(csos_lag3 = dplyr::lag(csos, n = 3L, order_by = year))  %>% 
  as.data.frame()

merge <- 
  merge %>% group_by(ISO) %>% 
  mutate(csos_lag4 = dplyr::lag(csos, n = 4L, order_by = year))  %>% 
  as.data.frame()

merge <- 
  merge %>% group_by(ISO) %>% 
  mutate(csos_lag5 = dplyr::lag(csos, n = 5L, order_by = year))  %>% 
  as.data.frame()


mod <- merge[complete.cases(merge[c("csos",
                                    "csos_lag1",
                                    "wdi_oilrent",
                                    "v2xel_frefair",
                                    "v2elreggov",
                                    "v2x_neopat",
                                    "logpop",
                                    "ISO",
                                    "year", "xpers", "eci" , "bti_IGs")]),]
#mod <- mod[mod$v2x_regime2=="Autocracy",]




texreg::screenreg(m0 <- lmer(csos ~ 1 +
                               (1|ISO),
                             data = mod))
performance::icc(m0)  # 0.973 ( multilevel warranted)


texreg::screenreg(m1 <- lmer(csos ~ 
                             wdi_oilrent +
                                    v2xel_frefair +
                                   v2elreggov +
                             v2x_neopat +
                                   logpop + xpers + eci +
                             (1|ISO),
                              data = mod)
                                       )



mod$predicted <- predict(m1)   # Save the predicted values
mod$residuals <- residuals(m1) # Save the residual values


pred <- mod[c("ISO","predicted", "csos")]
pred <- 
  pred %>% group_by(ISO) %>% 
  summarise(predicted = mean(predicted, na.rm=T),
            csos = mean(csos, na.rm=T))  %>% 
  as.data.frame()
pred$ISO <- factor(pred$ISO, levels = pred$ISO[order(pred$predicted)])



res <- mod[c("ISO","residuals")]
res <- 
  res %>% group_by(ISO) %>% 
  summarise(residual = mean(residuals, na.rm=T))  %>% 
  as.data.frame()
res$ISO <- factor(res$ISO, levels = res$ISO[order(res$residual)])

write.csv(res, file = "data//cases.csv")


p1 <- ggplot(res, aes(x=ISO, y=residual,
                text = paste(ISO))) +
         geom_point() +
  geom_point(data=res[res$ISO=="SGP" | res$ISO=="PAK" | res$ISO=="BGD" |res$ISO=="MYS", ], aes(x=ISO, y=residual,
                                             text = paste(ISO)), colour="red", size=5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())



p2 <- ggplot(mod, aes(x = predicted, y = csos,
                      text = paste(
                        "ISO: ", ISO, "\n",
                        "year: ", year, "\n",
                        sep = ""
                      ))) +
  geom_point(shape = 16, size = 1, show.legend = T, color = "grey90") +
  geom_smooth(method = "lm", formula = y ~ x , linetype = "dashed", color = "black", alpha = 0.2) +
  geom_path(aes(group = ISO, color = ISO),
            show.legend = F)+
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian()

ggsave(filename = "results/Case selection.jpg", width = 5, height = 4, dpi = 600, units = "in",  device = "jpg")




ggplotly(p1)

