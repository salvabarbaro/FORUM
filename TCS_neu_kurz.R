## Eurobarometer-Data to Feliu et. al. (2019)
library(haven)
library(dplyr)
library(hutils)
library(survey)
library(srvyr)
library(ggplot2)
library(ggpp)
library(countrycode)
library(ggpubr)
library(lubridate)
library(eurostat)
library(rjson)
library(jsonlite)
library(rgho)
library(ggrepel)

setwd("path")
EB2006 <- read_dta("ZA4527_v1-0-1EB2006.dta") %>%
  mutate(smoker = ifelse((v192 == 1 | v193 == 1 | v194 == 1), 1, 0) ) %>%
  mutate(quitter = ifelse(v194 == 1, 1, 0)) %>%
  mutate(v7 = ifelse(v7 %in% c("DE-E", "DE-W"), "DE", v7)) %>%
  mutate(v7 = ifelse(v7 %in% c("GB-NIR", "GB-GBN"), "GB", v7)) %>%
  mutate(v7 = ifelse(v7 %in% c("CY-TCC"), "CY", v7)) 
eb2006 <- as_survey_design(EB2006, weights = v40) %>% group_by(v7) %>% 
  reframe(., mn.smok2006 = survey_mean(smoker)) %>%
  rename(iso2c = v7)
rm(EB2006)
#
EB2014 <- read_dta("ZA5930_v2-0-0EB2014.dta") %>%
  mutate(smoker = ifelse(qc1 == 1, 1, 0) ) %>%
  mutate(former.smoker = ifelse(qc1 == 2,         1, 0)) %>%
  mutate(ever.smoker =   ifelse(qc1 %in% c(1, 2), 1, 0)) %>%
  mutate(isocntry = ifelse(isocntry %in% c("DE-E", "DE-W"), "DE", isocntry)) %>%
  mutate(isocntry = ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), "GB", isocntry))
eb2014 <- as_survey_design(EB2014, weights = w23) %>% 
  group_by(isocntry) %>% 
  summarize(., mn.smok2014 = survey_mean(smoker, na.rm = T)) %>%
  rename(iso2c = isocntry)
rm(EB2014)
eb.c <- left_join(x = eb2006, y = eb2014, by = "iso2c") %>% 
  select(., c("iso2c", "mn.smok2006", "mn.smok2014")) %>%
  mutate(diff2014 = mn.smok2014 - mn.smok2006) %>% 
  mutate(reldiff2014 = diff2014 / mn.smok2006)
#######################################################################################
## with EB-Data 2006 - 2020
EB2020 <- read_dta("EB2020_93.2.dta") %>%
  mutate(smoker = ifelse(qc1 == 1, 1, 0)) %>%
  mutate(isocntry = ifelse(isocntry %in% c("DE-E", "DE-W"), "DE", isocntry))
eb2020 <- as_survey_design(EB2020, weights = w23) %>% group_by(isocntry) %>% 
  summarise(., mn.smok2020 = survey_mean(smoker)) %>% 
  setNames(c("iso2c", "mn.smok2020", "se2020"))

eb.compare <- left_join(x = eb.c, y = eb2020, by = "iso2c") %>% 
  select(., -c("se2020")) %>%
  mutate(diff2020 = mn.smok2020 - mn.smok2006) %>% 
  mutate(reldiff2020 = diff2020 / mn.smok2006) 
tobaccocontrolscale07_19 <- read.csv(url("https://gitlab.rlp.net/sbarbaro/tobacco-taxation-eu/-/raw/main/tobaccocontrolscale07_19.csv?inline=false")) %>%
  mutate(iso2c = countrycode(sourcevar = Country, origin = 'country.name', destination = 'iso2c') )
plot.data <- left_join(x = eb.compare, y = tobaccocontrolscale07_19,
                       by = "iso2c") #%>% filter(., !iso2c == "GB")
############################################################################################
res14 <- cor.test(plot.data$reldiff2014, plot.data$tcs07, 
                method = "spearman", alternative = "two.sided") %>% 
  broom::tidy() %>% select(., c("estimate", "p.value")) %>%
  mutate_at(vars(estimate, p.value), ~round(., 3) )
#
res20 <- cor.test(plot.data$reldiff2020, plot.data$tcs19, 
                  method = "spearman", alternative = "two.sided") %>% 
  broom::tidy() %>% select(., c("estimate", "p.value")) %>%
  mutate_at(vars(estimate, p.value), ~round(., 3) )


g.eb0714 <- ggplot(data = plot.data,
                   aes(x = tcs07, y = reldiff2014)) +
  geom_point(size = 3, color = "purple") + 
  geom_smooth(method = "lm", se = T, col = "red") +
  geom_text_repel(aes(label = iso2c), size = 6) +
  theme_gray(base_size = 18) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, .35)) +
  annotate(geom = "table", x = 52, y = 0.28, label = res14, 
           vjust = 1, hjust = 0, size = 6)  +
  labs(y = "Rel. changes in smoking prevalances 2006-14",
       x = "TCS total scores in 2007",
       #title = "Smoking prevalence estimates",
       #subtitle = "Data Source: Eurobarometer"
       )

g.eb0720 <- ggplot(data = plot.data,
                   aes(x = tcs19, y = reldiff2020)) +
  geom_point(size = 3, color = "purple") + 
  geom_smooth(method = "lm", se = T, col = "red") +
  theme_gray(base_size = 18) +
  geom_text_repel(aes(label = iso2c), size = 6) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.7, .35)) +
  annotate(geom = "table", x = 52, y = 0.28, label = res20, 
           vjust = 1, hjust = 0, size = 6)  +
  labs(y = "Rel. changes in smoking prevalances 2006-20",
       x = "TCS total scores in 2019",
       #title = "Smoking prevalence estimates",
       #subtitle = "Data Source: Eurobarometer"
       )

all.eb.pics <- ggarrange(g.eb0714, g.eb0720, ncol = 2)
all.eb.pics
ggsave("TCSEffectiveness.pdf", plot = all.eb.pics)










