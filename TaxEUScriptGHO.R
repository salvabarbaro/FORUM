## Source code related to the paper entitled: How to tax the European tobacco market?
setwd("~/Documents/Research/FORUM")
# First step: load required libraries
library(rgho)  # who api server 
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(scales)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(lubridate)
library(hutils)
library(eurostat)
library(ggmap)
library(maps)
library(mapproj)
library(countrycode)
library(rjson)
library(jsonlite)
library(rstatix)
library(tidyr)
###########################################################################################
### EU-Countries from eurostat package
data("eu_countries") 
eu_countries <- eu_countries %>% 
  mutate(name2 = ifelse(name == "Czechia", "Czech Republic", name)) %>% slice(., -28) %>%
  mutate(iso2c = ifelse(code != "EL", code, "GR")) %>% 
  mutate(iso3c = countrycode(iso2c,  origin = 'iso2c', destination = 'iso3c'))
# eu.map for plotting TCS scores (not yet included)
eu.map <- map_data('world')  %>% filter(region %in% eu_countries$name2) %>%
  mutate(iso3c = countrycode(region,  origin = 'country.name', destination = 'iso3c')) %>%
  mutate(iso2c = countrycode(region,  origin = 'country.name', destination = 'iso2c'))
reg.withoutEU <- c("AFR", "AMR", "EMR", "SEAR", "WPR")
###########################################################################################
#### FIG 1:
ETY <- read.csv(url("https://gitlab.rlp.net/sbarbaro/tobacco-taxation-eu/-/raw/main/ETY.csv?ref_type=heads&inline=false"))
ety.df <- melt(data = ETY, id.vars = c("Country.Name", "Country.Code", "Year"))
#
ety.pic01 <- ggplot(data = subset(ety.df, variable %in% c("ETY.RSPWAP.1000"))) +
  geom_hline(yintercept = mean(ETY$EU.WAVG.ETY), color = "forestgreen", linewidth = 2) +
  geom_col(aes(x = Country.Code, y = value), fill = "steelblue", color = "red") +
  theme_bw(base_size = 16) + 
  labs(title = "Excise Tax Yield on RSP WAP", 
       #       subtitle = "Data Source: European Commission, 2019",
       x = "Country", y = "EUR per 1,000 sticks")
#
ety.pic02 <- ggplot(data = subset(ety.df, variable %in% c("ETY.CHEAP.1000"))) +
  geom_hline(yintercept = mean(ETY$EU.MIN.ED), color = "deeppink4", linewidth = 2) +
  geom_col(aes(x = Country.Code, y = value), fill = "steelblue", color = "red") +
  theme_bw(base_size = 16) + 
  labs(title = "Excise Tax Yield on Cheapest Cigarette", 
       #       subtitle = "Data Source: European Commission, 2019",
       x = "Country", y = "EUR per 1,000 sticks")
ety.pic <- ggarrange(ety.pic01, ety.pic02, ncol = 1, nrow = 2)
rm(ety.pic02, ety.pic01)
ggsave("etypic.pdf", plot = ety.pic)
ety.pic
rm(ety.pic, ETY, ety.df)
############################################################################################
### Figure 2: Caption: "Prevalence estimates in the EU-27 and in other regions."
## get WHO metadata (adds world regions to countries)
#gho.meta <- fromJSON(url("https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues")) %>% 
#  as.data.frame %>% setNames(., c("URL", "iso3c", "COUNTRY", "v1", "v2", "REGION.CODE", "REGION")) %>%
#  select(., -c("v1", "v2"))
## get WHO data on tob-smoking prevalence: M_Est_smk_curr_std: Estimate of current tobacco smoking prevalence #
smk.df <- get_gho_data(code = "M_Est_smk_curr_std", filter = NULL) %>% 
  rename(iso3c = COUNTRY)  %>%
  mutate(Region1 = ifelse(!ParentLocation == "Europe", ParentLocation, NA ),
         Region2 = ifelse(iso3c %in% eu_countries$iso3c, "EU27", NA),
         Region = ifelse(is.na(Region1) == F, Region1, Region2),
         year = ymd(YEAR, truncated = 2L))
#
##################################################################################################
tobsmk.df <- smk.df %>% 
  filter(year <= "2020-01-01", year > "2004-01-01", SEX %in% c("SEX_FMLE", "SEX_MLE")) %>% 
  group_by(Region, SEX, year) %>% 
  reframe(value = mean(NumericValue, na.rm = T))
#
gender.list <- as.list(unique(tobsmk.df$SEX))
#
fig2.fun2 <- function(gender){
  ggplot(data = tobsmk.df %>% 
           filter(., SEX %in% gender, is.na(Region) == F),
         aes(x = year, y = value / 100, 
             group = Region,
             colour = Region)) +
    scale_y_log10(labels = scales::percent, 
                  limits = c(.01, .50)) +
    geom_line() + geom_point() +
    #    scale_color_brewer(palette = "Set1") +
    scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
    geom_text(data = tobsmk.df %>% 
                filter(SEX %in% gender, 
                       year == "2010-01-01",
                       is.na(Region) == F), 
              aes(label = Region), 
              hjust = -1.0, vjust = -0.0, 
              check_overlap = TRUE, 
              size = 6,
              show.legend = FALSE) + 
    theme_gray(base_size = 22) + 
    theme(legend.position = "right") +
    labs(title = "",
         subtitle = paste("gender =", gender ),
         x = "year", y = "", color = "Region") 
}
fig2.plots <- lapply(gender.list, fig2.fun2)
fig2 <- ggarrange(plotlist = fig2.plots, 
                  common.legend = T, 
                  legend = "bottom",
                  ncol = 2, nrow = 1)  
fig2
ggsave("~/Documents/Research/FORUM/Forum2024/SubmissionJune24/fig2NEW.pdf", 
       plot = fig2,
       width = 16,
       height = 10)


## European Females
eu.fem <- smk.df %>% filter(., Region == "EU27", SEX == "SEX_FMLE", YEAR < 2021) #, YEAR %in% 2010:2018
#
ggplot(data = eu.fem,
       aes(x = YEAR, y = NumericValue, 
           group = iso3c, colour = iso3c)) +
  geom_line() + geom_point() +
  theme_bw(base_size = 22) +
  labs(x = "Year", 
       y = "Smoking Prevalence in %",
 #      title = "Female smoking prevalence in European countries 2000 - 2018",
#       subtitle = "Data: World Health Organisation, GHO", 
       col = "Country")
ggsave("femaleprevalence.pdf")
##################################################################################
## As table
eu.fem.tb <- eu.fem %>% select(., c( "iso3c", "YEAR",  "NumericValue")) %>%
  pivot_wider(names_from = YEAR, values_from = NumericValue, names_sort = T) %>%
  arrange(iso3c) %>%
  mutate(Recent.Trend = ifelse( `2020` > `2010`, "uparrow", "downarrow"))

library(kableExtra)
eu.fem.tb %>%
  kbl(booktabs = TRUE, format = "latex", 
      caption = "Female smoking prevalence in European countries 2000 - 2020. Data: World Health Organisation, GHO", 
      digits = 1) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("~/Documents/Research/FORUM/Forum2024/SubmissionJune24/eu_fem_tb.tex")





# Use stargazer to export the table
stargazer(eu.fem.tb, 
          summary = FALSE, 
          rownames = FALSE, 
          type = "latex", 
          header = FALSE, 
          title = "Female smoking prevalence in European countries 2000 - 2020. Data: World Health Organisation, GHO", 
          table.placement = "t", 
          style = "default", 
          out = "eu_fem_tb.tex",
          digits = 1, 
          booktabs = TRUE)


#########################################################################################
# Fig. 3
#library(gesisdata)                        # Remote   #
#library(wdman)                            # Data Load#
#delete the LICENSE.chromedriver file                #
#selenium(retcommand=T)                               #
#gesis_download(file_id = "ZA7739", 
#               email = "",  # gesis access e-mail
#               password = "", # gesis access password
#               download_dir = "") # enter the download directory
# Alternative: download the dta file and read it with the help of the haven library.
######################################################
library(haven)  # loading the file non-remotedly

##########################################################################################
### Use the Eurobarometer dataset 
ZA7739 <- read_dta("~/Documents/IPE/TaxationEU/EB2020_93.2.dta")
#################################################################
ZA7739$min.w23 <- ZA7739$w23 * 10
eu28 <- weight2rows(ZA7739, "min.w23") # 
#####################################################################################
### weighting method with library(survey): less convenient, similar results
#eu28.survey <- svydesign(ids = ~1, data = ZA7739, weights = ZA7739$w23)
#svymean(~d11, eu28.survey, na.rm = T)
#svyquantile(~d11, eu28.survey, quantile=c(0.25,0.5,0.75), ci=F)
# Function to generate country-specific values
prev.ecig.fun <- function(c){
  cnty <- subset(eu28, isocntry %in% c)
  cnty.f <- subset(eu28, isocntry %in% c & d10 == 2)
  cnty.m <- subset(eu28, isocntry %in% c & d10 == 1)
  nb.smok <- length(cnty$qc1[cnty$qc1 == 1])/length(cnty$qc1)  # current cig smokers
  nb.quit <- length(cnty$qc1[cnty$qc1 == 2])/length(cnty$qc1)  # former  cig smokers
  nb.ecig <- length(cnty$qc3_1[cnty$qc3_1 == 1])/length(cnty$qc3_1) # current e-cig vaper
  nb.smok.f <- length(cnty.f$qc1[cnty.f$qc1 == 1])/length(cnty.f$qc1)  # current cig smokers (females)
  nb.quit.f <- length(cnty.f$qc1[cnty.f$qc1 == 2])/length(cnty.f$qc1)  # former  cig smokers (females)
  nb.ecig.f <- length(cnty.f$qc3_1[cnty.f$qc3_1 == 1])/length(cnty.f$qc3_1) # current e-cig vaper (females)
  nb.smok.m <- length(cnty.m$qc1[cnty.m$qc1 == 1])/length(cnty.m$qc1)  # current cig smokers (males)
  nb.quit.m <- length(cnty.m$qc1[cnty.m$qc1 == 2])/length(cnty.m$qc1)  # former  cig smokers (males)
  nb.ecig.m <- length(cnty.m$qc3_1[cnty.m$qc3_1 == 1])/length(cnty.m$qc3_1) # current e-cig vaper (males)
  c.tb <- list(nb.smok, nb.quit, nb.ecig,
               nb.smok.f, nb.quit.f, nb.ecig.f,
               nb.smok.m, nb.quit.m, nb.ecig.m)
  return(c.tb)
}
c.type = c("Central", "Central", "Eastern", "Mediterranean", "Eastern", "Northern", "Eastern",
           "Mediterranean", "Mediterranean", "Northern", "Central", "Eastern", "Eastern", "Northern",
           "Mediterranean", "Eastern", "Central", "Eastern", "Mediterranean", "Central", "Eastern", 
           "Mediterranean", "Eastern", "Northern", "Eastern", "Eastern", "Central", "Central", "Northern")
cou.list = unique(eu28$isocntry) 
prev.ecig <- lapply(cou.list, prev.ecig.fun)
`%ni%` <- Negate(`%in%`)  # define a negate-function (opposite of %in%)
outc <- lapply(prev.ecig[1:length(cou.list)], unlist) %>% 
  do.call(rbind.data.frame, .) %>% 
  setNames(c("curr.cig.smok", "cig.smok.quitter", "ecig.vaper",
             "curr.cig.smok.f", "cig.smok.quitter.f", "ecig.vaper.f",
             "curr.cig.smok.m", "cig.smok.quitter.m", "ecig.vaper.m")) %>%
  mutate(country = cou.list) %>% 
  mutate(country.type = c.type) %>% 
  select(country, everything()) %>% 
  subset(., country %ni% c("GB"))
rm(prev.ecig, cou.list)
## Fig 3: Combustible use and vaping behaviour in European countries, 2020.
gg04 <- ggplot(data = outc, aes(y = curr.cig.smok, x = ecig.vaper)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_brewer(palette = "Set1") +
  geom_smooth(method = "lm", se = T, formula = y ~ x) +
  geom_label_repel(aes(label = country, colour = country.type), 
                   alpha = 0.7, show.legend = FALSE, size = 7) +
  #  ylim(0.17, 0.38) +
  geom_point(aes(colour = country.type)) +
  theme_bw(base_size = 22) +
  labs(title = "Curr. smok. prevalence vs e-cig. vaper",
#       subtitle = "Data: Eurobarometer (gesis ZA7739, 2020)",
       x = "Current E-Cig. vaper", y ="Current cigarette smoker", color = "Region")
##
gg05 <- ggplot(data = outc, aes(y = cig.smok.quitter, x = ecig.vaper)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  geom_point(aes(colour = country.type)) +
  scale_colour_brewer(palette = "Set1") +
  geom_smooth(method = "lm", se = T, formula = y ~ x) +
  geom_label_repel(aes(label = country, colour = country.type), 
                   alpha = 0.7, show.legend = FALSE, size = 7) +
  theme_bw(base_size = 22) +
  labs(title = "Cig.-quitter vs e-cig. vaper",
#       subtitle = "Data: Eurobarometer (gesis ZA7739, 2020)",
       x = "Current E-Cig. vaper", y ="cigarette quitter", color = "Region")
##
## Sorted by gender

outc %>% cor_test(curr.cig.smok, ecig.vaper) # both genders
outc %>% cor_test(curr.cig.smok.f, ecig.vaper.f) # females
outc %>% cor_test(curr.cig.smok.m, ecig.vaper.m) # males
#
outc %>% cor_test(cig.smok.quitter, ecig.vaper) # both genders
outc %>% cor_test(cig.smok.quitter.f, ecig.vaper.f) # females
outc %>% cor_test(cig.smok.quitter.m, ecig.vaper.m) # males


fig3 <- ggarrange(plotlist = list (gg04, gg05), ncol = 2, common.legend = TRUE, legend = "bottom")
fig3
ggsave("~/Documents/Research/FORUM/Forum2024/SubmissionJune24/fig3.pdf", 
       plot = fig3, width = 16, height = 9) #, width = 16, height = 9
rm(gg04, gg05, fig3)
###################################################################################################
## Fig 4
## reasons for starting vaping / using heated tob (fig 4: boxplots)
reason.ecig.df <- melt(data = eu28, 
                       id.vars = c("isocntry", "d10"),
                       measure.vars = c("qc11a_1", "qc11a_2", "qc11a_3", "qc11a_4", "qc11a_5", "qc11a_6", "qc11a_7", "qc11a_8"),
                       na.rm = TRUE)
# remove NA, then count #1 / #(0, 1)
reas.list <- c("qc11a_1", "qc11a_2", "qc11a_3", "qc11a_4", "qc11a_5", "qc11a_6", "qc11a_7", "qc11a_8")
#reas.list <- c(eu28$qc11a_1 , eu28$qc11a_2)
#
reason.fun <- function(rv){
  rvar <- rv[is.na(rv) == FALSE] 
  r.share <- sum(rvar) / length(rvar)
  return(r.share)
}  
#
reas.res <- sapply(eu28[,361:368], reason.fun)
reas.res
#
# country-specific answers
reason.fun2 <- function(cou){
  reas.res.at <- sapply(subset(eu28, isocntry %in% c(cou))[,361:368], reason.fun)
}
reas.res2 <- lapply(cou.list, reason.fun2)
#
df2ctype <- c("Central", "Central", "Eastern", "Mediterranean", "Eastern", "Central", "Central", "Northern",
              "Eastern", "Mediterranean", "Northern", "Central", "Northern", "Mediterranean", "Eastern", "Eastern",
              "Northern", "Mediterranean", "Eastern", "Central", "Eastern", "Mediterranean", "Central", "Eastern",
              "Mediterranean", "Eastern", "Northern", "Eastern", "Eastern")
df2 <- lapply(reas.res2[1:length(cou.list)], unlist) %>% 
  do.call(rbind.data.frame, .) %>% 
  setNames(c("Reduce.tob.", "Cool.attractive", "Places",
             "Price", "Friends", "Flavour",
             "Less.harmful", "Others")) %>%
  mutate(country = cou.list) %>% 
  mutate(country.type = df2ctype) %>%
  select(country, everything()) %>% 
  melt(., id.vars = c("country", "country.type"))

#
gg.reason1 <- ggplot(data = df2, aes(y = value, x = variable)) +
  scale_y_continuous(labels = scales::percent) +
  geom_boxplot(fill = "lightgray", notch = FALSE, color = "orange", alpha = 0.7) +
  geom_jitter(aes(y = value, x = variable, colour = country.type), width = 0.2) +
  stat_summary(fun=mean, geom="point", shape=20, size=8, color="orange", fill="orange", alpha = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  #annotate(geom = "text", x = 5, y = .7, 
  #         label = "Which of the following factors, if any, were important in your decision to start using e-cigarettes?",
  #         color = "steelblue", size = 5) +
  theme_bw(base_size = 18) + theme(legend.position = "bottom") + 
  labs(x = "", y = "", color = "Region", subtitle = "Which of the following factors, if any, were important in your decision to start using e-cigarettes?")
### does the same hold true for 'heat not burn'?

reas.list <- c("qc11b_1", "qc11b_2", "qc11b_3", "qc11b_4", "qc11b_5", "qc11b_6", "qc11b_7", "qc11b_8")

# country-specific answers
reason.fun2 <- function(cou){
  reas.res.at <- sapply(subset(eu28, isocntry %in% c(cou))[,371:378], reason.fun)
}
reas.res2 <- lapply(cou.list, reason.fun2)

df2ctype <- c("Central", "Central", "Eastern", "Mediterranean", "Eastern", "Central", "Central", "Northern",
              "Eastern", "Mediterranean", "Northern", "Central", "Northern", "Mediterranean", "Eastern", "Eastern",
              "Northern", "Mediterranean", "Eastern", "Central", "Eastern", "Mediterranean", "Central", "Eastern",
              "Mediterranean", "Eastern", "Northern", "Eastern", "Eastern")
df3 <- lapply(reas.res2[1:length(cou.list)], unlist) %>% 
  do.call(rbind.data.frame, .) %>% 
  setNames(c("Reduce.tob.", "Cool.attractive", "Places",
             "Price", "Friends", "Flavour",
             "Less.harmful", "Others")) %>%
  mutate(country = cou.list) %>% 
  mutate(country.type = df2ctype) %>%
  select(country, everything()) %>% 
  melt(., id.vars = c("country", "country.type"))

#
gg.reason2 <- ggplot(data = df3, aes(y = value, x = variable)) +
  scale_y_continuous(labels = scales::percent) +
  geom_boxplot(fill = "lightgray", notch = FALSE, color = "orange", alpha = 0.7) +
  geom_jitter(aes(y = value, x = variable, colour = country.type), width = 0.2) +
  stat_summary(fun=mean, geom="point", shape=20, size=8, color="orange", fill="orange", alpha = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  #annotate(geom = "text", x = 5, y = .7, 
  #         label = "Which of the following factors, if any, were important in your decision to start using e-cigarettes?",
  #         color = "steelblue", size = 5) +
  theme_bw(base_size = 18) + theme(legend.position = "bottom") + 
  labs(x = "", y = "", color = "Region", 
       subtitle = "Which of the following factors, [...] using heated tobacco products?")
fig4 <- ggarrange(gg.reason1, gg.reason2, nrow = 2, common.legend = TRUE, legend = "bottom")
fig4
ggsave("fig4.pdf", plot = fig4)
rm(gg.reason1, gg.reason2, fig4)

#########################################################################################################
##########################################################################################################
