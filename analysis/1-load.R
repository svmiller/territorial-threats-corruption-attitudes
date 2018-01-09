LB <- read_csv("~/Dropbox/data/latinobarometro/LATESP02.csv")
Terr_LB <- read_csv("~/Dropbox/data/latinobarometro/terr-lb.csv")

GB <- read.csv("~/Dropbox/data/globalbarometer/gbs-round1-jun2009.csv") %>% tbl_df()
Terr_GB <- read.csv("~/Dropbox/data/globalbarometer/gbs-round1-macro.csv") %>% tbl_df()

WVS <- haven::read_dta("~/Dropbox/data/wvs/wvs1981_2008_v20090914.dta")
Terr_WVS <- read.csv("~/Dropbox/data/wvs-macro/wvs-macro-master.csv") %>% tbl_df() %>% select(ccode:tarterrmid5yc, econthreatindex) %>%
  rename(year = surveyyr,
         eti = econthreatindex) %>%
  mutate(wave = car::recode(wave, "2005=5; 2000=4; 1995=3")) 

CNTS <- read_csv("~/Dropbox/data/cnts/2012/cnts-2012.csv")

FP <- readxl::read_excel("~/Dropbox/data/freedom-house/FOTP1980-FOTP2017_Public-Data.xlsx", sheet=2) %>% 
  repair_names()


SFI <- readxl::read_xls("~/Dropbox/data/state-fragility/SFIv2012a.xls")
EPR <- haven::read_dta("~/Dropbox/data/ethnic-power-relations/EPR3CountryNewReduced.dta")
WRP <- read_csv("~/Dropbox/data/cow/religion/WRP_national.csv")

# Let's go ahead and clean FP here. Man, FP... who does this shit...
# https://gist.github.com/svmiller/9d053ee75f79e0bbf8d79d69a45c95fd
# Ugh, all right. Let's do this...


FP <- dplyr::slice(FP, -1:-3)

colnames(FP) <- sapply(FP[1,], as.character)

colnames(FP) <- str_c(colnames(FP), seq(1,length(colnames(FP))))

colnames(FP)[1] <- "country"
FP <- slice(FP, -1)

# I'm assuming you're interested in the "Total Score"...
FP %>%
  select(country,contains("Total")) -> FP

# I'm still not the most fluent in sapply within tidyverse framework. Comments here welcome.
FP[,2:ncol(FP)] <- sapply(FP[,2:ncol(FP)],
                          function(x)ifelse(x == "-",NA, as.numeric(x)))

# Total Scores start for the 1993 coverage year (i.e. the 1994 report).
colnames(FP)[2:dim(FP)[2]] <- seq(1993,1993+dim(FP)[2]-2)

FP %>%
  gather(year, fotp, `1993`:`2016`) %>%
  mutate(year = as.numeric(year),
         ccode = countrycode(country, "country.name", "cown"),
         fotp = 100 - fotp) %>%
  select(-country) -> FP