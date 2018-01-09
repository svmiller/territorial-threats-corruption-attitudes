# GB: Clean micro ----------
# This won't take long...

GB %<>%
  filter(country != 502) %>% # Get rid of Palestine
  mutate(gbsccode = country,
         corruptmip = car::recode(mip1, "-4=NA; -1=NA; 100:619=0; 620=1; 630:998=0; 999=1"),
         agegroups = car::recode(age2, "-4= NA; -1=NA; 997:999 = NA"),
         age = car::recode(age1, "-4=NA; -1=NA; 85:130 = 85; 997:999 = NA"),
         female = car::recode(gender, "1=0; 2=1; 9=NA; -1=NA; -4=NA"),
         collegeed = car::recode(edu, "-4=NA; -1=NA; 1:6=0; 7=1; 8=0; 9=NA"),
         personalecon = car::recode(eco_p1, "-4=NA; -1=NA; 1=-2; 2=-1; 3=0; 4=1; 5=2; 7:9=NA"),
         unemployed = car::recode(job, "-4=NA; -1=NA; 1:4=0; 5=1; 6:7=0; 8:9=NA; 10:80=0"),
         trust = car::recode(intru, "-4=NA; -1=NA; 1=0; 2=1; 7:9=NA")) %>%
  select(gbsccode:ncol(.))

# Curiously, GB data don't have the year recorded at hte micro-level. Odd.
# I have that in the macro-level data set though and it's easily discerned from the website.
# Let's go ahread and get rid of Palestine, though.

Terr_GB %>% filter(country != "Palestine") -> Terr_GB

# GB: Start with the macro stuff now. ----------
# Territorial threat indicators already done.

# GB: Economic Threat Index ----------
# Consists of: GDP per capita, GDP per capita loss, inflation, unemployment

Econ <- WDI(country = "all", indicator = c("NY.GDP.PCAP.KD", "SL.UEM.TOTL.ZS", "FP.CPI.TOTL.ZG"),
            start = 1997, end = 2007) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(gdppercap =  NY.GDP.PCAP.KD, unemp = SL.UEM.TOTL.ZS, cpi = FP.CPI.TOTL.ZG) %>%
  select(-iso2c, -country)

Econ %>%
  group_by(ccode) %>%
  mutate(l1_gdp = lag(gdppercap, 1),
         l2_gdp = lag(gdppercap, 2),
         l3_gdp = lag(gdppercap, 3),
         l4_gdp = lag(gdppercap, 4),
         l5_gdp = lag(gdppercap, 5),
         gdp_5yd = l5_gdp - l1_gdp,
         gdp_4yd = l4_gdp - l1_gdp,
         gdp_3yd = l3_gdp - l1_gdp,
         l1_cpi = lag(cpi, 1),
         l1_cpi = log(l1_cpi + abs(min(l1_cpi, na.rm=T)) + 1),
         l1_cpi = ifelse(ccode == 660 & year == 2006, 5.57, l1_cpi), # http://www.indexmundi.com/lebanon/inflation_rate_(consumer_prices).html
         l1_cpi = ifelse(ccode == 713 & year == 2005, 2.305, l1_cpi), # http://www.indexmundi.com/taiwan/inflation_rate_(consumer_prices).html
         l1_unemp = lag(unemp, 1),
         z_gdp = arm::rescale(l1_gdp)*-1,
         z_gdploss = arm::rescale(gdp_5yd),
         z_cpi = arm::rescale(l1_cpi),
         z_unemp = arm::rescale(l1_unemp),
         eti = (1/4)*(z_gdp + z_gdploss + z_cpi + z_unemp),
         year = year + 1) %>%
  select(ccode, year, eti) -> Econ


Mac_GB <- left_join(Terr_GB, Econ)


# GB: Societal threat index ----------

CNTS %>%
  filter(!is.na(ccode) & year >= 1945) %>%
  arrange(ccode, year) %>%
  select(ccode, year, domestic9) %>%
  mutate(year = year + 1) %>%
  rename(wci = domestic9) %>%
  mutate(wci = log(wci + 1)) %>%
  left_join(Mac_GB, .) -> Mac_GB

# GB: UDS democracy data ----------

QuickUDS::democracy %>%
  select(cown, year, uds_2014_mean) %>%
  rename(ccode = cown, uds = uds_2014_mean) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_GB, .) -> Mac_GB

# GB: Women in Parliament data ----------

WDI(country = "all", indicator="SG.GEN.PARL.ZS",
          start = 1995, end = 2007) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(wip = SG.GEN.PARL.ZS) %>%
  group_by(ccode) %>%
  select(ccode, year, wip) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  fill(wip) %>%
  ungroup() %>%
  left_join(Mac_GB, .) -> Mac_GB


# GB: Freedom of the Press ----------


left_join(Mac_GB, FP) -> Mac_GB

# GB: WRP data ----------

tibble(
  ccode = c(unique(Mac_GB$ccode)),
  styear = 2000,
  endyear= 2010
) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) -> GByears


WRP %>%
  rename(ccode = state) %>%
  select(ccode, year, chrstgenpct, judgenpct, islmgenpct, budgenpct, 
         zorogenpct, hindgenpct, sikhgenpct, shntgenpct, bahgenpct, taogenpct, jaingenpct, confgenpct,
         syncgenpct, anmgenpct, nonreligpct, othrgenpct) %>%
  rowwise() %>%
  mutate(relfrac = 1 - sum(chrstgenpct^2 +
                             judgenpct^2 +
                             islmgenpct^2 +
                             budgenpct^2 +
                             zorogenpct^2 +
                             hindgenpct^2 +
                             sikhgenpct^2 +
                             bahgenpct^2 +
                             taogenpct^2 +
                             jaingenpct^2 +
                             confgenpct^2 +
                             syncgenpct^2 +
                             anmgenpct^2 +
                             nonreligpct^2 +
                             othrgenpct^2)) %>%
  select(ccode, year, relfrac) %>%
  left_join(GByears, .) %>%
  group_by(ccode) %>%
  fill(relfrac) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_GB, .) -> Mac_GB
  

# GB: EPR data ----------

EPR %>%
  select(cowcode, year, ethfrac) %>%
  rename(ccode = cowcode) %>%
  mutate(ccode = as.integer((ccode))) %>%
  left_join(Mac_GB, .) -> Mac_GB

Mac_GB %>%
  mutate(ethfrac = ifelse(ccode == 830, 0.388, ethfrac),
         ethfrac = ifelse(ccode == 402, 0.4174, ethfrac)) -> Mac_GB

# GB: State Fragility Index ----------

SFI %>%
  select(country, year, sfi) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_GB, .) -> Mac_GB

# GB: Government final consumption expenditure data ----------

WDI(country = "all", indicator="NE.CON.GOVT.ZS",
    start = 2000, end = 2007) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(govexp = NE.CON.GOVT.ZS) %>%
  select(ccode, year, govexp) %>%
  arrange(ccode, year) %>%
  left_join(Mac_GB, .) -> Mac_GB



left_join(GB, Mac_GB) %>%
  select(country, gbsccode, year, everything()) %>%
  mutate(z_age = arm::rescale(age),
         z_class = arm::rescale(personalecon),
         tarterrmid = ifelse(tarterrmid5yc > 0, 1, 0),
         initterrmid = ifelse(initterrmid5yc > 0, 1, 0),
         tarnonterrmid = ifelse(tarnonterrmid5yc > 0, 1, 0),
         initnonterrmid = ifelse(initnonterrmid5yc > 0, 1, 0),
         z_eti = arm::rescale(eti),
         z_wci = arm::rescale(wci),
         z_uds = arm::rescale(uds),
         z_wip = arm::rescale(wip),
         z_fotp = arm::rescale(fotp),
         z_sfi = arm::rescale(sfi),
         z_ethfrac = arm::rescale(ethfrac),
         z_relfrac = arm::rescale(relfrac),
         z_govexp = arm::rescale(govexp)) -> Data_GB

write_csv(Data_GB, "analysis/data-gb.csv")
