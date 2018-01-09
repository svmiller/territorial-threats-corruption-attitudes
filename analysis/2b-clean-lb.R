# LB: Clean micro ----------
# This won't take long...

LB %<>%
  mutate(country = idenpa,
         year = 2002,
         bribejd = car::recode(p52wvsb, "0=NA; 1=0; 2:10=1"),
         tolgovcord = car::recode(p23uf, "0=NA; 1:2=0; 3:4=1"),
         age = s2,
         female = s1 -1,
         collegeed = car::recode(s6, "1:14=0;15=1;16:17=0"),
         unemployed = car::recode(s8a, "1:3=0;4=1;5:7=0"),
         socialclass = car::recode(s20, "1=2;2=1;3=0;4=-1;5=-2"),
         trust = car::recode(p29st, "1=1;2=0;0=NA"))  %>%
  select(country:trust)

# LB: Territorial Threat Indicators ----------
# As is...


# LB: Economic Threat Index ----------
# Consists of: GDP per capita, GDP per capita loss, inflation, unemployment

Econ <- WDI(country = "all", indicator = c("NY.GDP.PCAP.KD", "SL.UEM.TOTL.ZS", "FP.CPI.TOTL.ZG"),
            start = 1990, end = 2002) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  filter(ccode >= 70 & ccode < 200) %>%
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
         l1_unemp = lag(unemp, 1),
         z_gdp = arm::rescale(l1_gdp)*-1,
         z_gdploss = arm::rescale(gdp_5yd),
         z_cpi = arm::rescale(l1_cpi),
         z_unemp = arm::rescale(l1_unemp),
         eti = (1/4)*(z_gdp + z_gdploss + z_cpi + z_unemp),
         year = year + 1) %>%
  select(ccode, year, eti) -> Econ

Mac_LB <- left_join(Terr_LB, Econ)

# LB: Societal threat index ----------

CNTS %>%
  filter(!is.na(ccode) & year >= 1945) %>%
  arrange(ccode, year) %>%
  select(ccode, year, domestic9) %>%
  mutate(year = year + 1) %>%
  rename(wci = domestic9) %>%
  mutate(wci = log(wci + 1)) %>%
  left_join(Mac_LB, .) -> Mac_LB

# LB: UDS democracy data ----------

QuickUDS::democracy %>%
  select(cown, year, uds_2014_mean) %>%
  rename(ccode = cown, uds = uds_2014_mean) %>%
  filter(ccode > 20 & ccode < 200) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_LB, .) -> Mac_LB

# LB: Women in Parliament data ----------

WP <- WDI(country = "all", indicator="SG.GEN.PARL.ZS",
          start = 1995, end = 2005) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  filter(ccode >= 70 & ccode < 200) %>%
  rename(wip = SG.GEN.PARL.ZS) %>%
  group_by(ccode) %>%
  select(ccode, year, wip) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  fill(wip) %>%
  ungroup()

left_join(Mac_LB, WP) -> Mac_LB


# LB: Freedom of the Press ----------


left_join(Mac_LB, FP) -> Mac_LB

# LB: WRP data ----------

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
                             shntgenpct^2 +
                             bahgenpct^2 +
                             taogenpct^2 +
                             jaingenpct^2 +
                             confgenpct^2 +
                             syncgenpct^2 +
                             anmgenpct^2 +
                             nonreligpct^2 +
                             othrgenpct^2)) %>%
  select(ccode, year, relfrac) %>%
  filter(year == 2000) %>%
  mutate(year = 2002) %>%
  left_join(Mac_LB, .) -> Mac_LB
  

# LB: EPR data ----------

EPR %>%
  select(cowcode, year, ethfrac) %>%
  rename(ccode = cowcode) %>%
  mutate(ccode = as.integer((ccode))) %>%
  left_join(Mac_LB, .) -> Mac_LB

# LB: State Fragility Index ----------

SFI %>%
  rename(statename = country) %>%
  select(statename, year, sfi) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_LB, .) -> Mac_LB

# LB: Government final consumption expenditure data ----------

WDI(country = "all", indicator="NE.CON.GOVT.ZS",
    start = 2000, end = 2007) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(govexp = NE.CON.GOVT.ZS) %>%
  select(ccode, year, govexp) %>%
  arrange(ccode, year) %>%
  left_join(Mac_LB, .) -> Mac_LB


left_join(LB, Mac_LB) %>%
  mutate(z_age = arm::rescale(age),
         z_class = arm::rescale(socialclass),
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
         z_govexp = arm::rescale(govexp)) -> Data_LB

write_csv(Data_LB, "analysis/data-lb.csv")


