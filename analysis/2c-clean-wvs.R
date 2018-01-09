# WVS: Clean micro ----------
# This won't take long...

WVS %<>%
  filter(s002 >= 3) %>%
  mutate(wvsccode = s003,
         wave = s002,
         year = s020,
         age = x003,
         female = x001 - 1,
         collegeed = car::recode(x025, "1:7=0; 8=1"),
         unemployed = car::recode(x028, "1:6=0; 7=1; 8=0"),
         satisf = c006 - 1,
         trust = car::recode(a165, "1=1; 2=0"),
         bribejd = car::recode(f117, "1=0; 2:10=1")) %>%
  select(wvsccode, wave, year, age, female, collegeed, unemployed, satisf, trust, bribejd)


# WVS: Territorial Threat Indicators ----------
# As is...


# WVS: Economic Threat Index ----------
# As is... took care of this a while ago.


# WVS: UDS democracy data ----------

QuickUDS::democracy %>%
  select(cown, year, uds_2014_mean) %>%
  rename(ccode = cown, uds = uds_2014_mean) %>%
  mutate(year = year + 1) %>%
  left_join(Terr_WVS, .) -> Mac_WVS

# WVS: Women in Parliament data ----------

WP <- WDI(country = "all", indicator="SG.GEN.PARL.ZS",
          start = 1990, end = 2009) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(wip = SG.GEN.PARL.ZS) %>%
  mutate(ccode = ifelse(iso2c == "RS", 345, ccode)) %>%
  group_by(ccode) %>%
  select(ccode, year, wip) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  fill(wip) %>%
  ungroup()

left_join(Mac_WVS, WP) -> Mac_WVS

Mac_WVS %>%
  mutate(wip = ifelse(ccode == 345 & year == 2001, 7.2, wip),
         wip = ifelse(ccode == 370 & year == 2000, 10.3, wip),
         wip = ifelse(ccode == 155 & year == 1996, 7.5, wip),
         wip = ifelse(ccode == 345 & year == 1996, 2.9, wip),
         wip = ifelse(ccode == 344 & year == 1996, 7.9, wip),
         wip = ifelse(ccode == 346 & year == 1998, 4.5, wip),
         wip = ifelse(ccode == 349 & year == 1995, 14.4, wip),
         wip = ifelse(ccode == 359 & year == 1996, 4.8, wip),
         wip = ifelse(ccode == 366 & year == 1996, 12.9, wip),
         wip = ifelse(ccode == 367 & year == 1996, 9, wip),
         wip = ifelse(ccode == 370 & year == 1996, 3.8, wip),
         wip = ifelse(ccode == 372 & year == 1996, 6.9, wip),
         wip = ifelse(ccode == 369 & year == 1996, 3.8, wip),
         wip = ifelse(ccode == 475 & year == 1995, 2.2, wip)) -> Mac_WVS

# WVS: Freedom of the Press ----------


left_join(Mac_WVS, FP) -> Mac_WVS

Mac_WVS %>%
  mutate(
    fotp = ifelse(ccode == 345 & year == 2006, 100-40, fotp),
    fotp = ifelse(ccode == 352 & year == 2006, 100-22, fotp),
    fotp = ifelse(ccode == 365 & year == 2006, 100-72, fotp),
    fotp = ifelse(ccode == 345 & year == 2001, 100-56, fotp),
    fotp = ifelse(ccode == 365 & year == 1999, 100-59, fotp),
    fotp = ifelse(ccode == 345 & year == 1996, 100-77, fotp),
    fotp = ifelse(ccode == 365 & year == 1995, 100-55, fotp)
  ) -> Mac_WVS

# WVS: WRP data ----------

tibble(
  ccode = c(unique(Mac_WVS$ccode)),
  styear = 1994,
  endyear= 2010
) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(ccode, year) %>%
  select(ccode, year) -> WVSyears


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
  left_join(WVSyears, .) %>%
  group_by(ccode) %>%
  fill(relfrac) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_WVS, .) -> Mac_WVS
  

# WVS: EPR data ----------

EPR %>%
  select(cowcode, year, ethfrac) %>%
  rename(ccode = cowcode) %>%
  mutate(ccode = as.integer((ccode))) %>%
  left_join(Mac_WVS, .) -> Mac_WVS

Mac_WVS %>%
  mutate(ethfrac = ifelse(ccode == 830, 0.388, ethfrac),
         ethfrac = ifelse(ccode == 232, .7139, ethfrac),
         ethfrac = ifelse(ccode == 352, 0.0939, ethfrac),
         ethfrac = ifelse(ccode == 212, 0.5302, ethfrac),
         ethfrac = ifelse(ccode == 338, 0.0414, ethfrac),
         ethfrac = ifelse(ccode == 390, 0.0819, ethfrac),
         ethfrac = ifelse(ccode == 395, .0798, ethfrac)) -> Mac_WVS


# WVS: State Fragility Index ----------

SFI %>%
  rename(stateabb = scode) %>%
  select(stateabb, year, sfi) %>%
  mutate(year = year + 1) %>%
  left_join(Mac_WVS, .) -> Mac_WVS

Mac_WVS %>%
  mutate(sfi = ifelse(is.na(sfi) & ccode == 2 & year == 2006, 2, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 220 & year == 2006, 1, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 345 & year == 2006, 8, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 360 & year == 2005, 7, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 530 & year == 2007, 20, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 816 & year == 2006, 8, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 345 & year == 2001, 12, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 360 & year == 1999, 9, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 155 & year == 2006, 2, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 816 & year == 2001, 9, sfi),
         sfi = ifelse(is.na(sfi) & ccode == 140 & year == 2006, 6, sfi),
         ) -> Mac_WVS

# WVS: Government final consumption expenditure data ----------

WDI(country = "all", indicator="NE.CON.GOVT.ZS",
    start = 1998, end = 2009) %>% tbl_df() %>%
  mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
  rename(govexp = NE.CON.GOVT.ZS) %>%
  mutate(ccode = ifelse(iso2c == "RS", 345, ccode)) %>%
  select(ccode, year, govexp) %>%
  arrange(ccode, year) %>%
  filter(!is.na(ccode)) %>%
  left_join(Mac_WVS, .) -> Mac_WVS

WVS %>%
  mutate(wvsccode = as.integer(wvsccode),
         wave = as.integer(wave),
         year = as.integer(year)) %>%
left_join(., Mac_WVS) %>%
  mutate(z_age = arm::rescale(age),
         z_class = arm::rescale(satisf),
         tarterrmid = ifelse(tarterrmid5yc > 0, 1, 0),
         initterrmid = ifelse(initterrmid5yc > 0, 1, 0),
         tarnonterrmid = ifelse(tarnonterrmid5yc > 0, 1, 0),
         initnonterrmid = ifelse(initnonterrmid5yc > 0, 1, 0),
         z_eti = arm::rescale(eti),
         z_uds = arm::rescale(uds),
         z_wip = arm::rescale(wip),
         z_fotp = arm::rescale(fotp),
         z_sfi = arm::rescale(sfi),
         z_ethfrac = arm::rescale(ethfrac),
         z_relfrac = arm::rescale(relfrac),
         z_govexp = arm::rescale(govexp)) -> Data_WVS

write_csv(Data_WVS, "analysis/data-wvs.csv")
