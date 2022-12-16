
flow <- c("No of posts in SwedeHF", nrow(rsdata))

# remove duplicated indexdates
rsdata <- rsdata %>%
  group_by(lopnr, shf_indexdtm) %>%
  arrange(shf_source) %>%
  slice(n()) %>%
  ungroup()
flow <- rbind(flow, c("Remove posts with duplicated index dates", nrow(rsdata)))

rsdata <- left_join(rsdata,
  ateranvpnr %>% mutate(AterPnr = 1),
  by = c("lopnr" = "LopNr")
)
rsdata <- rsdata %>%
  filter(is.na(AterPnr)) %>% # reused personr
  select(-AterPnr)
flow <- rbind(flow, c("Remove posts with reused PINs", nrow(rsdata)))

ejireg <- fall_ej_i_register %>%
  mutate(indexdtm = ymd(datum)) %>%
  select(lopnr, indexdtm) %>%
  group_by(lopnr, indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(notinreg = 1)
rsdata <- left_join(rsdata,
  ejireg,
  by = c("lopnr", "shf_indexdtm" = "indexdtm")
) %>%
  filter(is.na(notinreg)) %>% # not in scb register
  select(-notinreg)
flow <- rbind(flow, c("Remove posts that are not present in SCB register", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_age >= 18 & !is.na(shf_age))
flow <- rbind(flow, c("Remove posts < 18 years", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2020-11-01"))
flow <- rbind(flow, c("Indexdate >= 1 November 2020", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef %in% c("<30", "30-39")) %>%
  mutate(shf_ef = droplevels(shf_ef))
flow <- rbind(flow, c("EF < 40%", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14)
flow <- rbind(flow, c(">14 days follow-up (to avoid immortal time bias*)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()
flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

colnames(flow) <- c("Criteria", "N")