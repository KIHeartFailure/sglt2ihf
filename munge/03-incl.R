

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata401 %>%
  filter(casecontrol == "Case SwedeHF")
flow <- c("Posts in SHFDB4", nrow(rsdata))

#rsdata <- rsdata %>% 
#  filter(!(shf_source == "New SHF" & shf_type == "Follow-up"))
#flow <- c("Exclude follow-up visits for New SwedeHF", nrow(rsdata))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2020-11-01")) 
flow <- rbind(flow, c("Indexdate >= 1 November 2020", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef %in% c("<30", "30-39"))
flow <- rbind(flow, c("EF < 40%", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14) %>%
  mutate(shf_indexdtm14 = shf_indexdtm + 14, 
         sos_outtime_death = as.numeric(censdtm - shf_indexdtm14))

flow <- rbind(flow, c(">14 days follow-up (to avoid immortal time bias*)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

colnames(flow) <- c("Criteria", "N")
