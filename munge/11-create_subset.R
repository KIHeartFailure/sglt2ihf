rsdata <- rsdata %>%
  mutate(subset = (shf_diabetestype != "Type I" | is.na(shf_diabetestype)) &
    sos_com_dialysis == "No" &
    (shf_gfrckdepi >= 20 | is.na(shf_gfrckdepi)))

flow <- rbind(flow, c("Supplementary - No type I diabetes, no eGFR < 20, no dialysis", nrow(rsdata %>% filter(subset))))