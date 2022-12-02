rsdata <- rsdata %>%
  mutate(scbyear = if_else(shf_indexyear > 2021, 2020, shf_indexyear - 1))

lisa <- lisa %>%
  mutate(
    scb_region = case_when(
      Lan == "01" ~ "Stockholm",
      Lan == "03" ~ "Uppsala",
      Lan == "04" ~ "Sodermanland",
      Lan == "05" ~ "Ostergotland",
      Lan == "06" ~ "Jonkoping",
      Lan == "07" ~ "Kronoberg",
      Lan == "08" ~ "Kalmar",
      Lan == "09" ~ "Gotland",
      Lan == "10" ~ "Blekinge",
      Lan == "12" ~ "Skane",
      Lan == "13" ~ "Halland",
      Lan == "14" ~ "Vastra Gotaland",
      Lan == "17" ~ "Varmland",
      Lan == "18" ~ "Orebro",
      Lan == "19" ~ "Vastmanland",
      Lan == "20" ~ "Dalarna",
      Lan == "21" ~ "Gavleborg",
      Lan == "22" ~ "Vasternorrland",
      Lan == "23" ~ "Jamtland",
      Lan == "24" ~ "Vasterbotten",
      Lan == "25" ~ "Norrbotten"
    ),
    scb_maritalstatus = case_when(
      Civil %in% c("A", "EP", "OG", "S", "SP") ~ "Single/widowed/divorced",
      Civil %in% c("G", "RP") ~ "Married"
    ),
    scb_famtype = case_when(
      FamTypF %in% c(11, 12, 13, 21, 22, 23, 31, 32, 41, 42) ~ "Cohabitating",
      FamTypF %in% c(50, 60) ~ "Living alone"
    ),
    Sun2000niva = coalesce(Sun2000niva_old, Sun2000niva_Old, Sun2020Niva_Old),
    scb_education = case_when(
      Sun2000niva %in% c(1, 2) ~ "Compulsory school",
      Sun2000niva %in% c(3, 4) ~ "Secondary school",
      Sun2000niva %in% c(5, 6, 7) ~ "University"
    ),
    scb_dispincome = coalesce(DispInk04, DispInk)
  ) %>%
  select(LopNr, year, starts_with("scb_"))

rsdata <- left_join(
  rsdata,
  lisa,
  by = c("lopnr" = "LopNr", "scbyear" = "year")
) %>%
  select(-scbyear)
