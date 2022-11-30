

# Define sglt2 crossover from DDR -------------------------------------------

matchlm <- left_join(matchrsdata,
  lmsel,
  by = "lopnr"
) %>%
  mutate(tmp_sglt2 = stringr::str_detect(
    ATC,
    global_sglt2atc
  )) %>%
  filter(
    EDATUM <= ymd("2021-12-31"),
    EDATUM >= shf_indexdtm14,
    tmp_sglt2
  )

# assume that if you are on you are on until LAST dispension + 3 mo OR
# (last follow-up - 5 mo) OR death - 5 mo
# independent of how long time between dispensions
# although sort of crude, is probably the option that will reflect reality
# the best for the majority of patients.

crossoverfunc <- function(time, event) {
  time <- sym(time)
  event <- sym(event)

  matchlm2 <- matchlm %>%
    filter(EDATUM <= shf_indexdtm14 + !!time) %>%
    group_by(lopnr) %>%
    arrange(EDATUM) %>%
    slice(c(1, n())) %>%
    mutate(firstlast = ifelse(row_number() == 1, "firstdtm", "lastdtm")) %>%
    ungroup()

  matchlm3 <- left_join(
    matchrsdata,
    matchlm2 %>% select(lopnr, firstlast, EDATUM),
    by = "lopnr"
  ) %>%
    mutate(
      enddtm = shf_indexdtm14 + !!time,
      crossoverdtm = case_when(
        sos_ddr_sglt2 == "Yes" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30,
        sos_ddr_sglt2 == "No" & firstlast == "firstdtm" ~ EDATUM,
        sos_ddr_sglt2 == "No" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30
      ),
      crossover_sos_ddr_sglt2 = case_when(
        sos_ddr_sglt2 == "Yes" ~ "No",
        sos_ddr_sglt2 == "No" & firstlast == "firstdtm" ~ "Yes",
        sos_ddr_sglt2 == "No" & firstlast == "lastdtm" ~ "No"
      ),
      crossover = 1
    ) %>%
    filter(!is.na(crossoverdtm)) %>%
    select(-sos_ddr_sglt2, -enddtm, -EDATUM, -firstlast) %>%
    rename(sos_ddr_sglt2 = crossover_sos_ddr_sglt2)

  matchcrossover <- bind_rows(
    matchrsdata,
    matchlm3
  ) %>%
    mutate(dtmuse = coalesce(crossoverdtm, shf_indexdtm14)) %>%
    group_by(lopnr) %>%
    arrange(dtmuse) %>%
    mutate(
      start = case_when(
        row_number() == 1 ~ 0,
        TRUE ~ as.numeric(dtmuse - shf_indexdtm14)
      ),
      stop = case_when(
        row_number() == n() ~ !!time,
        TRUE ~ lead(start)
      ),
      !!event := case_when(
        row_number() == n() ~ as.character(!!event),
        TRUE ~ "No"
      )
    ) %>%
    ungroup() %>%
    arrange(lopnr, dtmuse) %>%
    select(lopnr, shf_indexdtm14, sos_ddr_sglt2, start, stop, !!event, par)
  return(matchcrossover)
}

matchcross_deathcvhosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_deathcvhosphf"
)
matchcross_deathcv <- crossoverfunc(
  time = "sos_outtime_death",
  event = "sos_out_deathcv"
)
matchcross_hosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_hosphf"
)
matchcross_death <- crossoverfunc(
  time = "sos_outtime_death",
  event = "sos_out_death"
)
matchcross_hosprenal <- crossoverfunc(
  time = "sos_outtime_hosprenal",
  event = "sos_out_hosprenal"
)
