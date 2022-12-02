
dors <- dors %>%
  group_by(LopNr) %>%
  arrange(ULORSAK) %>%
  slice(n()) %>% # select ULORSAk not ""
  ungroup()

dors <- dors %>%
  mutate(sos_deathdtm = ymd(case_when(
    substr(DODSDAT, 5, 8) == "0000" ~ paste0(substr(DODSDAT, 1, 4), "0701"),
    substr(DODSDAT, 7, 8) == "00" ~ paste0(substr(DODSDAT, 1, 6), "15"),
    TRUE ~ DODSDAT
  ))) %>%
  rename(
    sos_deathcause = ULORSAK,
    lopnr = LopNr
  ) %>%
  select(-DODSDAT) %>%
  filter(sos_deathdtm <= global_endfollowup)


# Migration ---------------------------------------------------------------

migration <- inner_join(rsdata %>%
                              select(lopnr, shf_indexdtm),
                        migration %>%
                          filter(Posttyp == "Utv"),
                        by = c("lopnr" = "LopNr")
) %>%
  mutate(tmp_migrationdtm = ymd(Datum)) %>%
  filter(
    tmp_migrationdtm > shf_indexdtm,
    tmp_migrationdtm <= global_endfollowup
  ) %>%
  group_by(lopnr, shf_indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, shf_indexdtm, tmp_migrationdtm)

rsdata <- left_join(rsdata,
                    migration,
                    by = c("lopnr", "shf_indexdtm")
)


# Death -------------------------------------------------------------------

rsdata <- left_join(rsdata,
                    dors %>% select(lopnr, sos_deathcause, sos_deathdtm),
                    by = "lopnr"
)

rsdata <- rsdata %>%
  mutate(
    censdtm = coalesce(
      pmin(sos_deathdtm, tmp_migrationdtm, na.rm = TRUE),
      global_endfollowup
    )
  ) %>%
  select(-shf_deathdtm)