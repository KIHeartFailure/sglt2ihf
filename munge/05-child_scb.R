child <- bind_rows(barnadop, barnbio)

child2 <- inner_join(rsdata %>% select(lopnr, shf_indexdtm, shf_indexyear),
  child,
  by = c("lopnr" = "LopNr")
)

child2 <- child2 %>%
  filter(fodelsearbarn < shf_indexyear) %>%
  group_by(lopnr, shf_indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(scb_child = 1) %>%
  select(lopnr, shf_indexdtm, scb_child)

rsdata <- left_join(rsdata,
  child2,
  by = c("lopnr", "shf_indexdtm")
) %>%
  mutate(scb_child = factor(replace_na(scb_child, 0),
    levels = 0:1,
    labels = c("No", "Yes")
  ))
