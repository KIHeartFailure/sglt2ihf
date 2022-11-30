
# Outcomes ----------------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm14,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphf",
  diakod = global_hficd,
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm14,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "counthosphf",
  diakod = global_hficd,
  noof = TRUE,
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm14,
  sosdate = INDATUM,
  diavar = HDIA,
  opvar = OP_all,
  type = "out",
  name = "hosprenal",
  diakod = " N1[7-9]| KAS00| KAS10| KAS20| Z491| Z492",
  opkod = " DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE
)