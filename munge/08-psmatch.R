

# Propensity scores -------------------------------------------------------

ps <- as_tibble(matrix(NA, nrow = nrow(rsdata), ncol = 11), .name_repair = "universal")

for (i in 1:10) {
  imrsdata_ps <- mice::complete(imprsdata, i)
  if (i == 1) ps[, 1] <- imrsdata_ps$lopnr
  pslog <- glm(formula(paste0(
    "sos_ddr_sglt2num ~ ",
    paste(modvars,
      collapse = " + "
    )
  )),
  data = imrsdata_ps,
  family = binomial
  )
  ps[, i + 1] <- pslog$fitted
}

rsdata <- left_join(rsdata,
  ps %>%
    mutate(ps = rowSums(.[2:11]) / 10) %>%
    select(...1, ps),
  by = c("lopnr" = "...1")
)

cal <- c(0.01 / sd(rsdata$ps))

set.seed(2334325)
match1 <- Match(
  Tr = rsdata$sos_ddr_sglt2num,
  X = rsdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)
set.seed(2334325)
match2 <- Match(
  Tr = rsdata$sos_ddr_sglt2num,
  X = rsdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)
set.seed(2334325)
match3 <- Match(
  Tr = rsdata$sos_ddr_sglt2num,
  X = rsdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 3
)
set.seed(2334325)
match4 <- Match(
  Tr = rsdata$sos_ddr_sglt2num,
  X = rsdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 4
)
set.seed(2334325)
match5 <- Match(
  Tr = rsdata$sos_ddr_sglt2num,
  X = rsdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 5
)
matchingn <- paste0(
  "1:1: N = ", match1$wnobs, ", ",
  "1:2: N = ", match2$wnobs, ", ",
  "1:3: N = ", match3$wnobs, ", ",
  "1:4: N = ", match4$wnobs, ", ",
  "1:5: N = ", match5$wnobs
)

rsdata$par <- rep(NA, nrow(rsdata))

rsdata$par[c(unique(match1$index.treated), match1$index.control)] <- c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))
matchrsdata <- rsdata[c(unique(match1$index.treated), match1$index.control), ]
