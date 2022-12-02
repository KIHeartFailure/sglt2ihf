

# Impute missing values ---------------------------------------------------

rsdata <- rsdata %>% select(-shf_bmiimp, -shf_bmiimp_cat)

noimpvars <- names(rsdata)[!names(rsdata) %in% modvars]

ini <- mice(rsdata, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change mthod used in impuation to prop odds model
meth <- ini$method
meth[c("scb_education")] <- "polr"
meth[noimpvars] <- ""

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imprsdata <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "rsdata"),
    .packages = "mice"
  ) %dopar% {
    mice(rsdata,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# check i all impvars have been fully imputed

datacheck <- mice::complete(imprsdata, 1)

for (i in seq_along(modvars)) {
  if (any(is.na(datacheck[, modvars[i]]))) stop("Missing for imp vars")
}


for (i in 1:26) {
  if (any(is.na(datacheck[, modvars[i]]))) stop("Missing for imp vars")
}