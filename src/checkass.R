
ProjectTemplate::reload.project()

dataass <- mice::complete(imprsdata, 6)

# mult
ormod <- glm(formula(paste0("sos_ddr_sglt2 == 'Yes' ~ ", paste(modvars, collapse = " + "))),
                             family = binomial(link = "logit"), data = rsdata
)

# Outliers ---------------------------------------------------------------

plot(ormod, which = 4, id.n = 3)


# Multicollinearity -------------------------------------------------------

car::vif(ormod)
