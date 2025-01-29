#### Cleaning real data ####
rawsimdata$pid <- match(rawsimdata$pid, unique(rawsimdata$pid))

# Create data frame with only variables that we need
rawsimdata <- rawsimdata[, c("pid", "pingTotal", "depressedmood_state", "loneliness_state_pmc", "socintsatisfaction_state_pmc", "responsiveness_state_pmc", "selfdisclosure_state_pmc", "otherdisclosure_state_pmc")]

