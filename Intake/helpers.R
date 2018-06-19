intake <- readRDS("../data/intakeClean.rds")
names <- c("result", "Year", "Charge Category", "Age Group", "Gender", "Race/Ethnicity", "Police Department", "Number of Defendents")
names(intake) <- names
