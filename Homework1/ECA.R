# This dataset is from the California Department of Disabled Services, and we can
# observe Simpson's Paradox in the way that when we look at average expenditures
# for all ethnic groups combined, Hispanics have lower expenditures than White non-Hispanics.
# But when we group the data into age cohorts, Hispanics have higher expenditures than Whites
# for every cohort except for one (18-21 coded has cohort 4)

spExample <- function()
{
    df <- read.csv("./californiaDDSDataV2.csv", header = TRUE)
    df2 <- subset(df, Ethnicity == 'White not Hispanic' | Ethnicity == 'Hispanic')
    
    # assigning numeric values to Age Cohort
    df2$Age.Cohort[df2$Age.Cohort == "0 to 5"] <- 1
    df2$Age.Cohort[df2$Age.Cohort == "6 to 12"] <- 2
    df2$Age.Cohort[df2$Age.Cohort == "13 to 17"] <- 3
    df2$Age.Cohort[df2$Age.Cohort == "18 to 21"] <- 4
    df2$Age.Cohort[df2$Age.Cohort == "22 to 50"] <- 5
    df2$Age.Cohort[df2$Age.Cohort == "51+"] <- 6
    df2$Age.Cohort <- as.numeric(as.character(df2$Age.Cohort))
    
    # assigning numeric values to Ethnicity
    df2$Ethnicity[df2$Ethnicity == "White not Hispanic"] <- 1
    df2$Ethnicity[df2$Ethnicity == "Hispanic"] <- 2
    df2$Ethnicity <- as.numeric(as.character(df2$Ethnicity))
    
    # assigning numeric values to Gender
    df2$Gender[df2$Gender == "Male"] <- 1
    df2$Gender[df2$Gender == "Female"] <- 2
    df2$Gender <- as.numeric(as.character(df2$Gender))
    
    numericSimpson(df2, "Ethnicity", "Expenditures", "Age.Cohort", 6)
}