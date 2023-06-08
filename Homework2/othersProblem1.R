setClass("relProx", slots = list(quantls = "list"))

predict.relProx <- function(relProxObj,newCases,proxyName,catName) {
  # cut again if necessary using levels from relProxObj
  if (is.factor(newCases[[catName]]) == FALSE) {
    print("need proper cut but will still work for law school data")
    #newCases[[catName]] <- cut(newCases[[catName]], attr(relProxObj, "catLevels"))
  }
  
  # for each new case
  percentiles <- vector()
  
  rows <- nrow(newCases)
  for (i in 1:rows) {
    # find the proper quantile using category
    catNewCase <- newCases[i,catName]

    # find proportion of quantiles that are <= newCases[i,proxyName]
    num <- sum(relProxObj@quantls[[catNewCase]] <= newCases[i,proxyName])
    den <- length(relProxObj@quantls[[catNewCase]])
    prop <- num/den

    # replace raw value with proportion
    #newCases[i,proxyName] <- prop
    percentiles <- append(percentiles, prop)
  }
  
  #return(newCases)
  return(percentiles)
}

relativeProxy <- function(data,proxyName,catName,nQuantls,nCut=NULL) {
  # for law school data:
  # make list of 5 elements
  # each has 5 SAT values, each representing 1 of the 5 quantiles
  
  # first ensure catName is a factor
  if (is.null(nCut) == FALSE) {
    # need cut function that saves break points
    data[[catName]] <- cut(data[[catName]], breaks = nCut)
  }
  
  # to be returned
  catList <- list()
  
  catLevels <- levels(as.factor(data[[catName]]))
  for (i in 1:length(catLevels)) {
    # this is the subset of rows within a certain category
    cat <- data[data[[catName]] == catLevels[i],]
    
    # find quantiles within that category
    catQuantl <- list(quantile(cat[[proxyName]], probs = seq(0, 1, 1/nQuantls)))
    
    catList <- append(catList, catQuantl)
    names(catList) <- c(names(catList)[1:(length(names(catList))-1)], catLevels[i])
  }
  
  # make new object and assign values
  newRelProx <- new("relProx")
  newRelProx@quantls <- catList
  attr(newRelProx, "catLevels") <- catLevels
  
  return(newRelProx)
}


empiricalStudy <- function()
{
  # requirements #
  library(qeML)
  # library(fairml)
  load("../datasets/law.school.admissions.rda")
  # load("./law.school.admissions.rda")
  
  # data management & setup #
  lsa <- law.school.admissions                            # LSAT used
  lsa <- lsa[, names(lsa) %in% c(
    "fam_inc", 
    "lsat", 
    "ugpa", 
    "cluster", 
    "bar"
  )]
  lsa$bar <- as.numeric(factor(lsa$bar))                  # T/F to numeric
  
  w <- relativeProxy(lsa, "lsat", "fam_inc", 5, NULL)     # divide by income
  u <- predict(w, lsa, "lsat", "fam_inc")                 # predict on training set
  
  # replace columns
  lsa_mod <- lsa
  lsa_mod$lsat <- u                                       # relative LSAT used
  lsa_mod$fam_inc <- NULL                                 
  
  lsa_none <- lsa_mod                                     # neither used
  lsa_none$lsat <- NULL                                    
  
  # modeling w/ qeKNN() #
  trad_model <- qeKNN(lsa, "bar")
  mod_model <- qeKNN(lsa_mod, "bar")
  none_model <- qeKNN(lsa_none, "bar")
  
  # assessment #
  # data setup for fairness
  family_inc <- lsa$fam_inc
  trad_pred_cases <- lsa[, !names(lsa) %in% c("bar")]
  mod_pred_cases <- lsa_mod[, !names(lsa_mod) %in% c("bar")]
  none_pred_cases <- lsa_none[, !names(lsa_none) %in% c("bar")]
  
  # kendall correlation between income and prediction bar is our Y
  fair_vec <- c(
    cor.test( family_inc, as.vector(predict(trad_model, trad_pred_cases)), method = "kendall" )$estimate,
    cor.test( family_inc, as.vector(predict(mod_model, mod_pred_cases)), method = "kendall" )$estimate,
    cor.test( family_inc, as.vector(predict(none_model, none_pred_cases)), method = "kendall" )$estimate
  )
  
  # test accuracy is for our utility
  util_vec <- c(
    trad_model$testAcc, 
    mod_model$testAcc,
    none_model$testAcc
  )
  
  study <- data.frame(
    model_names = c("Traditional w/ LSAT", "Relative LSAT w/ Income", "No LSAT or Income"),
    fairness = fair_vec,
    utility = util_vec
  )

  print(study)
}



empiricalStudy()






