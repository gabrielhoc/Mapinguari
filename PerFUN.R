####################################################
############### Non linear functions ###############
####################################################

# Create non linear functions to be used on PerfFUN formula (Those are the functions from the Quinn, 2017 paper).
# These functions do not evaluate a value, they exist for model construction only, just like smooth terms functions on mgcv.
# I have to create SelfStart() methods for each function, but that is a mathematical problem, not a programming one.

LinearRate <- function(x, a = 1, b = 1){

  NonLinearFUN <- eval(bquote(function(a, b) (1 / (a + b*.(as.name(x))))))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b)

  return(list(NonLinearFUN,start_pars))
}

HeipPower <- function(x, a = 1, b = 1, k = 1){

  NonLinearFUN <- eval(bquote(function(a, b, k) a*.(as.name(x))^b + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

TautiExponential <- function(x, a = 0, b = 0 , k = 0){

  NonLinearFUN <- eval(bquote(function(a, b, k) a*exp(b*.(as.name(x))) + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

Quadratic <- function(x, a = 0, b = 0, k = 0){

  NonLinearFUN <- eval(bquote(function(a, b, k) a*.(as.name(x))^2 + b*.(as.name(x)) + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

Belehradek <- function(x, a = 0, b = 0, k = 0){

  Xmin <- get("Xmin")

  # Currently, this grabs Xmin from the environment inside PerfFUN, but that is not ideal, since it makes the function not self contained.
  # However, this value is dependent on the data, so I don't see how can it be done in another way.
  # Perhaps set Xmin as a argument and grabbing from the function environment as default.
  # This will be solved once a SelfStart() method is created for the functions.

  NonLinearFUN <- eval(bquote(function(a, b, k, Xmin) a*(.(as.name(x)) - Xmin)^b + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

ModifiedArrhenius <- function(x, a = 0, b = 0, k = 0){

  Xmin <- get("Xmin")

  NonLinearFUN <- eval(bquote(function(a, b, k, Xmin) 1/(a*exp(b*(1/(.(as.name(x)) - Xmin)))) + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

Briere2 <- function(x, a = 0, b = 0, k = 0) {

  Xmin <- get("Xmin")
  Xmax <- get("Xmax")

  NonLinearFUN <- eval(bquote(function(a, b, k, Xmin, Xmax) 1/(a*.(as.name(x))*(.(as.name(x)) - Xmin)*(Xmax - .(as.name(x)))^(1/b)) + k))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  start_pars <- c(a = a, b = b, k = k)

  return(list(NonLinearFUN,start_pars))
}

#######################################################
############### Curate formula function ###############
#######################################################

# Curate formula: this function breaks down a formula into nonlinear, linear and random components.
# This is useful in function PerfFUN for plotting and for building models.
# It uses a lot of flow control, which is not ideal, according to Hadley Wickham's book, Advanced R.
# Currently, it requires that random effects be between parenthesis (e.g. + (1|id))

curate_formula <- function(formula) {

  dependent_vars <- formula[[2]]
  independent_vars <- formula[[3]]

  nonlinear_vars <- list()
  random_vars <- list()
  linear_vars <- list()

  curate_vars <- function(vars) {

    if (vars[[1]] == '|') {
      stop("Please write random effects between parenthesis")
    }
    if (vars[[1]] == '+') {

      if (class(vars[[length(vars)]]) == 'call') {
        nonlinear_vars[[length(nonlinear_vars) + 1]] <<- vars[[length(vars)]]
        curate_vars(vars[[length(vars) - 1]])

      } else if (class(vars[[length(vars)]]) == '(') {
        random_vars[[length(random_vars) + 1]] <<- vars[[length(vars)]]
        curate_vars(vars[[length(vars) - 1]])

      } else if (class(vars[[length(vars)]]) == 'name') {
        linear_vars[[length(linear_vars) + 1]] <<- vars[[length(vars)]]
        curate_vars(vars[[length(vars) - 1]])

      }

    } else if (class(vars) == 'call') {
      nonlinear_vars[[length(nonlinear_vars) + 1]] <<- vars

    } else if (class(vars) == '(') {
      random_vars[[length(random_vars) + 1]] <<- vars

    } else if (class(vars) == 'name') {
      linear_vars[[length(linear_vars) + 1]] <<- vars

    }

  }

  curate_vars(independent_vars)
  return(list(dependent=dependent_vars,nonlinear = nonlinear_vars, random = random_vars, linear = linear_vars))

}

################################################
############### PerfFUN function ###############
################################################

# This is the actual function to do the Thermal Performance Curves.
# The two main features are the formula interface and the ability to fit any non linear model using nlmer, so the functions from the Quinn 2017 paper can be fitted.

PerfFUN <- function(formula,
                    data,
                    correlation="test" # You can input a correlation structure or let the function do all and compare

                    # GAMM arguments
                    # knots_vector=NULL
                    # positive_only=TRUE, #
                    # criteria='AIC'
) {

  # load required packages
  # Book R packages says not to include require() and library() inside the functions.
  # There is something called 'manespace assignment' to deal with that, but I still haven't read this part.

  require('mgcv')
  #require("ggplot2")
  #dependencies: grid
  #require("grid")
  require('lme4')
  require('plyr')

  # Breaking formula down

  formula_bits <- curate_formula(formula)

  response_var <- formula_bits$dependent

  # We need the intact formula terms for building formulas, as well as which are the predictors are on those components, for plotting.

  nonlinear_functions <- as.list(rep(NA,length(formula_bits$nonlinear)))
  nonlinear_preds <- as.list(rep(NA,length(formula_bits$nonlinear)))

  # Wickham's book recommends using 'apply' functions instead of for loops whenever possible.

  for (i in 1:length(formula_bits$nonlinear)) {

    nonlinear_functions[[i]] <- formula_bits$nonlinear[[i]][[1]]
    nonlinear_preds[[i]] <- formula_bits$nonlinear[[i]][[2]]
  }

  # reverse so they are in the same order as the formula

  nonlinear_functions <- rev(nonlinear_functions)
  nonlinear_preds <- rev(nonlinear_preds)

  random_groups <- as.list(rep(NA,length(formula_bits$random)))
  random_preds <- as.list(rep(NA,length(formula_bits$random)))
  random_terms <- as.list(rep(NA,length(formula_bits$random)))

  for (i in 1:length(formula_bits$random)) {

    random_groups[[i]] < -formula_bits$random[[i]][[2]][[3]]
    random_preds[[i]] <- formula_bits$random[[i]][[2]][[2]]
    random_terms[[i]] <- paste(random_preds[[i]],"|",random_groups[[i]],sep = "")
  }

  random_groups <- rev(random_groups)
  random_preds <- rev(random_preds)
  random_terms <- rev(random_terms)

  linear_preds <- formula_bits$linear
  linear_preds <- rev(linear_preds)

  # set strings for formula building and build formula for different cases

  linear_vars_string <- ifelse(length(linear_preds) >= 1,
                             paste("+", linear_preds, sep = "+"),
                             ""
  )

  random_vars_string <- paste(random_terms,sep = "+")

  method <- ifelse(nonlinear_functions[[1]] == 's' |
                   nonlinear_functions[[1]] == 'te' |
                   nonlinear_functions[[1]] == 'ti' |
                   nonlinear_functions[[1]] == 't2', 'gamm', 'nlmer')

  switch(method,

         gamm = {

           nonlinear_vars_string <- paste(formula_bits$nonlinear,collapse = "+")
           gamm_formula <- paste(response_var, "~", nonlinear_vars_string, linear_vars_string, sep = "")

         },

         nlme = {

           nonlinear_vars_string <- ""

           derivFUN_list <- alply(1:length(nonlinear_functions), 1, function(j){

             passing_list <- do.call(eval(nonlinear_functions[[j]]),list(as.character(nonlinear_preds[[j]])))
             passingFUN <- passing_list[[1]]
             start_pars <- passing_list[[2]]

             names_formals <- names(formals(passingFUN))
             pars_names <- setdiff(names_formals, as.character(nonlinear_preds[[j]]))

             nonlinear_vars_string <<- ifelse(nonlinear_vars_string == "",
                                            paste(nonlinear_vars_string, "derivFUN_list[[",j,"]](", paste(names_formals,collapse = ","),")", sep = ""),
                                            paste(nonlinear_vars_string, " + derivFUN_list[[",j,"]](", paste(names_formals,collapse = ","),")", sep = "")
             )

             return(deriv(body(eval(passingFUN)),
                          namevec = pars_names,
                          function.arg = passingFUN))

           })

           nlme_formula <- paste(response_var,  "~", nonlinear_vars_string, linear_vars_string,sep = "")

         })

  if (correlation == "test") {

    ############################## Correlation schemes by random variables #################################

    random_formula <- as.formula(paste("~",random_terms[[1]]))

    ## corAR1 : autoregressive process
    cor_1 <- corAR1(form = random_formula)  # no autocorrelation
    cor_2 <- corAR1(0.1,form = random_formula)  # autocorrelation set to 0.1

    ## corCAR1 : continuous autoregressive process
    cor_3 <- corCAR1(0.1,form = random_formula) # correlation between two observations one unit of time apart set to 0.1

    ## corGaus : Gaussian spatial correlation
    cor_5 <- corGaus(form=random_formula)
    cor_6 <- corGaus(form=random_formula, nugget = TRUE) # account for nugget effect

    ## corLin : representing a linear spatial correlation structure
    # cor_7 <- corLin(form=~1|id)
    # cor_8 <- corLin(form=~1|id, nugget = TRUE)
    # cor_7, cor_8, -- those do not behave well -- Then excluded

    ## corExp : exponential spatial correlation structure
    cor_9 <- corExp(form=random_formula)
    cor_10 <- corExp(form=random_formula, nugget = TRUE)

    ## corRatio : Rational quadratics spatial correlation.
    cor_11 <- corRatio(form=random_formula)
    cor_12 <- corRatio(form=random_formula, nugget = TRUE)

    ## corSpher : spherical spatial correlation.
    cor_13 <- corSpher(form=random_formula)
    cor_14 <- corSpher(form=random_formula, nugget = TRUE)

    ## corARMA : autoregressive moving average process
    cor_16 <- corARMA(form = random_formula, p=0, q=1)
    cor_17 <- corARMA(form = random_formula, p=1, q=0)
    cor_18 <- corARMA(form = random_formula, p=1, q=1)
    cor_19 <- corARMA(form = random_formula, p=1, q=2)
    cor_20 <- corARMA(form = random_formula, p=2, q=1)

    cor_str_id_list <- NULL
    cor_str_id_list <- list(cor_1, cor_2, cor_3, cor_5, cor_6, cor_9, cor_10, cor_11, cor_12, cor_13, cor_14, cor_16, cor_17, cor_18, cor_19, cor_20)

    cor_str_names <- NULL
    cor_str_names <- c( "corAR1(form=~1|id)",
                        "corAR1(0.1,form=~1|id)",
                        "corCAR1(0.1,form=~1|id)",
                        "corGaus(form=~1|id)",
                        "corGaus(form=~1|id, nugget = TRUE)",
                        "corExp(form=~1|id)",
                        "corExp(form=~1|id, nugget = TRUE)",
                        "corRatio(form=~1|id)",
                        "corRatio(form=~1|id, nugget = TRUE)",
                        "corSpher(form=~1|id)",
                        "corSpher(form=~1|id, nugget = TRUE)",
                        "corARMA(form = ~ 1 | id, p=0, q=1)",
                        "corARMA(form = ~ 1 | id, p=1, q=0)",
                        "corARMA(form = ~ 1 | id, p=1, q=1)",
                        "corARMA(form = ~ 1 | id, p=1, q=2)",
                        "corARMA(form = ~ 1 | id, p=2, q=1)")

  } else {

    cor_str_id_list <- list(correlation)

  }

  if (is.null(knots_vector)) {
    knots_vector <- unlist(unique(round(data[as.character(nonlinear_preds[[1]])])))
  } else {knots <- knots_vector}

  k <- length(knots_vector)

  ######################################### models by ID ###################################

  model_id_list <- alpply(1:length(cor_str_id_list),1,function(i){

    model_temp <- switch(method,

                       gamm = {try(gamm(as.formula(gamm_formula),
                                       correlation = cor_str_id_list[[i]],
                                       data = data,
                                       method = "REML",
                                       knots = list(x = knots_vector)), silent = TRUE)
                       },

                       nlme = {try(nlme(nlmer_formula,
                                       fixed = a + b ~ 1,
                                       data = data,
                                       random = a ~ 1 | id,
                                       correlation = cor_str_id_list[[i]],
                                       start = c(a = 0.382741, b = -0.006065)), silent = TRUE)
                       })

    if ('try-error' %in% class(model_temp)) { return('correlation structure failed')
      next
    } else {return(list(model_temp))}
  }
  )

  # names list based on correlation names

  names(models_id_list) <- cor_str_names

  # remove fail correlation structures

  oo <- models_id_list[models_id_list != 'correlation structure failed']

  models_id_list <- oo

  # save models $gam and $lme to list
  # save models $lme to list for anova comparisons
  # save models $gam and $lme to list

  models_id_list_master<-lapply(list(1:length(models_id_list)), function(i){
    switch(method,
           gamm=return(

             list(
               models_id_list_lme=list(models_id_list[[i]]$lme),
               models_id_list_nl=list(models_id_list[[i]]$gam),
               models_id_list_lme_logLik=list(gamm_models_id_list_lme[[i]]$logLik),
               models_id_list_lme_logLik_unlist=unlist(gamm_models_id_list_lme_logLik)
             )

           ),

           nlme=return(

             list(
               models_id_list_lme=list(models_id_list[[i]]$lme),
               models_id_list_nl=list(models_id_list[[i]]$gam),
               models_id_list_lme_logLik=list(gamm_models_id_list_lme[[i]]$logLik),
               models_id_list_lme_logLik_unlist=unlist(gamm_models_id_list_lme_logLik)
             )

           )
    )
  }
  )
  #################################### model comparison ###################################

  ## creates a dataframe with list of retained models

  updated_names_gamm_models_id_list <- as.data.frame(names(gamm_models_id_list))

  ## gets AIC and BIC of the retained models

  all_models_scores_df <- NULL # clean up previous runs

  for (i in 1:length(gamm_models_id_list_lme)){
    if (!exists("all_models_scores_df")){
      AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
      BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
      logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
      all_models_scores_df <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
    }

    # if the merged dataset does exist, append to it
    if (exists("all_models_scores_df")){
      AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
      BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
      logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
      temp_scores <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
      all_models_scores_df<-rbind(all_models_scores_df, temp_scores)
      rm(temp_scores)
    }
  }

  moto <- cbind(updated_names_gamm_models_id_list, all_models_scores_df) # binds rows

  #Compare models: either use AIC, BIC or Log-Likelihood ratio tests from table below to decide on your top model

  moto$delta_AIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$AIC_gamm_models))
  moto$delta_BIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$BIC_gamm_models))
  moto$knots <- k

  names(moto) <- c("gamm_correlation_structure_class", "AIC", "BIC", "logLik", "delta_AIC", "delta_BIC", "knots")

  ### select the best model for prediction

  best_gamm_model_AIC <- gamm_models_id_list[[which.min(moto[,5] )]] ## model_best_AIC <- which.min(moto[,5] )
  best_gamm_model_BIC <- gamm_models_id_list[[which.min(moto[,6] )]] ## model_best_BIC <- which.min(moto[,6] )

  ## summary for best models
  cat("\n\n")
  cat("best_gamm_model_AIC")
  cat("\n")
  print (summary(best_gamm_model_AIC$gam))
  cat("\n\n")
  print (best_gamm_model_AIC)
  cat("\n\n")
  cat("best_gamm_model_AIC -- adjusted R squared")
  cat("\n")
  print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
  cat("\n\n")
  cat("best_gamm_model_BIC")
  cat("\n")
  print (summary(best_gamm_model_BIC$gam))
  cat("\n\n")
  print (best_gamm_model_BIC)
  cat("\n\n")
  cat("best_gamm_model_BIC -- adjusted R squared")
  cat("\n")
  print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
  cat("\n\n")
  cat("Model comparison results")
  cat("\n")
  print(moto)
  cat("\n\n")

  # set default axis for plotting (will try the non linear predictors then the linear predictos)

  y_axis<-as.name(response_var)

  if(length(nonlinear_preds)>=1){ # if you have at least one nonlinear predictor, it is x axis
    x_axis<-as.name(nonlinear_preds[[1]])

    if(length(nonlinear_preds)>=2){ # if you have a second nonlinear predictor, it is z axis
      z_axis<-as.name(nonlinear_preds[[2]])

    } else if(length(linear_preds)>=1){ # if you don't have a second nonlinear predictor, first linear predictor is z axis
      z_axis<-as.name(linear_preds[[1]])

    } else { # if you don't have a second nonlinear predictor or any linear predictor, there is no z axis
      z_axis<-NULL
    }

  } else { # if you don't have nonlinear predictors, the first linear predictor is x axis
    x_axis<-as.name(linear_preds[[1]])

    if(length(linear_preds)>=2){  # if you don't have nonlinear predictors and have a second linear predictor, it is z axis
      z_axis<-as.name(linear_preds[[2]])

    } else { # if you don't have nonlinear predictors or a second linear predictor, there is no z axis
      z_axis<-NULL
    }

  }
}


#### plot points measured visual inspection (move this to the end)

plot_dispersion <- ggplot(data,aes(y= y_axis, x= x_axis, color = as.name(random_vars_names[1]) )) +
  geom_point(size = 6, alpha = 0.7) +
  theme_bw() +
  theme(legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,"cm"),
        legend.margin = unit(0,"cm"),
        panel.grid.major = element_line(colour = "gray"),
        panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  geom_rug(col="darkred",alpha=.7)

plot(plot_dispersion_species)

devAskNewPage(ask=T)
plot(best_gamm_model_AIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") #
title("Best GAMM model AIC")

if(size == TRUE) {

  devAskNewPage(ask=T)
  vis.gam(best_gamm_model_AIC$gam,
          view=c('size','temp'),
          type = "response",
          theta=55,
          phi=30,
          color="heat",
          ticktype="detailed",
          n.grid=50,
          ...) # color: gray; theta is the perspective angle of the graph
  title("Best GAMM model AIC")

}

devAskNewPage(ask=T)
plot(plot_dispersion_species)

devAskNewPage(ask=T)
plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") #
title("Best GAMM model BIC")

TPC_function <- function (temp_value, size_value, acctemp_value) {
  formula_gam <- best_gamm_model_AIC$gam
  pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value)
  P <- as.vector(predict.gam(formula_gam,pred_data))
  if(P > 0) {
    return(P)
  } else {return(0) }

  return(TPC_FUN)

}
