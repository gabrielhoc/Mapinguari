#############################################################################################################################
#PerfGAMM####################################################################################################################
#############################################################################################################################

# features of other versions: identify ctmin, ctmax and optimum

# Find out if lme4 does maximum likelihood estimation
# Finish the pipeline for one function

PerfFUN<- function (formula,
                    data,
                    method,
                    start_pars=NULL,
                    
                    # GAMM arguments
                    # knots=NULL,
                    # correlation=corAR1(form=~get(random_vars[1])),
                    # positive_only=TRUE,
                    # criteria='AIC'
                    
                    ... 
) {
  
  # load required packages
  
  require('mgcv')
  # dependencies: nlme
  require("ggplot2")
  #dependencies: grid
  # does grid loads when I loaf ggplot2?
  require("grid")
  
  # transform formula into string to identify formula components via pattern matching
  
  variables_vector<-unlist(strsplit(toString(formula6),"~",fixed=T))
  dependent_var<-variables_vector[1]
  independent_vars<-unlist(strsplit(variables_vector[2],"+",fixed=T))
  
  smooth_index<-grep("s\\(|te\\(|ti\\(|t2\\(", independent_vars)
  random_index<-grep("\\|", independent_vars)
  
  smooth_vars<-independent_vars[smooth_index]
  random_vars<-independent_vars[random_index]
  linear_vars<-independent_vars[-c(smooth_index,random_index)]
  
  smooth_vars_names<-eval(parse(text=smooth_vars))[1]
  random_vars_names<-all.vars(as.formula(paste("~",random_vars)))
  linear_vars_names<-all.vars(as.formula(paste("~",linear_vars)))
  
  # set default axis for plotting
  
  y_axis<-as.name(dependent_var[1])
  
  if(method=='GAMM'){
    x_axis<-as.name(smooth_vars_names[1])
    z_axis<-as.name(linear_vars_names[1])
  } else {
    x_axis<-as.name(linear_vars_names[1])
    z_axis<-as.name(linear_vars_names[2])
  }
  
  # list of functions
  
  LinearRate<-function(a,b,x,intercept) 1/(a + b*x) + intercept
  LinearSum<-function(a,b,x,intercept) a + b*x + intercept
  HeipPower<-function(a,b,x,intercept) a*(linear_vars[1])^b + intercept
  TautiExponential<-function(a,b,x,intercept) a*exp(b*linear_vars[1]) + intercept
  Quadratic<-function(a,b,x,intercept)a*x^2 + b*x + intercept
  Belehradek<-function(a,b,x,Xmin,intercept) a*(x-Xmin)^b + intercept
  ModifiedArrhenius<-function(a,b,x,Xmin,intercept) 1/(a*exp(b*(1/(x-Xmin)))) + intercept
  Briere2<-function(a,b,x,Xmin,Xmax,intercept) 1/(a*x*(x-Xmin)*(Xmax-x)^(1/b)) + intercept
  
  methodFUN<-as.name(method)
  names_formals<-names(formals(methodFUN))
  pars_names<-setdiff(names_formals, linear_vars_names)
  
  if(is.null(start_pars)){
    
    start_pars<-rep(0,length(pars_names)) # this is a placeholder, find a way to set starting values for parameters
    names(start_pars)<-pars_names
    
  }
  
  derivFUN<-deriv(body(methodFUN),
                  namevec = pars_names,
                  function.arg=methodFUN)
  
  TPC_FUN<-nlmer(mpg~ do.call(derivFUN,sapply(names_formals,as.name)) ~ (intercept|as.name(random_vars_names[1])),
                 start=start_pars,
                 data=data)
  
  return(TPC_FUN)
  
}


















if(method=='GAMM'){
  
  if (is.null(knots)) {
    knots <- unique(round(data$smooth_vars_names))
  } else {knots <- knots}
  
  # how to substitute this in formula?
  k <- length(uni_temp)
  
  if(correlation=="test"){
    
    ############################## Correlation schemes by random variables #################################
    
    ## corAR1 : autoregressive process
    cor_1 <- corAR1(form=~get(random_vars[1]))  # no autocorrelation
    cor_2 <- corAR1(0.1,form=~get(random_vars[1]))  # autocorrelation set to 0.1
    
    ## corCAR1 : continuous autoregressive process
    cor_3 <- corCAR1(0.1,form=~get(random_vars[1])) # correlation between two observations one unit of time apart set to 0.1
    
    ## corGaus : Gaussian spatial correlation
    cor_5 <- corGaus(form=~get(random_vars[1]))
    cor_6 <- corGaus(form=~get(random_vars[1]), nugget = TRUE) # account for nugget effect
    
    ## corLin : representing a linear spatial correlation structure
    # cor_7 <- corLin(form=~1|id)
    # cor_8 <- corLin(form=~1|id, nugget = TRUE)
    # cor_7, cor_8, -- those do not behave well -- Then excluded
    
    ## corExp : exponential spatial correlation structure
    cor_9 <- corExp(form=~get(random_vars[1]))
    cor_10 <- corExp(form=~get(random_vars[1]), nugget = TRUE)
    
    ## corRatio : Rational quadratics spatial correlation.
    cor_11 <- corRatio(form=~get(random_vars[1]))
    cor_12 <- corRatio(form=~get(random_vars[1]), nugget = TRUE)
    
    ## corSpher : spherical spatial correlation.
    cor_13 <- corSpher(form=~get(random_vars[1]))
    cor_14 <- corSpher(form=~get(random_vars[1]), nugget = TRUE)
    
    ## corARMA : autoregressive moving average process
    cor_16 <- corARMA(form = ~ get(random_vars[1]), p=0, q=1)
    cor_17 <- corARMA(form = ~ get(random_vars[1]), p=1, q=0)
    cor_18 <- corARMA(form = ~ get(random_vars[1]), p=1, q=1) 
    cor_19 <- corARMA(form = ~ get(random_vars[1]), p=1, q=2)
    cor_20 <- corARMA(form = ~ get(random_vars[1]), p=2, q=1)
    
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
    
    cor_str_id_list<-list(correlation)
    
  }
  
  ######################################### models by ID ###################################
  
  gamm_models_id_list <- list()
  
  for (i in 1:length(cor_str_id_list)) {
    gamm_temp <- try(gamm(formula,
                          correlation=cor_str_id_list[[i]],
                          data=data,
                          method="REML",
                          knots=list(x=knots)), silent=TRUE)
    if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i] <- 'correlation structure failed'
    next
    } else {gamm_models_id_list[i] <- list(gamm_temp)}
  }
  
  # names list based on correlation names
  
  names(gamm_models_id_list) <- cor_str_names
  
  # remove fail correlation structures
  
  oo <- gamm_models_id_list[gamm_models_id_list != 'correlation structure failed']
  
  gamm_models_id_list <- oo
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_lme <- list()
  
  for (i in 1:length(gamm_models_id_list)) {
    gamm_models_id_list_lme[i] <- list(gamm_models_id_list[[i]]$lme)
  } # save models $lme to list for anova comparisons
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_gam <- list()
  
  for (i in 1:length( gamm_models_id_list)) {
    gamm_models_id_list_gam[i] <- list(gamm_models_id_list[[i]]$gam)
  } # save models $gam to list for anova comparisons
  
  gamm_models_id_list_lme_logLik <- list()
  
  for (i in 1:length( gamm_models_id_list_lme)) {
    gamm_models_id_list_lme_logLik[i] <- list(gamm_models_id_list_lme[[i]]$logLik)
  }
  
  gamm_models_id_list_lme_logLik_unlist <- unlist(gamm_models_id_list_lme_logLik)
  
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
  
}

TPC_function <- function (temp_value, size_value, acctemp_value) {
  formula_gam <- best_gamm_model_AIC$gam
  pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value)
  P <- as.vector(predict.gam(formula_gam,pred_data))
  if(P>0) {
    return(P)
  } else { return(0) }
  
}

}

































#############################################################################################################################
#PerfGAMM2_2###################################################################################################################
#############################################################################################################################
PerfGAMM2 <- function (performance_data, 
                       dependent_variable,
                       smooth_predictors,
                       linear_predictors,
                       quadratic_predictors = NULL,
                       criterion = "BIC",
                       knots_vector = 7,
                       lm_approx_terms_value = 10,
                       fix_body_size_for_lm_value,
                       plot_variable_on_x,
                       plot_variable_on_y,
                       plot_variable_on_z,
                       output_directory) {
  
  # required libraries
  
  require('ggplot2')
  require('grid')
  require('mgcv')
  require('pROC')
  
  # input data 
  
  list_input_performance_ini <- performance_data
  dependent_variable_names <- dependent_variable
  smooth_variable_names <- smooth_predictors
  linear_predictors_names <- linear_predictors
  quadratic_predictors_names <- quadratic_predictors
  
  criterion <- criterion
  knots_vector <- knots_vector
  
  lm_approx_terms <- lm_approx_terms_value
  size_value_pseudo <- fix_body_size_for_lm_value
  
  plot_variable_on_x <- plot_variable_on_x
  plot_variable_on_y <- plot_variable_on_y
  plot_variable_on_z <- plot_variable_on_z
  
  out_dir <- output_directory
  
  master_directory <- getwd()
  
  # read file if not data frame
  
  if(class(list_input_performance_ini) == "data.frame") {
    list_input_performance_raw <- list_input_performance_ini
  } else {
    if(grepl(pattern = "*.csv$", list_input_performance_ini) == TRUE) {list_input_performance_raw <- read.table(file = list_input_performance_ini, header=TRUE, sep=",", stringsAsFactors = FALSE) }
    if(grepl(pattern = "*.txt$", list_input_performance_ini) == TRUE) {list_input_performance_raw <- read.table(file = list_input_performance_ini, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
  }
  
  # remove na performances
  
  list_input_performance_raw[complete.cases(list_input_performance_raw),]
  
  # data frame
  
  names_Perf_raw <- names(list_input_performance_raw)
  names_Perf_raw <- gsub("Performance", "performance", names_Perf_raw)
  names_Perf_raw <- gsub("Size", "size", names_Perf_raw)
  names_Perf_raw <- gsub("Temp", "temp", names_Perf_raw)
  names_Perf_raw <- gsub("temperature", "temp", names_Perf_raw)
  names_Perf_raw <- gsub("Temperature", "temp", names_Perf_raw)
  
  names(list_input_performance_raw) <- names_Perf_raw
  
  # get species names
  
  species_name <- unique(list_input_performance_raw$species)
  species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$genus_species), species_name)
  species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$Genus_species), species_name)
  species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$Genus_Species), species_name)
  
  # function of not into
  
  "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
  
  # subset_groups: 
  
  performance_reference_1 <- list_input_performance_raw[ , which(names(list_input_performance_raw) %in% c("id", dependent_variable_names,smooth_variable_names,
                                                                                                          linear_predictors_names,quadratic_predictors_names))]
  performance_reference <- performance_reference_1[complete.cases(performance_reference_1),]
  
  get_numbers_individuals <- unique(as.character(performance_reference$id))
  get_numbers_PER <- nrow(performance_reference)
  
  cat("\n--------------------------       Performance       --------------------------\n")
  cat("Species name: ", species_name, " \n")
  cat(paste("Total individuals included:", " ", length(get_numbers_individuals), "\n", sep=""))
  cat(paste("Total observations:", " ", get_numbers_PER, "\n", sep=""))
  cat("\n-------------------------------------------------------------------------------------------\n")
  
  
  ## explore two 2D
  
  #### plot points againts lat
  
  require(grid)
  plot_dispersion_species <- ggplot(performance_reference, aes_string(x=smooth_variable_names[1],y= dependent_variable_names[1], color = "species_name")) + 
    geom_point(size = 1, alpha = 0.7) + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(legend.text = element_text(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    geom_rug(col="darkred",alpha=.7)
  
  ##########################################################################################
  ############################## Correlation schemes by ID #################################
  
  ## corAR1 : autoregressive process
  cor_1 <- corAR1(form=~1|id)  # no autocorrelation
  cor_2 <- corAR1(0.1,form=~1|id)  # autocorrelation set to 0.1
  
  ## corCAR1 : continuous autoregressive process
  cor_3 <- corCAR1(0.1,form=~1|id) # correlation between two observations one unit of time apart set to 0.1
  
  ## corGaus : Gaussian spatial correlation
  cor_5 <- corGaus(form=~1|id)
  cor_6 <- corGaus(form=~1|id, nugget = TRUE) # account for nugget effect
  
  ## corLin : representing a linear spatial correlation structure
  # cor_7 <- corLin(form=~1|id)
  # cor_8 <- corLin(form=~1|id, nugget = TRUE)
  # cor_7, cor_8, -- those do not behave well -- Then excluded
  
  ## corExp : exponential spatial correlation structure
  cor_9 <- corExp(form=~1|id)
  cor_10 <- corExp(form=~1|id, nugget = TRUE)
  
  ## corRatio : Rational quadratics spatial correlation
  cor_11 <- corRatio(form=~1|id)
  cor_12 <- corRatio(form=~1|id, nugget = TRUE)
  
  ## corSpher : spherical spatial correlation
  cor_13 <- corSpher(form=~1|id)
  cor_14 <- corSpher(form=~1|id, nugget = TRUE)
  
  ## corARMA : autoregressive moving average process
  cor_16 <- corARMA(form = ~ 1 | id, p=0, q=1)
  cor_17 <- corARMA(form = ~ 1 | id, p=1, q=0)
  cor_18 <- corARMA(form = ~ 1 | id, p=1, q=1) 
  cor_19 <- corARMA(form = ~ 1 | id, p=1, q=2)
  cor_20 <- corARMA(form = ~ 1 | id, p=2, q=1)
  
  cor_str_id_list <- NULL
  cor_str_id_list <- list(cor_1, cor_2, cor_3, cor_5, cor_6, cor_9, cor_10, cor_11, cor_12, cor_13, cor_14, cor_16, cor_17, cor_18, cor_19, cor_20)
  
  cor_str_names <- NULL
  cor_str_names <- c( "corAR1(form=~1|id): autoregressive process", 
                      "corAR1(0.1,form=~1|id): autoregressive process", 
                      "corCAR1(0.1,form=~1|id): continuous autoregressive process", 
                      "corGaus(form=~1|id): Gaussian spatial correlation", 
                      "corGaus(form=~1|id, nugget = TRUE): Gaussian spatial correlation", 
                      "corExp(form=~1|id): exponential spatial correlation structure", 
                      "corExp(form=~1|id, nugget = TRUE): exponential spatial correlation structure", 
                      "corRatio(form=~1|id): Rational quadratics spatial correlation", 
                      "corRatio(form=~1|id, nugget = TRUE): Rational quadratics spatial correlation",
                      "corSpher(form=~1|id): spherical spatial correlation", 
                      "corSpher(form=~1|id, nugget = TRUE): spherical spatial correlation", 
                      "corARMA(form = ~ 1 | id, p=0, q=1): autoregressive moving average process",
                      "corARMA(form = ~ 1 | id, p=1, q=0): autoregressive moving average process", 
                      "corARMA(form = ~ 1 | id, p=1, q=1): autoregressive moving average process", 
                      "corARMA(form = ~ 1 | id, p=1, q=2): autoregressive moving average process",
                      "corARMA(form = ~ 1 | id, p=2, q=1): autoregressive moving average process")
  
  ##########################################################################################
  ##########    subseting to dependent, predicto, covariate, id    #################
  ##########################################################################################
  
  performance_input <- as.data.frame(performance_reference)
  performance_input$order <- row.names(performance_input)
  
  ##########################################################################################
  ##########################################################################################
  
  # use knots
  
  uni_temp <- seq(1:knots_vector)
  k_final <- knots_vector # k is number of knots
  
  ##########################################################################################
  ##############         The formula construction       ####################
  
  # linear
  
  if(!is.null(linear_predictors_names)) {
    if(length(linear_predictors_names) == 1) {
      collapsed_linear <- paste(linear_predictors_names, sep="")
    } 
    if(length(linear_predictors_names) > 1) {
      collapsed_linear <- paste(linear_predictors_names, sep="", collapse=" + ")
    }
  }
  
  if(is.null(linear_predictors_names)) {collapsed_linear <- NULL}
  
  # quadratic
  
  if(!is.null(quadratic_predictors_names)) {
    if(length(quadratic_predictors_names) == 1) {
      collapsed_quadratic <- paste("(", quadratic_predictors_names, ")^2", sep="")
    } 
    if(length(quadratic_predictors_names) > 1) {
      collapsed_quadratic_1 <- paste("(", quadratic_predictors_names, sep="", collapse=")^2 + ")
      collapsed_quadratic <- paste0(collapsed_quadratic_1, ")^2", sep="")
    }
  }
  
  if(is.null(quadratic_predictors_names)) {collapsed_quadratic <- NULL}
  
  ################ constructing the user formula
  
  ## for non null collapsed_linear
  
  if(!is.null(collapsed_linear)) {
    if(!is.null(collapsed_quadratic)) {
      user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                    collapsed_linear, ' + ', collapsed_quadratic) 
    }
    if(is.null(collapsed_quadratic)) {
      user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                    collapsed_linear)
    }
  }
  
  ## for null collapsed_linear
  
  if(is.null(collapsed_linear)) {
    if(!is.null(collapsed_quadratic)) {
      user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                    collapsed_quadratic) 
    }
    if(is.null(collapsed_quadratic)) {
      user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs")')
    }
  }
  
  cat("\n--------------------------       Performance formula       --------------------------\n")
  cat("processing formula: ", user_string_formula, "\n")
  cat("number of k_final knots: ", k_final, "\n")
  cat("\n---------------------------------------------------------------------------------------------------\n")
  
  ##########################################################################################
  ##############  Loop to correation structures with k_final knots  ####################
  
  gamm_models_id_list <- list()
  gamm_models_id_failed <- list()
  counter_fail <- 0
  gamm_models_id_success <- list()
  counter_success <- 0
  
  
  for (i in 1:length(cor_str_id_list)) {
    
    gamm_temp <- try(gamm(as.formula(user_string_formula),
                          correlation=cor_str_id_list[[i]],
                          data=performance_input,
                          method="REML",
                          knots=list(x=uni_temp)), silent=TRUE)
    
    cat(paste0("processeing correltaion srt: ", class(cor_str_id_list[[i]])[1], "\n"))
    
    if ('try-error' %in% class(gamm_temp)) { counter_fail <- counter_fail + 1
    gamm_models_id_failed[[counter_fail]] <- cor_str_names[i]
    next
    
    } else {counter_success <- counter_success + 1
    gamm_models_id_list[counter_success] <- list(gamm_temp)
    gamm_models_id_success[[counter_success]]<- cor_str_names[i]
    }
    
  }
  
  
  # models list to vector
  
  gamm_models_id_failed_vector <- as.vector(do.call(rbind,gamm_models_id_failed))
  gamm_models_id_success_vector <- as.vector(do.call(rbind,gamm_models_id_success))
  
  # stop if no succesul performances
  
  if(is.null(gamm_models_id_success_vector) == TRUE) { print(plot_dispersion_species)
    stop(print("No correlation structure was successful"))
  } else {
    cat("\nthe number of successful correlation structures is : ", length(gamm_models_id_success_vector), "...\n")
  }
  
  # names list based on correlation names
  
  names(gamm_models_id_list) <- gamm_models_id_success_vector
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_lme <- list()
  
  for (i in 1:length( gamm_models_id_list)) {
    gamm_models_id_list_lme[i] <- list(gamm_models_id_list[[i]]$lme)
  } # save models $lme to list for anova comparisons
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_gam <- list()
  
  for (i in 1:length( gamm_models_id_list)) {
    gamm_models_id_list_gam[i] <- list(gamm_models_id_list[[i]]$gam)
  } # save models $gam to list for anova comparisons
  
  gamm_models_id_list_lme_logLik <- list()
  
  for (i in 1:length( gamm_models_id_list_lme)) {
    gamm_models_id_list_lme_logLik[i] <- list(gamm_models_id_list_lme[[i]]$logLik)
  }
  
  gamm_models_id_list_lme_logLik_unlist <- unlist(gamm_models_id_list_lme_logLik)
  
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
  moto$knots <- k_final
  
  names(moto) <- c("gamm_correlation_structure_class", "AIC", "BIC", "logLik", "delta_AIC", "delta_BIC", "knots")
  
  # reorder moto based on Delta BIC
  
  moto <- moto[ order(moto[,6]), ]
  
  ### select the best model for prediction
  
  best_gamm_model_AIC <- gamm_models_id_list[[which.min(moto[,5] )]] ## model_best_AIC <- which.min(moto[,5] ) 
  best_gamm_model_BIC <- gamm_models_id_list[[which.min(moto[,6] )]] ## model_best_BIC <- which.min(moto[,6] )
  
  ## which one to return
  
  ifelse (criterion == 'BIC', best_model <- best_gamm_model_BIC, best_model <- best_gamm_model_AIC)
  
  # calculate CTmax, CTmin and Topt
  
  temp_test_no_order <- as.data.frame(c(seq(-10,55, by = 0.01), -10, 55)) 
  temp_test <- as.data.frame(temp_test_no_order[order(temp_test_no_order[,1]), ])
  names(temp_test) <-'temp'
  
  # get means of selected predictors
  
  performance_input_select <- subset(performance_input, select = names(performance_input) %!in% c("id","temp", "performance", "order"))
  
  predictors_df_list <- list()
  
  for(i in 1:ncol(performance_input_select)) {
    if(is.numeric(performance_input_select[1,i])) {
      one_column_mean <- as.data.frame(t(colMeans(performance_input_select[i])), stringsAsFactors = F)
    } else {
      one_column_mean <- as.data.frame(t(performance_input_select[1,i]), stringsAsFactors = F)
      names(one_column_mean) <- names(performance_input_select[i])
    }
    predictors_df_list[[i]] <- one_column_mean
  }
  
  predictors_df <- do.call(cbind, predictors_df_list)
  data_test <- merge(temp_test, predictors_df, all.x = TRUE)
  predicted_performance <- as.vector(predict.gam(best_model$gam,data_test))
  data_test$predicted_performance <- predicted_performance
  
  # Toptimum
  
  Toptimum_mean_predictors <- data_test[data_test$predicted_performance == max(data_test$predicted_performance) , ]
  
  # CTmin 
  
  one_species_max_to_correct_0 <- data_test[which.max(data_test$predicted_performance),]
  one_species_pre_opt_to_correct_0 <- subset(data_test, data_test$temp < one_species_max_to_correct_0$temp)
  
  one_species_pre_opt_ID_to_correct_0_min <- one_species_pre_opt_to_correct_0[which.min(one_species_pre_opt_to_correct_0$predicted_performance),]
  one_species_pre_opt_ID_to_correct_0_neg <- one_species_pre_opt_to_correct_0[one_species_pre_opt_to_correct_0$predicted_performance < 0,]
  
  if(nrow(one_species_pre_opt_ID_to_correct_0_neg) < 1) {
    row_to_report_CTmin <- -10
  } else {
    row_to_report_CTmin <- ifelse(one_species_pre_opt_ID_to_correct_0_min$temp < max(one_species_pre_opt_ID_to_correct_0_neg$temp),
                                  max(one_species_pre_opt_ID_to_correct_0_neg$temp), one_species_pre_opt_ID_to_correct_0_min$temp)
  }
  
  # CTmax
  
  one_species_post_opt_to_correct_0 <- subset(data_test, data_test$temp > one_species_max_to_correct_0$temp)
  one_species_post_opt_ID_to_correct_0 <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]
  
  one_species_post_opt_ID_to_correct_0_min <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]
  one_species_post_opt_ID_to_correct_0_neg <- one_species_post_opt_to_correct_0[one_species_post_opt_to_correct_0$predicted_performance < 0,]
  
  if(nrow(one_species_post_opt_ID_to_correct_0_neg) < 1) {
    row_to_report_CTmax <- 55
  } else {
    row_to_report_CTmax <- ifelse(one_species_post_opt_ID_to_correct_0_min$temp > min(one_species_post_opt_ID_to_correct_0_neg$temp),
                                  min(one_species_post_opt_ID_to_correct_0_neg$temp), one_species_post_opt_ID_to_correct_0_min$temp)
  }
  
  #### report these
  
  ## print to screen
  
  cat("------------   TPC GAMM based on given k_final   ------------\n\n")
  print(species_name)
  cat("\n") 
  cat("------------   Model comparison results   ------------")
  cat("\n\n")
  print(moto)
  cat("\n") 
  cat("------------   best_gamm_model_BIC -- adjusted R squared")
  cat("\n\n")
  print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
  cat("\n")
  cat("------------   best_gamm_model_BIC   ------------")
  cat("\n")
  print (summary(best_gamm_model_BIC$gam))
  cat("\n\n")
  cat("------------   best_gamm_model_AIC -- adjusted R squared")
  cat("\n\n")
  print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
  cat("\n")
  cat("------------   best_gamm_model_AIC   ------------")
  cat("\n")
  print (summary(best_gamm_model_AIC$gam))
  cat("\n\n")
  cat("------------   Toptimum   ------------")
  cat("\n")
  print (Toptimum_mean_predictors)
  cat("\n\n")
  cat("------------   CTmax   ------------")
  cat("\n")
  print (row_to_report_CTmax)
  cat("\n\n")
  cat("------------   CTmin   ------------")
  cat("\n")
  print (row_to_report_CTmin)
  cat("\n\n")
  cat("----------------------------------------\n")
  
  plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray", main = paste("Best GAMM model BIC -- ", species_name))
  
  ## write summary for best models # dir.create
  
  if(is.null(out_dir) == FALSE) {
    dir.create(out_dir, showWarnings = F)
    setwd(out_dir)
    
    writeLines(capture.output( {
      cat("------------   TPC GAMM on given k_final   ------------\n\n")
      print(species_name)
      cat("\n") 
      cat("------------   Model comparison results   ------------")
      cat("\n")
      print(moto)
      cat("\n") 
      cat("------------   best_gamm_model_BIC -- adjusted R squared")
      cat("\n")
      print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
      cat("\n")
      cat("------------   best_gamm_model_BIC   ------------")
      cat("\n")
      print (summary(best_gamm_model_BIC$gam))
      cat("\n\n")
      print (best_gamm_model_BIC)
      cat("\n")
      cat("------------   best_gamm_model_AIC -- adjusted R squared")
      cat("\n")
      print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
      cat("\n")
      cat("------------   best_gamm_model_AIC   ------------")
      cat("\n")
      print (summary(best_gamm_model_AIC$gam))
      cat("\n\n")
      print (best_gamm_model_AIC)
      cat("\n")
      cat("------------   Toptimum   ------------")
      cat("\n")
      print (Toptimum_mean_predictors)
      cat("\n")
      cat("------------   CTmax   ------------")
      cat("\n")
      print (row_to_report_CTmax)
      cat("\n")
      cat("------------   CTmin   ------------")
      cat("\n")
      print (row_to_report_CTmin)
      cat("\n")
      cat("----------------------------------------\n")
      
    }
    ),con=paste(species_name, '_performance_calc_k_AUTO_raw_gamm_fit.txt', sep=""))
    
    
    pdf(paste0(species_name,'_performance_best_fit_3D_gam.pdf')) # only plot objects can be made pdfs
    plot(plot_dispersion_species)
    plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
    title(paste("Best GAMM model BIC -- ", species_name))
    vis.gam(best_gamm_model_BIC$gam,
            view = c(plot_variable_on_x, plot_variable_on_z),
            type = "response",
            theta=50,
            phi=30,
            color="topo",
            xlab= plot_variable_on_x,
            ylab= plot_variable_on_z,
            zlab= plot_variable_on_y,
            ticktype="detailed",
            n.grid=60) # color: gray; theta is the perspective angle of the graph
    title(paste("Best GAMM model BIC -- ", species_name))
    dev.off()
    
    
    # set back to master directory
    
    setwd(master_directory)
    
  }
  
  
  # if return functions
  
  TPCFUN_GAM <- function (temp_value, size_value) {
    formula_gam <- best_model$gam
    pred_data_raw <- data.frame(temp=temp_value, size = size_value)
    predictors_df_subset <- subset(predictors_df, select = c(-size))
    pred_data <- merge(pred_data_raw, predictors_df_subset, all.x = TRUE)
    P <- as.vector(predict.gam(formula_gam,pred_data))
    if(P>0) {
      return(P)
    } else { return(0) }
  }
  
  ########################## pseduo gamm fit function ###################################
  
  n_terms_in_lm_function <- 5
  
  # internal values
  
  lower_temp_value = 0
  upper_temp_value = 55
  interval_segments_size_value = 0.01
  TCP_gamm_function_name = best_model$gam
  svl_value_mean = size_value_pseudo
  n_terms_in_lm_function = lm_approx_terms
  
  # user input
  
  lower_temp <- lower_temp_value
  upper_temp <- upper_temp_value
  interval_segments_size <- interval_segments_size_value
  TCP_gamm_function <- TCP_gamm_function_name
  svl_value_input <- svl_value_mean
  n_terms_in_lm <- n_terms_in_lm_function
  
  # simulation to gam predicted points
  
  simulation_TPC <- as.data.frame(c(seq(lower_temp,upper_temp, by = interval_segments_size))) # Species specific range
  names(simulation_TPC) <-'Temp'
  
  for (i in 1:length(simulation_TPC[,1])) {
    simulation_TPC$TPC [i] <- TPCFUN_GAM(simulation_TPC[i,1],svl_value_input)
  }
  
  lm_model <- lm(TPC ~ poly(Temp, n_terms_in_lm, raw=TRUE), data=simulation_TPC) # 9 terms
  
  TPCFUN_PSEUDO_GAM <- function (temp_value, size_value) {
    dummy_var <- size_value # this is necessary to keep the code consistent
    formula_lm <- lm_model
    pred_data <- data.frame(Temp=temp_value)
    P <- as.vector(predict(formula_lm, newdata=pred_data))
    
    if(temp_value >= row_to_report_CTmax) { return(0) }
    if(temp_value <= row_to_report_CTmin) { return(0) }
    if((temp_value > row_to_report_CTmin) && (temp_value < row_to_report_CTmax)) { 
      if(P>0) {return(P)
      } else {return(0) } }
    
  }
  
  # compare and plot gam and pseudo gam
  
  plot_data_gam_simulations <- simulation_TPC
  list_pseudo_results<- lapply(plot_data_gam_simulations$Temp, TPCFUN_PSEUDO_GAM, size_value=1)
  plot_data_gam_simulations$pseudoTPC <- unlist(list_pseudo_results)
  
  # compare using nlme
  
  roc1_gam <- roc(plot_data_gam_simulations$TPC, plot_data_gam_simulations$Temp)
  roc2_pseudogam <- roc(plot_data_gam_simulations$pseudoTPC, plot_data_gam_simulations$Temp)
  roc_test_gam_pseudo <- roc.test(roc1_gam, roc2_pseudogam, method="bootstrap", boot.n = 300, progress = "none")
  
  gam_AUC <- roc_test_gam_pseudo$estimate[1]
  pseudogam_AUC <- roc_test_gam_pseudo$estimate[2]
  roc_test_p_value <- roc_test_gam_pseudo$p.value
  roc_test_D <- roc_test_gam_pseudo$statistic
  
  text_results_roc <- paste0("TPCFUN_GAM-AUC: ", round(gam_AUC,4), " TPCFUN_PSEUDO_GAM-AUC: ", round(pseudogam_AUC,4), " D-statistic:", round(roc_test_D,4), " p_value: ", round(roc_test_p_value,4))
  
  cat("\n-------------Results of the roc.test (pROC package) using 300 boostrap replications-------------\n")
  print(roc_test_gam_pseudo)
  
  # plot
  plot(plot_data_gam_simulations$Temp, plot_data_gam_simulations$TPC,type="l",col="red")
  lines(plot_data_gam_simulations$Temp, plot_data_gam_simulations$pseudoTPC,col="green")
  title(main = "red line is TPCFUN_GAM and green line is TPCFUN_PSEUDO_GAM")
  mtext(text_results_roc, side = 3, cex = .7)
  mtext(paste0("n_terms_in_lm =", n_terms_in_lm), side = 2, cex = .5)
  
  # write plot
  
  if(is.null(out_dir) == FALSE) {
    dir.create(out_dir, showWarnings = F)
    setwd(out_dir)
    
    pdf(paste0(species_name,'_comparison_between_performance_functions.pdf')) # only plot objects can be made pdfs
    plot(plot_data_gam_simulations$Temp, plot_data_gam_simulations$TPC,type="l",col="red")
    lines(plot_data_gam_simulations$Temp, plot_data_gam_simulations$pseudoTPC,col="green")
    title(main = "red line is TPCFUN_GAM and green line is TPCFUN_PSEUDO_GAM")
    mtext(text_results_roc, side = 3, cex = .7)
    mtext(paste0("n_terms_in_lm =", n_terms_in_lm), side = 2, cex = .5)
    dev.off()
    
    # set back to master directory
    
    setwd(master_directory)
    
  }
  
  GAM_functions_list <- list(TPCFUN_GAM = TPCFUN_GAM, TPCFUN_PSEUDO_GAM = TPCFUN_PSEUDO_GAM)
  
  return(GAM_functions_list)
  
} # end function

##############             END of function            ############

# Ameiva_ameiva performance

TPCFUN_GAMM2 <- PerfGAMM2 (performance_data = "~/Desktop/Luisa_data/check/perf_ccryptus.csv",
                           dependent_variable = "performance",
                           smooth_predictors = "temp",
                           linear_predictors = "size",
                           quadratic_predictors = NULL,
                           criterion = "BIC",
                           knots_vector = 7,
                           lm_approx_terms_value = 10,
                           fix_body_size_for_lm_value = 61.8,
                           plot_variable_on_x = "size",
                           plot_variable_on_y = "performance",
                           plot_variable_on_z = "temp",
                           output_directory = "~/Desktop/Luisa_data/check")

# chato

TPCFUN_GAMM2_other <- PerfGAMM2 (performance_data = "/Volumes/Genomic2/luisa_6/Chatogekko_amazonicus/Chatogekko_amazonicus_input_data/perf_chato.csv",
                                 dependent_variable = "performance",
                                 smooth_predictors = "temp",
                                 linear_predictors = "size",
                                 quadratic_predictors = NULL,
                                 criterion = "BIC",
                                 knots_vector = 7,
                                 lm_approx_terms_value = 10,
                                 fix_body_size_for_lm_value = 17.75178571,
                                 plot_variable_on_x = "size",
                                 plot_variable_on_y = "performance",
                                 plot_variable_on_z = "temp",
                                 output_directory = "/Volumes/Genomic2/luisa_6/Ameiva_ameiva/Ameiva_ameiva_output_files")













































































#############################################################################################################################
#PerfGAMM2_3###################################################################################################################
#############################################################################################################################

setwd("~/Desktop/Aura_2")

PerfGAMM3_Multiple <- function (path_performance_data_file_value, 
                                dependent_variable_names_value,
                                tensor_product_smooths_te_value,
                                smooth_predictors_names_value,
                                linear_predictors_names_value,
                                quadratic_predictors_names_value = NULL,
                                cubic_predictors_names_value = NULL,
                                smooth_predictors_interactions_bulk_value = NULL,
                                discrete_factor_variable_by_value,
                                user_formula_value = NULL,
                                criterion_value = "BIC",
                                knots_vector_value = 7,
                                plot_variable_on_x_value,
                                plot_variable_on_y_value,
                                plot_variable_on_z_value,
                                output_directory_value) {
  
  # required libraries
  
  require('ggplot2')
  require('grid')
  require('mgcv')
  
  # input data 
  
  path_performance_data_file <- path_performance_data_file_value
  
  dependent_variable_names <- dependent_variable_names_value
  tensor_product_smooths_te <- tensor_product_smooths_te_value
  smooth_variable_names <- smooth_predictors_names_value
  linear_predictors_names <- linear_predictors_names_value
  quadratic_predictors_names <- quadratic_predictors_names_value
  cubic_predictors_names <- cubic_predictors_names_value
  
  smooth_predictors_interactions_bulk <- smooth_predictors_interactions_bulk_value
  discrete_factor_variable_by <- discrete_factor_variable_by_value
  user_formula_from_screen <- user_formula_value
  
  criterion <- criterion_value
  knots_vector <- knots_vector_value
  
  plot_variable_on_x <- plot_variable_on_x_value
  plot_variable_on_y <- plot_variable_on_y_value
  plot_variable_on_z <- plot_variable_on_z_value
  
  output_directory <- output_directory_value 
  
  master_directory <- getwd()
  
  # read file if not data frame
  
  if(class(path_performance_data_file) == "data.frame") {
    list_input_performance_raw <- path_performance_data_file
  } else {
    if(grepl(pattern = "*.csv$", path_performance_data_file) == TRUE) {list_input_performance_raw <- read.table(file = path_performance_data_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
    if(grepl(pattern = "*.txt$", path_performance_data_file) == TRUE) {list_input_performance_raw <- read.table(file = path_performance_data_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
  }
  
  # get species names
  
  species_name <- try(unique(list_input_performance_raw$species))
  
  if(is.null(species_name)) {
    species_name <- try(unique(list_input_performance_raw$genus_species))
  }
  
  # function of not into
  
  "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
  
  # subset_groups: 
  
  performance_reference_1 <- list_input_performance_raw[ , which(names(list_input_performance_raw) %in% c("id", dependent_variable_names,
                                                                                                          tensor_product_smooths_te, 
                                                                                                          smooth_variable_names,
                                                                                                          linear_predictors_names,quadratic_predictors_names, cubic_predictors_names_value))]
  performance_reference <- performance_reference_1[complete.cases(performance_reference_1),]
  
  get_numbers_individuals <- unique(as.character(performance_reference$id))
  get_numbers_PER <- nrow(performance_reference)
  
  cat("\n--------------------------       Input data       --------------------------\n")
  cat("Species name: ", species_name, " \n")
  cat(paste("Total individuals included:", " ", length(get_numbers_individuals), "\n", sep=""))
  cat(paste("Total observations:", " ", get_numbers_PER, "\n", sep=""))
  cat("\n-------------------------------------------------------------------------------------------\n")
  
  ## explore two 2D
  
  #### plot points againts lat
  
  require(grid)
  plot_dispersion_species <- ggplot(performance_reference, aes_string(x=tensor_product_smooths_te[1],y= dependent_variable_names[1], color = "species_name")) + 
    geom_point(size = 1, alpha = 0.7) + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(legend.text = element_text(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    geom_rug(col="darkred",alpha=.7)
  
  ##########################################################################################
  ############################## Correlation schemes by ID #################################
  
  ## corAR1 : autoregressive process
  cor_1 <- corAR1(form=~1|id)  # no autocorrelation
  cor_2 <- corAR1(0.1,form=~1|id)  # autocorrelation set to 0.1
  
  ## corCAR1 : continuous autoregressive process
  cor_3 <- corCAR1(0.1,form=~1|id) # correlation between two observations one unit of time apart set to 0.1
  
  ## corGaus : Gaussian spatial correlation
  cor_5 <- corGaus(form=~1|id)
  cor_6 <- corGaus(form=~1|id, nugget = TRUE) # account for nugget effect
  
  ## corLin : representing a linear spatial correlation structure
  # cor_7 <- corLin(form=~1|id)
  # cor_8 <- corLin(form=~1|id, nugget = TRUE)
  # cor_7, cor_8, -- those do not behave well -- Then excluded
  
  ## corExp : exponential spatial correlation structure
  cor_9 <- corExp(form=~1|id)
  cor_10 <- corExp(form=~1|id, nugget = TRUE)
  
  ## corRatio : Rational quadratics spatial correlation
  cor_11 <- corRatio(form=~1|id)
  cor_12 <- corRatio(form=~1|id, nugget = TRUE)
  
  ## corSpher : spherical spatial correlation
  cor_13 <- corSpher(form=~1|id)
  cor_14 <- corSpher(form=~1|id, nugget = TRUE)
  
  ## corARMA : autoregressive moving average process
  cor_16 <- corARMA(form = ~ 1 | id, p=0, q=1)
  cor_17 <- corARMA(form = ~ 1 | id, p=1, q=0)
  cor_18 <- corARMA(form = ~ 1 | id, p=1, q=1) 
  cor_19 <- corARMA(form = ~ 1 | id, p=1, q=2)
  cor_20 <- corARMA(form = ~ 1 | id, p=2, q=1)
  
  cor_str_id_list <- NULL
  cor_str_id_list <- list(cor_1, cor_2, cor_3, cor_5, cor_6, cor_9, cor_10, cor_11, cor_12, cor_13, cor_14, cor_16, cor_17, cor_18, cor_19, cor_20)
  
  cor_str_names <- NULL
  cor_str_names <- c( "corAR1(form=~1|id): autoregressive process", 
                      "corAR1(0.1,form=~1|id): autoregressive process", 
                      "corCAR1(0.1,form=~1|id): continuous autoregressive process", 
                      "corGaus(form=~1|id): Gaussian spatial correlation", 
                      "corGaus(form=~1|id, nugget = TRUE): Gaussian spatial correlation", 
                      "corExp(form=~1|id): exponential spatial correlation structure", 
                      "corExp(form=~1|id, nugget = TRUE): exponential spatial correlation structure", 
                      "corRatio(form=~1|id): Rational quadratics spatial correlation", 
                      "corRatio(form=~1|id, nugget = TRUE): Rational quadratics spatial correlation",
                      "corSpher(form=~1|id): spherical spatial correlation", 
                      "corSpher(form=~1|id, nugget = TRUE): spherical spatial correlation", 
                      "corARMA(form = ~ 1 | id, p=0, q=1): autoregressive moving average process",
                      "corARMA(form = ~ 1 | id, p=1, q=0): autoregressive moving average process", 
                      "corARMA(form = ~ 1 | id, p=1, q=1): autoregressive moving average process", 
                      "corARMA(form = ~ 1 | id, p=1, q=2): autoregressive moving average process",
                      "corARMA(form = ~ 1 | id, p=2, q=1): autoregressive moving average process")
  
  ##########################################################################################
  ##########    subseting to dependent, predicto, covariate, id    #################
  ##########################################################################################
  
  performance_input <- as.data.frame(performance_reference)
  performance_input$order <- row.names(performance_input)
  
  # set discrete factor variable to factor
  
  if(!is.null(discrete_factor_variable_by)){
    for(i in 1:length(discrete_factor_variable_by)) {
      performance_input[[discrete_factor_variable_by[i]]] <- as.factor(performance_input[[discrete_factor_variable_by[i]]])
    }
  }
  
  ##########################################################################################
  ##########################################################################################
  
  # use knots
  
  uni_temp <- seq(1:knots_vector)
  k_final <- knots_vector # k is number of knots
  
  ##########################################################################################
  ##############         The formula construction       ####################
  
  if(is.null(user_formula_from_screen)) {
    
    # tensor
    
    if(!is.null(tensor_product_smooths_te)) {
      if(length(tensor_product_smooths_te) == 1) {
        collapsed_tensor <- paste0(' te(',tensor_product_smooths_te,',k=k_final,bs="cs") ')
      } 
      if(length(tensor_product_smooths_te) > 1) {
        collapsed_tensor_1 <- paste0(' te(',tensor_product_smooths_te,',k=k_final,bs="cs") ')
        collapsed_tensor <- paste0(collapsed_tensor_1, collapse= "+", sep="")
      }
    }
    
    if(is.null(tensor_product_smooths_te)) {collapsed_tensor <- NULL}
    
    # smooth
    
    if(!is.null(smooth_variable_names)) {
      if(length(smooth_variable_names) == 1) {
        collapsed_smooth <- paste0(' s(',smooth_variable_names,',k=k_final,bs="cs") ')
      } 
      if(length(smooth_variable_names) > 1) {
        collapsed_smooth_1 <- paste0(' s(',smooth_variable_names,',k=k_final,bs="cs") ')
        collapsed_smooth <- paste0(collapsed_smooth_1, collapse= "+", sep="")
      }
    }
    
    if(is.null(smooth_variable_names)) {collapsed_smooth <- NULL}
    
    # linear
    
    if(!is.null(linear_predictors_names)) {
      if(length(linear_predictors_names) == 1) {
        collapsed_linear <- paste(linear_predictors_names, sep="")
      } 
      if(length(linear_predictors_names) > 1) {
        collapsed_linear <- paste(linear_predictors_names, sep="", collapse=" + ")
      }
    }
    
    if(is.null(linear_predictors_names)) {collapsed_linear <- NULL}
    
    # quadratic
    
    if(!is.null(quadratic_predictors_names)) {
      if(length(quadratic_predictors_names) == 1) {
        collapsed_quadratic <- paste("(", quadratic_predictors_names, ")^2", sep="")
      } 
      if(length(quadratic_predictors_names) > 1) {
        collapsed_quadratic_1 <- paste("(", quadratic_predictors_names, sep="", collapse=")^2 + ")
        collapsed_quadratic <- paste0(collapsed_quadratic_1, ")^2", sep="")
      }
    }
    
    if(is.null(quadratic_predictors_names)) {collapsed_quadratic <- NULL}
    
    # cubic
    
    if(!is.null(cubic_predictors_names)) {
      if(length(cubic_predictors_names) == 1) {
        collapsed_cubic <- paste("(", cubic_predictors_names, ")^3", sep="")
      } 
      if(length(cubic_predictors_names) > 1) {
        collapsed_cubic_1 <- paste("(", cubic_predictors_names, sep="", collapse=")^3 + ")
        collapsed_cubic <- paste0(collapsed_cubic_1, ")^3", sep="")
      }
    }
    
    if(is.null(cubic_predictors_names)) {collapsed_cubic <- NULL}
    
    # interactions
    
    collapsed_interactions <- smooth_predictors_interactions_bulk
    
    ################ constructing the user formula
    
    oo1 <- collapsed_smooth
    oo2 <- collapsed_linear
    oo3 <- collapsed_quadratic
    oo4 <- collapsed_cubic
    oo5 <- collapsed_interactions
    
    # collapsed_interactions <- NULL
    # collapsed_smooth <- NULL
    # collapsed_linear <- NULL
    
    ## tensor must exist
    
    user_string_formula_raw <- paste0(dependent_variable_names, ' ~ ', collapsed_tensor, ' + ',
                                      ifelse(is.null(collapsed_smooth), paste0(" "), paste0(collapsed_smooth, ' + ')), 
                                      ifelse(is.null(collapsed_linear), paste0(" "), paste0(collapsed_linear, ' + ')), 
                                      ifelse(is.null(collapsed_quadratic), paste0(" "), paste0(collapsed_quadratic, ' + ')),
                                      ifelse(is.null(collapsed_cubic), paste0(" "), paste0(collapsed_cubic, ' + ')),
                                      ifelse(is.null(collapsed_interactions), paste0(" "), paste0(collapsed_interactions)))
    
    # reduce multiple " " to " "
    
    while (grepl(" ", user_string_formula_raw) == TRUE) {
      user_string_formula_raw <- gsub(" ", " ", user_string_formula_raw)
    }
    
    # replace last '+' if floating
    
    user_string_formula <- gsub("\\+ $", "", user_string_formula_raw)
    
    cat("\n--------------------------         GAM formula         --------------------------\n\n")
    cat("processing formula: ", user_string_formula, "\n")
    cat("number of k_final knots: ", k_final, "\n")
    cat("\n---------------------------------------------------------------------------------------------------\n")
    
  } # close loop if user did not provide formula
  
  ##########################################################################################
  
  if(!is.null(user_formula_from_screen)) {
    user_string_formula <- user_formula_from_screen
    
    cat("\n--------------------------      GAM User provided formula     --------------------------\n\n")
    cat("processing formula: ", user_string_formula, "\n")
    cat("\n This formula requires that correct names on the input file match those on the above formula \n")
    cat("\n---------------------------------------------------------------------------------------------------\n")
    
  }
  
  # performance_log10_HR ~ te(temp,k=k_final,bs="cs") + s(acctemp,k=k_final,bs="cs") + latitude + temp 
  
  ##########################################################################################
  ##############  Loop to correation structures with k_final knots  ####################
  
  gamm_models_id_list <- list()
  gamm_models_id_failed <- list()
  counter_fail <- 0
  gamm_models_id_success <- list()
  counter_success <- 0
  
  for (i in 1:length(cor_str_id_list)) {
    
    # i <- 1
    
    gamm_temp <- try(gamm(as.formula(user_string_formula),
                          correlation=cor_str_id_list[[i]],
                          data=performance_input,
                          method="REML",
                          knots=list(x=uni_temp)), silent=FALSE)
    
    cat(paste0("processeing correltaion srt: ", class(cor_str_id_list[[i]])[1], "\n"))
    
    if ('try-error' %in% class(gamm_temp)) { counter_fail <- counter_fail + 1
    gamm_models_id_failed[[counter_fail]] <- cor_str_names[i]
    next
    
    } else {counter_success <- counter_success + 1
    gamm_models_id_list[counter_success] <- list(gamm_temp)
    gamm_models_id_success[[counter_success]]<- cor_str_names[i]
    }
    
  }
  
  
  # models list to vector
  
  gamm_models_id_failed_vector <- as.vector(do.call(rbind,gamm_models_id_failed))
  gamm_models_id_success_vector <- as.vector(do.call(rbind,gamm_models_id_success))
  
  # stop if no succesul performances
  
  if(is.null(gamm_models_id_success_vector) == TRUE) { print(plot_dispersion_species)
    stop(print("No correlation structure was successful -- if user provided formula make sure that variable names match with input file"))
  } else {
    cat("\nthe number of successful correlation structures is : ", length(gamm_models_id_success_vector), "...\n")
  }
  
  # names list based on correlation names
  
  names(gamm_models_id_list) <- gamm_models_id_success_vector
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_lme <- list()
  
  for (i in 1:length( gamm_models_id_list)) {
    gamm_models_id_list_lme[i] <- list(gamm_models_id_list[[i]]$lme)
  } # save models $lme to list for anova comparisons
  
  # save models $gam and $lme to list
  
  gamm_models_id_list_gam <- list()
  
  for (i in 1:length( gamm_models_id_list)) {
    gamm_models_id_list_gam[i] <- list(gamm_models_id_list[[i]]$gam)
  } # save models $gam to list for anova comparisons
  
  gamm_models_id_list_lme_logLik <- list()
  
  for (i in 1:length( gamm_models_id_list_lme)) {
    gamm_models_id_list_lme_logLik[i] <- list(gamm_models_id_list_lme[[i]]$logLik)
  }
  
  gamm_models_id_list_lme_logLik_unlist <- unlist(gamm_models_id_list_lme_logLik)
  
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
  moto$knots <- k_final
  
  names(moto) <- c("gamm_correlation_structure_class", "AIC", "BIC", "logLik", "delta_AIC", "delta_BIC", "knots")
  
  # reorder moto based on Delta BIC
  
  moto <- moto[ order(moto[,6]), ]
  
  ### select the best model for prediction
  
  best_gamm_model_AIC <- gamm_models_id_list[[which.min(moto[,5] )]] ## model_best_AIC <- which.min(moto[,5] ) 
  best_gamm_model_BIC <- gamm_models_id_list[[which.min(moto[,6] )]] ## model_best_BIC <- which.min(moto[,6] )
  
  
  # plot 3D 
  
  plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
  title(paste("Best GAMM model BIC -- ", species_name))
  vis.gam(best_gamm_model_BIC$gam,
          view = c(plot_variable_on_x, plot_variable_on_z),
          type = "response",
          theta=50,
          phi=30,
          color="topo",
          xlab= plot_variable_on_x,
          ylab= plot_variable_on_z,
          zlab= plot_variable_on_y,
          ticktype="detailed",
          n.grid=60) # color: gray; theta is the perspective angle of the graph
  title(paste("Best GAMM model BIC -- ", species_name))
  
  ## which one to return
  
  ifelse (criterion == 'BIC', best_model <- best_gamm_model_BIC, best_model <- best_gamm_model_AIC)
  
  # calculate CTmax, CTmin and Topt
  
  temp_test_no_order <- as.data.frame(c(seq(-10,55, by = 0.01), -10, 55)) 
  temp_test <- as.data.frame(temp_test_no_order[order(temp_test_no_order[,1]), ])
  names(temp_test) <-'temp'
  
  # get means of selected predictors
  
  performance_input_select <- subset(performance_input, select = names(performance_input) %!in% c("id","temp", "performance", "order"))
  
  predictors_df_list <- list()
  
  for(i in 1:ncol(performance_input_select)) {
    if(is.numeric(performance_input_select[1,i])) {
      one_column_mean <- as.data.frame(t(colMeans(performance_input_select[i])), stringsAsFactors = F)
    } else {
      one_column_mean <- as.data.frame(t(performance_input_select[1,i]), stringsAsFactors = F)
      names(one_column_mean) <- names(performance_input_select[i])
    }
    predictors_df_list[[i]] <- one_column_mean
  }
  
  predictors_df <- do.call(cbind, predictors_df_list)
  data_test <- merge(temp_test, predictors_df, all.x = TRUE)
  predicted_performance <- as.vector(predict.gam(best_model$gam,data_test))
  data_test$predicted_performance <- predicted_performance
  
  # Toptimum
  
  Toptimum_mean_predictors <- data_test[data_test$predicted_performance == max(data_test$predicted_performance) , ]
  
  # CTmin 
  
  one_species_max_to_correct_0 <- data_test[which.max(data_test$predicted_performance),]
  one_species_pre_opt_to_correct_0 <- subset(data_test, data_test$temp < one_species_max_to_correct_0$temp)
  
  one_species_pre_opt_ID_to_correct_0_min <- one_species_pre_opt_to_correct_0[which.min(one_species_pre_opt_to_correct_0$predicted_performance),]
  one_species_pre_opt_ID_to_correct_0_neg <- one_species_pre_opt_to_correct_0[one_species_pre_opt_to_correct_0$predicted_performance < 0,]
  
  if(nrow(one_species_pre_opt_ID_to_correct_0_neg) < 1) {
    row_to_report_CTmin <- -10
  } else {
    row_to_report_CTmin <- ifelse(one_species_pre_opt_ID_to_correct_0_min$temp < max(one_species_pre_opt_ID_to_correct_0_neg$temp),
                                  max(one_species_pre_opt_ID_to_correct_0_neg$temp), one_species_pre_opt_ID_to_correct_0_min$temp)
  }
  
  # CTmax
  
  one_species_post_opt_to_correct_0 <- subset(data_test, data_test$temp > one_species_max_to_correct_0$temp)
  one_species_post_opt_ID_to_correct_0 <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]
  
  one_species_post_opt_ID_to_correct_0_min <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]
  one_species_post_opt_ID_to_correct_0_neg <- one_species_post_opt_to_correct_0[one_species_post_opt_to_correct_0$predicted_performance < 0,]
  
  if(nrow(one_species_post_opt_ID_to_correct_0_neg) < 1) {
    row_to_report_CTmax <- 55
  } else {
    row_to_report_CTmax <- ifelse(one_species_post_opt_ID_to_correct_0_min$temp > min(one_species_post_opt_ID_to_correct_0_neg$temp),
                                  min(one_species_post_opt_ID_to_correct_0_neg$temp), one_species_post_opt_ID_to_correct_0_min$temp)
  }
  
  
  #### report these
  
  ## print to screen
  
  cat("------------   TPC GAMM based on given k_final   ------------\n\n")
  print(species_name)
  cat("\n") 
  cat("------------   TPC formula   ------------")
  cat("\n\n")
  cat( user_string_formula, "\n")
  cat("k_final is", k_final, "\n")
  cat("\n") 
  cat("------------   Model comparison results   ------------")
  cat("\n\n")
  print(moto)
  cat("\n") 
  cat("------------   best_gamm_model_BIC -- adjusted R squared")
  cat("\n\n")
  print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
  cat("\n")
  cat("------------   best_gamm_model_BIC   ------------")
  cat("\n")
  print (summary(best_gamm_model_BIC$gam))
  cat("\n\n")
  cat("------------   best_gamm_model_AIC -- adjusted R squared")
  cat("\n\n")
  print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
  cat("\n")
  cat("------------   best_gamm_model_AIC   ------------")
  cat("\n")
  print (summary(best_gamm_model_AIC$gam))
  cat("\n\n")
  cat("------------   Toptimum   ------------")
  cat("\n")
  print (Toptimum_mean_predictors)
  cat("\n\n")
  cat("------------   CTmax   ------------")
  cat("\n")
  print (row_to_report_CTmax)
  cat("\n\n")
  cat("------------   CTmin   ------------")
  cat("\n")
  print (row_to_report_CTmin)
  cat("\n\n")
  
  plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray", main = paste("Best GAMM model BIC -- ", species_name))
  
  ## write summary for best models # dir.create
  
  if(is.null(output_directory) == FALSE) {
    dir.create(output_directory, showWarnings = F)
    setwd(output_directory)
    
    ## write summary for best models
    
    writeLines(capture.output( {
      cat("------------   TPC GAMM on given k_final   ------------\n\n")
      print(species_name)
      cat("\n") 
      cat("------------   TPC formula   ------------")
      cat("\n\n")
      cat( user_string_formula, "\n")
      cat("k_final is", k_final, "\n")
      cat("\n") 
      cat("------------   Model comparison results   ------------")
      cat("\n\n")
      print(moto)
      cat("\n") 
      cat("------------   best_gamm_model_BIC -- adjusted R squared")
      cat("\n\n")
      print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
      cat("\n")
      cat("------------   best_gamm_model_BIC   ------------")
      cat("\n")
      print (summary(best_gamm_model_BIC$gam))
      cat("\n\n")
      print (best_gamm_model_BIC)
      cat("\n\n")
      cat("------------   best_gamm_model_AIC -- adjusted R squared")
      cat("\n\n")
      print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
      cat("\n")
      cat("------------   best_gamm_model_AIC   ------------")
      cat("\n")
      print (summary(best_gamm_model_AIC$gam))
      cat("\n\n")
      print (best_gamm_model_AIC)
      cat("\n\n")
      cat("------------   Toptimum   ------------")
      cat("\n")
      print (Toptimum_mean_predictors)
      cat("\n\n")
      cat("------------   CTmax   ------------")
      cat("\n")
      print (row_to_report_CTmax)
      cat("\n\n")
      cat("------------   CTmin   ------------")
      cat("\n")
      print (row_to_report_CTmin)
      cat("\n\n")
      
    }
    ),con=paste(species_name, '_performance_calc_k_AUTO_raw_gamm_fit.txt', sep=""))
    
    pdf(paste(species_name,'species_best_fit_3D_gam.pdf')) # only plot objects can be made pdfs
    plot(plot_dispersion_species)
    plot(best_gamm_model_AIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
    title(paste("Best GAMM model AIC -- ", species_name))
    vis.gam(best_gamm_model_AIC$gam,
            view = c(plot_variable_on_x, plot_variable_on_z),
            type = "response",
            theta=50,
            phi=30,
            color="topo",
            xlab= plot_variable_on_x,
            ylab= plot_variable_on_z,
            zlab= plot_variable_on_y,
            ticktype="detailed",
            n.grid=60) # color: gray; theta is the perspective angle of the graph
    title(paste("Best GAMM model AIC -- ", species_name))
    dev.off()
    
  }
  
  # set back to master directory
  
  setwd(master_directory)
  
  # return model
  
  return(best_model)
  
}

##########################################################################################
########################         END of function        ##################
##########################################################################################

# Aura test user formula : 1

PerfGAMM3_Multiple (path_performance_data_file_value = "~/Desktop/Aura_2/Pleurodema_thaul_HR.txt", 
                    dependent_variable_names_value = "performance_log10_HR",
                    tensor_product_smooths_te_value = "acctemp",
                    smooth_predictors_names_value = "temp",
                    linear_predictors_names_value = "latitude",
                    quadratic_predictors_names_value = NULL,
                    cubic_predictors_names_value = NULL,
                    smooth_predictors_interactions_bulk_value = NULL,
                    discrete_factor_variable_by_value = NULL,
                    user_formula_value = 'performance_log10_HR~te(I(temp^2),acctemp, bs="cs", k=3) + s(temp,bs="cs", k=3)+ latitude',
                    criterion_value = "BIC",
                    knots_vector_value = 3,
                    plot_variable_on_x_value = "acctemp",
                    plot_variable_on_y_value = "performance_log10_HR",
                    plot_variable_on_z_value = "temp",
                    output_directory_value = "~/Desktop/Aura_2") 


# first test : HR

#--- modelo dic_1

best_gamm_model_HR <- PerfGAMM3_Multiple (path_performance_data_file_value = "~/Desktop/Aura_2/Pleurodema_thaul_HR.txt", 
                                          dependent_variable_names_value = "performance_log10_HR",
                                          tensor_product_smooths_te_value = "temp",
                                          smooth_predictors_names_value = NULL,
                                          linear_predictors_names_value = c("acctemp", "latitude"),
                                          quadratic_predictors_names_value = NULL,
                                          cubic_predictors_names_value = NULL,
                                          smooth_predictors_interactions_bulk_value = NULL,
                                          discrete_factor_variable_by_value = NULL,
                                          user_formula_value = NULL,
                                          criterion_value = "BIC",
                                          knots_vector_value = 10,
                                          plot_variable_on_x_value = "acctemp",
                                          plot_variable_on_y_value = "performance_log10_HR",
                                          plot_variable_on_z_value = "temp",
                                          output_directory_value = "~/Desktop/Aura_2") 


best_gamm_model_HR <- TPC_performance_gamm_3D_estimator (path_performance_data_file_value = "~/Escritorio/TPC_GAMM/Pleurodema_thaul_HR.csv", 
                                                         dependent_variable_names_value = "performance_log10_HR",
                                                         tensor_product_smooths_te_value = "temp",
                                                         smooth_predictors_names_value = NULL,
                                                         linear_predictors_names_value = c("acctemp", "latitude"),
                                                         quadratic_predictors_names_value = NULL,
                                                         cubic_predictors_names_value = NULL,
                                                         smooth_predictors_interactions_bulk_value = NULL,
                                                         discrete_factor_variable_by_value = NULL,
                                                         criterion_value = "BIC",
                                                         knots_vector_value = 10,
                                                         plot_variable_on_x_value = "acctemp",
                                                         plot_variable_on_y_value = "performance_log10_HR",
                                                         plot_variable_on_z_value = "temp",
                                                         output_directory_value = "~/Escritorio/TPC_GAMM") 

#modelo 18dic
best_gamm_model_HR <- TPC_performance_gamm_3D_estimator (path_performance_data_file_value = "~/Escritorio/TPC_GAMM/Pleurodema_thaul_HR.csv", 
                                                         dependent_variable_names_value = "performance_log10_HR",
                                                         tensor_product_smooths_te_value = "temp",
                                                         smooth_predictors_names_value = c("acctemp","I(temp^2)"),
                                                         linear_predictors_names_value = "latitude",
                                                         quadratic_predictors_names_value = NULL,
                                                         cubic_predictors_names_value = NULL,
                                                         smooth_predictors_interactions_bulk_value = NULL,
                                                         discrete_factor_variable_by_value = NULL,
                                                         criterion_value = "BIC",
                                                         knots_vector_value = 3,
                                                         plot_variable_on_x_value = "acctemp",
                                                         plot_variable_on_y_value = "performance_log10_HR",
                                                         plot_variable_on_z_value = "temp",
                                                         output_directory_value = "~/Escritorio/TPC_GAMM") 

#modelo 19dic
best_gamm_model_HR <- TPC_performance_gamm_3D_estimator (path_performance_data_file_value = "~/Escritorio/TPC_GAMM/Pleurodema_thaul_HR.csv", 
                                                         dependent_variable_names_value = "performance_log10_HR",
                                                         tensor_product_smooths_te_value = "temp",
                                                         smooth_predictors_names_value = "acctemp",
                                                         linear_predictors_names_value = "latitude",
                                                         quadratic_predictors_names_value = NULL,
                                                         cubic_predictors_names_value = NULL,
                                                         smooth_predictors_interactions_bulk_value = c("temp","I(temp^2)"),
                                                         discrete_factor_variable_by_value = NULL,
                                                         criterion_value = "BIC",
                                                         knots_vector_value = 3,
                                                         plot_variable_on_x_value = "acctemp",
                                                         plot_variable_on_y_value = "performance_log10_HR",
                                                         plot_variable_on_z_value = "temp",
                                                         output_directory_value = "~/Escritorio/TPC_GAMM") 


# JS
best_gamm_model_JS <- TPC_performance_gamm_3D_estimator (path_performance_data_file_value = "~/Escritorio/TPC_GAMM/Pleurodema_thaul_JS.csv", 
                                                         dependent_variable_names_value = "performance",
                                                         tensor_product_smooths_te_value = "temp",
                                                         smooth_predictors_names_value = c("acctemp","I(temp^2)"),
                                                         linear_predictors_names_value = "latitude",
                                                         quadratic_predictors_names_value = NULL,
                                                         cubic_predictors_names_value = NULL,
                                                         smooth_predictors_interactions_bulk_value = NULL,
                                                         discrete_factor_variable_by_value = NULL,
                                                         criterion_value = "BIC",
                                                         knots_vector_value = 3,
                                                         plot_variable_on_x_value = "acctemp",
                                                         plot_variable_on_y_value = "performance",
                                                         plot_variable_on_z_value = "temp",
                                                         output_directory_value = "~/Escritorio/TPC_GAMM") 

