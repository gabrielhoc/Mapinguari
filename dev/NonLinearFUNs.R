####################################################
############### Non linear functions ###############
####################################################

# Create non linear functions to be used on PerfFUN formula (Those are the functions from the Quinn, 2017 paper).
# These functions do not evaluate a value, they exist for model construction only, just like smooth terms functions on mgcv.
# I have to create SelfStart() methods for each function, but that is a mathematical problem, not a programming one.

# Look at package deSolve for nonlinear equations
# functions nls(), getInitial() and ode()
# can you do mixed effect with this?

LinearRate <- function(x, a, b){

  NonLinearFUN <- eval(bquote(function(a, b) (1 / (a + b*.(as.name(x))))))
  formals(NonLinearFUN) <- c(setNames(alist(dummy = ), as.character(x)), formals(NonLinearFUN))

  return(NonLinearFUN(x, a, b))
}

HeipPower <- function(x, a, b, k){

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
