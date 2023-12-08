
# Function to fit a variety of growth models to length increment data:
#   - gomp - Gompertz
#   - gompM - Gompertz w/mediating covariate
#   - svbgf - Specialised von Bertalanffy
#   - svbgfM - Specialised von Bertalanffy w/mediating covariate
#   - gvbgf - Generalised von Bertalanffy
#   - gvbgfM - Generalised von Bertalanffy w/mediating covariate

laaGrowth <- function(dat, control = list(maxiter = 500), nTry = 10, 
  models = c("svbgf", "gvbgf", "svbgfM", "gvbgfM", "gomp", "gompM")){
  
  mods <- vector("list", length = length(models))
  names(mods) <- models
    
  # svbgf - special VBGF -----
  if("svbgf" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K = runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9)
      upper = c(K = 1, Linf = max(dat$l2)*2)
      fm <- formula(l2 ~ l+(Linf-l)*(1-exp(-K*dt)))
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["svbgf"]] <- fit.i
    }
  }
  
  # svbgfM - special VBGF w covar -----
  if("svbgfM" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K= runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5),
        C1 = runif(1, 1e-10, 1e-5))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9, C1 = -1e10)
      upper = c(K = 1, Linf = max(dat$l2)*2, C1 = 1e10)
      fm <- formula(l2 ~ l+((Linf + C1*covar1)-l) * (1-exp(-K*dt)))
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["svbgfM"]] <- fit.i
    }
  }

  # gvbgf - generalized VBGF -----
  if("gvbgf" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K = runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5),
        D = runif(1, min = 0.1, 1))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9, D = 0.1)
      upper = c(K = 1, Linf = max(dat$l2)*2, D = 3)
      fm <- formula(l2 ~ (Linf^(1/D)*(1-exp(-K*dt))+l^(1/D)*exp(-K*dt))^D)
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["gvbgf"]] <- fit.i
    }
  } 
  
  # gvbgfM - generalized VBGF w/ covar -----
  if("gvbgfM" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K = runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5),
        D = runif(1, min = 0.1, 1),
        C1 = runif(1, 1e-10, 1e-5))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9, D = 0.1, C1 = -1e10)
      upper = c(K = 1, Linf = max(dat$l2)*2, D = 3, C1 = 1e10)
      fm <- formula(l2 ~ ((Linf+covar1*C1)^(1/D)*(1-exp(-K*dt))+l^(1/D)*exp(-K*dt))^D)
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["gvbgfM"]] <- fit.i
    }
  }  
  
  
  # gomp - Gompertz -----
  if("gomp" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K = runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9)
      upper = c(K = 1, Linf = max(dat$l2)*2)
      fm <- formula(l2 ~ exp(log(Linf)*(1-exp(-K*dt))+log(l)*exp(-K*dt)))
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["gomp"]] <- fit.i
    }
  }  
  
  # gompM - Gompertz w covar -----
  if("gompM" %in% models){
    i = 1
    while(i <= nTry){
      start = c(K = runif(1, 0.05, 0.4), 
        Linf = runif(1, min = max(dat$l2)*1, max(dat$l2)*1.5),
        C1 = runif(1, 1e-10, 1e-5))
      lower = c(K = 0.01, Linf = max(dat$l2)*0.9, C1 = -1e10)
      upper = c(K = 1, Linf = max(dat$l2)*2, C1 = 1e10)
      fm <- formula(l2 ~ exp(log(Linf+C1*covar1)*(1-exp(-K*dt))+log(l)*exp(-K*dt)))
      fit.i <- try(silent = TRUE, expr = do.call(nls, list(fm, data=dat, 
        start = start, lower = lower, upper = upper, algorithm = "port", 
        control = control, weight = dat$n)))
      if(class(fit.i) != "try-error"){
        break()
      }else{
        i=i+1
      }
    }
    if(class(fit.i) != "try-error"){
      mods[["gompM"]] <- fit.i
    }
  }
  
  BICs <- unlist(lapply(mods, FUN = function(x){if(!is.null(x)){BIC(x)}}))
  best <- unname(which.min(BICs))
  mod <- mods[[best]]
  mod$name <- names(mods)[best]
  return(mod)
}




