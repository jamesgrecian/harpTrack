################################################################
### Define a function to permute the bootstrapped regression ###
################################################################

# this saves repeating the same 100 lines of code for each model

# input will be a dataset
# output will be tibble of predictions
# define column of interest
# define number of permutations
# define fraction to sample

boot_regression_ice <- function(data, permutations = 1000, fraction = 0.1){
  
  # receiving data frame for test statistics
  lala <- data.frame(dw = rep(NA, permutations),
                     intercept = rep(NA, permutations),
                     slope = rep(NA, permutations),
                     R2m = rep(NA, permutations),
                     R2c = rep(NA, permutations),
                     P = rep(NA, permutations))
  
  # receiving data frame for trend lines
  zoo <- matrix(NA, nrow = permutations, ncol = 250)
  
  # randomly resample dataframe and run lme model
  # pull out durbin Watson estimate of autocorrelation
  # pull out slope and intercept
  # calculate R-squared
  # store trend lines - use these to calculate confidence intervals...
  
  # set up progress bar based on number of permutations
  pb <- txtProgressBar(min = 1, max = permutations, style = 3)
  
  for (i in 1:permutations){
    setTxtProgressBar(pb, i) # update progress bar
    m1 <- lme(day ~ IceDay,
              random = ~ 1 | id,
              data = data %>% sample_frac(fraction) %>% arrange(id, date))
    
    lala[i, 1] <- car::durbinWatsonTest(as.numeric(residuals(m1))) # values of 2 mean no autocorrelation
    lala[i, 2] <- m1$coefficients$fixed[1]
    lala[i, 3] <- m1$coefficients$fixed[2]
    lala[i, 4] <- r.squaredGLMM(m1)[1]
    lala[i, 5] <- r.squaredGLMM(m1)[2]
    lala[i, 6] <- as.numeric(summary(m1)$tTable[,"p-value"][2])
    
    zoo[i,] <- as.numeric(predict(m1, newdata = data.frame(IceDay = 1:250), level = 0))
    }
  
  # calculate the parameters for confidence interval ribbons
  prob_025 <- rep(NA, 250)
  prob_100 <- rep(NA, 250)
  prob_250 <- rep(NA, 250)
  prob_500 <- rep(NA, 250)
  prob_750 <- rep(NA, 250)
  prob_900 <- rep(NA, 250)
  prob_975 <- rep(NA, 250)
  
  for (i in 1:250){
    prob_025[i] <- quantile(zoo[,i], probs = .025)
    prob_100[i] <- quantile(zoo[,i], probs = .1)
    prob_250[i] <- quantile(zoo[,i], probs = .25)
    prob_500[i] <- quantile(zoo[,i], probs = .5)
    prob_750[i] <- quantile(zoo[,i], probs = .75)
    prob_900[i] <- quantile(zoo[,i], probs = .9)
    prob_975[i] <- quantile(zoo[,i], probs = .975)
  }
  
  CI_95 <- tibble(name = "CI_95",
                  x = 1:250,
                  ymin = prob_025,
                  ymax = prob_975)
  CI_80 <- tibble(name = "CI_80",
                  x = 1:250,
                  ymin = prob_100,
                  ymax = prob_900)
  CI_50 <- tibble(name = "CI_50",
                  x = 1:250,
                  ymin = prob_250,
                  ymax = prob_750)
  trend <- tibble(name = "trend",
                  x = 1:250,
                  y = prob_500)
  CI_output <- rbind(CI_50, CI_80, CI_95)
  
  # extract summaries...
  cat("\n")
  message("Slope is ", round(mean(lala$slope), 2), " +/- ", round(sd(lala$slope), 2))
  message("R-sq is ", round(mean(lala$R2c), 2), " +/- ", round(sd(lala$R2c), 2))
  message("P = ", round(mean(lala$P), 3))

  return(list(trend, CI_output, lala))

}

boot_regression_bloom <- function(data, permutations = 1000, fraction = 0.1){
  
  # receiving data frame for test statistics
  lala <- data.frame(dw = rep(NA, permutations),
                     intercept = rep(NA, permutations),
                     slope = rep(NA, permutations),
                     R2m = rep(NA, permutations),
                     R2c = rep(NA, permutations),
                     P = rep(NA, permutations))
  
  # receiving data frame for trend lines
  zoo <- matrix(NA, nrow = permutations, ncol = 250)
  
  # randomly resample dataframe and run lme model
  # pull out durbin Watson estimate of autocorrelation
  # pull out slope and intercept
  # calculate R-squared
  # store trend lines - use these to calculate confidence intervals...
  
  # set up progress bar based on number of permutations
  pb <- txtProgressBar(min = 1, max = permutations, style = 3)
  
  for (i in 1:permutations){
    setTxtProgressBar(pb, i) # update progress bar
    m1 <- lme(day ~ BloomDay,
              random = ~ 1 | id,
              data = data %>% sample_frac(fraction) %>% arrange(id, date))
    
    lala[i, 1] <- car::durbinWatsonTest(as.numeric(residuals(m1))) # values of 2 mean no autocorrelation
    lala[i, 2] <- m1$coefficients$fixed[1]
    lala[i, 3] <- m1$coefficients$fixed[2]
    lala[i, 4] <- r.squaredGLMM(m1)[1]
    lala[i, 5] <- r.squaredGLMM(m1)[2]
    lala[i, 6] <- as.numeric(summary(m1)$tTable[,"p-value"][2])
    
    zoo[i,] <- as.numeric(predict(m1, newdata = data.frame(BloomDay = 1:250), level = 0))
  }
  
  # calculate the parameters for confidence interval ribbons
  prob_025 <- rep(NA, 250)
  prob_100 <- rep(NA, 250)
  prob_250 <- rep(NA, 250)
  prob_500 <- rep(NA, 250)
  prob_750 <- rep(NA, 250)
  prob_900 <- rep(NA, 250)
  prob_975 <- rep(NA, 250)
  
  for (i in 1:250){
    prob_025[i] <- quantile(zoo[,i], probs = .025)
    prob_100[i] <- quantile(zoo[,i], probs = .1)
    prob_250[i] <- quantile(zoo[,i], probs = .25)
    prob_500[i] <- quantile(zoo[,i], probs = .5)
    prob_750[i] <- quantile(zoo[,i], probs = .75)
    prob_900[i] <- quantile(zoo[,i], probs = .9)
    prob_975[i] <- quantile(zoo[,i], probs = .975)
  }
  
  CI_95 <- tibble(name = "CI_95",
                  x = 1:250,
                  ymin = prob_025,
                  ymax = prob_975)
  CI_80 <- tibble(name = "CI_80",
                  x = 1:250,
                  ymin = prob_100,
                  ymax = prob_900)
  CI_50 <- tibble(name = "CI_50",
                  x = 1:250,
                  ymin = prob_250,
                  ymax = prob_750)
  trend <- tibble(name = "trend",
                  x = 1:250,
                  y = prob_500)
  CI_output <- rbind(CI_50, CI_80, CI_95)
  
  # extract summaries...
  cat("\n")
  message("Slope is ", round(mean(lala$slope), 2), " +/- ", round(sd(lala$slope), 2))
  message("R-sq is ", round(mean(lala$R2c), 2), " +/- ", round(sd(lala$R2c), 2))
  message("P = ", round(mean(lala$P), 3))
  
  return(list(trend, CI_output, lala))
}


# bootstrap regression checking for differences between years while correcting for age
boot_regression_ice_years <- function(data, permutations = 1000, fraction = 0.1){
  
  # receiving data frame for test statistics
  lala <- data.frame(dw = rep(NA, permutations),
                     intercept = rep(NA, permutations),
                     slope1 = rep(NA, permutations),
                     slope2 = rep(NA, permutations),
                     slope3 = rep(NA, permutations),
                     R2m = rep(NA, permutations),
                     R2c = rep(NA, permutations),
                     P = rep(NA, permutations))
  
  # receiving data frame for trend lines
  zoo <- matrix(NA, nrow = permutations, ncol = 250)
  
  # randomly resample dataframe and run lme model
  # pull out durbin Watson estimate of autocorrelation
  # pull out slope and intercept
  # calculate R-squared
  # store trend lines - use these to calculate confidence intervals...
  
  # set up progress bar based on number of permutations
  pb <- txtProgressBar(min = 1, max = permutations, style = 3)
  
  for (i in 1:permutations){
    setTxtProgressBar(pb, i) # update progress bar
    m1 <- lme(day ~ IceDay + age + year,
              random = ~ 1 | id,
              data = data %>% sample_frac(fraction) %>% arrange(id, date))
    
    lala[i, 1] <- durbinWatsonTest(as.numeric(residuals(m1))) # values of 2 mean no autocorrelation
    lala[i, 2] <- m1$coefficients$fixed[1]
    lala[i, 3] <- m1$coefficients$fixed[2]
    lala[i, 4] <- m1$coefficients$fixed[3]
    lala[i, 5] <- m1$coefficients$fixed[4]
    lala[i, 6] <- r.squaredGLMM(m1)[1]
    lala[i, 7] <- r.squaredGLMM(m1)[2]
    lala[i, 8] <- as.numeric(summary(m1)$tTable[,"p-value"][4])
    
    zoo[i,] <- as.numeric(predict(m1, newdata = data.frame(IceDay = 1:250, age = T, year = 2019), level = 0))
  }
  
  # calculate the parameters for confidence interval ribbons
  prob_025 <- rep(NA, 250)
  prob_100 <- rep(NA, 250)
  prob_250 <- rep(NA, 250)
  prob_500 <- rep(NA, 250)
  prob_750 <- rep(NA, 250)
  prob_900 <- rep(NA, 250)
  prob_975 <- rep(NA, 250)
  
  for (i in 1:250){
    prob_025[i] <- quantile(zoo[,i], probs = .025)
    prob_100[i] <- quantile(zoo[,i], probs = .1)
    prob_250[i] <- quantile(zoo[,i], probs = .25)
    prob_500[i] <- quantile(zoo[,i], probs = .5)
    prob_750[i] <- quantile(zoo[,i], probs = .75)
    prob_900[i] <- quantile(zoo[,i], probs = .9)
    prob_975[i] <- quantile(zoo[,i], probs = .975)
  }
  
  CI_95 <- tibble(name = "CI_95",
                  x = 1:250,
                  ymin = prob_025,
                  ymax = prob_975)
  CI_80 <- tibble(name = "CI_80",
                  x = 1:250,
                  ymin = prob_100,
                  ymax = prob_900)
  CI_50 <- tibble(name = "CI_50",
                  x = 1:250,
                  ymin = prob_250,
                  ymax = prob_750)
  trend <- tibble(name = "trend",
                  x = 1:250,
                  y = prob_500)
  CI_output <- rbind(CI_50, CI_80, CI_95)
  
  return(list(trend, CI_output, lala))
}

# bootstrap regression checking for differences between years while correcting for age
boot_regression_bloom_years <- function(data, permutations = 1000, fraction = 0.1){
  
  # receiving data frame for test statistics
  lala <- data.frame(dw = rep(NA, permutations),
                     intercept = rep(NA, permutations),
                     slope1 = rep(NA, permutations),
                     slope2 = rep(NA, permutations),
                     slope3 = rep(NA, permutations),
                     R2m = rep(NA, permutations),
                     R2c = rep(NA, permutations),
                     P = rep(NA, permutations))
  
  # receiving data frame for trend lines
  zoo <- matrix(NA, nrow = permutations, ncol = 250)
  
  # randomly resample dataframe and run lme model
  # pull out durbin Watson estimate of autocorrelation
  # pull out slope and intercept
  # calculate R-squared
  # store trend lines - use these to calculate confidence intervals...
  
  # set up progress bar based on number of permutations
  pb <- txtProgressBar(min = 1, max = permutations, style = 3)
  
  for (i in 1:permutations){
    setTxtProgressBar(pb, i) # update progress bar
    m1 <- lme(day ~ BloomDay + age + year,
              random = ~ 1 | id,
              data = data %>% sample_frac(fraction) %>% arrange(id, date))
    
    lala[i, 1] <- durbinWatsonTest(as.numeric(residuals(m1))) # values of 2 mean no autocorrelation
    lala[i, 2] <- m1$coefficients$fixed[1]
    lala[i, 3] <- m1$coefficients$fixed[2]
    lala[i, 4] <- m1$coefficients$fixed[3]
    lala[i, 5] <- m1$coefficients$fixed[4]
    lala[i, 6] <- r.squaredGLMM(m1)[1]
    lala[i, 7] <- r.squaredGLMM(m1)[2]
    lala[i, 8] <- as.numeric(summary(m1)$tTable[,"p-value"][4])
    
    zoo[i,] <- as.numeric(predict(m1, newdata = data.frame(BloomDay = 1:250, age = T, year = 2019), level = 0))
  }
  
  # calculate the parameters for confidence interval ribbons
  prob_025 <- rep(NA, 250)
  prob_100 <- rep(NA, 250)
  prob_250 <- rep(NA, 250)
  prob_500 <- rep(NA, 250)
  prob_750 <- rep(NA, 250)
  prob_900 <- rep(NA, 250)
  prob_975 <- rep(NA, 250)
  
  for (i in 1:250){
    prob_025[i] <- quantile(zoo[,i], probs = .025)
    prob_100[i] <- quantile(zoo[,i], probs = .1)
    prob_250[i] <- quantile(zoo[,i], probs = .25)
    prob_500[i] <- quantile(zoo[,i], probs = .5)
    prob_750[i] <- quantile(zoo[,i], probs = .75)
    prob_900[i] <- quantile(zoo[,i], probs = .9)
    prob_975[i] <- quantile(zoo[,i], probs = .975)
  }
  
  CI_95 <- tibble(name = "CI_95",
                  x = 1:250,
                  ymin = prob_025,
                  ymax = prob_975)
  CI_80 <- tibble(name = "CI_80",
                  x = 1:250,
                  ymin = prob_100,
                  ymax = prob_900)
  CI_50 <- tibble(name = "CI_50",
                  x = 1:250,
                  ymin = prob_250,
                  ymax = prob_750)
  trend <- tibble(name = "trend",
                  x = 1:250,
                  y = prob_500)
  CI_output <- rbind(CI_50, CI_80, CI_95)
  
  return(list(trend, CI_output, lala))
}


  
  
  