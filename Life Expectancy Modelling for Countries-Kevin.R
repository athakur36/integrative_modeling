
size <- dim(countries)[1]  
set.seed(913288760)      # use your student ID number as the seed.
subsample <- sample(size, size*0.8)  
countries80 <- countries[subsample, ]  

#1 Build Model
  #initial model
  model1 <- lm(LifeExpectancy ~ LandArea + Population + Rural + Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + Cell, data=countries80)

  #Predictor Selection:
    model2 <- lm(LifeExpectancy ~ LandArea + Population + Rural + Health + BirthRate + ElderlyPop + CO2 + GDP + Cell, data=countries80)
    
  #R2adj
    select <- leaps(xvars, LifeExpectancy, method = "adjr2", nbest = 1, names = varnames)  # the method here does not matter
    cbind(select$size, select$which, select$adjr2) 
    
    #Forward Selection
    countries80_forward <- regsubsets(LifeExpectancy ~ Health + BirthRate + GDP + Cell, data=countries80, method = "forward", nvmax=6)
    cbind(summary(countries80_forward)$which, "adjusted r^2" = summary(countries80_forward)$adjr2)
    
    
    #Backward Selection
    countries80_backward <- regsubsets(LifeExpectancy ~ LandArea + Population + Rural + Health + BirthRate + ElderlyPop + CO2 + GDP + Cell, data=countries80, method = "backward", nvmax=10)
    cbind(summary(countries80_backward)$which, "adjusted r^2" = summary(countries80_backward)$adjr2)
    
    

par(mfrow=c(2,2))
  #ScatterPlots
  plot(countries80$LandArea,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Land Area", xlab="LandArea (Sq Km)", ylab="Life Expectancy (Average Years")
  plot(countries80$Population,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Population", xlab="Population (Million)", ylab="Life Expectancy (Average Years)")
  plot(countries80$Rural,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Rural Population", xlab="Rural (% Population living in Rurality)", ylab="Life Expectancy (Average Years)")
  plot(countries80$Health,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Healthcare Expenditure", xlab="Health (% Government Expenditure on Healthcare)", ylab="Life Expectancy (Average Years)")
  plot(countries80$Internet,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against % Population with Internet Access", xlab="Internet (% Population w/ Access)", ylab="Life Expectancy (Average Years)")
  plot(countries80$BirthRate,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Birth Rate", xlab="Birth Rate (Per 1000 People)", ylab="Life Expectancy (Average Years)")
  plot(countries80$ElderlyPop,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Elderly Population", xlab="Elderly Population (% Population 65y/o or older)", ylab="Life Expectancy (Average Years)")
  plot(countries80$CO2,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against CO2 Emissions", xlab="CO2 Emissions (Metric Tons per capita)", ylab="Life Expectancy (Average Years)")
  plot(countries80$GDP,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against GDP", xlab="GDP (per capita)", ylab="Life Expectancy (Average Years)")
  plot(countries80$Cell,countries80$LifeExpectancy, main="ScatterPlot of Country Life Expectancy against Cell Phone Subscriptions", xlab="Cell Phone Subscriptions (per 100 people)", ylab="Life Expectancy (Average Years)")
 
  par(mfrow=c(2,2))
  plot(countries80$LandArea, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Land Area", xlab="Land Area (Sq Km)", ylab="Residuals of Life Expectancy")
  plot(countries80$Population, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Population", xlab="Population (Million)", ylab="Residuals of Life Expectancy")
  plot(countries80$Rural, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Rural Population", xlab="Rural (% Population living in Rurality)", ylab="Residuals of Life Expectancy")
  plot(countries80$Health, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against % Government Expenditure on Healthcare", xlab="Health (% Government Expenditure)", ylab="Residuals of Life Expectancy")
  plot(countries80$Internet, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against % Population w/ Internet Access", xlab="Internet (% Population w/ Access)", ylab="Residuals of Life Expectancy")
  plot(countries80$BirthRate, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Birth Rate", xlab="Birth Rate (Per 1000 People)", ylab="Residuals of Life Expectancy")
  plot(countries80$ElderlyPop, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Elderly Population", xlab="Elderly Population (% Population 65y/o or older)", ylab="Residuals of Life Expectancy")
  plot(countries80$CO2, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against CO2 Emissions", xlab="CO2 Emissions (Metric tons per capita)", ylab="Residuals of Life Expectancy")
  plot(countries80GDP, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against GDP per Capita", xlab="GDP (per capita)", ylab="Residuals of Life Expectancy")
  plot(countries80$Cell, resid(countries80$LifeExpectancy), main="Residual plot of Life Expectancy against Cell Phone Subscriptions", xlab="Cell Phone Subscriptions (per 100 people)", ylab="Residuals of Life Expectancy")
  
  
  #Residual Plots
  model2 <- lm(LifeExpectancy ~ Health + BirthRate + GDP + Cell, data=countries80)
  model2.res=resid(model2)
    par(mfrow=c(1,1))
    abline(h=100000, col="red")
    abline(h=-100000, col="red")
    abline(h=0,col="blue")
    #Health
    plot(countries80$Health, model2.res, main="Residual plot of Residuals against Predictor Health", xlab="Health (% Government Expenditure)", ylab="Residuals")
    #BirthRate
    plot(countries80$BirthRate, model2.res, main="Residual plot of Residuals against Predictor Birthrate",xlab="Birth Rate (Per 1000 People)", ylab="Residuals of Life Expectancy")
    #GDP
    GDPnew <-subset(countries80$GDP,countries80$GDP < 60000)
    GDPresid <- subset(model2.res, countries80$GDP < 60000)
    plot(GDPnew,GDPresid, main="Residual Plot of Residuals against Predictor GDP (Outliers Omitted)", xlab="GDP (Per Capita)", ylab="Residuals")
    #Cell
    plot(countries80$Cell, model2.res, main="Residual plot of Residuals against Predictor GDP", xlab="Cell Phone Subscriptions (per 100 people)", ylab="Residuals of Life Expectancy")
    
    
    


    #Residual Plot against fitted line
    plot(fitted(model2), model2.res, main="Residual plot (Yhat Untransformed)", xlab="Fitted Y_Hat", ylab="Residuals")
    
    
    
    
  #Omission of outliers
         
  #QQ Plot
  par(mfrow=c(1,1))
  qqnorm(resid(model2),main="Normal Probability Plot of the Residuals")
  qqline(resid(model2), col="red")
  
  #Multicollinearity
  #pairwise scatter
    pairs(countries80[,3:12])
  #correlation matrix
    cor(cbind(LandArea, Population, Rural, Health, LifeExpectancy, Internet, BirthRate, ElderlyPop, CO2, GDP, Cell))
  
    collinearcheck <- lm(LifeExpectancy~Health + Internet + ElderlyPop, data=countries80)
    summary(collinearcheck)
    
    car::vif(model1)
  
  #Y-Transformation
    boxcox(LifeExpectancy ~ Health + BirthRate + GDP + Cell, data = countries80)
    
    par(mfrow=c(2,3))
    # Untransformed Model
    qqnorm(resid(model2),main="QQ plot of Untransformed Model")
    qqline(resid(model2), col="red")
    
    #Quadratic Y Transformed Model
    sqrdLE = (countries80$LifeExpectancy)^2
    transmodel = lm(sqrdLE ~ countries80$Health + countries80$BirthRate + countries80$GDP + countries80$Cell)   
    qqnorm(resid(transmodel), main="QQ Plot of Quadratic Y-transformed Model")
    qqline(resid(transmodel), col="red")
    
    # Log Y Transformed Model
    logLE = log(countries80$LifeExpectancy)
    logtransmodel = lm(logLE ~ countries80$Health + countries80$BirthRate + countries80$GDP + countries80$Cell)
    qqnorm(resid(logtransmodel), main="Normal Probability Plot of log Y-transformed Model")
    qqline(resid(logtransmodel), col="red")
    
    #Cubic Y Transformed Model
    cubLE = (countries80$LifeExpectancy)^3
    cubtransmodel = lm(cubLE ~ countries80$Health + countries80$BirthRate + countries80$GDP + countries80$Cell)
    qqnorm(resid(cubtransmodel), main="QQ Plot of Cubic Y-transformed Model")
    qqline(resid(cubtransmodel), col="red")
    plot(fitted(cubtransmodel), resid(cubtransmodel), main="Residual plot of Transformed Yhat", xlab="Fitted Y_hat (Y^3 transformed)", ylab="Residuals")
    
    plot(fitted(transmodel), resid(transmodel), main="Residual plot of Residuals against Fitted Values on Y-transformed model", xlab="Fitted Values of Y-transformed model", ylab="Residuals")
    abline(h=0, col="blue")
    abline(h=800, col="red")
    abline(h=-800, col="red")
    
    #F-test
    cubtransmodel = lm(cubLE ~ countries80$Health + countries80$BirthRate + countries80$GDP + countries80$Cell)
    n <- dim(countries80)[1]
    p <- 5
    SSR <- sum((cubtransmodel$fitted.values - mean(cubLE))^2)
    SSE <- sum(cubtransmodel$residuals^2)
    Fstat <- (SSR/(p-1))/(SSE/(n-p))
    pval <- pf(Fstat, p-1, n-p, lower.tail = FALSE)
    
    #R^2
    newLE <- countries80$LifeExpectancy^3
    summary(cubtransmodel)$adj.r.squared
    
    #F-test for Lack of Fit:
    model <- lm(newLE ~ LandArea + Rural + Health + BirthRate + ElderlyPop + CO2 + GDP + Cell, data=countries80)
    SSE <- sum(model$residuals^2)
    
    n <- 148  # sample size
    c <- length(unique(countries80$Cell)) # number of distinct x values
    gm <- merge(cars, aggregate(dist ~ speed, FUN = mean, data = cars),
                by="speed", suffixes = c("" ,".group_mean"))
    SSPE <- sum((gm$dist - gm$dist.group_mean)^2)
    SSLF <- SSE - SSPE                                                                 
    f <- (SSLF/(c-2))/(SSPE/(n-c))
    pf(f, df1 = c-2, df2 = n-c, lower.tail = FALSE)
    
    model1 <- lm(dist ~ factor(speed), data = cars)
    anova(model, model1)
    
    xxLand <- factor(countries80$LandArea)
    xxRural <- factor(countries80$Rural)
    xxHealth <- factor(countries80$Health)
    xxBirth <- factor(countries80$BirthRate)
    xxElder <- factor(countries80$ElderlyPop)
    xxCO2 <- factor(countries80$CO2)
    xxGDP <- factor(countries80$GDP)
    xxCell <- factor(countries80$Cell)
    fit1 <- lm(LifeExpectancy ~ xxLand + xxRural + xxHealth + xxBirth + xxElder + xxCO2 + xxGDP + xxCell, data=countries80)
    
    summary(model)
    summary(countries80$LifeExpectancy ~ countries80$LandArea + countries80$Rural + countries80$Health + countries80$BirthRate + countries80$ElderlyPop + countries80$CO2 + countries80$GDP + countries80$Cell)
    