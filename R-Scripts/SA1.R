# Get Sum of 2 random variables
# Input Params:
# mean1 = mean of first random variable
# mean2 = mean of second random variable
# var1 = Variance of second random variable
# var2 = Variance of second random variable
# coef1 = Coefficiant for first random variable
# coef2 = Coefficient for second random variable
getSum2RandomVariables <- function(mean1, mean2, var1, var2, coef1, coef2)
{
  mean <- ceof1 * mean1 + coef2 * mean2
  print(paste("Mean:",mean))
  
  var <- (coef1 * coef1) * (var1) +
        (coef2 * coef2) * (var2)
  print(paste("Variance:", var))
}

# Get Z value for a datapoint of a random variable
# Input Params:
# x = single data point for the random variable
# mean = mean of random variable
# sd = Standard Deviation of the random variable
getZVal <- function(x, mean, sd)
{
  z <- (x - mean) / sd
  z
}

# Get Confidence interval for mean of a random variable, when population standard deviation is known
# Input Params:
# sampleMean = mean of sample data for the random variable
# popSD = population standard deviation of the random variable
# sampleSize = Size of the sample
# confidence = desired confidence in percentage
getConfidenceIntervalMeanPopSDKnown <- function(sampleMean, 
                                            popSD, 
                                            sampleSize,
                                            confidence)
{
    alpha <- 1 - (confidence / 100)
    one_minus_alpha_by_two<-(1 - (alpha/2))  
    z_alpha<-qnorm(one_minus_alpha_by_two)
    moe <- (z_alpha * popSD ) / sqrt(sampleSize)
    low_range <- sampleMean - moe
    high_range <- sampleMean + moe
    paste("[", low_range, " - ", high_range, "]")
}

# Get Confidence interval for mean of a random variable.when population standard deviation is not known
# Input Params:
# sampleMean = mean of sample data for the random variable
# sampleSD = sample standard deviation for the random variable
# sampleSize = Size of the sample
# confidence = desired confidence in percentage
getConfidenceIntervalMeanGeneral <- function(sampleMean, 
                                                sampleSD, 
                                                sampleSize,
                                                confidence)
{
  alpha <- 1 - (confidence / 100)
  one_minus_alpha_by_two<-(1 - (alpha/2))  
  z_alpha<-qt(one_minus_alpha_by_two)
  moe <- (z_alpha * sampleSD ) / sqrt(sampleSize)
  low_range <- sampleMean - moe
  high_range <- sampleMean + moe
  paste("[", low_range, " - ", high_range, "]")
}

# Get Confidence interval for proportions of a random variable
# Input Params:
# proportion = proportion in the sample of the random variable
# sampleSize = Size of the sample
# confidence = desired confidence in percentage
getConfidenceIntervalProportions <-function(proportion,
                                            sampleSize,
                                            confidence)
{
  alpha <- 1 - (confidence / 100)
  one_minus_alpha_by_two<-(1 - (alpha/2))  
  z_alpha<-qnorm(one_minus_alpha_by_two)
  moe <- (z_alpha * sqrt((proportion*(1-proportion))/sampleSize))
  print(paste("MOE: ",moe))
  low_range<-proportion - moe
  high_range<-proportion + moe
  paste("[", low_range, " - ", high_range, "]")
}

# Get p-value for a hypothesis related to proportions for a random variable
# Input Params:
# sampleProportion = proportion in sample data for the random variable
# sampleSize = Size of the sample
# nullHypothesisPoint = Proportion value that separates null and alternate hypothesis
# isSampleStrengthOnLeft = (T/F). Does p-value contain the region to the left of null hypothesis point?
getPValHypothesisProportions <-function(sampleProportion, 
                                        sampleSize, 
                                        nullHypothesisPoint,
                                        isSampleStrengthOnLeft)
{
  se <- sqrt(nullHypothesisPoint * (1 - nullHypothesisPoint) / sampleSize)
  print(paste("SE:", se))
  z <- (sampleProportion - nullHypothesisPoint) / se
  print(paste("z:", z))
  if(isSampleStrengthOnLeft)
    pval <- pnorm(z)  
  else
    pval <- 1 - pnorm(z)
  pval
}

# Get p-value for a hypothesis related to proportions for a random variable
# Input Params:
# sampleMean =  Mean of sample data for the random variable
# sampleSD = Standard Deviation of sample data for the random variable
# sampleSize = Size of the sample
# nullHypothesisPoint = Proportion value that separates null and alternate hypothesis
# isSampleStrengthOnLeft = (T/F). Does p-value contain the region to the left of null hypothesis point?
getPValHypothesisMeans <- function(sampleMean, sampleSD,
                                   sampleSize, nullHypothesisPoint,
                                   isSampleStrengthOnLeft)
{
  se <- sampleSD / sqrt(sampleSize)
  print(paste("SE:", se))
  t <- (sampleMean - nullHypothesisPoint) / se
  print(paste("t:", t))
  df <- sampleSize - 2
  print(paste("DF:", df))
  if(isSampleStrengthOnLeft)
  {
    pval <- pt(t, df)
  }
  else
  {
    pval <- 1 - pt(t, df)
  }
  pval
}

# Get p-value for a hypothesis related to proportions for 2 random variables
# Input Params:
# proportion1 = proportion in sample data for the first random variable
# sample1Size = Size of the sample for first random variable
# proportion2 = proportion in sample data for the second random variable
# sample2Size = Size of the sample for second random variable
# nullHypothesisPoint = Proportion value that separates null and alternate hypothesis
# isSampleStrengthOnLeft = (T/F). Does p-value contain the region to the left of null hypothesis point?
# isPopVarSame = (T/F) Is Population variance same for both samples
getPValHypothesis2SamplesProportions <- function(proportion1, sample1Size,
                                                 proportion2, sample2Size,
                                                 nullHypothesisPoint,
                                                 isSampleStrengthOnLeft,
                                                 isPopVarSame)
{
  if(isPopVarSame)
  {
    combinedSampleProportion <- ((proportion1 * sample1Size + proportion2 * sample2Size) / 
                                (sample1Size + sample2Size))
    print(paste("Combined Sample Proportion:", combinedSampleProportion))
    se <- sqrt((combinedSampleProportion) * 
                 (1 - combinedSampleProportion) * 
                 ((1 / sample1Size) + (1 / sample2Size)))
    print(paste("SE:", se))
  }
  else
  {
    se <- sqrt(((proportion1 / sample1Size) * (1 - proportion1)) + 
                 ((proportion2 / sample2Size) * (1 - proportion2)))
    print(paste("SE:", se))
  }
  
  z <- (proportion1 - proportion2 - nullHypothesisPoint) / se
  print(paste("z:", z))
  if(isSampleStrengthOnLeft)
    pval <- pnorm(z)
  else
    pval <- 1 - pnorm(z)
  pval
}

# Get p-value for a hypothesis related to Means for 2 random variables
# Input Params:
# sample1Mean = mean of sample data for the first random variable
# sample1Var = variance of sample data for the first random variable
# sample1Size = Size of the sample for first random variable
# sample2Mean = mean of sample data for the second random variable
# sample2Var = variance of sample data for the second random variable
# sample2Size = Size of the sample for second random variable
# nullHypothesisPoint = Proportion value that separates null and alternate hypothesis
# isSampleStrengthOnLeft = (T/F). Does p-value contain the region to the left of null hypothesis point?
# isPopVarSame = (T/F) Is Population variance same for both samples
getPValHypothesis2SamplesMeans <- function(sample1Mean, sample1Var, sample1Size,
                                      sample2Mean, sample2Var, sample2Size,
                                      nullHypothesisPoint, 
                                      isSampleStrengthOnLeft, isPopVarSame)
{
  if(isPopVarSame)
  {
    sp <- getPooledVariance(sample1Var, sample1Size, sample2Var, sample2Size)
    print(paste("pooled variance:", sp))
    se <- sqrt((sp / sample1Size) + (sp / sample2Size))
    print(paste("SE:", se))
    df <- sample1Size + sample2Size - 2
    print(paste("DF:", df))
  }
  else
  {
    se <- sqrt((sample1Var / sample1Size) + (sample2Var / sample2Size))
    print(paste("SE:", se))
    df0 <- (((sample1Var / sample1Size) + (sample2Var / sample2Size)) *
          ((sample1Var / sample1Size) + (sample2Var / sample2Size)))   /
          ( (((sample1Var / sample1Size) * (sample1Var / sample1Size)) / 
             (sample1Size - 1)) +
            (((sample2Var / sample2Size) * (sample2Var / sample2Size)) / 
              (sample2Size - 1)) )
    df <- floor(df0)
    print(paste("DF:", df))
  }
  
  t <- (sample1Mean - sample2Mean - nullHypothesisPoint) / se
  print(paste("t:", t))
  if(isSampleStrengthOnLeft)
    pval <- pt(t, df)
  else
    pval <- 1 - pt(t, df)
  pval
}
getPooledVariance <- function(sample1Var, sample1Size,
                              sample2Var, sample2Size)
{
  varPooled <- ((sample1Size - 1) * sample1Var + 
                  (sample2Size - 1) * sample2Var) / 
              (sample1Size + sample2Size - 2)
  varPooled
  return (varPooled)
}
