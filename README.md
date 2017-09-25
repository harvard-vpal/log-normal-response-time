# log-normal-response-time

fitGaussian.R contains a function that fits the lognormal model to logarithms response times by maximizing likelihood. Since the variables it assumes are LOGARITHMS of time, the fitted distribution is normal (=gaussian).

logNormalTimes.R is a script that prepares the data, calls fitGaussian() and records the results of the fit.
