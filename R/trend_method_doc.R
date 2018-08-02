#' Default trend methods
#'
#'The default trend estimation and test depends on the nature of the calculated index: \cr
#'
#'For continuous data (e.g. sum, mean, prcptot, spi) ny default an ordinary linear regression is calculated with time as predictor for the trend estimate and a students-t test. If the data is not normally distributed, a logarithmic transformation is applied before the trend calculation. \cr
#'
#'For count data (e.g dd, cdd, cwd) by default a logistic regression is calculated with time as predictor for the trend estimation and a students-t test. Aditionally, a correction for overdispersion is applied. \cr
#'
#'If you choose trend = "MannKendall" instead of trend = TRUE the non-parametric Mann-Kendall test based on the relative ranking in the series is applied and a Theil-Sen slope is estimated. See e.g. Yue et al. 2002 or Mann (1945), Kendall (1975).
#'@keywords internal
#'@name trend_methods
NULL
#'