# Regression equation
# ========================================
# Linear
fit <- lm(mpg ~ cyl + hp, data = mtcars)
summary(fit)
# This is the result:
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 36.90833    2.19080  16.847  < 2e-16 ***
# cyl         -2.26469    0.57589  -3.933  0.00048 ***
# hp          -0.01912    0.01500  -1.275  0.21253


plot(mpg ~ cyl, data = mtcars, xlab = "Cylinders", ylab = "Miles per gallon")
abline(coef(fit)[1:2])

## rounded coefficients for better output
cf <- round(coef(fit), 2) 

## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("mpg = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " cyl ",
             ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " hp")

## printing of the equation
mtext(eq, 3, line=-1)

# probability of newdata
newdata <- data.frame(cyl=c(4:8), hp=c(100,110,120,130,140))
predict(fit, newdata, type="response")
# above is only showing the result prediction
# probability prediction should be calculated using glm
# y value should be within 0 to 1 for binomial family
fit2 <- glm(am ~ cyl + hp, data = mtcars, family=binomial)
summary(fit2)
predict(fit2, newdata, type="response")


# =======================================================
# polynomial
# fit model can be written into following model:
model <- lm(noisy.y ~ poly(q,3))
model <- lm(noisy.y ~ x + I(X^2) + I(X^3))

# sample
model <- lm(mpg ~ poly(cyl,2, raw=T), data = mtcars)
summary(model)

str(summary(model)) #structure of the model summary
coef(model) # coefisien of the formula
summary(model)$r.squared
summary(model)$adj.r.squared
sign(coef(model)) # coef sign check positive/negative

# to obtain the "normal" polynomial (not orthogonal) we can add (raw=T)


# result:
# Call:
#   lm(formula = mpg ~ poly(cyl, 2, raw = T), data = mtcars)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.2636 -1.8357  0.0286  1.3893  7.2364 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             47.3390    11.6471   4.064 0.000336 ***
# poly(cyl, 2, raw = T)1  -6.3078     4.1723  -1.512 0.141400    
# poly(cyl, 2, raw = T)2   0.2847     0.3451   0.825 0.416072    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.223 on 29 degrees of freedom
# Multiple R-squared:  0.7325,	Adjusted R-squared:  0.714 
# F-statistic:  39.7 on 2 and 29 DF,  p-value: 4.979e-09

# Equation can be read as: mpg = 47.3390-6.3078*cyl+0.2847*cyl^2

# ======================================

# logaritmic

