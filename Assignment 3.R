## Q1 

# a)

require(mice)

df <- nhanes

cc <- sum(complete.cases(df) == T) 

# b) 

str(df)
summary(df)

imp_def15 <- mice(df, m = 5, seed = 1)

df_comp15 <- complete(imp_def15)

model15 <- with(imp_def15, lm(bmi ~ age + hyp + chl))

pooled15 <- pool(model15)

# c)

# seed = 2, m = 5

imp_def25 <- mice(df, m = 5, seed = 2)

df_comp25 <- complete(imp_def25)

model25 <- with(imp_def25, lm(bmi ~ age + hyp + chl))

pooled25 <- pool(model25)

# seed = 3, m= 5

imp_def35 <- mice(df, m = 5, seed = 3)

df_comp35 <- complete(imp_def35)

model35 <- with(imp_def35, lm(bmi ~ age + hyp + chl))

pooled35 <- pool(model35)

pooled35

# seed = 4, m= 5

imp_def45 <- mice(df, m = 5, seed = 4)

df_comp45 <- complete(imp_def45)

model45 <- with(imp_def45, lm(bmi ~ age + hyp + chl))

pooled45 <- pool(model45)

# seed = 5, m= 5

imp_def55 <- mice(df, m = 5, seed = 5)

df_comp55 <- complete(imp_def55)

model55 <- with(imp_def55, lm(bmi ~ age + hyp + chl))

pooled55 <- pool(model55)

# seed = 6, m= 5

imp_def65 <- mice(df, m = 5, seed = 6)

df_comp65 <- complete(imp_def65)

model65 <- with(imp_def65, lm(bmi ~ age + hyp + chl))

pooled65 <- pool(model65)

pooled65

lambdas5 <- data.frame(seed.1 = pooled15$pooled$lambda,
                      seed.2 = pooled25$pooled$lambda,
                      seed.3 = pooled35$pooled$lambda,
                      seed.4 = pooled45$pooled$lambda,
                      seed.5 = pooled55$pooled$lambda,
                      seed.6 = pooled65$pooled$lambda, 
                      row.names =c("Intercept", "age", "hyp", "chl"))

summary(pooled65, conf.int = T)

pdf(file = 'lambdas5.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)

plot(c(1,2,3,4,5,6), lambdas5[2,], type = 'b', pch = 16, col = 'red', 
     ylim = c(0,1), xlab = 'seed', ylab = 'lambda')
points(c(1,2,3,4,5,6), lambdas5[3,], type = 'b', pch = 16, col = 'blue')
points(c(1,2,3,4,5,6), lambdas5[4,], type = 'b', pch = 16, col = 'green')
# points(c(1,2,3,4,5,6), lambdas[1,], type = 'b', pch = 16, col = 'purple')

legend(5.3, 1, legend = c('age', 'hyp', 'chl'), border = 'white', 
       col = c('red', 'blue', 'green'), pch = 16, bty = 'n')

dev.off()

# d)

# seed = 1, m = 100

imp_def100 <- mice(df, m = 100, seed = 1)

df_comp100 <- complete(imp_def100)

model100 <- with(imp_def100, lm(bmi ~ age + hyp + chl))

pooled100 <- pool(model100)

# seed = 2, m= 100

imp_def200 <- mice(df, m = 100, seed = 2)

df_comp200 <- complete(imp_def200)

model200 <- with(imp_def200, lm(bmi ~ age + hyp + chl))

pooled200 <- pool(model200)

# seed = 3, m= 100

imp_def300 <- mice(df, m = 100, seed = 3)

df_comp300 <- complete(imp_def300)

model300 <- with(imp_def300, lm(bmi ~ age + hyp + chl))

pooled300 <- pool(model300)

# seed = 4, m= 100

imp_def400 <- mice(df, m = 100, seed = 4)

df_comp400 <- complete(imp_def400)

model400 <- with(imp_def400, lm(bmi ~ age + hyp + chl))

pooled400 <- pool(model400)

# seed = 5, m= 100

imp_def500 <- mice(df, m = 100, seed = 5)

df_comp500 <- complete(imp_def500)

model500 <- with(imp_def500, lm(bmi ~ age + hyp + chl))

pooled500 <- pool(model500)

# seed = 6, m= 100

imp_def600 <- mice(df, m = 100, seed = 6)

df_comp600 <- complete(imp_def600)

model600 <- with(imp_def600, lm(bmi ~ age + hyp + chl))

pooled600 <- pool(model600)

lambdas100 <- data.frame(seed.1 = pooled100$pooled$lambda,
                         seed.2 = pooled200$pooled$lambda,
                         seed.3 = pooled300$pooled$lambda,
                         seed.4 = pooled400$pooled$lambda,
                         seed.5 = pooled500$pooled$lambda,
                         seed.6 = pooled600$pooled$lambda, 
                       row.names =c("Intercept", "age", "hyp", "chl"))

pdf(file = 'lambdas100.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)

plot(c(1,2,3,4,5,6), lambdas100[2,], type = 'b', pch = 16, col = 'red', 
     ylim = c(0,1), xlab = 'seed', ylab = 'lambda')
points(c(1,2,3,4,5,6), lambdas100[3,], type = 'b', pch = 16, col = 'blue')
points(c(1,2,3,4,5,6), lambdas100[4,], type = 'b', pch = 16, col = 'green')
# points(c(1,2,3,4,5,6), lambdas[1,], type = 'b', pch = 16, col = 'purple')

legend(5.3, 1, legend = c('age', 'hyp', 'chl'), border = 'white', 
       col = c('red', 'blue', 'green'), pch = 16, bty = 'n')

dev.off()

## Q2

load('dataex2(1).Rdata')

df <- dataex2

beta1 <- 3
count_sri <- 0 

for (i in 1:100){
  
  imp <- mice(df[,,i], m = 20, method = 'norm.nob', print = F, seed = 1)
  
  model <- with(imp, lm(Y~X))
  pooled <- pool(model)
  
  lower <- summary(pooled, conf.int = T)[2,7] 
  upper <- summary(pooled, conf.int = T)[2,8]
  
  print(upper)
  
  if (beta1 < upper & beta1 > lower){
    
    count_sri <- count_sri + 1
    
  }
  
}

Ecov_sri = count_sri/100

count_boot <- 0 

for (i in 1:100){
  
  imp <- mice(df[,,i], m = 20, method = 'norm.boot', print = F, seed = 1)
  
  model <- with(imp, lm(Y~X))
  pooled <- pool(model)
  
  lower <- summary(pooled, conf.int = T)[2,7] 
  upper <- summary(pooled, conf.int = T)[2,8]
  
  if (beta1 < upper & beta1 > lower){
    
    count_boot <- count_boot + 1
    
  }
  
}

Ecov_boot <- count_boot/100

Ecov_sri
Ecov_boot

## Q4

load('dataex4(1).Rdata')

sum(complete.cases(dataex4) == T) 

# a)

df_x1 <- dataex4

imp_x1 <- mice(df_x1, m = 50, seed = 1, print = F)

model_x1 <- with(imp_x1, lm(y ~ x1 + x2 + x1*x2))

pooled_x1 <- pool(model_x1)

summary(pooled_x1, conf.int = T)

# b) 

x1x2 <- df_x1$x1*df_x1$x2 # interaction variable

df_x1x2 <- cbind(df_x1, x1x2) 

imp0 <- mice(df_x1x2, maxit = 0) #dry run to adjust method and pred

meth <- imp0$method

meth["x1x2"] <- "~I(x1*x2)"
pred <- imp0$predictorMatrix

pred[c("x1", "x2"), "x1x2"] <- 0 

# x1x2 is last column so order doesn't need to be chanegd

imp_x1x2 <- mice(df_x1x2, method= meth, pred = pred, m = 50, seed = 1, print = F)

model_x1x2 <- with(imp_x1x2, lm(y ~ x1 + x2 + x1:x2))

pooled_x1x2 <- pool(model_x1x2)

summary(pooled_x1x2, conf.int = T)

# c) 

x3 <- df_x1$x1*df_x1$x2 # interaction variable

df_x3 <- cbind(df_x1, x3) 

imp_x3 <- mice(df_x3, m = 50, seed = 1, print = F)

model_x3 <- with(imp_x3, lm(y ~ x1 + x2 + x3))

pooled_x3 <- pool(model_x3)

summary(pooled_x3, conf.int = T)

## Q5

load("NHANES2.RData") 

df <- NHANES2

dim(df)
str(df)
summary(df)

require(mice)

md.pattern(df)

install.packages('JointAI')

require(MASS)

plot_all(df)


