library(ISLR)
library(dplyr)
library(car)
library(coop)

df_boston <-  Boston
head(df_boston)

#Scaled values for the data set 
df_1 <- lapply(df_boston, scaler)

# Dependent variable is Medv and rest are the independent variables 
lm_fit_1 <-  lm(medv~., data=df_1)
vif(lm_fit_1)

#Dropping variables with high vif 
lm_fit_1 <-  lm(medv~.-(rad+tax), data=df_1)
summary(lm_fit_1)

#dropping age and indus
lm_fit_1 <-  lm(medv~.-(rad+tax+age+indus), data=df_1)
summary(lm_fit_1)

# Dropping Black and crim 
lm_fit_1 <-  lm(medv~.-(rad+tax+age+indus+black+crim), data=df_1)
summary(lm_fit_1)

# Dropping more variables
lm_fit_1 <-  lm(medv~.-(rad+tax+age+indus+crim+chas+black+zn), data=df_1)
summary(lm_fit_1)

lm_fit_1 <-  lm( medv ~ (  rm * lstat * ptratio + crim * dis * chas + nox + rad +  tax  ), data=df_1 )

summary( lm_fit_1 )
#( nox + ptratio +  dis + lstat )


lm_fit_1 <-  lm( medv ~ (  rm * lstat * ptratio +  crim * dis + crim*chas + nox + rad + tax  ), data=df_1 )
summary( lm_fit_1 )

