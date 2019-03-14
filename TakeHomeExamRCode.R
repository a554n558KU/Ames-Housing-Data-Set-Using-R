# Read Data into R and Split to Train/Test:
Ames <- read.csv("AmesHousing.csv")
set.seed(1)
# Remove Missing Values from Data:
Ames2 = na.omit(Ames)
train <- sample(2258,1800)
test <- (c(1:2258)[-train])
num.ames=data.frame(Ames2[,c(2,3,14:17,23,31,33:35,40:49,51,53,56,58,59,63:68,70,71,74)])
str(num.ames)
complete.cases(num.ames)
complete.cases(train)

# Plots:
plot(SalePrice ~., data = num.ames, subset = train)


# Create First Model:
fit <- lm(SalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + TotRms.AbvGrd +
            Garage.Yr.Blt + Wood.Deck.SF +  Open.Porch.SF, data = num.ames, subset = train)
summary(fit)
plot(fit$res~fit$fitted, main = "Diagnostic Check Model 1")
hist(fit$res, main = "Diagnostic Check Model Histogram",
     col = c("blue", "red", "green"))
qqnorm((fit$res))
shapiro.test(fit$res)

# BoxCox:
library(MASS)
boxcox(SalePrice~TotRms.AbvGrd,data = num.ames)
boxcox(SalePrice~Garage.Yr.Blt,data = num.ames)
boxcox(SalePrice~Open.Porch.SF, data = num.ames)
boxcox(SalePrice~Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + TotRms.AbvGrd +
         Garage.Yr.Blt + Wood.Deck.SF, data = num.ames)
num.ames$logSalePrice <- log(num.ames$SalePrice)
fit <- lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + TotRms.AbvGrd +
            Garage.Yr.Blt + Wood.Deck.SF + Open.Porch.SF, data = num.ames, subset = train)
summary(fit)
plot(fit$res~fit$fitted)

# Checks after Removing TotRms.AbvGrd and Garage.Yr.Blt
fit <- lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = num.ames, subset = train)
summary(fit)
plot(fit$res~fit$fitted, main = "Diagnostic Check Model 2")
hist(fit$res, main = "Diagnostic Check Model Histogram",
     col = c("blue", "red", "green"))
qqnorm((fit$res))
shapiro.test(fit$res)

# Which Categorical Variables Should be Included:
model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street, data = Ames2, subset = train)
anova(model1,model2)
summary(model1)
str(Ames2)


model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config, data = Ames2, subset = train)
anova(model1,model2)

model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation, data = Ames2, subset = train)
anova(model1,model2)

model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation + Bsmt.Exposure, data = Ames2, subset = train)
anova(model1,model2)

model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation + Bsmt.Exposure + Heating.QC, data = Ames2, subset = train)
anova(model1,model2)

model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation + Bsmt.Exposure + Heating.QC + Central.Air, data = Ames2, subset = train)
anova(model1,model2)

model1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF, data = Ames2, subset = train)
model2 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation + Bsmt.Exposure + Heating.QC + Central.Air + Functional, data = Ames2, subset = train)
anova(model1,model2)

# New Model: Both Numeric and Non-Numeric:
fitnew = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Foundation + Bsmt.Exposure + Heating.QC + Central.Air + Functional, data = Ames2, subset = train)
summary(fitnew)
plot(fitnew$res~fitnew$fitted, main = "Diagnostic Check Model 3")
hist(fitnew$res, main = "Diagnostic Check Model Histogram",
     col = c("blue", "red", "green"))
qqnorm((fitnew$res))
shapiro.test(fitnew$res)

fit = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF + Open.Porch.SF + Street + Lot.Config + Bsmt.Exposure + Heating.QC + Central.Air + Functional, data = Ames2, subset = train)
summary(fit)
num.ames$logSalePrice
str(fit)


# Dealing with Outliers:
library(MASS)
# Store the standardized residuals
fit1 = lm(num.ames$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add +
           BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF +
           Open.Porch.SF + Street + Lot.Config + Bsmt.Exposure + Heating.QC + Central.Air +
           Functional, data = Ames2)
hist(stdres(fit1))
Ames2$sres=stdres(fit1)
length(Ames2)
length(Ames)
Ames=subset(Ames2,abs(Ames2$sres) <3)

length(num.ames$Overall.Qual)
length(num.ames$logSalePrice)
length(Ames)
length(Ames$Overall.Qual)
x=data.frame(Ames[,c(2,3,14:17,23,31,33:35,40:49,51,53,56,58,59,63:68,70,71,74)])
x$logSalePrice <- log(x$SalePrice)
# Refit the Model without the Outliers in the Data:
fit1 <- lm(x$logSalePrice ~ Overall.Qual + Year.Built + Year.Remod.Add +
             BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + Gr.Liv.Area + Wood.Deck.SF +
             Open.Porch.SF + Street + Lot.Config + Bsmt.Exposure + Heating.QC + Central.Air +
             Functional, data = Ames, na.action = na.exclude)
summary(fit1)
plot(fit1$res~fit1$fitted, main = "Diagnostic Check Model 4")
hist(fit1$res, main = "Diagnostic Check Model Histogram",
     col = c("blue", "red", "green"))
qqnorm((fit1$res))
shapiro.test(fit1$res)
nrow(Ames2)
nrow(Ames)

# Question 2: Predict Housing Price
Predict.Data <- read.csv("AmesHousing_predict.csv")
summary(Predict.Data)
names(Predict.Data)



Predict.Data <- read.csv("AmesHousing_predict.csv")
newdata <- Predict.Data
pred <- predict(fit1,newdata,interval="prediction")
pred


