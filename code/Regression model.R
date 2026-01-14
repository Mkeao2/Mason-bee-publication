## Makes a linear regression model 

m2 <- lm(rmean ~ (Protein), data = Altered.diet.ratios)
summary(m2)


## Predicts model values 

newdat = data.frame(rmean = seq(min(Nat$Ratio), max(Nat$Ratio), length.out = 6))
newdat$pred = predict(m2, newdata = newdat)

plot(rmean ~ (Ratio^2), ylim = c(9.3,10.1), main = "Natural Diet Model",
     xlab = "P:L Ratio", ylab = "Mean survival time (days)",
     frame = TRUE)
with(newdat, lines(x = Ratio, y = pred))


newdata = data.frame(Ratio=1)

predict(m2, Nat=1, interval ="confidence")


## Plot model

x <- Altered.diet.ratios$Protein
y <- Altered.diet.ratios$rmean

plot(x, y, xlim = c(0,600), ylim = c(10.2,11), main = "Altered Diet Model",
     xlab = "Protein concentration", ylab = "Mean survival time (days)",
     frame = TRUE)
abline(m2, col="black")

##making model for osmia mortality 

attach(OsmiaM)

##makes Control the comparison diet
OsmiaM$Diet <- as.factor(OsmiaM$Diet)

OsmiaM$Diet <- relevel(OsmiaM$Diet, "Control")

levels(OsmiaM$Diet)

##Makes the GLM with diet as a factor
model4 <- glm(formula = Survival ~ Diet, family = "binomial", data = OsmiaM)
summary(model4)

##Compares between treatment groups

library(multcomp)

summary(glht(model4, linfct=mcp(Diet="Tukey")))

cld(glht(model4, linfct=mcp(Diet="Tukey")))
