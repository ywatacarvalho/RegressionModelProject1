
data(mtcars)
help(mtcars)

str(mtcars)

table(factor(mtcars$carb))
table(factor(mtcars$gear))

mtcars$am <- factor(mtcars$am, labels = c("automatic", "manual"));

par(mfrow = c(2,2), mar = c(4,4,2,1));

with(mtcars, plot(wt, mpg, main = "Miles per galon vs weight"));
with(mtcars, plot(hp, mpg, main = "Miles per galon vs hp"));
with(mtcars, boxplot(mpg ~ carb, col = "blue", ylab = "mpg", main = "Miles per galon vs carburetors"));
with(mtcars, boxplot(mpg ~ cyl, col = "green", ylab = "mpg", main = "Miles per galon vs cylinders"));





par(mfrow = c(2,2), mar = c(4,4,2,1));

with(mtcars, plot(wt, mpg, main = "Miles per galon vs weight", type="n"));
with(subset(mtcars, am == "automatic"), points(wt, mpg, col = "blue"));
with(subset(mtcars, am == "manual"), points(wt, mpg, col = "red"));
legend("topright", pch=1, col = c("blue", "red"), legend = c("automatic", "manual"));

with(mtcars, plot(hp, mpg, main = "Miles per galon vs hp", type="n"));
with(subset(mtcars, am == "automatic"), points(hp, mpg, col = "blue"));
with(subset(mtcars, am == "manual"), points(hp, mpg, col = "red"));
legend("topright", pch=1, col = c("blue", "red"), legend = c("automatic", "manual"));

with(mtcars, boxplot(wt ~ am, col = "blue", ylab = "weight", main = "Miles per galon vs transmission"));

with(mtcars, boxplot(hp ~ am, col = "red", ylab = "hp", main = "Miles per galon vs transmission"));



m1 <- lm(mpg ~ am + factor(cyl) + wt + disp + 
               hp + drat + factor(gear) + qsec +
               vs + factor(carb), data = mtcars)

m2 <- lm(mpg ~ factor(am) + factor(cyl) + wt + disp + 
             hp + drat + factor(gear) + qsec +
             vs, data = mtcars)

m3 <- lm(mpg ~ factor(am) + factor(cyl) + wt + disp + 
             hp + drat + qsec +
             vs, data = mtcars);

m4 <- lm(mpg ~ factor(am) + factor(cyl) + wt + 
             hp, data = mtcars);
summary(m4)

m5 <- lm(mpg ~ factor(am) + wt + 
             hp, data = mtcars);
summary(m5)

anova(m1, m2, m3, m4, m5)
anova(m4, m5)
