df <- read.csv("NY-House-Dataset.csv")
head(df)
df$log_price <- log1p(df$PRICE)  # log1p() to avoid log(0) issues
head(df,2)
# Create the histogram of the log-transformed PRICE
hist(df$log_price, 
     probability = TRUE,  # Density scale
     main = "Log-Transformed Distribution of House Prices with Normal Curve",
     xlab = "Log(Price)", 
     ylab = "Density", 
     col = "lightblue", 
     border = "black", 
     breaks = 50)

# Overlay the normal curve
log_mean <- mean(df$log_price)
log_sd <- sd(df$log_price)
curve(dnorm(x, mean = log_mean, sd = log_sd), 
      add = TRUE, 
      col = "red", 
      lwd = 2, 
      xlim = range(df$log_price))
# Create a scatter plot for PRICE vs PROPERTYSQFT
x <- df$PROPERTYSQFT
y <- df$PRICE
plot(x # independent variable
     ,y # dependent variable (MPG)
     , main = "Scatter Plot: PRICE vs PROPERTY SQFT"
     , xlab = "PROPERTSQFT (Sqft)" # x-axis label
     , ylab = "PRICE" # y-axis label
     , pch = 19 # point shape (filled circle)
     , frame = T # surround chart with a frame
     )
model <- lm(y ~ x, data = df) # compute the linear model
abline(model, col = "blue") # draw the model as a blue line
