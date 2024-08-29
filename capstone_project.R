# First try with mock data
x = c(1, 2, 3, 4, 5, 6, 7)
y = c(1, 3, 6, 2, 7, 4, 5)

result = cor(x, y, method = "pearson")

cat("Pearson correlation coefficient is: ", result)

# More info
result = cor.test(x, y, method = "pearson")
result

# First try with real data from TCC
data = read.csv("th25.csv")

x = data$automatic
y = data$manual

result = cor.test(x, y, method = "pearson")
result

# First try with plots
library(ggplot2)

# Scatter plot with correlation coefficient
ggplot(data = data, aes(x = automatic, y = manual)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = mean(data$automatic), y = max(data$manual), 
           label = paste("Correlation =", round(result$estimate, 2)), 
           color = "red", hjust = 0, vjust = 1) +
  labs(title = "Scatter Plot of Automatic vs. Manual with Correlation Coefficient", 
       x = "Automatic Analysis", y = "Manual Analysis") +
  theme_minimal()

# Doing some data processing...
rownames(data) <- data$video
colnames(data)[colnames(data) == "automatic"] <- "auto25p"

data2 = read.csv("th375.csv")
data3 = read.csv("th25.csv")

colnames(data2)[colnames(data2) == "automatic"] <- "auto375p"
colnames(data3)[colnames(data3) == "automatic"] <- "auto5p"

data <- merge(x = data, y = data2, by = c("video", "manual"))
data <- merge(x = data, y = data3, by = c("video", "manual"))

data <- subset(data, select = -video)

# Data distribution
boxplot(data, xlab = "Observer", ylab = "Duration (s)")

# Trying a multiple plot
par(mfrow=c(3,1))
plot(auto25p~manual, data=data)
plot(auto375p~manual, data=data)
plot(auto5p~manual, data=data)
dev.off()

# Correlation matrix
corrmat <- cor(data)
corrmat

# Trying different forms of visualization
library("ellipse")
plotcorr(corrmat, mar = c(1, 1, 1, 1))

library(ggcorrplot)
ggcorrplot(corrmat, type = "lower", lab = TRUE)
ggcorrplot(corrmat, method = "circle", type = "lower", lab = TRUE)

library("GGally")
ggpairs(data)
