# First try with mock data
x = c(1, 2, 3, 4, 5, 6, 7)
y = c(1, 3, 6, 2, 7, 4, 5)

result = cor(x, y, method = "pearson")

cat("Pearson correlation coefficient is:", result)

result = cor.test(x, y, method = "pearson")

print(result)

# First try with real data from TCC
data = read.csv("comparison_th5.csv")

x = data$automatic
y = data$manual

result = cor.test(x, y, method = "pearson")

print(result)

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
data2 = read.csv("comparison_th375.csv")
data3 = read.csv("comparison_th025.csv")

# Rename column
colnames(data)[colnames(data) == "automatic"] <- "auto5p"
colnames(data2)[colnames(data2) == "automatic"] <- "auto375p"
colnames(data3)[colnames(data3) == "automatic"] <- "auto25p"

data <- merge(x = data, y = data2, by = c("video", "manual"))
data <- merge(x = data, y = data3, by = c("video", "manual"))

rownames(data) <- data$video
data <- subset(data, select = -video)

# Trying a multiple plot
par(mfrow=c(3,1))
plot(auto5p~manual, data=data)
plot(auto375p~manual, data=data)
plot(auto25p~manual, data=data)

dev.off()

corrmat <- cor(data)
corrmat

library(ggcorrplot)

ggcorrplot(corrmat, type = "lower", lab = TRUE)

library("ellipse")

plotcorr(corrmat, mar = c(1, 1, 1, 1))

library("GGally")
ggpairs(data)

relu <- function(x){
  return(pmax(0,x))
}

x <- seq(-2, 2, length.out = 41)

relu_values <- relu(x)

ggplot(data.frame(x = x, y = relu_values), aes(x = x, y = y)) +
  geom_line(size = 1.5, color = "blue") +
  labs(
    title = "Função ReLU",
    x = "x",
    y = "ReLU(x)"
  ) +
  theme_minimal()

ggplot(data) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

boxplot(data, xlab = "Observer", ylab = "Duration (s)")

pearsonmat <- cor.test(data$manual, data$auto25p)
print(pearsonmat)
