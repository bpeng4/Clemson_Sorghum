library(ggplot2)
library(ggpmisc)
Weight<-read.csv(file = "/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Weight_Association/ClemsomRun1_4 CSV OUT.csv")
CT<-read.csv(file = "/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Weight_Association/Highlight_Clemsom.csv")
Merged<-merge(Weight, CT, by.x = "ID", by.y = "ID")
Merged$CT <- as.numeric(Merged$CT)
ggplot(Merged, aes(x = CT , y = Amount)) +
  geom_point() +  # Add dots
  geom_smooth(formula = y ~ x,method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, size =6) +  # Display equation and R-squared
  theme_minimal() +
  labs(x = "CT", y = "Weight(mg)", title = "Dot Plot with Linear Regression") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 17), # Adjust x-axis title font size
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )


