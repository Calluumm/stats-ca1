setwd(~) #set my working dir
#install.packages("ggplot2") #comment out if installed
library(ggplot2) #use the library :D
## Trim original file down to a trimmed state
untrimmed <- read.csv("global_bleaching_environmental_untrimmed.csv", stringsAsFactors = FALSE)
vars <- c("Depth_m", "Percent_Bleaching", "Ocean_Name")
trimmed <- untrimmed[, vars]
trimmed <- trimmed[trimmed$Ocean_Name == "Indian", ]
trimmed <- trimmed[!is.na(trimmed$Ocean_Name), ]
write.csv(trimmed, "trim_set.csv", row.names = FALSE)
## Read data and filter it a bit
data <- read.csv("trim_set.csv", stringsAsFactors = FALSE) #set our trimmed dataset as the data variable
data$Depth_m <- num(data$Depth_m) #set the depth as numeric
data$Percent_Bleaching <- num(data$Percent_Bleaching) #set the percent bleaching as numeric
data <- na.omit(data[, c("Depth_m","Percent_Bleaching")]) #omit na values in our 2 columns
##Initial test and interpretations
print(length(data$Percent_Bleaching)) #it's 1986, doesn't exceed the 5000 limit for shapiro
print(shapiro.test(data$Percent_Bleaching)) #not normal, let's transform
print(shapiro.test(data$Depth_m)) #While the W value is close to 1 the tiny p value created from our large sample is indicating it is not normal
print(shapiro.test(log(data$Percent_Bleaching+1))) #not normal even when log (make sure to add 1 as you can't log 0)
print(shapiro.test(sqrt(data$Percent_Bleaching))) #not normal even when sqrt

##Graphing
#histogram making for % bleaching
orig <- data$Percent_Bleaching # original values
logp <- log1p(orig) #log transform
sq <- sqrt(orig) #sqrt transform
transdf <- data.frame( #dataframe of above 3 values
  value = c(orig, logp, sq),
  transform = factor(rep(c("original", "log1p", "sqrt"), each = length(orig)), levels = c("original","log1p","sqrt")) #labels for transform factors
)
#creates labels for our histograms for shapiro wilk test results
n_orig <- shapiro.test(orig)
n_logp <- shapiro.test(logp)
n_sqrt <- shapiro.test(sq)
labels_df <- data.frame( #makes a dataframe out of our labels and attacthes them to the respective transforms
  transform = factor(c("original","log1p","sqrt"), levels = c("original","log1p","sqrt")),
  label = c(
    sprintf("W=%.3f\np=%s", unname(n_orig$statistic), format.pval(n_orig$p.value, digits = 3)), #sprintf is just a string formatter, using placeholders filled
    sprintf("W=%.3f\np=%s", unname(n_logp$statistic), format.pval(n_logp$p.value, digits = 3)), #by the shapiro test results (rounded to 3 dp)
    sprintf("W=%.3f\np=%s", unname(n_sqrt$statistic), format.pval(n_sqrt$p.value, digits = 3))
  )
)
his3 <- ggplot(transdf, aes(x = value)) + #creates a series of 3 histograms from the 3 values
  geom_histogram(fill = "gray", color = "black", bins = 30) + #colours it
  facet_wrap(~ transform, scales = "free_x", nrow = 1) + #wraps the 3 histograms into individual plots
  labs(title = "Percent Bleaching: Un-transformed with log and square root transforms respectively", #labels the plots
       x = "Transform specific values", y = "Frequency") + 
  theme_minimal() + #background theme
  geom_text(data = labels_df, mapping = aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE, hjust = 1.05, vjust = 1.2, size = 3) #adds the pre-made labels and attatches them to correct plots
ggsave("hist_bleach_withtransform.png", plot = his3, width = 12, height = 4, dpi = 300) #saves the hist series as a png

#depth histogram, exactly the same as above but for depth

orig <- data$Depth_m #the original value
logp <- log1p(orig) #original data in log(original+1) (thats what log1p does)
sq <- sqrt(orig) #original data square rooted
transdf <- data.frame( #create a data frame with the 3 values
  value = c(orig, logp, sq), #set the 3 values
  transform = factor(rep(c("original", "log1p", "sqrt"), each = length(orig)), levels = c("original","log1p","sqrt")) #factor their transforms
)
#does the same for depth as we did percent bleaching, testing for normality 
n_orig <- shapiro.test(orig) 
n_logp <- shapiro.test(logp)
n_sqrt <- shapiro.test(sq)
labels_df_depth <- data.frame(
  transform = factor(c("original","log1p","sqrt"), levels = c("original","log1p","sqrt")),
  label = c(
    sprintf("W=%.3f\np=%s", unname(n_orig$statistic), format.pval(n_orig$p.value, digits = 3)), 
    sprintf("W=%.3f\np=%s", unname(n_logp$statistic), format.pval(n_logp$p.value, digits = 3)),
    sprintf("W=%.3f\np=%s", unname(n_sqrt$statistic), format.pval(n_sqrt$p.value, digits = 3))
  )
)
his3 <- ggplot(transdf, aes(x = value)) + #create a historgram series from the 3 values
  geom_histogram(fill = "gray", color = "black", bins = 30) + #sets hist colour
  facet_wrap(~ transform, scales = "free_x", nrow = 1) + #wraps the 3 transforms into their own plots
  labs(title = "Depth: Un-transformed with log and square root transforms respectively", #labels for all 3 plots
       x = "Transform specific values", y = "Frequency") +
  theme_minimal() +
  geom_text(data = labels_df_depth, mapping = aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE, hjust = 1.05, vjust = 1.2, size = 3)
ggsave("hist_depth_withtransform.png", plot = his3, width = 12, height = 4, dpi = 300) #saves the hist series as a png


#Statistical testing for non-parametric, continuous data
#Spearman's rank correlation test
print(cor.test(data$Depth_m, data$Percent_Bleaching, method="spearman")) #rho of 0.025, p of 0.2618 #same as when we were in lab


# Scatter plot creation
p <- ggplot(data, aes(Depth_m, Percent_Bleaching)) + #plot a scatter between depth and percent bleaching
  geom_jitter(height = 0, width = 0.2, size = 1.6, alpha = 0.6, colour = "#2b8cbe") + #ensures points dont overlap
  #geom_smooth(method = "lm", se = FALSE, colour = "#f03b20", size = 0.8) + #regular (linear) line of best fit for linear models
  geom_smooth(method = "loess", se = TRUE, colour = "#045a8d", linetype = "dashed", size = 0.8, alpha = 0.5) + #LOESS line of best fit
  labs(title = "Percentage Bleaching vs Depth", x = "Depth (m)", y = "Percentage Bleaching (%)") + #title, x/y axis labels
  theme_minimal(base_size = 14) + #background theme
  coord_cartesian(ylim = c(0, 100)) #limits the y axis as % cant exceed 0 or 100 in this context, stops the LOESS line exceeding such
ggsave("scatter.png", plot = p, width = 9, height = 6, dpi = 300) #saves the plot as a png


