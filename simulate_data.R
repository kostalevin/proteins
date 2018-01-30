# simulate data - angles
means <- runif(n = 10, min = 0, max = 180)
angles <- matrix(data = rep(x = means, 100), ncol = 10, byrow = T)
noise <- rnorm(n = 1000, mean = 0, sd = 10)
angles <- angles + noise
angles <- ifelse(angles > 0 & angles < 180, angles, NA)
# angles <- angles %% 180.0

boxplot(angles)

library(ggplot2)
library(reshape2)

# transform data for plotting
angles_df <- melt(angles)
names(angles_df) <- c('observation', 'group', 'angle')
angles_df$group <- as.factor(angles_df$group)

ggplot(angles_df, mapping = aes(x = angle, colour = group)) + geom_density(adjust=2)
ggplot(angles_df, mapping = aes(y = angle, x = group)) + geom_boxplot()

