library(car)
library(rgl)

atoms_count <- 8
steps_count <- 30

set.seed(1)
data <- data.frame(atom = LETTERS[1:atoms_count],
                   x = runif(atoms_count),
                   y = runif(atoms_count),
                   z = runif(atoms_count))

data <- data[rep(1:nrow(data), each = steps_count),]

data$x <- data$x + rnorm(1:atoms_count, mean = 0, sd = 0.02)
data$y <- data$y + rnorm(1:atoms_count, mean = 0, sd = 0.02)
data$z <- data$z + rnorm(1:atoms_count, mean = 0, sd = 0.02)

scatter3d(x = data$x, y = data$y, z = data$z, groups = data$atom, 
          ellipsoid = TRUE, grid = T, ellipsoid.alpha = 0.1, 
          surface = F, sphere.size = 0.1, fogtype = 'linear')


aggregate(data[,2:4], by=list(data$atom), c(mean, sd))

