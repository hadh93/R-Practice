x <- rnorm(100)  # random sample from normal distribution (x100)

dens <- density(x)    # calculate density
dens

plot(dens)            # plot density

