# Good and bad controls ---------------------------------------------------
library(dagitty)
library(tidyverse)
library(ggdag)
library(brms)

theme_set(theme_dag())

dag1 <- 
dagitty("{
        X -> Y;
        U -> Y;
        U -> Z;
        Z -> X
        }") 

ggdag(dag1)
impliedConditionalIndependencies(dag1)


# Simulate confound y -----------------------------------------------------
n <- 200
b_xy <- 1
b_uy <- -1
b_uz <- -1
b_zx <- 1

set.seed(10)
u <- rethinking::rbern(n)
z <- rnorm(n, b_uz * u)
x <- rnorm(n, b_zx*z)
y <- rnorm(n, b_xy*x + b_uy*u)
d <- list(x = x, y = y, z = z)

m1 <- brm(data = d,
          family = gaussian,
          y ~ 1 + x)
m1

m2 <- brm(data = d,
          family = gaussian,
          y ~ 1 + x + z)

library(easystats)
plot(describe_posterior(m1))

get_parameters(m1) -> post1
get_parameters(m2) -> post2


ggplot(post1, aes(x = b_x)) +
  geom_density(fill = "orange")



# Good and bad controls --------------------------------------------------

# "Yolo Strategy" lmao, sad but super funny

dagitty("{
        X -> Z;
        Z -> Y;
        U -> Z;
        U -> Y
        }") |> ggdag()

f <- function(n = 100, bXZ = 1, bZY = 1) {
  x <- rnorm(n)
  u <- rnorm(n)
  z <- rnorm(n, bXZ * x + u)
  y <- rnorm(n, bZY * z + u)
  bX <- coef(lm(y~x))[1]
  bXZ <- coef(lm(y~x+z))[2]
  return(c(bX,bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 10)
dens(sim[1, ], lwd = 3, col = 2, xlab = "Posterior Mean", ylim = c(0,3))
dens(sim[2,], lwd = 3, col = 1, add = T)



# DO NOT TOUCH THAT COLLIDER ----------------------------------------------

dagitty("{
        X -> Z;
        u -> Z;
        Z -> Y;
        X -> Y
        u [unobserved]
        }") |> ggdag()
 

# Case Control bias -------------------------------------------------------

# do not ascendance of your outcome to your model !!!

dagitty("{
        Pred -> Outcome;
        Outcome -> ascendance
        }") |> ggdag()
                     
f <- function(n = 100, bXY = 1, bYZ = 1) {
  x <- rnorm(n)
  y <- rnorm(n, bXY * x)
  z <- rnorm(n, bYZ * y)
  bX <- coef(lm(y~x))['x']
  bXZ <- coef(lm(y~x+z))['x']
  return(c(bX,bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 10)
dens(sim[1, ], lwd = 3, col = 2, xlab = "Posterior Mean", ylim = c(0,9),
     xlim = c(0,2))
dens(sim[2,], lwd = 3, col = 1, add = T)


# Precision Parasite -------------------------------------------------------
dagitty("{
        Z -> X;
        X -> Y
        }") |> ggdag()

f <- function(n = 100, bZX = 1, bXY = 1) {
  z <- rnorm(n)
  x <- rnorm(n, bZX * z)
  y <- rnorm(n, bXY * x)
  bX <- coef(lm(y~x))['x']
  bXZ <- coef(lm(y~x+z))['x']
  return(c(bX,bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 10)
dens(sim[1, ], lwd = 3, col = 2, xlab = "Posterior Mean", ylim = c(0,9),
     xlim = c(0,2))
dens(sim[2,], lwd = 3, col = 1, add = T)

# Bias Amplification -------------------------------------------------------
dagitty("{
        Z -> X;
        X -> Y;
        X <- u -> Y
        }") |> ggdag()

f <- function(n = 100, bZX = 1, bXY = 1) {
  z <- rnorm(n)
  u <- rnorm(n)
  x <- rnorm(n, bZX * z + u)
  y <- rnorm(n, bXY * x + u )
  bX <- coef(lm(y~x))['x']
  bXZ <- coef(lm(y~x+z))['x']
  return(c(bX,bXZ))
}

sim <- mcreplicate(1e4, f(bXY = 0), mc.cores = 10)
dens(sim[1, ], lwd = 3, xlab = "Posterior Mean", ylim = c(0,6),
     xlim = c(-.5,2))
dens(sim[2,], lwd = 3, col = 2, add = T)

tibble(
  n = 1000,
  z = rbern(n),
  u = rnorm(n),
  x = rnorm(n, 7 * z + u),
  y = rnorm(n,  0 * x +u)
) |> 
  mutate(z = fct(as.character(z))) |> 
  ggplot(aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = F, col = "black") +
  geom_point(shape = 1, size = 3, aes(col = z), alpha = .4) +
  geom_smooth(aes(col = z),
              se = F,
              method = "lm") + theme_bw()
