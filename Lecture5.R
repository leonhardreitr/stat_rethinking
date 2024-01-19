library(tidyverse)
library(ggdag)
library(dagitty)
theme_set(ggthemes::theme_few() + theme(plot.title.position = "plot"))

#### Fork
# X and Y share common cause Z

fork_dag <- dagitty("dag{
  Z -> Y ;
  Z -> X  }")
ggdag(fork_dag)

# We see that X is _||_ Y | Z
impliedConditionalIndependencies(fork_dag)

fork_tib <- tibble(
  n = 1000,
  Z = rethinking::rbern(n, 0.5),
  X = rethinking::rbern(n, (1 - Z) * 0.1 + Z * 0.9),
  Y = rethinking::rbern(n, (1 - Z) * 0.1 + Z * 0.9)
)

# we see without controlling or Z, X and Y appear to be correlated
cor(fork_tib$X, fork_tib$Y)


# how ever, as soon as we controll for Z there is no correlation anymore

cat("Dude where is my corrlation, it is only", cor(fork_tib$X[fork_tib$Z == 0], fork_tib$Y[fork_tib$Z == 0])[1])

cat("Dude where is my corrlation, it is only", cor(fork_tib$X[fork_tib$Z == 1], fork_tib$Y[fork_tib$Z == 1])[1])

# make continuous to plot
fork_tib <- tibble(
  n = 300,
  Z = rethinking::rbern(n, 0.5),
  X = rnorm(n, 2 * Z - 1),
  Y = rnorm(n, 2 * Z - 1)
)


fork_tib |>
  ggplot(aes(X, Y)) +
  geom_point(
    shape = 1, aes(col = as.factor(Z)),
    size = 3, alpha = .7
  ) +
  geom_smooth(method = "lm", se = F, col = "black") +
  geom_smooth(aes(col = as.factor(Z)),
    method = "lm", se = F, lty = "dashed"
  ) +
  scale_color_manual("",
    values = c("firebrick", "steelblue"),
    labels = c("Z = 0", "Z = 1")
  ) +
  theme(
    legend.position = "top"
  )

lm(data = fork_tib, Y ~ X + Z)


# Fork example ------------------------------------------------------------
# The causes ar not in the data
library(patchwork)
data(WaffleDivorce, package = "rethinking")
d <- WaffleDivorce
cor(d$Marriage, d$Divorce)


p1 <-
  dagitty("dag{
  M -> D}") |>
  ggdag() + ggtitle("This one?") + theme_dag_blank()

dag_div <-
  dagitty("dag{
  M -> D;
  A -> D;
  A -> M}")

p2 <-
  ggdag(dag_div) + ggtitle("Or this one?") + theme_dag_blank()

p1 + p2

# Break the FORK!!!!

library(brms)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d <- d |> 
  mutate(D = rethinking::standardize(Divorce),
         A = rethinking::standardize(MedianAgeMarriage),
         M = rethinking::standardize(Marriage))

# prior predictive check
n <- 90
a <- rnorm(n, 0, 10)
bM <- rnorm(n,0, 10)
bA <- rnorm(n,0, 10)

plot(NULL, xlim = c(-2,2), ylim = c(-2,2))

Aseq <- seq(from = -3, to = 3, len = 30)
for (i in 1:n) {
  mu <- a[i] + bA[i] * Aseq
  lines(Aseq, mu, lwd = 2, col = "pink")
}

# better prior
n <- 90
a <- rnorm(n, 0, .2)
bM <- rnorm(n,0, .5)
bA <- rnorm(n,0, .5)

plot(NULL, xlim = c(-2,2), ylim = c(-2,2),
xlab = "Median Age of Mariage",
ylab = "Divorce Rate")

Aseq <- seq(from = -3, to = 3, len = 30)
for (i in 1:n) {
  mu <- a[i] + bA[i] * Aseq
  lines(Aseq, mu, lwd = 2, col = "pink")
}

# Model

m1 <- brm(
  D ~ M + A,
  data = d,
  family = gaussian,
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(exponential(1), class = sigma)),
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  seed = 5,
  sample_prior = T)

library(easystats)
model_parameters(m1)

summary(m1)

# Pipe
# Z as a mediator
dag_pipe <-
  dagitty("dag{
  X -> Z;
  Z -> Y}")
ggdag(dag_pipe)

# Once we control for Z there is no more assoc
impliedConditionalIndependencies(dag_pipe)

pipe_tib <- tibble(
  n = 1000,
  X = rethinking::rbern(n, 0.5),
  Z = rethinking::rbern(n, (1 - X) * 0.1 + X * 0.9),
  Y = rethinking::rbern(n, (1 - Z) * 0.1 + Z * 0.9)
)

cat("My correlation is", cor(pipe_tib$X, pipe_tib$Y)[1])
cat("Nvm, my correlation is", cor(pipe_tib$X[pipe_tib$Z == 1], pipe_tib$Y[pipe_tib$Z == 1])[1])
cat("Nvm, my correlation is", cor(pipe_tib$X[pipe_tib$Z == 0], pipe_tib$Y[pipe_tib$Z == 0])[1])

pipe_tib <- tibble(
  n = 300,
  X = rnorm(n),
  Z = rethinking::rbern(n, rethinking:::inv_logit(X)),
  Y = rnorm(n, (2*Z-1)))

pipe_tib |> 
  ggplot(aes(X, Y)) +
  geom_point(
    shape = 1, aes(col = as.factor(Z)),
    size = 3, alpha = .7
  ) +
  geom_smooth(method = "lm", se = F, col = "black") +
  geom_smooth(aes(col = as.factor(Z)),
              method = "lm", se = F, lty = "dashed"
  ) +
  scale_color_manual("",
                     values = c("steelblue", "red"),
                     labels = c("Z = 0", "Z = 1")
  ) +
  theme(
    legend.position = "top"
  )

# is post treatment bias basicaly a pipe?

# The collider
# X and Y not assoc unless we stratify by Z
col_pipe <-
  dagitty("dag{
  X -> Z;
  Y -> Z}")
ggdag(col_pipe)

impliedConditionalIndependencies(col_pipe)

col_tib <- 
  tibble(
    n = 1000,
    x = rethinking::rbern(n),
    y = rethinking::rbern(n),
    z = rethinking::rbern(n, if_else(x + y > 0, .9,.2))
  )
cor(col_tib$x,col_tib$y)
cor(col_tib$x[col_tib$z == 1],col_tib$y[col_tib$z == 1])
cor(col_tib$x[col_tib$z == 0],col_tib$y[col_tib$z == 0])


col_tib <- 
  tibble(
    n = 100,
    x = rnorm(n),
    y = rnorm(n),
    z = rethinking::rbern(n, rethinking::inv_logit(2*x+2*y-2))
  )

col_tib |> 
  ggplot(aes(x, y)) +
  geom_point(
    shape = 1, aes(col = as.factor(z)),
    size = 3, alpha = .7
  ) +
  geom_smooth(method = "lm", se = F, col = "black") +
  geom_smooth(aes(col = as.factor(z)),
              method = "lm", se = F, lty = "dashed"
  ) +
  scale_color_manual("",
                     values = c("steelblue", "red"),
                     labels = c("Z = 0", "Z = 1")
  ) +
  theme(
    legend.position = "top"
  )

rethinking::sim_happiness() -> hap
cor(hap$happiness,hap$age)
cor(hap$happiness[hap$married == 1],hap$age[hap$married == 1])
cor(hap$happiness[hap$married == 0],hap$age[hap$married == 0])

# The descendant
des_pipe <-
  dagitty("dag{
  X -> Z;
  Z -> Y;
  Z -> A}")
ggdag(des_pipe)

impliedConditionalIndependencies(des_pipe)

des_tib <- 
  tibble(
    n = 1000,
    X = rethinking::rbern(n, 0.5),
    Z = rethinking::rbern(n, (1 - X) * 0.1 + X * 0.9),
    Y = rethinking::rbern(n, (1 - Z) * 0.1 + Z * 0.9),
    A = rethinking::rbern(n, (1 - Z) * 0.1 + Z * 0.9)
  )
des_tib |> filter(A == 0) |> 
  select(X,Y) |> corrr::correlate()
