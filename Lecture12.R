library(rethinking)
data("reedfrogs")
d <- reedfrogs

str(d)

d$tank <- 1:nrow(d)

dat <- list(
  S = d$surv,
  T = d$tank,
  D = d$density
)

mST <- ulam(
  alist(
    S ~ dbinom(D, p),
    logit(p) <- a[T],
    a[T] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = dat, chains = 4, cores = detectCores(), log_lik = T
)

precis(mST, 2)

mSTnomem <- ulam(
  alist(
    S ~ dbinom(D, p),
    logit(p) <- a[T],
    a[T] ~ dnorm(a_bar, 1),
    a_bar ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 4, log_lik = TRUE
)

precis(mSTnomem, 2)


compare(mST, mSTnomem, func = WAIC)


plot(d$propsurv,
  ylim = c(0, 1), pch = 16, xaxt = "n",
  xlab = "tank", ylab = "proportion survival", col = 1, yaxt = "n", cex = 1.2
)
axis(1, at = c(1, 16, 32, 48), labels = c(1, 16, 32, 48))
axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
# abline( h=mean(d$propsurv) , lwd=3 , col=1 , lty=3 )
# draw vertical dividers between tank densities
abline(v = 16.5, lwd = 0.5)
abline(v = 32.5, lwd = 0.5)
text(8, 0, "small tanks")
text(16 + 8, 0, "medium tanks")
text(32 + 8, 0, "large tanks")

post <- extract.samples(mST)
a_est <- apply(inv_logit(post$a), 2, mean)

# cols <- rep(2,48)
cols <- ifelse(d$pred == "pred", 2, 4)

for (i in 1:48) {
  pi <- PI(inv_logit(post$a[, i]), 0.89)
  lines(c(i, i), pi, lwd = 8, col = col.alpha(cols[i], 0.5))
}
points(1:48, a_est, lwd = 3, col = cols)
abline(a = .74, b = 0, lty = "dashed")

# with predators
dat$P <- ifelse(d$pred == "pred", 1, 0)
mSTP <- ulam(
  alist(
    S ~ dbinom(D, p),
    logit(p) <- a[T] + bP * P,
    bP ~ dnorm(0, 0.5),
    a[T] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = dat, chains = 4, log_lik = TRUE
)

post <- extract.samples(mSTP)
dens(post$bP, lwd = 4, col = 2, xlab = "bP (effect of predators)")


