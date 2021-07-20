require(AER)
data("NMES1988")
Poisson_rg <- glm(visits ~ emergency + hospital + health + chronic +
                    adl + region + age + afam + gender + school, family="poisson", data=NMES1988)
summary(Poisson_rg)

require(msm)
cov.m1 <- vcovHC(Poisson_rg, type="HC0") # sandwitch variance estimator
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(Poisson_rg), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(Poisson_rg)/std.err), lower.tail=FALSE),
               LL = coef(Poisson_rg) - 1.96 * std.err,
               UL = coef(Poisson_rg) + 1.96 * std.err)

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6)), 
                 coef(Poisson_rg), cov.m1)
rexp.est <- exp(r.est[1:6, -3])

# replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est