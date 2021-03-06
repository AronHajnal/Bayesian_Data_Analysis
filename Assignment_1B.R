set.seed(1)
 

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

mean_beta=rep(NaN,3)
sd_beta=rep(NaN,3)
x = seq(0,1,0.05)
beta_v = rbeta(1000, 10, 7)  # change normal to beta
mean_beta[1] = mean(beta_v)  # change median to mean
sd_beta[1] = sd(beta_v)
hist(beta_v, breaks = x, freq = F, ylim = c(0,11), col = "blue", xlab = "beta hist")
abline(v = mean_beta[1], col="red", lwd = 4)
text(mean_beta[1]+0.2, y = 7.5, labels = paste("mean: ", toString(round(mean_beta[1], 3))))



beta_v = rbeta(1000, 50, 35)  # change normal to beta
mean_beta[2] = mean(beta_v)  # change median to mean
sd_beta[2] = sd(beta_v)
hist(beta_v, breaks = x, freq = F, ylim = c(0,11), col = "blue", xlab = "beta hist")
abline(v = mean_beta[2], col="red", lwd = 4)
text(mean_beta[2]+0.2, y = 7.5, labels = paste("mean: ", toString(round(mean_beta[2], 3))))

beta_v = rbeta(1000, 100, 70)  # change normal to beta
mean_beta[3] = mean(beta_v)  # change median to mean
sd_beta[3] = sd(beta_v)
hist(beta_v, breaks = x, freq = F, ylim = c(0,11), col = "blue", xlab = "beta hist")
abline(v = mean_beta[3], col="red", lwd = 4)
text(mean_beta[3]+0.2, y = 7.5, labels = paste("mean: ", toString(round(mean_beta[3], 3))))


errbar(c(17, 85, 170), mean_beta, mean_beta-sd_beta, mean_beta+sd_beta, type = "b", col="blue", errbar.col = "red", ylim = c(0.5, 0.7))
title("mean of the beta dist. as a function of (alpha+beta)")


dev.print(file="homework.png", png, height = 800,
          width=800)
dev.off() 
