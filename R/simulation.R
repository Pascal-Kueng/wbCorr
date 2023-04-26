# Load the MASS package
library(MASS)

n_obs <- 20
n_participants <- 80

proportion_missing <- 0.2

mean_mean1 <- 6
mean_mean2 <- 6
mean_mean3 <- 6

mean_sd1 <- 1
mean_sd2 <- 1
mean_sd3 <- 1

# Within-person correlations
mean_within_rho12 <- 0.1
mean_within_rho13 <- 0.3
mean_within_rho23 <- 0.8

# Between-person correlations
mean_between_rho12 <- -0.5
mean_between_rho13 <- -0.4
mean_between_rho23 <- -0.1

# Between-person correlation matrix
between_person_corr_matrix <- matrix(c(1, mean_between_rho12, mean_between_rho13,
                                       mean_between_rho12, 1, mean_between_rho23,
                                       mean_between_rho13, mean_between_rho23, 1),
                                     ncol = 3)

# Generate random effects for each participant
set.seed(42)
random_effects <- mvrnorm(n_participants, mu = c(0, 0, 0), Sigma = between_person_corr_matrix)


# distribution of characteristics
mean1 <- rnorm(n_participants, mean = mean_mean1, sd = 0.1)
mean2 <- rnorm(n_participants, mean = mean_mean2, sd = 0.1)
mean3 <- rnorm(n_participants, mean = mean_mean3, sd = 0.1)

sd1 <- rnorm(n_participants, mean = mean_sd1, sd = 0.1)
sd2 <- rnorm(n_participants, mean = mean_sd2, sd = 0.1)
sd3 <- rnorm(n_participants, mean = mean_sd3, sd = 0.1)

within_rho12 <- rnorm(n_participants, mean = mean_within_rho12, sd = 0.01)
within_rho13 <- rnorm(n_participants, mean = mean_within_rho13, sd = 0.01)
within_rho23 <- rnorm(n_participants, mean = mean_within_rho23, sd = 0.01)

simulate_participant <- function(n_obs, means, sds, rhos, random_effects, proportion_missing = 0) {
  mean1 <- means[1] + random_effects[1]
  mean2 <- means[2] + random_effects[2]
  mean3 <- means[3] + random_effects[3]
  sd1 <- sds[1]
  sd2 <- sds[2]
  sd3 <- sds[3]

  rho12 <- rhos[1]
  rho13 <- rhos[2]
  rho23 <- rhos[3]

  mean_vector <- c(mean1, mean2, mean3)
  cov_matrix <- matrix(c(sd1^2, sd1*sd2*rho12, sd1*sd3*rho13,
                         sd2*sd1*rho12, sd2^2, sd2*sd3*rho23,
                         sd3*sd1*rho13, sd3*sd2*rho23, sd3^2),
                       ncol = 3)

  data <- mvrnorm(n_obs, mu=mean_vector, Sigma=cov_matrix)

  # Randomly replace values with NA
  if (proportion_missing > 0) {
    n_na <- round(n_obs * proportion_missing)
    if (n_na > 0) {
      na_idx1 <- sample(seq_len(n_obs), n_na/3)
      na_idx2 <- sample(seq_len(n_obs), n_na/3)
      na_idx3 <- sample(seq_len(n_obs), n_na/3)
      data[na_idx1, 1] <- NA
      data[na_idx2, 2] <- NA
      data[na_idx3, 3] <- NA
    }
  }

  return(data)
}

data = data.frame()
for (i in 1:n_participants) {
  participant_df <- data.frame(
    simulate_participant(n_obs,
                         means = c(mean1[i],
                                   mean2[i],
                                   mean3[i]),
                         sds = c(sd1[i],
                                 sd2[i],
                                 sd3[i]),
                         rhos = c(within_rho12[i],
                                  within_rho13[i],
                                  within_rho23[i]),
                         random_effects = random_effects[i,],
                         proportion_missing = proportion_missing))
  participant_df$participantID <- i

  colnames(participant_df) <- c("Var1", "Var2", "Var3", "participantID")

  participant_df$day <- 1:n_obs
  participant_df$var4 <- i
  participant_df$var5 <- paste('Nummer', i)
  data = rbind(participant_df, data)
}


library(wbCorr)
wbCorrObject <- wbCorr(data,
                         data$participantID,
                         method = "pearson")
wbCorrObject
get_tables(wbCorrObject)$between

a <- prettystatsBy::statsBy(data = data, group = data$participantID)
a$ci.bg
