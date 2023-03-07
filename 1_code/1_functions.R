
# data generation functions -----------------------------------------------

# simulation 1
# function to generate data from simulation process
gendata1 <- function(
    N_train = 1000,  # number of training observations
    N_test = 1000,   # number of observations in test set
    N_truth = 1000,  # number of observations for calculating true/oracle MSE
    sims = 1         # number of simulations
) {
  
  N <- N_train + N_test + N_truth
  X <- runif(N * sims, 0, 10)
  A <- rbinom(N * sims, 1, plogis(-1.5 + 0.3 * X))
  
  split <- factor(x = rep(c(
    rep("train", N_train),
    rep("test", N_test),
    rep("truth", N_truth)
  ),
  sims))
  
  Y <- 1 + X + 0.5 * X^2 - 3 * A * I(split != "truth") + rnorm(N * sims)
  
  dt <- data.table(
    sim = rep(1:sims, each = N), 
    split = split,
    X = X,
    A = A,
    Y = Y
  )
  
  setkey(dt, sim)
  
  return(dt)
}


# simulation 2
# function to generate data from simulation process
gendata2 <- function(
    N_train = 1000,  # number of training observations
    N_test = 1000,   # number of observations in test set
    N_truth = 1000,  # number of observations for calculating true/oracle MSE
    sims = 1         # number of simulations
) {
  
  N <- N_train + N_test + N_truth
  X <- mvrnorm(
    N * sims, 
    c(0.2, 0, 0.5),  
    diag(c(0.2, 0.2, 0.2))
    #sapply(1:3, function(x) 0.25^abs(1:3 - x))
  )
  
  A <- rbinom(
    N * sims, 
    1, 
    plogis(0.5 - 2 * X[, 1] + 3 * X[, 1]^2 + 
             2 * X[, 2] - X[, 3])
  )
  
  split <- factor(rep(c(
    rep("train", N_train),
    rep("test", N_test),
    rep("truth", N_truth)
  ),
  sims))
  
  A[split == "truth"] <- 0
  
  Y <- rbinom(
    N * sims, 
    1, 
    plogis(0.2 + 3 * X[, 1] -  2 * X[, 1]^2 + 2 * X[, 2] +
           + X[, 3] - 2 * A * I(split != "truth"))
  )
  
  dt <- data.table(
    sim = rep(1:sims, each = N), 
    split = split,
    X = as.data.table(X),
    A = A,
    Y = Y
  )
  
  setkey(dt, sim)
  
  return(dt)
}

# simulation functions ----------------------------------------------------

# simulation 1
# fit models to each sim
runsim1 <- function(df) {
  
  # weight models
  pA <-
    lapply(c("train", "test"), function(x) {
      fit <- glm(
        A ~ X,
        family = binomial(link = "logit"),
        data = subset(df, split == x)
      )
      
      predict(fit, 
              newdata = subset(df, split == x & A == 0), 
              type = "response")
    })
  
  names(pA) <- c("train", "test")
  
  # prediction models
  models <-
    list(
      naive_miss = 
        glm(
          Y ~ X,
          family = gaussian(link = "identity"),
          data = subset(df, split == "train")
        ),
      naive_correct = 
        glm(
          Y ~ X + I(X^2),
          family = gaussian(link = "identity"),
          data = subset(df, split == "train")
        ),
      ipw_miss =
        glm(
          Y ~ X,
          family = gaussian(link = "identity"),
          weights = 1 / (1 - pA[['train']]),
          data = subset(df, split == "train" & A == 0)
        ),
      ipw_correct =
        glm(
          Y ~ X + I(X^2),
          family = gaussian(link = "identity"),
          weights = 1 / (1 - pA[['train']]),
          data = subset(df, split == "train" & A == 0)
        )
    )
  
  
  # MSE 
  unweighted <- lapply(models, function(fit) {
    err <- df$Y[df$split == "test"] - 
      predict(fit, newdata = subset(df, split == "test"))
    
    mean(err^2)
  })
  
  weighted <- lapply(models, function(fit, pA) {
    err <- df$Y[df$split == "test" & df$A == 0] - 
      predict(fit, newdata = subset(df, split == "test" & A == 0))
    
    weighted.mean(err^2, 1 / (1 - pA[['test']]))
  }, pA = pA)
  
  truth <- lapply(models, function(fit) {
    err <- df$Y[df$split == "truth"] - 
      predict(fit, newdata = subset(df, split == "truth"))
    
    mean(err^2)
  })
  
  
  return(c(unweighted, weighted, truth))
}

# simulation 2
# fit models to each sim
runsim2 <- function(df, p) {
  
  # update progress bar
  p()
  
  # train prediction model
  model <- glm(
    Y ~ X.V1 + X.V2 + X.V3,
    family = binomial(link = "logit"),
    data = subset(df, split == "train")
  )
  
  # fitted predictions in the test set
  gX <- predict(model, 
                newdata = subset(df, split == "test"), 
                type = "response")
  
  # oracle prediction model
  # oracle_model <- glm(
  #   Y ~ X.V1 + X.V2 + X.V3,
  #   family = binomial(link = "logit"),
  #   data = subset(df, split == "truth")
  # )
  
  # fitted predictions in the test set
  oracle <- predict(model,
                    newdata = subset(df, split == "truth"), 
                    type = "response")
  
  # weight models for MSE
  Afits <- list(
    correct = glm(
      A ~ X.V1 + I(X.V1^2) + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    ),
    main_effects = glm(
      A ~ X.V1 + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    ),
    gam = gam(
      A ~ s(X.V1) + s(X.V2) + s(X.V3),
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    )
  )
  
  # conditional loss models for MSE
  hfits <- list(
    correct = glm(
      Y ~ X.V1 + I(X.V1^2) + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    ),
    main_effects = glm(
      Y ~ X.V1 + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    ),
    gam = gam(
      Y ~ s(X.V1) + s(X.V2) + s(X.V3),
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    )
  )
  
  # propensity scores
  pA <- lapply(Afits, function(fit) {
    predict(fit, 
            newdata = subset(df, split == "test" & A == 0), 
            type = "response")
  })
  
  names(pA) <- names(Afits)
  
  
  # conditional loss estimates  
  hL <- lapply(hfits, function(fit, gX) {
    pY <- predict(fit, 
                  newdata = subset(df, split == "test"), 
                  type = "response")
    
    pY - 2 * gX * pY + gX^2
  }, gX = gX)
  
  names(hL) <- names(hfits)
  
  # MSE 
  naive <- list(
    mean((df$Y[df$split == "test"] - gX)^2)
  )
  
  condlost <- lapply(hL, function(x) mean(x))
  
  weighted <- lapply(pA, function(pA, gX) {
    obs <- which(df$split == 'test' & df$A == 0)
    test_obs <- with(subset(df, split == 'test'), which(A == 0))
    
    err <- df$Y[obs] - gX[test_obs]
    
    weighted.mean(err^2, 1 / (1 - pA))
  }, gX = gX)
  
  specs <- list(
    list("correct", "correct"),
    list("correct", "main_effects"),
    list("main_effects", "correct"),
    list("main_effects", "main_effects"),
    list("gam", "gam")
  )
  
  # fix this
  dr <- lapply(specs, function(spec, pA, hL, gX) {
    obs <- which(df$split == 'test' & df$A == 0)
    test_obs <- with(subset(df, split == 'test'), which(A == 0))
    
    eif <- rep(0, nrow(subset(df, split == 'test')))
    
    eif[test_obs] <- 1 / (1 - pA[[spec[[2]]]]) *
      ((df$Y[obs] - gX[test_obs])^2 - hL[[spec[[1]]]][test_obs])
    
    eif <- hL[[spec[[1]]]] + eif
    
    mean(eif)
  }, pA = pA, hL = hL, gX = gX)
  
  mses_truth <- list(mean((df$Y[df$split == "truth"] - oracle) ^ 2))
  
  mses <- c(naive, condlost, weighted, dr, mses_truth)
  
  # AUC
  auc_naive <- cal_auc(df$Y[df$split == "test"], gX)
  auc_true <- cal_auc(df$Y[df$split == "truth"], oracle)
  
  aucs <- lapply(specs, function(spec) {
    ret <- cal_counter_auc(
      df = subset(df, split == 'test'), 
      pred = predict(model,
                     type = "response", 
                     newdata = subset(df, split == 'test')),
      A.fit = Afits[[spec[[2]]]],
      Y.fit = hfits[[spec[[1]]]],
      se.fit = FALSE
    )
    ret[seq(1, length(ret), by = 2)]
  })
  
  aucs <- list(
    auc_naive[1],
    list(
      "correct" = aucs[[1]][[2]],
      "main_effects" = aucs[[4]][[2]],
      "gam" = aucs[[5]][[2]]
    ),
    list(
      "correct" = aucs[[1]][[1]],
      "main_effects" = aucs[[4]][[1]],
      "gam" = aucs[[5]][[1]]
    ),
    list(
      aucs[[1]][[3]],
      aucs[[2]][[3]],
      aucs[[3]][[3]],
      aucs[[4]][[3]],
      aucs[[5]][[3]]
    ),
    auc_true[1]
  )
  
  return(c(mses, aucs))
}


# auc functions -----------------------------------------------------------

# calculate AUC
cal_auc = function(y, p) {
  r <- rank(c(p[y == 1], p[y == 0]))
  np <- sum(y == 1)
  nn <- sum(y == 0)
  auc <- (sum(r[1:np]) - np * (np + 1) / 2) / (np * nn)
  q1 <- auc / (2 - auc)
  q2 <- 2 * auc ^ 2 / (1 + auc)
  se <- sqrt((auc * (1 - auc) + (np - 1) * (q1 - auc ^ 2) + 
                (nn - 1) * (q2 - auc ^ 2)) / np / nn)
  auc_res <- list(auc, se)
  names(auc_res) <- c("auc", "se")
  return(auc_res)
}

# calculate counterfactual AUC
cal_counter_auc <- function(df, pred, A.fit, Y.fit, se.fit = TRUE) {
  n <- nrow(df)
  
  # propensity scores
  pi_hat <- 1 - predict(A.fit, type = "response", newdata = df)
  if (inherits(A.fit, "gam")) {
    xpi = predict.gam(A.fit, type = "lpmatrix", newdata = as.data.frame(df))
    pi_hat <- as.vector(pi_hat)
  } else {
    xpi = model.matrix(formula(A.fit), data = df)
  }
  pi_ratio <- 1 / pi_hat

  # outcome predictions
  q_hat <- predict(Y.fit, type = "response", newdata = df)
  if (inherits(Y.fit, "gam")) {
    xq = predict.gam(Y.fit,  type = "lpmatrix", newdata = as.data.frame(df))
    q_hat <- as.vector(q_hat)
  } else {
    xq = model.matrix(formula(Y.fit), data = df)
  }
  
  # get all pairs
  ind_f <- outer(pred, pred, ">")
  
  # calculate using the matrix form
  mat_ipw0 = diag(df$A==0&df$Y==1)%*%(pi_ratio%*%t(pi_ratio))%*%diag(df$A==0&df$Y==0)
  mat_ipw1 = mat_ipw0*ind_f
  
  mat_om0 = q_hat%*%t(1-q_hat)
  mat_om1 = mat_om0*ind_f
  
  mat_dr0 = diag(df$A==0)%*%(pi_ratio*q_hat)%*%(t(pi_ratio)*t(1-q_hat))%*%diag(df$A==0); diag(mat_dr0) = 0
  mat_dr1 = mat_dr0*ind_f
  
  if (se.fit) {
    # calculate matrices for standard error estimate
    mat_ipw0x = array(0, c(n, n, ncol(xpi))); mat_ipw1x = array(0, c(n, n, ncol(xpi)))
    mat_dr0xi = array(0, c(n, n, ncol(xpi))); mat_dr1xi = array(0, c(n, n, ncol(xpi)))
    for (i in 1:ncol(xpi)){
      out_xpi = -outer(xpi[, i], xpi[, i], "+")
      mat_ipw0x[, , i] = out_xpi*mat_ipw0
      mat_dr0xi[, , i] = out_xpi*(mat_ipw0 - mat_dr0)
      mat_ipw1x[, , i] = mat_ipw0x[, , i]*ind_f
      mat_dr1xi[, , i] = mat_dr0xi[, , i]*ind_f
    }
    
    mat_om0x = array(0, c(n, n, ncol(xq))); mat_om1x = array(0, c(n, n, ncol(xq)))
    mat_dr0xo = array(0, c(n, n, ncol(xq))); mat_dr1xo = array(0, c(n, n, ncol(xq)))
    for (i in 1:ncol(xq)){
      out_xq = outer(xq[, i]*(1-q_hat), xq[, i]*q_hat, "-")
      mat_om0x[, , i] = out_xq*mat_om0
      mat_dr0xo[, , i] = out_xq*(mat_om0 - mat_dr0)
      mat_om1x[, , i] =  mat_om0x[, , i]*ind_f
      mat_dr1xo[, , i] = mat_dr0xo[, , i]* ind_f
    }
  }
  
  # iow estimator (ipw, ipw_se)
  ipw = sum(mat_ipw1)/sum(mat_ipw0)
  if (se.fit) {
    ga1 = -t(xpi)%*%diag(pi_hat*(1-pi_hat))%*%xpi
    ga2 = t(xpi*(df$A - pi_hat))
    q_ipw =  -(rowSums(mat_ipw0) + colSums(mat_ipw0))*ipw + rowSums(mat_ipw1) + colSums(mat_ipw1) +
      t(apply(mat_ipw0x, 3, sum)*ipw - apply(mat_ipw1x, 3, sum))%*%solve(ga1)%*%ga2
    ipw_se =  sqrt(sum(q_ipw^2))/sum(mat_ipw0)
  } else {
    ipw_se = NULL
  }
  
  # om estimator (om, om_se)
  om = sum(mat_om1)/sum(mat_om0)
  if (se.fit) {
    al1 = -t(xq)%*%diag(q_hat*(1-q_hat))%*%xq
    al2 = t(xq*(df$Y - q_hat))
    q_om =  -(rowSums(mat_om0) + colSums(mat_om0))*om + rowSums(mat_om1) + colSums(mat_om1) +
      t(apply(mat_om0x, 3, sum)*om - apply(mat_om1x, 3, sum))%*%solve(al1)%*%al2
    om_se =  sqrt(sum(q_om^2))/sum(mat_om0)
  } else {
    om_se = NULL
  }
  
  
  # dr estimator (dr, dr_se)
  dr = (sum(mat_ipw1) + sum(mat_om1) - sum(mat_dr1))/(sum(mat_ipw0) + sum(mat_om0) - sum(mat_dr0))
  if (se.fit) {
    q_dr =  -(rowSums(mat_ipw0) + colSums(mat_ipw0) + rowSums(mat_om0) + colSums(mat_om0) - rowSums(mat_dr0) - colSums(mat_dr0))*dr +
      rowSums(mat_ipw1) + colSums(mat_ipw1) + rowSums(mat_om1) + colSums(mat_om1) - rowSums(mat_dr1) - colSums(mat_dr1) +
      t(apply(mat_dr0xi, 3, sum)*dr - apply(mat_dr1xi, 3, sum))%*%solve(ga1)%*%ga2 +
      t(apply(mat_dr0xo, 3, sum)*dr - apply(mat_dr1xo, 3, sum))%*%solve(al1)%*%al2
    dr_se =  sqrt(sum(q_dr^2))/(sum(mat_ipw0) + sum(mat_om0) - sum(mat_dr0)) 
  } else {
    dr_se = NULL
  }
  
  return(est = list(ipw = ipw, ipw_se = ipw_se, 
                    om = om, om_se = om_se, dr = dr, dr_se = dr_se))
}


# bootstrap function ------------------------------------------------------

bootfun <- function(data, p) {
  
  # update progress
  p()
  
  # bootstrap replicates 
  data <- data[sample(1:nrow(mesa), nrow(mesa), replace = TRUE), ]
  
  # split into training and test
  split <- sample(1:nrow(data), floor(length(1:nrow(data)) / 2))
  
  train <- mesa[split, ]
  test <- mesa[-split, ]
  
  # fit naive model 
  naive_model <- glm(
    formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
      bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
    family = binomial(link = "logit"),
    data = train
  )
  
  # fit ipcw model 
  # models for numerator and denominator of stabilized weights
  treat_model <- list(
    "num" = glm(
      formula = reformulate(
        termlabels = model_vars,
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = train
    ),
    "den" = glm(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long),
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = train
    )
  )
  
  # get probs for numerator and denominator of weights
  p <- lapply(treat_model, function(x) predict(x, type = "response"))
  
  # add weights to training data
  train <- 
    train |>
    mutate(
      w = (A * p[['num']] + (1 - A) * (1 - p[['num']])) / 
        (A * p[['den']] + (1 - A) * (1 - p[['den']]))
    )
  
  # fit model
  ipw_model <- glm(
    formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
      bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
    family = binomial(link = "logit"),
    data = train[train$A == 0, ],
    weights = train$w[train$A == 0]
  )
  
  # make predictions in test set 
  p_naive <- predict(naive_model, newdata = test, type = "response")
  p_ipw <- predict(ipw_model, newdata = test, type = "response")
  
  
  # fit models for counterfactual mse 
  e_fit <- list(
    "num" = glm(
      formula = A ~ 1,
      family = binomial(link = "logit"),
      data = test
    ), 
    "den" = glm(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = test
    )
  )
  
  h_fit <- glm(
    formula = reformulate(
      termlabels = c(baseline_vars, baseline_vars_long), 
      response = "cvda"
    ),
    family = binomial(link = "logit"),
    data = subset(test, A == 0)
  )
  
  gam_fits <- list(
    "num" = glm(
      formula = A ~ 1,
      family = binomial(link = "logit"),
      data = test
    ), 
    "den" = gam(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = test
    ),
    "h" = gam(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "cvda"
      ),
      family = binomial(link = "logit"),
      data = subset(test, A == 0)
    )
  )
  
  # calculate mse in test set 
  e <- lapply(e_fit, function(x) predict(x, type = "response"))
  pY <- predict(h_fit, newdata = test, type = "response")
  h_naive <- pY - 2 * p_naive * pY + p_naive^2
  h_ipw <- pY - 2 * p_ipw * pY + p_ipw^2
  
  test <- 
    test |>
    mutate(
      Y = cvda,
      w = (A * e[['num']] + (1 - A) * (1 - e[['num']])) / 
        (A * e[['den']] + (1 - A) * (1 - e[['den']]))
    )
  
  auc_naive_naive <- cal_auc(test$cvda, p_naive)$auc
  auc_naive_ipw <- cal_auc(test$cvda, p_ipw)$auc
  aucs_naive <- cal_counter_auc(test, p_naive, e_fit$den, h_fit, se.fit = FALSE)
  aucs_ipw <- cal_counter_auc(test, p_ipw, e_fit$den, h_fit, se.fit = FALSE)
  
  test |>
    summarise(
      mse_naive_naive = mean((p_naive - cvda)^2),
      mse_naive_ipw = mean((p_ipw - cvda)^2),
      mse_ipw_naive = sum(I(A == 0) * w * (p_naive - cvda)^2) / sum(I(A == 0)),
      mse_ipw_ipw = sum(I(A == 0) * w * (p_ipw - cvda)^2) / sum(I(A == 0)),
      mse_cl_naive = mean(h_naive),
      mse_cl_ipw = mean(h_ipw),
      mse_dr_naive = mean(I(A == 0) * w * ((p_naive - cvda)^2 - h_naive) + h_naive),
      mse_dr_ipw = mean(I(A == 0) * w * ((p_ipw - cvda)^2 - h_ipw) + h_ipw)
    ) |>
    mutate(
      auc_naive_naive = auc_naive_naive,
      auc_naive_ipw = auc_naive_ipw,
      auc_ipw_naive = aucs_naive$ipw,
      auc_ipw_ipw = aucs_ipw$ipw,
      auc_cl_naive = aucs_naive$om,
      auc_cl_ipw = aucs_ipw$om,
      auc_dr_naive = aucs_naive$dr,
      auc_dr_ipw = aucs_ipw$dr
    ) |>
    pivot_longer(
      everything(),
      names_pattern = "(.*)_(.*)_(.*)",
      names_to = c(".value", "estimator", "model")
    ) |>
    pivot_wider(
      names_from = model,
      values_from = c(mse, auc)
    )
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

