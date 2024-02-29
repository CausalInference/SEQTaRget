#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
#' @export
internal.model <- function(DT, method, outcome.col, opts){
  if(method == "ITT"){
    model <- speedglm(formula = paste0(outcome.col, "~", opts$covariates),
                                DT,
                                family = binomial("logit"))
    names(model$coefficients) <- gsub("_bas", "", names(model$coefficients))

  }
  return(model)
}

#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table parallel foreach doRNG
#' @importFrom speedglm speedglm
#'
#' @keywords internal
#' @export
internal.survival <- function(DT, id.col, time.col, outcome.col, treatment.col, opts){
  if(opts$expand == TRUE) time.col <- "followup"
  if(opts$max.survival == "max") opts$max.survival <- max(DT[[time.col]])
  tx.col <- names(DT)[grep(treatment.col, names(DT))]

  handler <- function(DT, id.col, time.col, outcome.col, tx.col, opts){
    surv.model <- speedglm::speedglm(formula = paste0(outcome.col, "==1~", opts$covariates),
                                   data = DT,
                                   family = binomial("logit"))
    kept <- c("risk0", "risk1", "surv0", "surv1", time.col)

    RMDT <- copy(DT)[, eval(id.col) := paste0(get(id.col), "_", trial)
                ][get(time.col) == 0,
                   ][rep(1:.N, each = opts$max.survival + 1)
                     ][, `:=` (followup = seq(1:.N)-1,
                               followup_sq = (seq(1:.N)-1)^2), by = eval(id.col)
                      ][, eval(tx.col) := FALSE
                         ][, predFALSE := predict(surv.model, newdata = .SD, type = "response")
                           ][, eval(tx.col) := TRUE
                             ][, predTRUE := predict(surv.model, newdata = .SD, type = "response")
                               ][, `:=` (surv0 = cumprod(1 - predFALSE),
                                         surv1 = cumprod(1 - predTRUE)), by = eval(id.col)
                                 ][, `:=` (risk0 = 1 - surv0,
                                          risk1 = 1 - surv1)
                                  ][, ..kept]
    return(RMDT)
  }


  if(opts$bootstrap){
    UIDs <- unique(DT[[id.col]])
    subsample <- lapply(1:opts$nboot, function(x){
      set.seed(opts$seed + x)
      id.sample <- sample(UIDs,
                          round(opts$boot.sample*length(UIDs)), replace = FALSE)
      return(id.sample)
    })

    if(opts$parallel){
      if(opts$sys.type %in% c("Darwin", "Linux")){

        result <- parallel::mclapply(subsample, function(x){
          DT <- DT[get(id.col) %in% x, ]

          output <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
        }, mc.cores = opts$ncores)
        result <- rbindlist(result)

      } else if(opts$sys.type == "Windows"){
        result <- foreach(x = subsample, .combine = "rbind", .packages = c("data.table", "SEQuential")) %dopar% {
          DT <- DT[get(id.col) %in% x, ]
          output <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
        }
      }
    } else {
      # Non Parallel Bootstrapping ===============================================
      result <- lapply(subsample, function(x) {
        DT <- DT[get(id.col) %in% x, ]

        output <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
        })
      result <- rbindlist(result)
      }
    kept <- c("surv0_mu", "surv0_lb", "surv0_ub",
              "surv1_mu", "surv1_lb", "surv1_ub",
              "followup")
    DT <- unique(result[, `:=` (surv0_mu = mean(surv0),
                                surv1_mu = mean(surv1),
                                se_surv0 = sd(surv0)/sqrt(opts$nboot),
                                se_surv1 = sd(surv1)/sqrt(opts$nboot)), by = time.col
                        ][, `:=` (surv0_lb = surv0_mu - qnorm(0.975)*se_surv0,
                                  surv0_ub = surv0_mu + qnorm(0.975)*se_surv0,
                                  surv1_lb = surv1_mu - qnorm(0.975)*se_surv1,
                                  surv1_ub = surv1_mu + qnorm(0.975)*se_surv1,
                                  followup = get(time.col))
                          ][, ..kept])

    SDT <- rbind(DT[, .(followup, mu = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, type := "txFALSE"],
                 DT[, .(followup, mu = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, type := "txTRUE"])
    rm(DT)

    surv <- ggplot(SDT, aes(x = followup, y = mu, fill = type)) +
      geom_line(col = "black") +
      geom_ribbon(aes(ymax = ub, ymin = lb), alpha = 0.5) +
      theme_classic() +
      labs(x = "Time", y = "Survival", fill = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))

  } else if(!opts$bootstrap){
    DT <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
    surv <- melt(DT[, .(txFALSE = mean(surv0),
                        txTRUE = mean(surv1)), by = "followup"],
                 id.vars = "followup") |>
      ggplot(aes(x = followup, y = value, col = variable)) +
      geom_line() +
      theme_classic() +
      labs(x = "Time", y = "Survival", color = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))
  }
  return(surv)
}

internal.weights <- function(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
  if(is.na(opts$weight.covariates)) {
    opts$weight.covariates <- create.default.weight.covariates(DT, data, id.col, time.col, eligible.col, treatment.col, opts)
  }
  if(!opts$stabilized){
    if(opts$pre.expansion){
      data <- as.data.table(data)

      weight <- copy(data)[, `:=` (tx_lag = shift(get(treatment.col)),
                                   time_sq = get(time.col)^2), by = id.col]

      model1 <- speedglm::speedglm(formula = paste0(treatment.col, "==1~", opts$weight.covariates, "+", time.col,"+time_sq"),
                                   data = weight[tx_lag == 1, ],
                                   family = binomial("logit"))

      model0 <- speedglm::speedglm(formula = paste0(paste0(treatment.col, "==0~", opts$weight.covariates, "+", time.col, "+time_sq")),
                                   data = weight[tx_lag == 0],
                                   family = binomial("logit"))

      kept <- c("wt", time.col, id.col)
      out <- weight[tx_lag == 0, pred := predict(model0, newdata = .SD, type = "response")
                    ][tx_lag == 1, pred := predict(model1, newdata = .SD, type = "response")
                      ][get(time.col) == 0, pred := 1
                        ][, cmprd := cumprod(pred), by = eval(id.col)
                          ][, wt := 1/cmprd
                            ][, ..kept]

      if(opts$expand) setnames(out, time.col, "followup")

      percentile <- quantile(out$wt, probs = c(.25, .5, .75))
      stats <- list(min = min(out$wt),
                    max = max(out$wt),
                    sd = sd(out$wt),
                    p25 = percentile[[1]],
                    p50 = percentile[[2]],
                    p75 = percentile[[3]])

    } else if(!opts$pre.expansion){
      # NON STABILIZED - POST EXPANSION
    }
  } else if(opts$stabilized){

  }
  return(list(weighted_data = out,
              weighted_stats = stats))
}
