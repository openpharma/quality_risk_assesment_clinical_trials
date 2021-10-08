

qract_is_better <- function(x, y, type = "upper") {
  if (type == "upper") {
    return(x > y)
  } else {
    return(x < y)
  }
}


#' @title get upper or lower boundary of classification prediction probabilities
#' @description Ranks by probability and takes a top samples from min to max
#'   size. Then performs prop.test() comparing the frequency of events in the
#'   sample vs. the base rate frequency. The test is performed on all top
#'   samples and the highest frequency  that gives a significant result
#'   determined by a given threshold is returned.
#' @param df dataframe with columns pred_yes (proabilities between 0 and 1) and
#'   obs (binary events, 0 or 1)
#' @param type c("upper", "lower"), Default: 'upper'
#' @param stats logical, test boundary for statistical significance vs baserate,
#'   if not significant increase bin size
#' @param pval significance threshold, Default: 0.05
#' @param min_sample_size Default: 10
#' @param max_sample_size Default: 1000
#' @return double
#' @rdname qract_get_pred_boundary
#' @export
#' @importFrom broom tidy
qract_get_pred_boundary <- function(df,
                                    type = "upper",
                                    stats = FALSE,
                                    pval = 0.05,
                                    min_sample_size = 50,
                                    max_sample_size = 1000) {

  # checks -----------------------------------------------
  stopifnot(all(c("pred_yes", "obs") %in% colnames(df)))
  stopifnot(all(between(df$pred_yes, 0, 1), na.rm = TRUE))
  stopifnot(all(df$obs %in% c(0, 1), na.rm = TRUE))

  if (type == "upper") {
    df <- arrange(df, desc(pred_yes))
    alt_prop_test <- "greater"
  } else {
    df <- arrange(df, pred_yes)
    alt_prop_test <- "less"
  }
  
  if (stats) {
      best_prop <- base_prob
      base_prob <- mean(df$obs, na.rm = TRUE)
      
      for (i in seq(min_sample_size, max_sample_size, 1)) {
        suppressWarnings({
          prop_test <- prop.test(
            x = sum(df$obs[1:i], na.rm = TRUE),
            n = length(df$obs[1:i]),
            p = base_prob, alt_prop_test
          )
        })
    
        prop_test <- broom::tidy(prop_test)
    
        if (
          prop_test$p.value <= pval &
            qract_is_better(prop_test$estimate, best_prop, type)
        ) {
          best_prop <- prop_test$estimate
        }
      }
  } else {
    best_prop <- df %>%
      head(min_sample_size) %>%
      pull(pred_yes) %>%
      mean(na.rm = TRUE)
  }

  return(best_prop)
}

#' @title get prediction boundaries
#' @description applies qract_get_pred_boundary() returning lower and upper and baserate
#' @param df
#' @param category_id_str  Default: 'cnsn'
#' @param ... other parameters passed to qract_get_pred_boundary()
#' @return dataframe with columns lower, base_rate and upper
#' @rdname qract_get_pred_boundaries
#' @export
qract_get_pred_boundaries <- function(df, category_id_str = "cnsn", ...) {
  if ("category_id" %in% colnames(df)) {
    df <- filter(df, category_id == category_id_str)
  }

  if (is.character(df$obs)) {
    df <- mutate(df, obs = ifelse(obs == "yes", 1, 0))
  }

  tibble(
    lower = qract_get_pred_boundary(df, type = "lower", ...),
    base_rate = mean(df$obs, na.rm = TRUE),
    upper = qract_get_pred_boundary(df, type = "upper", ...)
  )
}


#' @title fit linear calibration
#' @description fit linear calibration
#' @param df dataframe
#' @param category_id_str
#' @return dataframe with slope and intercept
#' @details DETAILS
#' @rdname qract_fit_linear_calibration
#' @export
#' @importFrom broom tidy
qract_fit_linear_calibration <- function(df, category_id_str) {
  if ("category_id" %in% colnames(df)) {
    df <- filter(df, category_id == category_id_str)
  }
  if (is.character(df$obs)) {
    df <- mutate(df, obs = ifelse(obs == "yes", 1, 0))
  }

  m <- lm(obs ~ pred_yes, df)
  suppressWarnings({
    df_coef <- broom::tidy(coef(m))
  })
  df_coef %>%
    mutate(
      names = case_when(
        names == "(Intercept)" ~ "intercept",
        names == "pred_yes" ~ "slope"
      )
    ) %>%
    spread(key = names, value = x)
}

#' @title Linear Calibration mapable function
#' @description applies qract_fit_linear_calibration() and pred_bound(). Boundaries
#'   serve as upper and lower limit to linear cailbration. Unless upper and
#'   lower limit of linear calibration fit is more strict. Then linear
#'   calibration is used to calibrate upper and lower boundaries.
#' @param df PARAM_DESCRIPTION
#' @param category_id_str PARAM_DESCRIPTION, Default: NULL
#' @param ... additional arguments passed to qract_get_pred_boundaries()
#' @return dataframe with upper, base_rate, lower, slope, intercept and nested
#'   data frame with x-y coordinates for calibration plot
#' @rdname qract_linear_calib_map
#' @export
qract_linear_calib_map <- function(df, category_id_str = NULL, ...) {
  pred_bound <- qract_get_pred_boundaries(
    df,
    category_id_str = category_id_str,
    ...
  )

  lin_calib <- qract_fit_linear_calibration(
    df,
    category_id_str = category_id_str
  )

  x <- seq(0, 1, 0.01)
  y <- x * lin_calib$slope + lin_calib$intercept
  y <- ifelse(y > pred_bound$upper, pred_bound$upper, y)
  y <- ifelse(y < pred_bound$lower, pred_bound$lower, y)

  pred_bound <- pred_bound %>%
    mutate(
      upper = ifelse(upper > max(y), max(y), upper),
      lower = ifelse(lower < min(y), min(y), lower)
    )

  bind_cols(pred_bound, lin_calib) %>%
    mutate(plot_data = list(tibble(x = x, y = y)))
}

#' @title linear calibration
#' @description Applies mapable function qract_linear_calib_map() and unnests results
#' @param df_cv_pred_and_coefs PARAM_DESCRIPTION
#' @param ... additional arguments passed to qract_get_pred_boundaries()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname qract_lin_calib
#' @export
qract_lin_calib <- function(df_cv_pred_and_coefs, ...) {
  df_prep <- df_cv_pred_and_coefs %>%
    mutate(pred = map(pred, "pred_valid")) %>%
    select(category_id, year_start_act, pred) %>%
    unnest(pred) %>%
    unnest(pred) %>%
    group_by(category_id) %>%
    nest() %>%
    mutate(lin_calib = pmap(
      list(data, category_id),
      qract_linear_calib_map,
      ...
    )) %>%
    unnest(lin_calib) %>%
    select(-data)
}
