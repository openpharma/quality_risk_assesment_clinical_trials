qract_get_variables_from_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("f_manip_get_variables_from_formula called with non formula object")
  }

  vars <- formula %>%
    as.character() %>%
    .[[3]] %>%
    stringr::str_split(" \\+ ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    ## for long formulas as.character
    unlist() ## will add some whitespace to some variables

  if (vars[1] == ".") {
    stop('cannot extract variables from formula if formula was constructed with "~." ')
  }

  return(vars)
}

qract_str_index_2_int_index <- function(x, del = ",") {
  str_2_ints <- function(str, del) {
    str %>%
      str_split(del) %>%
      .[[1]] %>%
      as.integer()
  }

  map(x, str_2_ints, del = del)
}

qract_filter_na <- function(df, form, id_vars) {
  vars <- qract_get_variables_from_formula(form) %>%
    str_replace_all("(HH$)|(MH$)|(M$)|(ML$)|(LL$)|(NA$)", "") %>%
    unlist() %>%
    unique()

  # supress warnings about missing columns
  suppressWarnings({
    df_na <- df %>%
      select(one_of(id_vars), has_finding, starts_with(vars)) %>%
      gather(key = "key", value = "value", ends_with("NA")) %>%
      group_by_at(vars(everything(), -key, -value)) %>%
      summarise(n_na = sum(value), .groups = "drop")
  })

  stopifnot(nrow(df) == nrow(df_na))

  df_na <- df_na %>%
    ungroup() %>%
    # have at least 1var that is not NA
    filter(n_na == 0 | n_na != n_distinct(vars)) %>%
    select(-n_na)

  return(df_na)
}

qract_wr_glm <- function(year,
                         category_id,
                         index_train,
                         index_valid,
                         form,
                         data,
                         id_vars) {
  if (!"index" %in% names(data)) {
    print(paste(year, category_id))
    stop("columns index is missing from data")
  }

  if (!is.numeric(data$has_finding)) {
    data <- data %>%
      mutate(has_finding = ifelse(has_finding == "yes", 1, 0))
  }

  form <- as.formula(form)
  df_train <- filter(data, index %in% index_train)
  df_valid <- filter(data, index %in% index_valid)

  if (nrow(df_train) == 0 |
    nrow(df_valid) == 0) {
    print(paste(year, category_id))
    stop("invalid index passed")
  }

  df_train <- qract_filter_na(df_train, form, id_vars = id_vars)
  df_valid <- qract_filter_na(df_valid, form, id_vars = id_vars)

  safely_glm <- safely(glm)
  safely_predict <- safely(predict.glm)

  safe <- safely_glm(form, df_train, family = "binomial")

  if (is.null(safe$error)) {
    
    m <- safe$result
    
    # predictions -----------------------------------------------------------
    safe_pred_train <- safely_predict(m, newdata = df_train, type = "response")
    safe_pred_valid <- safely_predict(m, newdata = df_valid, type = "response")

    if (is.null(safe_pred_train$error)) {
      df_train$pred_yes <- safe_pred_train$result
    } else {
      df_train$pred_yes <- integer()
    }

    if (is.null(safe_pred_valid$error)) {
      df_valid$pred_yes <- safe_pred_valid$result
    } else {
      df_valid$pred_yes <- integer()
    }

    # coefficients ----------------------------------------------------------
    coe <- m %>%
      broom::tidy() %>%
      rename(names = term, x = estimate)
    
    # coefficients confidence intervals -------------------------------------
    safely_ci <- safely(confint)

    suppressMessages({
      safe_ci <- safely_ci(m)
    })

    if (is.null(safe_ci$error)) {
      coe <- coe %>%
        inner_join(
          safe_ci$result %>%
            as_tibble(rownames = ".rownames") %>%
            rename(names = `.rownames`, ci95_low = `2.5 %`, ci95_high = `97.5 %`),
          by = "names"
        )
    } else {
      coe <- coe %>%
        mutate(
          ci95_low = NA,
          ci95_high = NA
        )
    }
    
    # vif --------------------------------------------------------------------
    safely_vif <- purrr::safely(car::vif)
    safe_vif <- safely_vif(m)
    
    if (is.null(safe_vif$error)) {
      vif <- safe_vif$result
      vif_df <- tibble(
        names = names(vif),
        vif = vif
      )
      coe <- coe %>%
        left_join(vif_df, by = "names")
    } else {
      coe$vif <- NA
    }
    
    
  } else {
    print("glm could not be fitted")
    print(safe$error)
    df_train$pred_yes <- 0
    df_valid$pred_yes <- 0
    coe <- NULL
  }

  return(tibble(
    pred_train = list(select(df_train, activity_id_new, pred_yes, obs = has_finding)),
    pred_valid = list(select(df_valid, activity_id_new, pred_yes, obs = has_finding)),
    coefs = list(coe)
  ))
}

qract_pred_cv <- function(df_mm_bin, df_cv, df_form, id_vars) {
  df_cv %>%
    inner_join(df_form, by = "category_id") %>%
    mutate(
      index_past = qract_str_index_2_int_index(index_past),
      index_next_year = qract_str_index_2_int_index(index_next_year)
    ) %>%
    mutate(
      pred = pmap(
        list(year_start_act, category_id, index_past, index_next_year, formula_str),
        qract_wr_glm,
        df_mm_bin,
        id_vars
      )
    )
}
