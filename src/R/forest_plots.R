
qract_plot_coef <- function(x, ci95_low, ci95_high, df_coef_final) {
  df <- tibble(x = x, ci95_high = ci95_high, ci95_low = ci95_low)

  if (is.na(df$x)) {
    p <- ggplot() +
      geom_text(aes(x = 1, y = 1, label = "reference"),
        color = "darkgrey",
        size = 100
      )
  } else {
    p <- df %>%
      ggplot(aes(y = x, x = 1)) +
      geom_errorbar(aes(ymin = ci95_low, ymax = ci95_high),
        width = 0.5,
        size = 6
      ) +
      geom_point(size = 50) +
      geom_hline(yintercept = 0, linetype = 1, color = "red", size = 6) +
      coord_flip(ylim = c(min(df_coef_final$ci95_low), max(df_coef_final$ci95_high)))
  }

  p <- p +
    theme(
      line = element_blank(),
      text = element_blank(),
      panel.background = element_blank()
    )

  return(p)
}


# qract_plot_coef(x = 0.5, ci95_low = 0.25, ci95_high = 0.75)


qract_plot_hist <- function(var = "n_iss_due", min = 0, max = 2, df_mm) {
  values <- df_mm[[var]]
  values <- values[!is.na(values)]

  max_near_one <- near(max(values), 1)

  if (n_distinct(values) <= 2 | is.na(min) | is.na(max)) {
    p <- ggplot()
  } else {
    dens_var <- density(values, adjust = 5)

    df_plot <- tibble(
      x = dens_var$x,
      y = ifelse(x < 0, 0, dens_var$y),
      y_bin = ifelse(between(x, min, max), y, NA),
      perc_auc = cumsum(y) / sum(y)
    ) %>%
      filter(perc_auc < 0.99, x > 0)

    if (max_near_one) {
      df_plot <- df_plot %>%
        filter(x <= 1)
    }


    p <- df_plot %>%
      ggplot() +
      geom_ribbon(aes(x, ymin = 0, ymax = y_bin),
        fill = "grey",
        color = "white"
      ) +
      geom_ribbon(aes(x, ymin = 0, ymax = y),
        fill = NA,
        color = "black",
        size = 4
      )
  }

  p <- p +
    theme(
      line = element_blank(),
      text = element_blank(),
      panel.background = element_blank()
    )

  return(p)
}
# qract_plot_hist()


qract_plot_coef_time <- function(co = "medianelapsetimeHH", category_id_str = "ptpe", df_coef) {
  if (is.na(co) | !co %in% df_coef$names) {
    p <- ggplot()
  } else {
    df <- df_coef %>%
      filter(
        category_id == category_id_str,
        names == co,
        year_start_act < 2019
      ) %>%
      filter(!is.na(ci95_low), !is.na(ci95_high))

    lim_low <- min(df$ci95_low, na.rm = TRUE)
    lim_max <- max(df$ci95_high, na.rm = TRUE)

    lim_low <- ifelse(lim_low > 0, -0.1, lim_low)
    lim_max <- ifelse(lim_max < 0, 0.1, lim_max)

    p <- ggplot(df) +
      geom_ribbon(aes(x = year_start_act, ymin = ci95_low, ymax = ci95_high),
        fill = "lightgrey"
      ) +
      geom_line(aes(x = year_start_act, y = x),
        size = 5
      ) +
      geom_hline(
        yintercept = 0, color = "red",
        size = 5
      ) +
      theme(
        line = element_blank(),
        text = element_blank(),
        panel.background = element_blank()
      ) +
      coord_cartesian(ylim = c(lim_low, lim_max))
  }
  p <- p +
    theme(
      line = element_blank(),
      text = element_blank(),
      panel.background = element_blank()
    )

  return(p)
}
# qract_plot_coef_time()


qract_get_bin_stats <- function(var_str = "is_not_pediatric",
                                min = NA,
                                max = 1,
                                category_id_str = "cnsn",
                                df_coef_final,
                                df_feats,
                                df_mm) {
  data_type <- df_feats %>%
    filter(var == var_str) %>%
    pull(data_type)

  if (!var_str %in% colnames(df_mm)) {
    variable_str <- df_feats %>%
      filter(var == var_str) %>%
      pull(name_pretty)

    other_vars <- df_feats %>%
      filter(name_pretty == variable_str, var != var_str) %>%
      pull(var_bin)

    row_sums <- df_mm %>%
      select(one_of(other_vars)) %>%
      rowSums()

    df_mm <- df_mm %>%
      mutate(!!as.name(var_str) := ifelse(row_sums == 0, 1, 0))
  }

  coefs <- df_coef_final %>%
    left_join(df_feats, by = c(names = "name_matrix_bin")) %>%
    filter(category_id == category_id_str, !is.na(var)) %>%
    pull(var) %>%
    unique()

  df <- df_mm %>%
    filter(category_id == category_id_str) %>%
    group_by(activity_id_new) %>%
    mutate(rwn = row_number()) %>%
    arrange(activity_id_new, rwn) %>%
    select(one_of("activity_id_new", coefs, "has_finding", var_str)) %>%
    ungroup()

  nas <- df %>%
    mutate_all(is.na) %>%
    rowSums()

  df <- df %>%
    mutate(row_sum_na = nas) %>%
    filter(nas < length(coefs))

  if (is.na(min) | is.na(max)) {
    df_bin <- df %>%
      filter(is.na(!!as.name(var_str))) %>%
      mutate(has_finding = ifelse(has_finding == "yes", 1, 0))
  } else if (data_type == "num") {
    df_bin <- df %>%
      filter(!!as.name(var_str) >= min, !!as.name(var_str) < max) %>%
      mutate(has_finding = ifelse(has_finding == "yes", 1, 0))
  } else if (data_type == "cat") {
    df_bin <- df %>%
      filter(!!as.name(var_str) == 1) %>%
      mutate(has_finding = ifelse(has_finding == "yes", 1, 0))
  }

  tibble(
    n = nrow(df_bin),
    freq = nrow(df_bin) / nrow(df),
    n_findings = sum(df_bin$has_finding),
    ratio_findings = sum(df_bin$has_finding) / nrow(df_bin)
  ) %>%
    mutate(
      ratio_findings = ifelse(is.nan(ratio_findings), 0, ratio_findings)
    )
}

# qract_get_bin_stats()


qract_plot_tab <- function(df_cv_preds_and_coefs,
                           df_feats,
                           df_mm,
                           df_cat,
                           category_id_str = "cnsn") {
  df_coef <- df_cv_preds_and_coefs %>%
    mutate(data = map(pred, "coefs")) %>%
    select(year_start_act, category_id, data) %>%
    unnest(data) %>%
    unnest(data)

  df_coef_final <- df_coef %>%
    filter(year_start_act == max(year_start_act))

  # the final coefficients do not include the reference bin(s)
  # the reference reference bin(s) need to be inferred from unused bin(s)
  df_tab_prep1 <- df_coef_final %>%
    filter(category_id == category_id_str) %>%
    filter(names != "(Intercept)") %>%
    left_join(df_feats, by = c(names = "name_matrix_bin")) %>%
    full_join(df_feats, by = c(
      "var_bin", "var", "bin", "min", "max", "freq", "name_pretty", "max_corr",
      "pretty_min", "pretty_max", "name_pretty_bin", "data_type", "name_matrix"
    )) %>%
    group_by(name_pretty) %>%
    mutate(var_in_coef = ifelse(!all(is.na(x)), 1, 0)) %>%
    filter(var_in_coef == 1) %>%
    # max_corr is preferred over max because it does not allow gaps to adjacent bins
    mutate(max = ifelse(is.infinite(max_corr), max, max_corr)) %>%
    # we collapse unused feature ranges that fall between used feature ranges
    # and aggregate mins and max
    arrange(var, min) %>%
    mutate(
      gr = ifelse(!is.na(x) | !is.na(lag(x)), 1, 0),
      gr = cumsum(gr),
      # set bin labels to NA if no coef available
      name_matrix_bin = ifelse(is.na(x) & data_type == "num", NA, name_matrix_bin),
      name_pretty_bin = ifelse(is.na(x) & data_type == "num", NA, name_pretty_bin)
    ) %>%
    group_by(var, name_pretty, gr, name_matrix_bin, name_pretty_bin, data_type) %>%
    summarise(
      x = max(x),
      ci95_high = max(ci95_high),
      ci95_low = max(ci95_low),
      p.value = max(p.value),
      min = min(min),
      max = max(max),
      .groups = "drop"
    ) %>%
    group_by(var, name_pretty) %>%
    # we adjust min max of unused bin not to overlap with used bins
    # then remove zero size bins
    mutate(
      min_adj = ifelse(is.na(x) & !is.na(lag(max)), as.numeric(lag(max)), as.numeric(min)),
      max_adj = ifelse(is.na(x) & !is.na(lead(min)), as.numeric(lead(min)), as.numeric(max)),
      zero_size_bin = min_adj == max_adj,
      keep = !zero_size_bin | !is.na(x) | data_type == "cat"
    ) %>%
    filter(keep) %>%
    ungroup() %>%
    select(name_pretty,
      min = min_adj, max = max_adj, x, ci95_low, ci95_high,
      p.value, var, name_matrix_bin, name_pretty_bin, zero_size_bin
    )

  df_na <- df_tab_prep1 %>%
    select(name_pretty) %>%
    distinct() %>%
    mutate(
      min = NA,
      max = NA
    )

  df_tab_prep2 <- df_tab_prep1 %>%
    mutate(
      or = exp(x),
      or_ci95_low = exp(ci95_low),
      or_ci95_high = exp(ci95_high)
    ) %>%
    bind_rows(df_na) %>%
    arrange(name_pretty, min) %>%
    # missing var values for factor variables need to be filled to get NA stats
    group_by(name_pretty) %>%
    mutate(var = ifelse(is.na(var), max(var, na.rm = TRUE), var)) %>%
    ungroup() %>%
    mutate(freq = pmap(
      list(var, min, max),
      qract_get_bin_stats,
      category_id_str = category_id_str,
      df_coef_final = df_coef_final,
      df_feats = df_feats,
      df_mm = df_mm
    )) %>%
    unnest(freq) %>%
    ungroup()

  var_pop <- df_tab_prep2 %>%
    filter(str_detect(str_to_lower(name_pretty), "popu")) %>%
    pull(name_pretty) %>%
    str_replace_all(">", "over ")

  vars_rnk <- df_tab_prep2 %>%
    filter(str_detect(str_to_lower(name_pretty), "rank")) %>%
    pull(name_pretty)

  vars_days <- df_tab_prep2 %>%
    filter(str_detect(str_to_lower(name_pretty), "days on study")) %>%
    pull(name_pretty)

  vars_active <- df_tab_prep2 %>%
    filter(str_detect(str_to_lower(name_pretty), "active trials")) %>%
    pull(name_pretty)

  tb <- df_tab_prep2 %>%
    mutate(
      coef = row_number(),
      hist = row_number(),
      min = map_chr(min, qract_pretty_number),
      max = map_chr(max, qract_pretty_number),
      min_max = paste0("[", min, " - ", max, ")"),
      min_max = ifelse(min == "NA", "missing", min_max),
      zero_size_bin = ifelse(is.na(zero_size_bin), FALSE, zero_size_bin),
      min_max = ifelse(zero_size_bin, name_pretty_bin, min_max),
      name_pretty = str_replace(name_pretty, ">", "over "),
      coef_time = row_number(),
      n = paste0(n, " (", round(freq * 100, 1), "%)"),
      n_findings = paste0(n_findings, " (", round(ratio_findings * 100, 1), "%)"),
      or = paste0(round(or, 1), " (", round(or_ci95_low, 1), "-", round(or_ci95_high, 1), ")"),
      or = ifelse(is.na(or_ci95_low), "", or)
    ) %>%
    select(name_pretty,
      min_max,
      Distribution = hist,
      N = n,
      `N with Findings` = n_findings,
      `Coefficient (CI95)` = coef,
      `Odds Ratio (CI95)` = or,
      P = p.value,
      `Coefficient Over Time (CI95)` = coef_time
    ) %>%
    gt::gt(groupname_col = "name_pretty", rowname_col = "min_max", ) %>%
    text_transform(
      locations = cells_body(columns = "Coefficient (CI95)"),
      fn = function(rwn) {
        pmap(
          list(df_tab_prep2$x, df_tab_prep2$ci95_low, df_tab_prep2$ci95_high),
          qract_plot_coef,
          df_coef_final
        ) %>%
          map(ggplot_image, height = px(25), aspect_ratio = 8)
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = "Distribution"),
      fn = function(rwn) {
        pmap(
          list(df_tab_prep2$var, df_tab_prep2$min, df_tab_prep2$max),
          qract_plot_hist,
          df_mm
        ) %>%
          map(ggplot_image, height = px(25), aspect_ratio = 3)
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = "Coefficient Over Time (CI95)"),
      fn = function(rwn) {
        map(
          df_tab_prep2$name_matrix_bin,
          qract_plot_coef_time,
          category_id_str,
          df_coef
        ) %>%
          map(ggplot_image, height = px(25), aspect_ratio = 3)
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = c("P")),
      fn = function(x) {
        p <- round(as.double(x), 5)

        st <- case_when(
          p < 0.001 ~ "***",
          p < 0.005 ~ "**",
          p < 0.05 ~ "*",
          is.na(p) ~ "",
          TRUE ~ ""
        )

        p <- ifelse(is.na(p), "", p)

        paste(p, st)
      }
    ) %>%
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_body(columns = c("P"))
    ) %>%
    tab_header(
      title = paste(str_to_title(qract_pretty_category(category_id_str, df_cat)), "Related Audit and Inspection Findings"),
      subtitle = "Adjusted Risk Factors"
    ) %>%
    tab_footnote(
      footnote = "Number of Clinical Trial Site Audits and Inspections",
      locations = cells_column_labels(c("N", "N with Findings"))
    ) %>%
    tab_footnote(
      footnote = "Population Within 100 km Radius of Site",
      locations = cells_row_groups(var_pop)
    ) %>%
    tab_footnote(
      footnote = "Percent Rank of Site Within Study",
      locations = cells_row_groups(vars_rnk)
    ) %>%
    tab_footnote(
      footnote = "Sum of Days Since Enrollment of All Patients",
      locations = cells_row_groups(vars_days)
    ) %>%
    tab_footnote(
      footnote = "Active Trials in the Past Year in the Same Theraputic Area",
      locations = cells_row_groups(vars_active)
    ) %>%
    tab_footnote(
      footnote = "Red Reference Line Indicates Zero",
      locations = cells_column_labels(c("Coefficient (CI95)", "Coefficient Over Time (CI95)"))
    ) %>%
    tab_footnote(
      footnote = "Coefficient Value as New Data is Successively Added from 2011-2019",
      locations = cells_column_labels(c("Coefficient Over Time (CI95)"))
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_body(c("N", "N with Findings", "Odds Ratio (CI95)")),
        cells_column_labels(c("N", "N with Findings", "Odds Ratio (CI95)"))
      )
    ) %>%
    tab_style(
      style = cell_text(size = "small"),
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text(size = "small"),
      locations = cells_column_labels(c("Distribution", "N", "N with Findings", "Odds Ratio (CI95)", "P", "Coefficient (CI95)", "Coefficient Over Time (CI95)"))
    ) %>%
    tab_style(
      style = cell_text(size = "small"),
      locations = cells_stub()
    ) %>%
    tab_options(
      data_row.padding = px(2),
      footnotes.font.size = "small"
    )

  return(tb)
}


qract_forest_plots <- function(df_cv_preds_and_coefs,
                               df_feats,
                               df_mm,
                               df_cat,
                               category_ids) {
  tibble(category_id = category_ids) %>%
    mutate(
      tab = map(
        category_id,
        ~ qract_plot_tab(
          df_cv_preds_and_coefs,
          df_feats,
          df_mm,
          df_cat,
          category_id_str = .
        )
      )
    )
}


qract_save_forest_plots <- function(df_forest, path = "docs/png") {
  df_forest <- df_forest %>%
    mutate(file = paste0(path, "/", category_id, "_forest.png"))
    
  suppressWarnings(
    walk2(df_forest$tab, df_forest$file, gt::gtsave, vwidth = 5000)
  )
    
    return(df_forest$file)
}