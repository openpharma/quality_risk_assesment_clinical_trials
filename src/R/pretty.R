
library(tidyverse)

qract_pretty_category = function( x, df_cat, wrap_at = 0){
  
  # id_mod ----------------------------
  df_look_cif_mod = df_cat %>%
    select(clinical_impact_factor_id_mod, clinical_impact_factor) %>%
    rename(category_id = clinical_impact_factor_id_mod
           , name = clinical_impact_factor)
  
  df_look_sub_mod = df_cat %>%
    select(finding_sub_category_id_mod, finding_sub_category) %>%
    rename(category_id = finding_sub_category_id_mod
           , name = finding_sub_category)
  
  df_look_st_mod = df_cat %>%
    select(finding_statement_id_mod, finding_statement) %>%
    rename(category_id = finding_statement_id_mod
           , name = finding_statement)
  
  # id_mod ----------------------------
  df_look_cif = df_cat %>%
    select(clinical_impact_factor_id, clinical_impact_factor) %>%
    rename(category_id = clinical_impact_factor_id
           , name = clinical_impact_factor)
  
  df_look_sub = df_cat %>%
    select(finding_sub_category_id, finding_sub_category) %>%
    rename(category_id = finding_sub_category_id
           , name = finding_sub_category)
  
  df_look_st = df_cat %>%
    select(finding_statement_id, finding_statement) %>%
    rename(category_id = finding_statement_id
           , name = finding_statement)
  
  df_look = df_look_cif %>%
    bind_rows(df_look_sub) %>%
    bind_rows(df_look_st) %>%
    bind_rows(df_look_cif_mod) %>%
    bind_rows(df_look_sub_mod) %>%
    bind_rows(df_look_st_mod) %>%
    mutate( name = str_replace_all(name, '_', ' ') ) %>%
    filter_all( ~ ! is.na(.) ) %>%
    distinct() %>%
    group_by(category_id) %>%
    summarise( name = paste(name, collapse = ', '))
  
  df_look %>%
    filter( ! str_detect(category_id, 'other') ) %>%
    group_by(category_id) %>%
    mutate( rwn = max(row_number()) ) %>%
    arrange( desc(rwn) )
  
  df_pretty = tibble( category_id = x) %>%
    left_join(df_look, by = "category_id")
  
  out = df_pretty$name
  
  if( wrap_at > 0){
    out = str_wrap(out, width = wrap_at)
  }
  
  if( length(x) != length(out) ){
    stop('input and output length do not match')
  }
  
  return(out)
}

# df_cat = feather::read_feather('data/proc/category_lookup.feather')
# df_data = feather::read_feather('data/proc/modelling_matrix.feather')
# 
# qract_pretty_category(df_data$category_id, df_cat)



qract_pretty_number <- function(x, dig = 2) {
  
  digits <- log10(abs(x))
  
  if (is.na(x)) {
    pretty_str <- "NA"
  } else if (is.infinite(x)) {
    pretty_str <- as.character(x)
  } else if (x == 0){
    pretty_str <- "0"
  } else if (digits >= 6) {
    pretty_str <- paste(
      round(x/1e6, dig),
      "mil.")
  } else if (digits >= 3) {
    pretty_str <- paste(
      round(x/1e3, dig),
      "thous.")
  } else if (digits > -1) {
    pretty_str <- round(x, dig) %>%
      as.character()
  } else if (digits > -2) {
    pretty_str <- round(x, dig + 1) %>%
      as.character()
  }else if (digits > -3) {
    pretty_str <- round(x, dig + 2) %>%
      as.character()
  }else {
    pretty_str <- formatC(x, format = "e", digits = dig)
  }
  
  return(pretty_str)
  
}

# qract_pretty_number(18e8)
# qract_pretty_number(1.5e6)
# qract_pretty_number(1.5e4)
# qract_pretty_number(1.5e2)
# qract_pretty_number(0.52346545)
# qract_pretty_number(0.052346545)
# qract_pretty_number(0.0052346545)
# qract_pretty_number(0.00052346545)

# 
# qract_pretty_number(-18e8)
# qract_pretty_number(-1.5e6)
# qract_pretty_number(-1.5e4)
# qract_pretty_number(-1.5e2)
# qract_pretty_number(-0.52346545)
# qract_pretty_number(-0.052346545)
# qract_pretty_number(-0.0052346545)
# qract_pretty_number(-0.00052346545)

