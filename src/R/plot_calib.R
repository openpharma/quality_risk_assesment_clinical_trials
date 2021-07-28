

qract_plot_calibration = function(category_id_str = 'sfty'
                            , df_calib
                            , df_calib_fit = NULL
                            , df_lin_calib = NULL
                            , colors = NULL
                            , color_calib = "lightgreen"
                            , color_base_rate = "grey"
                            ){
  df_calib = df_calib %>%
    mutate( category_id = as.character(category_id) ) %>%
    filter( category_id %in% category_id_str )  %>%
    mutate( category_id = as.factor(category_id) )

  if( ! is_null(colors) ){
    df_calib = df_calib %>%
      mutate( color = colors[as.character(category_id)],
              color = ifelse( is.na(color), color_base_rate, color))
  }
  
  p = ggplot(df_calib, aes(mean_prop_bin, obs_prop) ) +
    geom_pointrange( aes( ymin = conf_low, ymax = conf_high, color = color), size = 0.5 ) +
    geom_abline(slope = 1, linetype = 2, color = 'lightgrey') +
    geom_vline( aes(xintercept = total_base_rate)
    , linetype = 3
    , color = 'black') +
    geom_hline( aes(yintercept = total_base_rate)
    , linetype = 3
    , color = 'black') +
    theme_minimal() +
    facet_wrap( ~ category_id, nrow = 1 ) +
    labs(y = 'observed probability + CI75', x = 'predicted probability', color = '') +
    lims( x = c(0,1), y = c(0,1)) +
    scale_color_identity() +
    theme(legend.position = 'none')
  
  # suppress replacing x-scale warning
  suppressMessages({
    p <- p +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("0", "0.25", "0.5", "0.75", "1"))
  })
  
  df_lin_calib = df_lin_calib %>%
    mutate( category_id = as.character(category_id) ) %>%
    filter( category_id %in% category_id_str )  %>%
    mutate( category_id = as.factor(category_id) ) %>%
    unnest(plot_data)
  
  p <- p +
    geom_line(aes(x, y), df_lin_calib,
              alpha = 0.75,
              color = color_calib,
              size = 1
              )
    
  return(p)
}

qract_plot_calibration_pub = function(category_id_str = 'sfty',
                                df_calib,
                                df_lin_calib,
                                df_cat,
                                uniform_color = "black",
                                ...){
  

  colors <- rep(uniform_color, length(category_id_str))
  
  names(colors) <- qract_pretty_category(category_id_str, df_cat = df_cat)
  
  df_calib <- df_calib %>%
    ungroup() %>%
    mutate(category_id = qract_pretty_category(category_id, df_cat = df_cat))
  
  df_lin_calib <- df_lin_calib %>%
    ungroup() %>%
    mutate(category_id = qract_pretty_category(category_id, df_cat = df_cat))
    
  category_id_str <- qract_pretty_category(category_id_str, df_cat = df_cat)
    
  p <- qract_plot_calibration(df_calib = df_calib,
                       df_lin_calib = df_lin_calib,
                       category_id_str = category_id_str,
                       colors = colors,
                       ...
                      )
  return(p)
}


