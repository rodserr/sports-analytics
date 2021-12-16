epl_theme <- function(){
  theme(
    text = element_text(family = 'oxanium'),
    panel.background = element_rect(fill = .purple_epl),
    plot.background = element_rect(color = .purple_epl, fill = .purple_epl),
    title = element_text(colour = "white", size = 13),
    axis.line = element_line(color = 'white', size = .2),
    panel.grid = element_blank(),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    legend.background = element_rect(color = .purple_epl, fill = .purple_epl),
    legend.text = element_text(color = 'white'),
    legend.key = element_rect(color = .purple_epl, fill = .purple_epl),
    strip.background = element_blank(),
    strip.text = element_text(colour = 'white')
  )
}

add_logo <- function(plot_path, logo_path, shield_path, logo_scale = 10, shield_scale = 10){
  
  # original function at: https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
  # Requires magick R Package https://github.com/ropensci/magick
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  shield_raw <- magick::image_read(shield_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  shield <- magick::image_scale(shield_raw, as.character(plot_width/shield_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  shield_width <- magick::image_info(shield)$width
  shield_height <- magick::image_info(shield)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  x_pos = plot_width - logo_width - 0.01 * plot_width
  y_pos = 0.01 * plot_height
  x_pos_shield = x_pos - shield_width - .025*x_pos
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos)) %>% 
    magick::image_composite(shield, offset = paste0("+", x_pos_shield, "+", y_pos))
  
}

hex_shot_map <- function(team_shot_df, .bins = c(15, 25)){
  op_shots <- team_shot_df %>% 
    filter(X > .5, situation == 'OpenPlay') %>%
    mutate(X = X*100, Y = 100-Y*100)
  
  op_shots %>% 
    ggplot(aes(X, Y)) +
    annotate_pitch(colour = 'white', fill = .purple_epl) +
    theme_pitch() +
    stat_binhex(alpha = 0.7, bins = .bins) +
    scale_fill_gradient(low = .red_epl, high = .green_epl, labels = label_number(accuracy = 1)) +
    geom_point(data = op_shots %>% filter(result == 'Goal'), aes(color = 'goal'), alpha = .4) +
    scale_color_manual(values = c('white'), labels = '', 
                       guide = guide_legend(order = 1, title.vjust = -.2, override.aes = list(alpha = .7, size = 3)) ) +
    facet_wrap(~against) +
    coord_flip(xlim = c(60, 100), ylim = c(0, 100)) +
    labs(title = 'Hex Map of Shots Location', fill = 'Number of shots', color = 'Goals',
         subtitle = 'Only Open-Play situations', caption = .caption) +
    epl_theme() +
    theme(
      legend.position = 'top',
      legend.justification = 'center',
      strip.text = element_text(size = 14),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

shot_map <- function(team_shot_df){
  team_shot_df %>% 
    filter(X > .5) %>%
    mutate(X = X*100, Y = 100-Y*100) %>% 
    ggplot(aes(X, Y, color = result == 'Goal', size = xG)) +
    annotate_pitch(colour = 'white', fill = .purple_epl) +
    theme_pitch() +
    geom_point(alpha = .7) +
    scale_color_manual(values = c(.red_epl, .green_epl)) +
    coord_flip(xlim = c(60, 100), ylim = c(0, 100)) +
    facet_wrap(~against) +
    ggtitle('Shots locations') +
    epl_theme() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

density_minute_shot <- function(team_shot_df){
  team_shot_df %>%
    filter(result == 'Goal') %>% 
    transmute(against, minute, type = 'Goal') %>% 
    bind_rows(team_shots %>% transmute(against, minute, type = 'Shot')) %>% 
    ggplot(aes(minute, color = type)) +
    geom_density() +
    facet_wrap(~against, scales = 'free_y', ncol = 2) +
    scale_color_manual(values = c('Goal' = .green_epl, 'Shot'=.red_epl)) +
    scale_x_continuous(labels = label_number(suffix = "'")) +
    labs(title = 'Distribution of Goals and Shots Across Game Minutes', y = '', color = '', x = 'Minute') +
    epl_theme() +
    theme(
      strip.text = element_text(size = 14),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
    )
}

cumulative_xG <- function(team_shot_df, team_name = 'Liverpool'){
  match_results <- team_shot_df %>%
    distinct(date, home_goals, away_goals, home_team, away_team) %>% 
    mutate(
      date,
      match_result = case_when(
        home_team == team_name & home_goals > away_goals ~ 'Win',
        home_team != team_name & home_goals < away_goals ~ 'Win',
        home_goals == away_goals ~ 'Tied',
        T ~ 'Loss'
      )
    )
  
  team_shot_df %>% 
    group_by(date, against) %>% 
    summarise(xG = sum(xG), .groups = 'drop') %>%
    pivot_wider(names_from = against, values_from = xG) %>% 
    arrange(date) %>% 
    mutate(xG_diff = For-Against, rolling_xG = cumsum(xG_diff)) %>%
    left_join(match_results, by = 'date') %>% 
    rownames_to_column('matchday') %>% 
    ggplot(aes(x = as.numeric(matchday), y = rolling_xG)) +
    geom_line(color = 'white') +
    geom_point(aes(color = match_result), size = 3) +
    scale_color_manual(values = c('Win' = .green_epl, 'Tied'='gray', 'Loss'= .red_epl)) +
    labs(title = 'Cumulative Expected Goal Differential (For - Against)', x = 'Match Day', 
         y = 'xG - xG Against', color = 'Match Result') +
    epl_theme()
}

attacking_network <- function(season_shots_df, team_name = 'Liverpool', 
                              .layout = 'fr', n_breaks = 5, direction_text = c(.58, .85)){
  
  connection_df <- season_shots_df %>%
    filter(team_shot == team_name, !is.na(player_assisted)) %>% 
    group_by(player, player_assisted) %>% 
    summarise(
      xG = sum(xG),
      n_plays = n(),
      .groups = 'drop'
    ) %>% 
    filter(n_plays > 2) %>% 
    slice_max(order_by = n_plays, n = 10, with_ties = F)
  
  graph <- connection_df %>%
    relocate(player_assisted) %>% 
    graph_from_data_frame()
  
  ggraph(graph, layout = .layout) + 
    geom_edge_arc(
      aes(
        alpha = ..index..,
        edge_width = n_plays,
        color = xG,
        start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name)
      )
    ) + 
    geom_node_text(aes(label = name), color = .accent) +
    scale_edge_alpha(guide = 'edge_direction') +
    scale_edge_color_gradient(low = .red_epl, high = .green_epl) +
    scale_edge_width(labels = label_number(accuracy = 1), 
                     breaks = breaks_extended(n = n_breaks)) +
    guides(edge_width = guide_legend(override.aes = list(edge_color = 'white', edge_alpha = .7)),
           edge_alpha = guide_edge_direction(arrow = F, override.aes = list(color = 'white') ) ) +
    labs(
      title = 'Top 10: Shot Interactions', 
      subtitle = 'Only Shots that involves an assisted player. Minimum of 3 interactions',
      edge_width = 'Number of plays', edge_alpha = 'Edge direction', edge_color = 'Total xG',
      tag = "Passer                Scorer", 
      caption = .caption
    ) +
    epl_theme() +
    theme(
      legend.position = 'top',
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.tag.position = direction_text,
      plot.tag = element_text(size = 8),
      legend.justification = 'left',
      plot.margin = unit(c(1, .2, .2, .2), "lines")
    )
}

radar_position <- function(league_distribution_df, team_name = 'Liverpool'){
  league_distribution_df %>%
    group_by(name) %>% 
    arrange(value) %>% 
    mutate(league_position = 1:n()) %>% 
    filter(team_shot == team_name) %>% 
    select(team_shot, name, league_position) %>% 
    pivot_wider(names_from = name, values_from = league_position) %>% 
    ggradar(values.radar = c("20th", "10th", "1st"), 
            # base.size = 1,
            grid.min = 1, grid.mid = 10, grid.max = 20,
            gridline.mid.colour = .red_epl,
            group.colours = .green_epl,
            background.circle.transparency = 0,
            gridline.min.linetype = "solid",
            gridline.mid.linetype = "dashed",
            gridline.max.linetype = "solid",
            grid.label.size = 4,
            axis.label.size = 3.5,
            group.line.width = 1, group.point.size = 3,
            plot.extent.x.sf = 1.5, plot.extent.y.sf = 1.2) +
    epl_theme() +
    theme(
      legend.position = 'top',
      legend.text = element_text(size = 9),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 17, hjust = .15)
    ) +
    labs(title = 'Position in League')
}

metric_league_distrib <- function(league_distribution_df, team_name = 'Liverpool'){
  league_distribution_df %>%
    ggplot(aes(x = value)) +
    geom_density(color = 'white') +
    geom_point(data = league_distribution_df %>% filter(team_shot == team_name), 
               aes(y = 0,  shape = team_name, color = team_name), 
               fill = .green_epl, alpha = .8, size = 3) +
    facet_wrap(~name, scales = 'free', ncol = 1, strip.position = 'left') +
    labs(y = '', x = '', title = 'League Distribution') +
    scale_x_continuous(breaks = ~c(min(.x), max(.x)), labels = ~round(.x, 2)) +
    scale_color_manual(values = .green_epl) +
    scale_shape_manual(values = 25) +
    guides(
      color = guide_legend(team_name),
      shape = guide_legend(team_name)
    ) +
    coord_cartesian(clip = 'off') +
    epl_theme() +
    theme(strip.text.y.left = element_text(angle = 0),
          strip.placement = 'inside',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(color = 'white', size = .2),
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          legend.position = 'top',
          legend.justification = c(-.13, 0),
          legend.title = element_blank(),
          plot.margin = unit(c(0, 1.4, 0, 0), "lines"),
          plot.title = element_text(hjust = -.15))
}


