library(tidyverse)
library(rvest)

get_schedule_months <- function(schedule_link){
  Sys.sleep(2)
  
  read_html(schedule_link) %>% 
    html_node(xpath = '//*[@id="content"]/div[2]') %>% 
    html_elements('a') %>% 
    html_attr('href')
  
}

get_schedule <- function(link_year_month){
  Sys.sleep(2)
  
  url <- paste0('https://www.basketball-reference.com', link_year_month)
  
  schedule <- read_html(url)
  
  box_score_links <- schedule %>%
    html_elements('td.center') %>%
    html_elements('a') %>%
    html_attr('href')
  
  if(is_empty(box_score_links)){
    box_score_links <- NULL
  }
  
  schedule %>% 
    html_node(xpath = '//*[@id="schedule"]') %>% 
    html_table() %>% 
    janitor::clean_names() %>% 
    select(date, start_et, visitor_neutral, pts_visitor = pts, home_neutral, pts_home = pts_2) %>% 
    filter(date != 'Playoffs') %>% 
    mutate(
      box_score_link = box_score_links,
      date = lubridate::mdy(date)
    )
  
}

clean_stats_table <- function(tb){
  tb %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names() %>% 
    filter(!starters %in% c('Team Totals', 'Reserves'),
           mp != 'Did Not Play')
  
}

.seasons <- 2015:2020
schedules <- .seasons %>% 
  set_names(paste0('s', .seasons)) %>% 
  map(~sprintf('https://www.basketball-reference.com/leagues/NBA_%s_games.html', .x)) %>% 
  map(get_schedule_months)

games <- schedules %>% 
  map(function(.x){
    
    .months <- str_extract(.x, '(?<=-).*(?=\\.html)')
    
    .x %>%
      set_names(.months) %>% 
      map(possibly(get_schedule, NULL))
    
  })
beepr::beep()

# In case connection error, verify and append manually
# games %>% 
#   map(~keep(.x, is_null)) %>% 
#   keep(~length(.x)>0)
# 
# games$s2016$february <- get_schedule("/leagues/NBA_2016_games-february.html")
# games$s2016$march <- get_schedule("/leagues/NBA_2016_games-march.html")

games_schedule <- games %>% 
  map_df(function(.x){
    .x %>% 
      map_df(~.x %>% 
               mutate(pts_visitor = as.integer(pts_visitor),
                      pts_home = as.integer(pts_home)))
  }, .id = 'season')

# games_schedule %>% write_csv('games_schedule.csv')
games_schedule %>% count(season)
games_schedule %>%
  filter(is.na(box_score_link)) %>%
  count(season)

# Match Stats----
get_game_stats <- function(.url){
  
  Sys.sleep(2)
  
  box_score <- read_html(paste0('https://www.basketball-reference.com/', .url))
  
  tabl_nodes <- box_score %>% 
    html_elements('table')
  
  ids <- tabl_nodes %>% html_attr('id') %>% str_remove('box-')
  
  tabl_nodes %>% 
    html_table() %>% 
    map(clean_stats_table) %>% 
    set_names(ids)
  
  advanced_stats <- ids %>% str_subset('game-advanced')
  basic_stats <- ids %>% str_subset('game-basic')
  quarter_stats <- ids %>% str_subset('q\\d-basic')
  
  list_stats <- tabl_nodes %>% 
    html_table() %>% 
    map(clean_stats_table) %>% 
    set_names(ids)
  
  player_stats <- full_join(
    
    list_stats[basic_stats] %>% 
      bind_rows(.id = 'team') %>% 
      mutate(team = str_extract(team, '^\\w{3}')),
    
    list_stats[advanced_stats] %>% 
      bind_rows(.id = 'team') %>% 
      mutate(team = str_extract(team, '^\\w{3}')) %>% 
      select(-mp),
    
    by = c('team', 'starters')
  ) %>% 
    mutate(
      mp = mp %>% str_replace(':', '.') %>% as.numeric(),
      across(-c('team', 'starters', 'bpm', 'x'), as.numeric)
    )
  
  quarter_results <- list_stats[quarter_stats] %>% 
    bind_rows(.id = 'id') %>% 
    separate(id, c('team', 'q', 'aux'), sep = '-') %>%
    mutate(pts = as.numeric(pts)) %>% 
    group_by(team, q) %>% 
    summarise(pts = sum(pts, na.rm = T), .groups = 'drop')
  
  list(
    player_stats = player_stats,
    quarter_results = quarter_results
  )
  
}

games_stats <- games_schedule$box_score_link %>% 
  set_names(games_schedule$box_score_link) %>% 
  map(possibly(get_game_stats, NULL))

quarter_results <- games_stats %>% 
  map_df(~.x %>% pluck('quarter_results'), .id = 'id')
quarter_results %>% write_csv('quarter_results.csv')

player_stats <- games_stats %>% 
  map_df(~.x %>% pluck('player_stats'), .id = 'id')
player_stats %>% write_csv('player_stats.csv')

# Second round of scraper
missing_games <- games_schedule$box_score_link[which(!games_schedule$box_score_link %in% player_stats$id)] %>% na.omit()

get_game_stats2 <- function(.url){
  
  Sys.sleep(2)
  
  box_score <- read_html(paste0('https://www.basketball-reference.com/', .url))
  
  tabl_nodes <- box_score %>% 
    html_elements('table')
  
  ids <- tabl_nodes %>% html_attr('id') %>% str_remove('box-')
  
  tabl_nodes %>% 
    html_table() %>% 
    map(clean_stats_table) %>% 
    set_names(ids)
  
  advanced_stats <- ids %>% str_subset('game-advanced')
  basic_stats <- ids %>% str_subset('game-basic')
  quarter_stats <- ids %>% str_subset('q\\d-basic')
  
  list_stats <- tabl_nodes %>% 
    html_table() %>% 
    map(clean_stats_table) %>% 
    set_names(ids)
  
  player_stats <- full_join(
    
    list_stats[basic_stats] %>% 
      bind_rows(.id = 'team') %>% 
      mutate(team = str_extract(team, '^\\w{3}')),
    
    list_stats[advanced_stats] %>% 
      bind_rows(.id = 'team') %>% 
      mutate(team = str_extract(team, '^\\w{3}')) %>% 
      select(-mp),
    
    by = c('team', 'starters')
  ) %>% 
    mutate(
      mp = mp %>% str_replace(':', '.') %>% as.numeric(),
      across(-c('team', 'starters'), as.numeric)
    )
  
  quarter_results <- list_stats[quarter_stats] %>% 
    bind_rows(.id = 'id') %>% 
    separate(id, c('team', 'q', 'aux'), sep = '-') %>%
    mutate(pts = as.numeric(pts)) %>% 
    group_by(team, q) %>% 
    summarise(pts = sum(pts, na.rm = T), .groups = 'drop')
  
  list(
    player_stats = player_stats,
    quarter_results = quarter_results
  )
  
}


pb <- progress_bar$new(total = length(missing_games))
games_stats2 <- missing_games %>% 
  set_names(missing_games) %>% 
  map(function(.x){
    pb$tick()
    gsp <- possibly(get_game_stats2, NULL)
    gsp(.x)
  })

player_stats <- bind_rows(
  player_stats,
  games_stats2 %>% 
    map_df(~.x %>% pluck('player_stats'), .id = 'id') %>%
    select(-na) %>% 
    mutate(across(-c('id', 'team', 'starters'), as.numeric), x = as.character(x))
)
player_stats %>% write_csv('player_stats.csv')

quarter_results <- bind_rows(
  quarter_results,
  games_stats2 %>% map_df(~.x %>% pluck('quarter_results'), .id = 'id')
)
quarter_results %>% write_csv('quarter_results.csv')


