# Libraries----
library(worldfootballR)
library(tidyverse)
library(ggsoccer)
library(ggraph)
library(igraph)
library(patchwork)
library(magick)
library(scales)
library(grid)
library(ggradar)
library(showtext)

# Setups----
.green_epl <- "#04b46c"
.purple_epl <- "#3f043f"
.red_epl <- "#fc045c"
.background <- 'black'
.accent <- 'white'

.caption <- 'data source: understat.com | twitter: @rodserrr'
font_add_google("Oxanium", "oxanium")
showtext_auto()

update_geom_defaults("text", list(colour = "white"))
# Data----
season_shots_raw <- understat_league_season_shots('EPL', 2021) %>% 
  mutate(team_shot = if_else(h_a == 'h', home_team, away_team))

unique(season_shots$team_shot)

.matchday_rival <- 'Newcastle United'
.shield_path <- "https://sportslogohistory.com/wp-content/uploads/2020/04/newcastle_united_fc_1988-pres.png"
.logo_path <- "lfc_prematch/epl-logo.png"

season_shots <- season_shots_raw %>% filter(lubridate::ymd_hms(date) < lubridate::today()-lubridate::days(2))
season_shots %>% distinct(team_shot, date) %>% count(team_shot)

.match_day <- 17
# season_shots %>% write_csv('season_shot_template.csv')
# season_shots <- read_csv('lfc_prematch/season_shot_template.csv')

team_shots <- season_shots %>%
  filter(home_team == .matchday_rival | away_team == .matchday_rival) %>% 
  mutate(against = if_else(team_shot != .matchday_rival, 'Against', 'For'),
         minute = as.numeric(minute))

league_distribution <- season_shots %>% 
  group_by(team_shot) %>% 
  summarise(
    `Shots per Game` = n()/n_distinct(date),
    `xG per Game` = sum(xG)/n_distinct(date), 
    `xG per Shot` = sum(xG)/n(),
    `Total Set Piece xG` = sum(xG[situation == 'SetPiece']),
    `Total Corner xG` = sum(xG[situation == 'FromCorner'])
  ) %>% 
  pivot_longer(-team_shot) %>% 
  mutate(name = as_factor(name) %>% forcats::fct_inorder())

# League Comparison-----
.comparison_path <- sprintf('lfc_prematch/plots/match%s_comparison.png', .match_day)
xg_densities <- metric_league_distrib(league_distribution, .matchday_rival)

xg_radar <- radar_position(league_distribution, .matchday_rival)

xg_densities+xg_radar + 
  plot_layout(widths = c(1, 2)) +
  plot_annotation(caption = .caption, 
                  theme = epl_theme())
ggsave(.comparison_path, width = 928, height = 480,
       units = "px", device = 'png', dpi = 110)

comparison_final <- add_logo(
  .comparison_path, 
  .logo_path, 
  .shield_path,
  logo_scale = 20, shield_scale = 17)
image_write(comparison_final, .comparison_path)

# Density minutes and cumulative xG----
.distrib_path <- sprintf('lfc_prematch/plots/match%s_distrib.png', .match_day)
minute_density_shot <- density_minute_shot(team_shots)
cumulative_xGD <- cumulative_xG(team_shots, team_name = .matchday_rival)

minute_density_shot/cumulative_xGD +
  plot_annotation(theme = epl_theme(), caption = .caption)

ggsave(.distrib_path, width = 928, height = 480, units = "px", device = 'png', dpi = 110)

distrib_final <- add_logo(
  .distrib_path, 
  .logo_path, 
  .shield_path,
  logo_scale = 20, shield_scale = 17)
image_write(distrib_final, .distrib_path)


# Shots-----
.hex_path <- sprintf('lfc_prematch/plots/match%s_hex.png', .match_day)
hex_shot_map(team_shots, c(18, 28))
ggsave(.hex_path, width = 928, height = 480, units = "px", device = 'png', dpi = 110)

hex_final <- add_logo(
  .hex_path, 
  .logo_path, 
  .shield_path,
  logo_scale = 20, shield_scale = 17)
image_write(hex_final, .hex_path)

# Network-----
.net_path <- sprintf('lfc_prematch/plots/match%s_net.png', .match_day)
attacking_network(season_shots, .matchday_rival, .layout = 'star', n_breaks = 3, direction_text = c(.21, .85))
ggsave(.net_path, width = 928, height = 480, units = "px", device = 'png', dpi = 110)

net_final <- add_logo(
  .net_path, 
  .logo_path, 
  .shield_path,
  logo_scale = 20, shield_scale = 17)
image_write(net_final, .net_path)
