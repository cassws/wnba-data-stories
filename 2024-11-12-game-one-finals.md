Liberty Lynx Game One
================
2024-10-12

## What a start!

This is a little R investigation to the question:

> How did the Lynx come back to win so decisively?

Let’s get started with some WNBA R packages.

``` r
library(wehoop)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
library(ggforce)
library(gtExtras)
```

    ## Loading required package: gt

``` r
wnba_pbp <- wehoop::load_wnba_pbp()
```

``` r
finals_game_one_id <- 401726988

finals_game_one_pbp <- wnba_pbp %>%
  filter(game_id == finals_game_one_id)
```

``` r
#finals_game_one_pbp %>%
#  View()
# 
player_court_id <- 2987891
player_phee_id <- 3917450
# 
# finals_game_one_pbp %>%
#   filter(athlete_id_1 == player_court_id) %>%
#   filter(shooting_play == TRUE) %>%
#   View()
# 
# finals_game_one_pbp %>%
#   filter(athlete_id_1 == player_phee_id) %>%
#   filter(shooting_play == TRUE) %>%
#   View()
# 
```

``` r
finals_game_one_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  filter(athlete_id_1 == player_court_id) %>%
  filter(shooting_play == TRUE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = scoring_play, size = 8)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle("This chart was an excuse to watch Courtney Williams replays") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
```

![](2024-11-12-game-one-finals_files/figure-gfm/court-shot-chart-1.png)<!-- -->

``` r
finals_game_one_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  filter(athlete_id_1 == player_phee_id) %>%
  filter(shooting_play == TRUE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = scoring_play, size = 8)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle("Phee is incredible") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
```

![](2024-11-12-game-one-finals_files/figure-gfm/phee-shot-chart-1.png)<!-- -->

``` r
# find all 4 point_plays

wnba_pbp %>%
  mutate(day = as.Date(wallclock)) %>%
  group_by(home_team_mascot, away_team_mascot, season, game_id, day, athlete_id_1,period_display_value, clock_display_value) %>%
  summarize(total_points = sum(score_value)) %>%
  filter(total_points > 3) %>%
  arrange(desc(total_points))
```

    ## `summarise()` has grouped output by 'home_team_mascot', 'away_team_mascot',
    ## 'season', 'game_id', 'day', 'athlete_id_1', 'period_display_value'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 20 × 9
    ## # Groups:   home_team_mascot, away_team_mascot, season, game_id, day,
    ## #   athlete_id_1, period_display_value [20]
    ##    home_team_mascot away_team_mascot season   game_id day        athlete_id_1
    ##    <chr>            <chr>             <int>     <int> <date>            <int>
    ##  1 Wings            Fever              2024 401620409 2024-09-01      3142191
    ##  2 Aces             Lynx               2024 401620285 2024-06-12      2529205
    ##  3 Dream            Aces               2024 401620257 2024-06-01      4065870
    ##  4 Fever            Mercury            2024 401620370 2024-08-16      4433403
    ##  5 Fever            Sun                2024 401620232 2024-05-20      3142191
    ##  6 Fever            Sun                2024 401620397 2024-08-29          869
    ##  7 Liberty          Lynx               2024 401726988 2024-10-11      2987891
    ##  8 Liberty          Sky                2024 401620239 2024-05-24      2998928
    ##  9 Liberty          Sun                2024 401620362 2024-07-16      4066533
    ## 10 Lynx             Sparks             2024 401620455 2024-09-20      2987891
    ## 11 Lynx             Storm              2024 401620226 2024-05-18      2529205
    ## 12 Lynx             Sun                2024 401725079 2024-10-09      2987891
    ## 13 Mercury          Sun                2024 401620440 2024-09-14      4068885
    ## 14 Mystics          Liberty            2024 401620448 2024-09-18      3056730
    ## 15 Sky              Fever              2024 401620313 2024-06-23      3142191
    ## 16 Sparks           Lynx               2024 401620270 2024-06-06      4398764
    ## 17 Storm            Mercury            2024 401620423 2024-09-08      2491205
    ## 18 Sun              Lynx               2024 401725077 2024-10-05      2529205
    ## 19 Wings            Aces               2024 401620396 2024-08-28      4281929
    ## 20 Wings            Sparks             2024 401620392 2024-08-25      4433630
    ## # ℹ 3 more variables: period_display_value <chr>, clock_display_value <chr>,
    ## #   total_points <int>

There was actually one FIVE-point play - kelsey mitchell was fouled by
arike shooting a 3 on Sept 1st. Arike got a technical for complaining -
so it was a 5-point play!

``` r
wnba_pbp %>%
  mutate(day = as.Date(wallclock)) %>%
  group_by(home_team_mascot, away_team_mascot, season, game_id, day, athlete_id_1,period_display_value, clock_display_value) %>%
  summarize(total_points = sum(score_value)) %>%
  filter(total_points > 3) %>%
  arrange(desc(total_points)) %>%
  select(total_points) %>%
  group_by(total_points) %>%
  summarise(plays_in_2024 = n()) %>%
  rename(total_points_on_play = total_points) %>%
  select(plays_in_2024, total_points_on_play) %>%
  gt() %>% 
  #gt_theme_guardian() %>% 
  tab_header(title = "Four point games in 2024... wait, what?")
```

    ## `summarise()` has grouped output by 'home_team_mascot', 'away_team_mascot',
    ## 'season', 'game_id', 'day', 'athlete_id_1', 'period_display_value'. You can
    ## override using the `.groups` argument.
    ## Adding missing grouping variables: `home_team_mascot`, `away_team_mascot`,
    ## `season`, `game_id`, `day`, `athlete_id_1`, `period_display_value`

<div id="iqdrnrklxq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#iqdrnrklxq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#iqdrnrklxq thead, #iqdrnrklxq tbody, #iqdrnrklxq tfoot, #iqdrnrklxq tr, #iqdrnrklxq td, #iqdrnrklxq th {
  border-style: none;
}
&#10;#iqdrnrklxq p {
  margin: 0;
  padding: 0;
}
&#10;#iqdrnrklxq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#iqdrnrklxq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#iqdrnrklxq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#iqdrnrklxq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#iqdrnrklxq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#iqdrnrklxq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#iqdrnrklxq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#iqdrnrklxq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#iqdrnrklxq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#iqdrnrklxq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#iqdrnrklxq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#iqdrnrklxq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#iqdrnrklxq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#iqdrnrklxq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#iqdrnrklxq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iqdrnrklxq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#iqdrnrklxq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#iqdrnrklxq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#iqdrnrklxq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iqdrnrklxq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#iqdrnrklxq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iqdrnrklxq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#iqdrnrklxq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iqdrnrklxq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#iqdrnrklxq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iqdrnrklxq .gt_left {
  text-align: left;
}
&#10;#iqdrnrklxq .gt_center {
  text-align: center;
}
&#10;#iqdrnrklxq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#iqdrnrklxq .gt_font_normal {
  font-weight: normal;
}
&#10;#iqdrnrklxq .gt_font_bold {
  font-weight: bold;
}
&#10;#iqdrnrklxq .gt_font_italic {
  font-style: italic;
}
&#10;#iqdrnrklxq .gt_super {
  font-size: 65%;
}
&#10;#iqdrnrklxq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#iqdrnrklxq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#iqdrnrklxq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#iqdrnrklxq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#iqdrnrklxq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#iqdrnrklxq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#iqdrnrklxq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#iqdrnrklxq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#iqdrnrklxq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Four point games in 2024... wait, what?</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="plays_in_2024">plays_in_2024</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="total_points_on_play">total_points_on_play</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="plays_in_2024" class="gt_row gt_right">19</td>
<td headers="total_points_on_play" class="gt_row gt_right">4</td></tr>
    <tr><td headers="plays_in_2024" class="gt_row gt_right">1</td>
<td headers="total_points_on_play" class="gt_row gt_right">5</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
finals_game_one_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  filter(athlete_id_1 == player_court_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(sequence_number == 482) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw,  size = 8)) +
  geom_point() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle("Wasn't this shot from the edge of the key?") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
```

![](2024-11-12-game-one-finals_files/figure-gfm/deep%20dive-1.png)<!-- -->

Now here’s Phee in the post-season

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_court_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  filter(scoring_play == TRUE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = type_text, size = 8, alpha = 0.4)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle(" Courtney Williams is KILLING IT with high-momentum jump shots near the free-throw line") +
    theme_void() +
   # theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text=element_text(size=6))
```

![](2024-11-12-game-one-finals_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_court_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  filter(scoring_play == FALSE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = type_text, size = 8, alpha = 0.4)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle(" Her traditional KILL ZONE is less successful this post-season. Here are her misses.") +
    theme_void() +
   # theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text=element_text(size=6))
```

![](2024-11-12-game-one-finals_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_court_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  group_by(type_text, scoring_play ) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = scoring_play , values_from = count, values_fill = 0) %>%
  mutate(attempts = `FALSE` + `TRUE`) %>%
  mutate(success_rate = `TRUE` / attempts) %>%
  rename(makes = `TRUE`) %>%
  arrange(desc(success_rate)) %>%
  select(-`FALSE`) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  mutate(makes_and_attempts = paste0(makes, " for ", attempts)) %>%
  rename(jumpshot_type = type_text) %>%
  select(jumpshot_type, makes_and_attempts, success_rate) %>%
  gt() %>% 
  #gt_theme_guardian() %>% 
  tab_header(title = "Since the start of this postseason, Courtney has struggled with pullup jump shots - except when she's mid-sprint!!")
```

    ## `summarise()` has grouped output by 'type_text'. You can override using the
    ## `.groups` argument.

<div id="dyewoahlym" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dyewoahlym table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dyewoahlym thead, #dyewoahlym tbody, #dyewoahlym tfoot, #dyewoahlym tr, #dyewoahlym td, #dyewoahlym th {
  border-style: none;
}
&#10;#dyewoahlym p {
  margin: 0;
  padding: 0;
}
&#10;#dyewoahlym .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dyewoahlym .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#dyewoahlym .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#dyewoahlym .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#dyewoahlym .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#dyewoahlym .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dyewoahlym .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dyewoahlym .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#dyewoahlym .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dyewoahlym .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#dyewoahlym .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#dyewoahlym .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dyewoahlym .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dyewoahlym .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#dyewoahlym .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dyewoahlym .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#dyewoahlym .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dyewoahlym .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dyewoahlym .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dyewoahlym .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dyewoahlym .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dyewoahlym .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dyewoahlym .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dyewoahlym .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dyewoahlym .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dyewoahlym .gt_left {
  text-align: left;
}
&#10;#dyewoahlym .gt_center {
  text-align: center;
}
&#10;#dyewoahlym .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dyewoahlym .gt_font_normal {
  font-weight: normal;
}
&#10;#dyewoahlym .gt_font_bold {
  font-weight: bold;
}
&#10;#dyewoahlym .gt_font_italic {
  font-style: italic;
}
&#10;#dyewoahlym .gt_super {
  font-size: 65%;
}
&#10;#dyewoahlym .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dyewoahlym .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dyewoahlym .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dyewoahlym .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dyewoahlym .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dyewoahlym .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dyewoahlym .gt_indent_5 {
  text-indent: 25px;
}
&#10;#dyewoahlym .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#dyewoahlym div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Since the start of this postseason, Courtney has struggled with pullup jump shots - except when she's mid-sprint!!</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="makes_and_attempts">makes_and_attempts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="success_rate">success_rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Driving Floating Bank Jump Shot">Driving Floating Bank Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Driving Floating Bank Jump Shot  makes_and_attempts" class="gt_row gt_left">1 for 1</td>
<td headers="Driving Floating Bank Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Fade Away Jump Shot">Fade Away Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Fade Away Jump Shot  makes_and_attempts" class="gt_row gt_left">1 for 1</td>
<td headers="Fade Away Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Running Pullup Jump Shot">Running Pullup Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Running Pullup Jump Shot  makes_and_attempts" class="gt_row gt_left">5 for 8</td>
<td headers="Running Pullup Jump Shot  success_rate" class="gt_row gt_right">0.6250000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Jump Shot">Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Jump Shot  makes_and_attempts" class="gt_row gt_left">13 for 21</td>
<td headers="Jump Shot  success_rate" class="gt_row gt_right">0.6190476</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Pullup Jump Shot">Pullup Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Pullup Jump Shot  makes_and_attempts" class="gt_row gt_left">14 for 46</td>
<td headers="Pullup Jump Shot  success_rate" class="gt_row gt_right">0.3043478</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Driving Floating Jump Shot">Driving Floating Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Driving Floating Jump Shot  makes_and_attempts" class="gt_row gt_left">0 for 1</td>
<td headers="Driving Floating Jump Shot  success_rate" class="gt_row gt_right">0.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Running Jump Shot">Running Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Running Jump Shot  makes_and_attempts" class="gt_row gt_left">0 for 2</td>
<td headers="Running Jump Shot  success_rate" class="gt_row gt_right">0.0000000</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Let’s compare with Phee

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_phee_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  group_by(type_text, scoring_play ) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = scoring_play , values_from = count, values_fill = 0) %>%
  mutate(attempts = `FALSE` + `TRUE`) %>%
  mutate(success_rate = `TRUE` / attempts) %>%
  rename(makes = `TRUE`) %>%
  arrange(desc(attempts)) %>%
  select(-`FALSE`) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  mutate(makes_and_attempts = paste0(makes, " for ", attempts)) %>%
  rename(jumpshot_type = type_text) %>%
  select(jumpshot_type, makes_and_attempts, success_rate) %>%
  gt() %>% 
  #gt_theme_guardian() %>% 
  tab_header(title = "Napheesa Collier shows a similar inclination towards more complex jump shots. But hers are pivot-y!")
```

    ## `summarise()` has grouped output by 'type_text'. You can override using the
    ## `.groups` argument.

<div id="vuhepfdpij" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vuhepfdpij table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vuhepfdpij thead, #vuhepfdpij tbody, #vuhepfdpij tfoot, #vuhepfdpij tr, #vuhepfdpij td, #vuhepfdpij th {
  border-style: none;
}
&#10;#vuhepfdpij p {
  margin: 0;
  padding: 0;
}
&#10;#vuhepfdpij .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vuhepfdpij .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#vuhepfdpij .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#vuhepfdpij .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#vuhepfdpij .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#vuhepfdpij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vuhepfdpij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vuhepfdpij .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#vuhepfdpij .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vuhepfdpij .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#vuhepfdpij .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#vuhepfdpij .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vuhepfdpij .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vuhepfdpij .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#vuhepfdpij .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vuhepfdpij .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#vuhepfdpij .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vuhepfdpij .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vuhepfdpij .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vuhepfdpij .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vuhepfdpij .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vuhepfdpij .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vuhepfdpij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vuhepfdpij .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vuhepfdpij .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vuhepfdpij .gt_left {
  text-align: left;
}
&#10;#vuhepfdpij .gt_center {
  text-align: center;
}
&#10;#vuhepfdpij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vuhepfdpij .gt_font_normal {
  font-weight: normal;
}
&#10;#vuhepfdpij .gt_font_bold {
  font-weight: bold;
}
&#10;#vuhepfdpij .gt_font_italic {
  font-style: italic;
}
&#10;#vuhepfdpij .gt_super {
  font-size: 65%;
}
&#10;#vuhepfdpij .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vuhepfdpij .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vuhepfdpij .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vuhepfdpij .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vuhepfdpij .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vuhepfdpij .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vuhepfdpij .gt_indent_5 {
  text-indent: 25px;
}
&#10;#vuhepfdpij .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#vuhepfdpij div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Napheesa Collier shows a similar inclination towards more complex jump shots. But hers are pivot-y!</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="makes_and_attempts">makes_and_attempts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="success_rate">success_rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Jump Shot">Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Jump Shot  makes_and_attempts" class="gt_row gt_left">10 for 32</td>
<td headers="Jump Shot  success_rate" class="gt_row gt_right">0.3125000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Turnaround Jump Shot">Turnaround Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Turnaround Jump Shot  makes_and_attempts" class="gt_row gt_left">10 for 16</td>
<td headers="Turnaround Jump Shot  success_rate" class="gt_row gt_right">0.6250000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Turnaround Fade Away Jump Shot">Turnaround Fade Away Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Turnaround Fade Away Jump Shot  makes_and_attempts" class="gt_row gt_left">5 for 11</td>
<td headers="Turnaround Fade Away Jump Shot  success_rate" class="gt_row gt_right">0.4545455</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Fade Away Jump Shot">Fade Away Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Fade Away Jump Shot  makes_and_attempts" class="gt_row gt_left">4 for 7</td>
<td headers="Fade Away Jump Shot  success_rate" class="gt_row gt_right">0.5714286</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Pullup Jump Shot">Pullup Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Pullup Jump Shot  makes_and_attempts" class="gt_row gt_left">2 for 5</td>
<td headers="Pullup Jump Shot  success_rate" class="gt_row gt_right">0.4000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Turnaround Bank Jump Shot">Turnaround Bank Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Turnaround Bank Jump Shot  makes_and_attempts" class="gt_row gt_left">2 for 2</td>
<td headers="Turnaround Bank Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Driving Floating Bank Jump Shot">Driving Floating Bank Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Driving Floating Bank Jump Shot  makes_and_attempts" class="gt_row gt_left">1 for 1</td>
<td headers="Driving Floating Bank Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Driving Floating Jump Shot">Driving Floating Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Driving Floating Jump Shot  makes_and_attempts" class="gt_row gt_left">0 for 1</td>
<td headers="Driving Floating Jump Shot  success_rate" class="gt_row gt_right">0.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Floating Jump Shot">Floating Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Floating Jump Shot  makes_and_attempts" class="gt_row gt_left">0 for 1</td>
<td headers="Floating Jump Shot  success_rate" class="gt_row gt_right">0.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Jump Shot Bank">Jump Shot Bank</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Jump Shot Bank  makes_and_attempts" class="gt_row gt_left">0 for 1</td>
<td headers="Jump Shot Bank  success_rate" class="gt_row gt_right">0.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Running Jump Shot">Running Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Running Jump Shot  makes_and_attempts" class="gt_row gt_left">1 for 1</td>
<td headers="Running Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="Step Back Jump Shot">Step Back Jump Shot</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Step Back Jump Shot  makes_and_attempts" class="gt_row gt_left">1 for 1</td>
<td headers="Step Back Jump Shot  success_rate" class="gt_row gt_right">1.0000000</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_phee_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  filter(scoring_play == TRUE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = type_text, size = 8, alpha = 0.4)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle(" High-pivot Napheesa Collier success this postseason!!") +
    theme_void() +
   # theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text=element_text(size=6))
```

![](2024-11-12-game-one-finals_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
wnba_pbp %>%
  mutate(coordinate_x_raw = 50-coordinate_x_raw) %>%
  mutate(day = as.Date(wallclock)) %>%
  filter(day >= "2024-09-22") %>%
  filter(athlete_id_1 == player_phee_id) %>%
  filter(shooting_play == TRUE) %>%
  filter(!grepl("Free Throw", type_text)) %>%
  filter(grepl("Jump Shot", type_text)) %>%
  filter(scoring_play == FALSE) %>%
  ggplot(aes(x = coordinate_x_raw, y = coordinate_y_raw, color = type_text, size = 8, alpha = 0.4)) +
  geom_jitter() +
    geom_segment(aes(x = 0, y = -4, xend = 50, yend = -4, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = 13, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 17, y = -4, xend = 17, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 19, y = -4, xend = 19, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 31, y = -4, xend = 31, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 33, y = -4, xend = 33, yend = 13, alpha =0.1), inherit.aes = FALSE) +
    geom_segment(aes(x = 22, y = -0.75, xend = 28, yend = -0.75, alpha =0.1), inherit.aes = FALSE) +
    geom_circle(aes(x0 = 25, y0 = 0, r = 0.75), inherit.aes = FALSE) +
    geom_arc(aes(x0 = 25, y0= 0, r = 22.145, start = -1.2, end = 1.2), inherit.aes = FALSE) +
    #geom_segment(aes(x = 3, y = -4, xend = 3, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    #geom_segment(aes(x = 47, y = -4, xend = 47, yend = 10, alpha =0.1), inherit.aes = FALSE) +
    scale_size(guide = "none") +
    scale_alpha(guide = "none") +
    xlab("")+
    ylab("")+
    ggtitle(" High-pivot Napheesa Collier success this postseason!!") +
    theme_void() +
   # theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text=element_text(size=6))
```

![](2024-11-12-game-one-finals_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
