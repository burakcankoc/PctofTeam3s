library(tidyverse)
library(lubridate)
library(httr)
library(hablar)
library(jsonlite)
library(gt)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://www.nba.com/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

get_dataPLAYER <- function(Season) {
  url <-
    paste0(
      "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Usage&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",
      Season,
      "-",
      sprintf('%02d', (Season + 1) %% 100),
      "&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
    )
  
  res <- GET(url = url, add_headers(.headers = headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  df <- df %>% 
    mutate(Season = Season + 1)
  
  return(df)
  
}

Season = c(2014:2020)

df_data <- 
  map_df(Season, get_dataPLAYER) %>%
  retype()

df_t <- df_data %>% 
  filter(MIN > 499) %>% 
  arrange(desc(PCT_FG3M)) %>% 
  slice(1:20) %>% 
  select(Season, PLAYER_NAME, TEAM_ABBREVIATION, PCT_FG3A, PCT_FG3M) %>% 
  ungroup()

df_t %>% 
  gt() %>% 
  tab_header(
    title = md("**Players Who Have Accounted for<br>the Highest % of Team's 3PM**"), 
    subtitle = md("Among players that played at least 500 minutes<br>2014-15 to 2020-21 Regular Season")
  ) %>% 
  cols_label(Season = "Season", 
             PLAYER_NAME = "Player",
             TEAM_ABBREVIATION = "Team",
             PCT_FG3A = "% of Team's 3PA", 
             PCT_FG3M = "% of Team's 3PM") %>% 
  fmt_percent(
    columns = vars(PCT_FG3A, PCT_FG3M),
    decimals = 1
  ) %>% 
  cols_width(1:2 ~ px(100)) %>% 
  cols_width(4:5 ~ px(100)) %>% 
  #cols_width(10 ~ px(60)) %>% 
  opt_row_striping() %>% 
  tab_options(
    table.background.color = "white",
    column_labels.font.size = 11,
    column_labels.font.weight = 'bold',
    row_group.font.weight = 'bold',
    row_group.background.color = "#E5E1D8",
    table.font.size = 10,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 10,
    table.font.names = "Franklin Gothic Medium", 
    data_row.padding = px(2),
    footnotes.padding = px(.5)
  )  %>%
  tab_source_note(source_note = md("Data Source: stats.nba.com<br>Table: Burak Can Koc")) %>% 
  data_color(
    columns = vars(PCT_FG3M),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = c(0,1), 
      na.color = "#005C55FF"
    )
  ) %>% 
  gtsave("ThreePointPctTeam.png")
