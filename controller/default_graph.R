source('utils/set_graph_caption.R')

age_table <- readRDS('data/age_table.rds')
sex_table <- readRDS('data/sex_table.rds')
weath_table <- readRDS('data/weath_table.rds')
week_table <- readRDS('data/week_table.rds')
place_table <- readRDS('data/place_table.rds')

default_graph_fn <- function() {
  data_framed_age <- set_graph_caption(age_table, '연령대별 주문한 음식', '주문횟수')
  data_framed_sex <- set_graph_caption(sex_table, '성별 주문한 음식', '주문횟수')
  data_framed_weath <- set_graph_caption(weath_table, '날씨별 주문한 음식', '주문횟수')
  data_framed_week <- set_graph_caption(week_table, '요일별 주문한 음식', '주문횟수')
  data_framed_place <- set_graph_caption(place_table, '구별 추천 음식', '주문횟수')

  good <- list(
    age = data_framed_age,
    sex = data_framed_sex,
    weath = data_framed_weath,
    week = data_framed_week,
    place = data_framed_place
  )

  return(good)
}
