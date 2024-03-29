source('utils/set_graph_caption.R')

age_table <- s3readRDS(object = "age_table.rds", bucket = "mechureal-realdata")
sex_table <- s3readRDS(object = "sex_table.rds", bucket = "mechureal-realdata")
weath_table <- s3readRDS(object = "weath_table.rds", bucket = "mechureal-realdata")
week_table <- s3readRDS(object = "week_table.rds", bucket = "mechureal-realdata")
place_table <- s3readRDS(object = "place_table.rds", bucket = "mechureal-realdata")

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
