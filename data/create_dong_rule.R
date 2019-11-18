source("model/dong_data.R", encoding = "UTF-8")

write.fst(dong.rule, path = "data/dong_data.fst", compress = 50)

put_object(file = 'data/dong_data.fst', object = 'dong_data.fst', bucket = 'mechureal-realdata')
