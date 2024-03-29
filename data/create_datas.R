## get variables
source("model/arules_data.R", encoding = "UTF-8")
source("model/age_table.R", encoding = "UTF-8")
source("model/place_table.R", encoding = "UTF-8")
source("model/sex_table.R", encoding = "UTF-8")
source("model/weath_table.R", encoding = "UTF-8")
source("model/week_table.R", encoding = "UTF-8")

# write data files
write.fst(all.rule, path = "data/arules_data.fst", compress = 50)

saveRDS(age.table, file = 'data/age_table.rds')
saveRDS(place.table, file = 'data/place_table.rds')
saveRDS(sex.table, file = 'data/sex_table.rds')
saveRDS(weath.table, file = 'data/weath_table.rds')
saveRDS(week.table, file = 'data/week_table.rds')

put_object(file = 'data/arules_data.fst', object = 'arules_data.fst', bucket = 'mechureal-realdata')
put_object(file = 'data/place_table.rds', object = 'place_table.rds', bucket = 'mechureal-realdata')
put_object(file = 'data/sex_table.rds', object = 'sex_table.rds', bucket = 'mechureal-realdata')
put_object(file = 'data/weath_table.rds', object = 'weath_table.rds', bucket = 'mechureal-realdata')
put_object(file = 'data/week_table.rds', object = 'week_table.rds', bucket = 'mechureal-realdata')
