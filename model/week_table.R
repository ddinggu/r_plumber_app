###
# 3. 요일변수 기준으로 업종 확인하기
###

source("./arules_data.R", encoding = "UTF-8")

#요일기준으로 업종 뽑기 
adj_rule_week <- subset(ttRule.pruned, (lhs %in% c("요일=월","요일=화","요일=수","요일=목","요일=금","요일=토","요일=일")))

week.df <- as(adj_rule_week,"data.frame")
week.df$rules <- as.character(week.df$rules)
week.df$lhs <- do.call(rbind,strsplit(week.df$rules,"=>"))[,1]
week.df$rhs <- do.call(rbind,strsplit(week.df$rules,"=>"))[,2]

mon <- grep("요일=월" , week.df$lhs)
tue <- grep("요일=화" , week.df$lhs)
wed <- grep("요일=수" , week.df$lhs)
thu <- grep("요일=목" , week.df$lhs)
fir <- grep("요일=금" , week.df$lhs) 
sat <- grep("요일=토" , week.df$lhs)
sun <- grep("요일=일" , week.df$lhs) 

as.matrix(table(week.df[mon,7])) #중국집 14
as.matrix(table(week.df[tue,7])) #중국집 9
as.matrix(table(week.df[wed,7])) #중국집 10
as.matrix(table(week.df[thu,7])) #중국집 11
as.matrix(table(week.df[fir,7])) #중국집 1, 치킨2 
as.matrix(table(week.df[sat,7])) #치킨 1, 패밀리레스토랑 1
as.matrix(table(week.df[sun,7])) #중국집1, 패밀리레스토랑1

week.table <- matrix(c(14,0,0,0,
                       9,0,0,0,
                       10,0,0,0,
                       11,0,0,0,
                       1,2,0,0,
                       0,1,0,1,
                       1,0,0,1),ncol=4 ,byrow = T)
rownames(week.table) <- c("월","화","수","목","금","토","일")
colnames(week.table) <- c("중국집" ,"치킨" , "피자", "패밀리레스토랑")

# 요일별 업종 table
week.table

# # 요일별 업종 그래프를 그리기위한 작업
# week.graph <- list()
# for(i in 1:7){
#   for(j in 1:4){
#     week.graph[[paste(row.names(week.table)[i],colnames(week.table)[j])]] <- 
#       apply(data.frame(요일별=row.names(week.table)[i],업종=colnames(week.table)[j]),2,
#             function(x){rep(x,week.table[i,j])})
#   }
# }

# week.graph <- data.frame(Reduce(rbind, week.graph))

# 요일별 업종 그래프
# library(ggplot2)

# ggplot(week.graph,aes(x=요일별,group=업종,fill=업종))+geom_bar(position = "dodge")+
#   geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9))+theme_classic()+
#   ggtitle("요일별 주문한 음식")+theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))+
#   scale_x_discrete(limits=c("월", "화", "수", "목", "금", "토", "일"))
