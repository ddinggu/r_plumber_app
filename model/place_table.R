###
# 4. 구별로 뽑기
###

source("./arules_data.R", encoding = "UTF-8")

adj_rule_place <- subset(ttRule.pruned, (lhs %in% paste0("시군구=",unique(food$시군구))))

place.df <- as(adj_rule_place,"data.frame")
place.df$rules <- as.character(place.df$rules)
place.df$lhs <- do.call(rbind,strsplit(place.df$rules,"=>"))[,1]
place.df$rhs <- do.call(rbind,strsplit(place.df$rules,"=>"))[,2]

place.rule <- list()
for(i in 1:length(unique(food$시군구))){
  place.rule[[as.character(unique(food$시군구)[i])]] <- grep(unique(food$시군구)[i],place.df$lhs)
}

place.idx <- lapply(1:length(unique(food$시군구)),function(x){place.df[place.rule[[x]],]})
sa <- lapply(1:length(unique(food$시군구)),function(x){as.data.frame(table(place.idx[[x]]$rhs))})

place.table <- matrix(,nrow =length(unique(food$시군구)),ncol=4,
                      dimnames=list(unique(food$시군구),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:25){
  place.table[i,"중국집"] <- ifelse(length(sa[[i]]$Freq)==0,0,sa[[i]][grep("중국집",sa[[i]]$Var1),"Freq"])
  place.table[i,"패밀리레스토랑"] <- ifelse(length(sa[[i]]$Freq)==0,0,sa[[i]][grep("패밀리레스토랑",sa[[i]]$Var1),"Freq"])
  place.table[i,"치킨"] <- ifelse(length(sa[[i]]$Freq)==0,0,sa[[i]][grep("치킨",sa[[i]]$Var1),"Freq"])
  place.table[i,"피자"] <- ifelse(length(sa[[i]]$Freq)==0,0,sa[[i]][grep("피자",sa[[i]]$Var1),"Freq"])
}

place.table[which(is.na(place.table),arr.ind=T)] <- 0

#구별 업종 table
# place.table

#구별 업종 그래프 그리기 위한 작업
# place.graph <- list()
# for(i in 1:25){
#   for(j in 1:4){
#     place.graph[[paste(row.names(place.table)[i],colnames(place.table)[j])]] <- 
#       apply(data.frame(시군구=row.names(place.table)[i],업종=colnames(place.table)[j]),2,
#             function(x){rep(x,place.table[i,j])})
#   }
# }
# place.graph <- data.frame(Reduce(rbind, place.graph))

#구별 업종 그래프
# ggplot(place.graph,aes(x=시군구,group=업종,fill=업종))+geom_bar(position = "dodge")+
#   geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9))+theme_classic()+
#   ggtitle("서울시 구별로 주문한 음식")+
#   theme(plot.title = element_text(size=20, face="bold",hjust = 0.5),
#         axis.text.x = element_text(angle = 45, hjust = 1))
