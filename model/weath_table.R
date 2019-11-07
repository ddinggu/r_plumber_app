###
# 5. 날씨별로 뽑기
### 

source("./arules_data.R", encoding = "UTF-8")

adj_rule_weath <- subset(ttRule.pruned, (lhs %in% paste0("날씨=",unique(food$날씨))))

weath.df <- as(adj_rule_weath,"data.frame")
weath.df$rules <- as.character(weath.df$rules)
weath.df$lhs <- do.call(rbind,strsplit(weath.df$rules,"=>"))[,1]
weath.df$rhs <- do.call(rbind,strsplit(weath.df$rules,"=>"))[,2]

weath.rule <- list()
for(i in 1:length(unique(food$날씨))){
  weath.rule[[as.character(unique(food$날씨)[i])]] <- grep(unique(food$날씨)[i],weath.df$lhs)
}

weath.idx <- lapply(1:length(unique(food$날씨)),function(x){weath.df[weath.rule[[x]],]})
sa_weath <- lapply(1:length(unique(food$날씨)),function(x){as.data.frame(table(weath.idx[[x]]$rhs))})
weath.table <- matrix(,nrow = length(unique(food$날씨)),ncol=4,
                      dimnames=list(unique(food$날씨),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:2){
  weath.table[i,"중국집"] <- ifelse(length(sa_weath[[i]]$Freq)==0,0,sa_weath[[i]][grep("중국집",sa_weath[[i]]$Var1),"Freq"])
  weath.table[i,"패밀리레스토랑"] <- ifelse(length(sa_weath[[i]]$Freq)==0,0,sa_weath[[i]][grep("패밀리레스토랑",sa_weath[[i]]$Var1),"Freq"])
  weath.table[i,"치킨"] <- ifelse(length(sa_weath[[i]]$Freq)==0,0,sa_weath[[i]][grep("치킨",sa_weath[[i]]$Var1),"Freq"])
  weath.table[i,"피자"] <- ifelse(length(sa_weath[[i]]$Freq)==0,0,sa_weath[[i]][grep("피자",sa_weath[[i]]$Var1),"Freq"])
}

weath.table[which(is.na(weath.table),arr.ind=T)] <- 0

#날씨별 업종 table
# weath.table

#날씨별 업종 그래프 그리기 위한 작업
# weath.graph <- list()
# for(i in 1:2){
#   for(j in 1:4){
#     weath.graph[[paste(row.names(weath.table)[i],colnames(weath.table)[j])]] <- 
#       apply(data.frame(날씨=row.names(weath.table)[i],업종=colnames(weath.table)[j]),2,
#             function(x){rep(x,weath.table[i,j])})
#   }
# }
# weath.graph <- data.frame(Reduce(rbind, weath.graph))

# 날씨별 업종 그래프
# ggplot(weath.graph,aes(x=날씨,group=업종,fill=업종))+geom_bar(position = "dodge")+
#   geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9))+theme_classic()+
#   ggtitle("날씨별 주문한 음식")+theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))
