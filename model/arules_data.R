library(arules) # 고용량 데이터 연관분석 패키지

food <- read.csv("../foodequal.csv", fileEncoding = "euc-kr")

# Apriori: 연관규칙 생성 알고리즘 --> 최소지지도 요건을 충족하지 못하는 아이템 규칙을 제외하여 계산 효율성증대
all <- apriori(food,
               parameter = list(minlen = 2, supp = 0.00005, conf = 0.6),
               appearance = list(rhs=c("업종=중국집","업종=치킨","업종=피자","업종=패밀리레스토랑"),
                                 default="lhs"))

ttRule.subset <- is.subset(all, all, sparse=FALSE)  # 불필요한 규칙 제거
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA  # 상삼각행렬
redundant <- colSums(ttRule.subset, na.rm=T) >= 1  # colSums(x) : x 열의 합을 구하는 함수
ttRule.pruned <- all[!redundant] # 불필요한(중복되는) 규칙을 제거

all.rule <- as(ttRule.pruned,'data.frame')
all.rule$rules <- as.character(all.rule$rules)
all.rule$lhs <- do.call(rbind,strsplit(all.rule$rules,"=>"))[,1]
all.rule$rhs <- do.call(rbind,strsplit(all.rule$rules,"=>"))[,2]

