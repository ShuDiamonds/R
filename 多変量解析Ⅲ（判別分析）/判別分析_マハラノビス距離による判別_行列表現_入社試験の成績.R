x <- read.csv("C:/Users/Furuhashi/Documents/Discriminant_Analysis/test_for_employment2.csv")　#入社試験の成績データの読み込み
x

x_S <- subset(x[,-3],  x$入社後の評価 < 1.5)		# 高評価グループの抽出
x_S

x_F <- subset(x[,-3],  x$入社後の評価 > 1.5)		# 低評価グループの抽出
x_F

mean_S <- colMeans(x_S)					#列ごとの平均を求める
mean_S

cov_S <- cov(x_S)						#分散・共分散行列を求める
cov_S

S_inv_S <- solve(cov_S)					#ΣSの逆行列をもとめる
S_inv_S

mean_F <- colMeans(x_F)					#列ごとの平均を求める
mean_F

cov_F <- cov(x_F)						#分散・共分散行列を求める
cov_F

S_inv_F <- solve(cov_F)					#ΣFの逆行列をもとめる
S_inv_F


maha_dis <- function(X, mean_X, S_inv){		#マハラノビス距離関数の定義
	dis <- t(X-mean_X) %*% S_inv %*% (X - mean_X)
	return(dis)
	}

n <- 1000							#生成乱数の数
col_num <- n*5						#出力行列の列数
result <- matrix(0, nrow=n,ncol=5)	#結果の出力用行列の定義

for(i in 1:n){
	ran <- runif(2)
	ran[1] <- ran[1]*8				#一様乱数の生成 x in [0, 8]
	ran[2] <- ran[2]*9-3				#一様乱数の生成 y in [-3, 6]

	md_S <- maha_dis(ran,mean_S,S_inv_S)	# マハラノビス距離　dS(x, y)
	md_F <- maha_dis(ran,mean_F,S_inv_F)	# マハラノビス距離　dF(x, y)

	result[i,1] <- ran[1]
	result[i,2] <- ran[2]
	result[i,3] <- md_S
	result[i,4] <- md_F
	
	if (md_S < md_F){
		result[i,5] <- 5				#高評価グループ　　1:◇
	}
	else{
		result[i,5] <- 7				#低評価グループ　　2:□
	}
}
result

plot(result[,1],result[,2],col=result[,5], pch = result[,5])　　#判別結果のプロット
par(new = "T")
plot(x[,1],x[,2],xlim = c(0,8),ylim = c(-3, 6), col=x$入社後の評価, pch=x$入社後の評価)