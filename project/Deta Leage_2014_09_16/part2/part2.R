#重回帰分析の練習２
#重回帰分析と変数の選択を行う。
#URL　http://www1.doshisha.ac.jp/~mjin/R/14.html

#変数の説明
#rating：　　総合評価
#complaints：従業員の苦情の取り扱い
#privileges：特別な特権は許さない
#learning：　学習の機会
#raises：　　能力基づいた昇給
#critical：　加重
#advancel:　 昇進


head(attitude)
#相関係数の表示
round(cor(attitude),2)
#プロット
pairs(attitude,panel=panel.smooth,attitude)
#ratingに対して全データを説明変数での重回帰分析実行
attitudelm1 <- lm(rating ~ ., data = attitude)
summary(attitudelm1)
#step関数で最適な説明変数を選択
attitudelm2<-step(attitudelm1)
summary(attitudelm2)

plot(attitudelm2,col="blue")


