
##############���`��A���f��

#���z�f�[�^�̓ǂݍ���
amount.1<-read.csv("amount1.csv")

head(amount.1)
summary(amount.1)

library(ggplot2)
#�U�z�}�̍쐬
ggplot(amount.1,aes(x=invest,y=amount)) +
geom_point() +
theme_bw(16)

#���`��A���f���̍쐬
amount1.lm1 <- lm(amount~invest, data=amount.1)
summary(amount1.lm1)

plot(amount1.lm1,which=1)

#�����^��A���f���̕`��
ggplot(amount.1,aes(x=invest,y=amount)) +
geom_point() +
theme_bw(16) +
geom_smooth(method="lm",formula=y~log(x))

#������A���f���̍쐬
amount1.lm2 <- lm(amount~log(invest), data=amount.1)
summary(amount1.lm2)

plot(amount1.lm2,which=1)

#############���W�X�e�B�b�N��A���f��
#�f�[�^�̐���
z <- data.frame(Titanic)
titanic.data <- data.frame(
Class=rep(z$Class,z$Freq),
sex=rep(z$Sex,z$Freq),
Age=rep(z$Age,z$Freq),
Survived=rep(z$Survived,z$Freq)
)

#���f���\�z
titanic.logit <- glm(Survived~.,data=titanic.data, family=binomial)
summary(titanic.logit)
#install.packages("epicalc")
library(epicalc)
logistic.display(titanic.logit,simplified=T)

#�Ȑ��̕\��
x<-1:length(titanic.data$Survived)
x1<-ifelse(titanic.data$Class=="2nd",1,0)
x2<-ifelse(titanic.data$Class=="3rd",1,0)
x3<-ifelse(titanic.data$Class=="Crew",1,0)
x4<-ifelse(titanic.data$sex=="Female",1,0)
x5<-ifelse(titanic.data$Age=="Adult",1,0)
#�Ȑ��̍쐬
y<- 0.6853-1.0181*x1-1.7778*x2-0.8577*x3+2.4201*x4-1.0615*x5
#plot(x,titanic.data$Survived, type="l")
par( new=T )
plot( x, y, type="l", col="blue", ylab="" )


#############����؃��f��
library(rpart)
#install.packages("partykit")
library(partykit)

#����؃��f���̍\�z
titanic.rp <- rpart(Survived~.,data=titanic.data)
#����؂̕`��
plot(as.party(titanic.rp),tp_args=T)

#############�听������
#���̗�ł�state.x77�̏��߂̂U���ڂ����̃x�N�g�����l����B
state.pca <- prcomp(state.x77[,1:6],scale=T)
#�\��
biplot(state.pca)

############# k-means�@�ɂ���K�w�N���X�^�����O
state.km <- kmeans(scale(state.x77[,1:6]),3)
#�听�����͂̌��ʂɃN���X�^�[�̏���t������
state.pca.df <- data.frame(state.pca$x)
state.pca.df$name <- rownames(state.pca.df)
state.pca.df$cluster <- as.factor(state.km$cluster)

#�`��
ggplot(state.pca.df,aes(x=PC1,y=PC2,label=name,col=cluster))+
geom_text()+
theme_bw(16)

#���[�_�[�`���[�g�̍쐬
#install.packages("fmsb")
library(fmsb)
#���[�_�[�`���[�g�p�Ƀf�[�^�𐮌`
df <- as.data.frame(scale(state.km$centers))
dfmax <- apply(df,2,max)+1
dfmin <- apply(df,2,min)-1
df <- rbind(dfmax,dfmin,df)
#���[�_�[�`���[�g�̕`��
radarchart(df,seg=5,plty=1,pcol=rainbow(3))
legend("topright",legend=1:3,col=rainbow(3),lty=1)

