#���̃v���O�����́A�Q�O�P�S�N�̃f�[�^���Ȃ��l�́A�N�ԒʎZ�Ő����O�ɂȂ�A
#�N�ԒʎZ�Ő��ƔN�ԒʎZ���ł�P��A�ŋ��߁A�\������v���O�����ł���B
#�P��A�ł́Ax���Ɍ���u���B�N�ԒʎZ�Ő��ƔN�ԒʎZ���ł��N�ԒʎZ�ŗ������܂�B

##########��������P��A���͂������Ȃ�


##########�������ʏ���
#�w�K�f�[�^�̓ǂݍ���
data.0 <- read.csv("train.csv")
summary(data.0)
head(data.0)

#�o�͗p�f�[�^�t���[����`
DF<-0
DF <- data.frame(ID=0,monthdata=0)
DF <- DF[-1,]

#�J�E���g�p�ϐ���`
x<- 0
dumy<-0


#�f�[�^��`
data.2014.ALL0 <- data.frame(�N�x=2014,���[�OID=0,
�`�[��ID=0,�Ŏ�ID=0,��=0,����=0,�Ő�=0,�Ő�=0,���_=0,
����=0,��ۑ�=0,�O�ۑ�=0,�{�ۑ�=0,�ۑ�=0,�œ_ =0,�O�U=0,
�l��=0,����=0,�]��=0,�]��=0,����=0,���ێ�=0,���E��=0,
�ŗ�=0,���ŗ�=0,�o�ۗ�=0,OPS=0,�N�ԒʎZ�Ő�=0,�N�ԒʎZ����=0,�N�ԒʎZ�ŗ�=0)

##########�f�[�^�����J�n
for(i in data.0$�Ŏ�ID)
{
#�P�x�s����ID�ɑ΂��ĉ��x���J��Ԃ��Ȃ��悤�ɂ��镪��
if(i!=dumy)
{
#x���J�E���g
x<- x+1
##########�P���R�[�h�ɑ΂��ĒP��A���͂��s��

#����ID�̃f�[�^�t���[����ǂ݂���
#data <- data.0[data.0$�Ŏ�ID=="1400001", ]
data <- data.0[data.0$�Ŏ�ID==i, ]
data.2014 <- data[data$�N�x==2014,]

	if(!length(data.2014$�N�x))
	{
	#�f�[�^����̎��A���ׂĂO�̃f�[�^����
	data.2014 <- data.2014.ALL0
	DF[2*x-1,1]<-paste(data$�Ŏ�ID[1],"_2014_6",sep = "")
	DF[2*x-1,2]<-0
	DF[2*x,1]<-paste(data$�Ŏ�ID[1],"_2014_7",sep = "")
	DF[2*x,2]<-0
	}else
	{
		if(length(data[data$��==6,]$��) || length(data[data$��==7,]$��))
		{
		#���N�ɂU���V���ɁA�f�[�^���X�V���ꂽ�ꍇ�A���N�U�C�V���ɂ��ω�����m��������ꍇ
		#���̕���ɓ���̂́A��o�f�[�^�̑S�̂̂U�����炢���߂�

		#�N�ԒʎZ�Ő��̗\��(month6�ɂU���́Amonth7�ɂV���i�K�̒ʎZ�Ő�������)
		datalm<- lm(�N�ԒʎZ�Ő�~��, data = data[data$��<8,])
		month6 <- datalm$coefficients[1]+datalm$coefficients[2]*6
		month7 <- datalm$coefficients[1]+datalm$coefficients[2]*7
			if(month6<data.2014[length(data.2014$�N�ԒʎZ�Ő�),]$�N�ԒʎZ�Ő�)
			{
			month6 <- data.2014[length(data.2014$�N�ԒʎZ�Ő�),]$�N�ԒʎZ�Ő�
			}
			if(month7<month6)
			{
			month7 <- month6
			}

		#�\�z�f�[�^�Ɖߋ��f�[�^���V�΂R�Ńu�����h����B
		#month6�@<- (month6*7+data.2014[length(data.2014$�N�ԒʎZ�Ő�),]$�N�ԒʎZ�Ő�*3)/10
		#month7�@<- (month7*7+data.2014[length(data.2014$�N�ԒʎZ�Ő�),]$�N�ԒʎZ�Ő�*3)/10



		#�N�ԒʎZ���ł̗\��(month26�ɂU���́Amonth27�ɂV���i�K�̒ʎZ���ł�����)
		datalm2<- lm(�N�ԒʎZ����~��, data = data[data$��<8,])
		month26 <- datalm2$coefficients[1]+datalm2$coefficients[2]*6
		month27 <- datalm2$coefficients[1]+datalm2$coefficients[2]*7

		
			if(month26<data.2014[length(data.2014$�N�ԒʎZ����),]$�N�ԒʎZ����)
			{
			month26 <- data.2014[length(data.2014$�N�ԒʎZ����),]$�N�ԒʎZ����
			}
			if(month27<month26)
			{
			month27 <- month26
			}

		#�\�z�f�[�^�Ɖߋ��f�[�^���V�΂R�Ńu�����h����B
		#month26�@<- (month26*7+data.2014[length(data.2014$�N�ԒʎZ����),]$�N�ԒʎZ����*3)/10
		#month27�@<- (month27*7+data.2014[length(data.2014$�N�ԒʎZ����),]$�N�ԒʎZ����*3)/10



		ab<-month26/month6
		cd<-month27/month7
			if(!is.na(ab) && !is.na(cd))
			{
			if(ab>cd)
			{
			cd<-ab
			}
			}	#�f�[�^���
		DF[2*x-1,1]<-paste(data$�Ŏ�ID[1],"_2014_6",sep = "")
		DF[2*x-1,2]<-ab
		DF[2*x,1]<-paste(data$�Ŏ�ID[1],"_2014_7",sep = "")
		DF[2*x,2]<-cd


		}else
		{
			#���N�ɂU���V���ɁA�f�[�^���X�V����Ȃ������ꍇ�A���N�U�C�V���ɂ��ω�����m�����Ȃ��̂ŁA
			#�Q�O�P�S�N�̍Ō�̌��̃��R�[�h�̒l�����̂܂܎g���ꍇ�ꍇ
			
			month6 <- data.2014[length(data.2014$�N�ԒʎZ�Ő�),]$�N�ԒʎZ�Ő�
			month7 <- month6
			month26 <- data.2014[length(data.2014$�N�ԒʎZ����),]$�N�ԒʎZ����
			month27 <- month26
			ab<-month26/month6
			cd<-month27/month7
				if(!is.na(ab) && !is.na(cd))
				{
				if(ab>cd)
				{
				cd<-ab
				}
				}
			
			#�f�[�^���
			DF[2*x-1,1]<-paste(data$�Ŏ�ID[1],"_2014_6",sep = "")
			DF[2*x-1,2]<-ab
			DF[2*x,1]<-paste(data$�Ŏ�ID[1],"_2014_7",sep = "")
			DF[2*x,2]<-cd
			
			
			}

	}

}
dumy<-i
}

##########�����܂łP���R�[�h�ɑ΂��Ă̒P��A����


#�����l:Na ���O�ɕϊ�
DF$monthdata<- ifelse(is.na(DF$monthdata),0,DF$monthdata)
#�}�C�i�X�̎��ɒl�𕽋ϒl�ɕϊ�
DF$monthdata<- ifelse(DF$monthdata<0,0.21374,DF$monthdata)
#���肦�Ȃ��ŗ��𕽋ϒl�ɕϊ�
#DF$monthdata<- ifelse(DF$monthdata>0.9,0.21374,DF$monthdata)


#����ID�͑S����1173��
#�m�F
x
length(data.0$�Ŏ�ID)
head(DF)
#�\���f�[�^�̕ۑ�
write.table( DF, file="outputfile3.csv", sep=",", fileEncoding="UTF-8" )



##############�f�[�^�̏ƍ��Ƒ��
#����Q�l�t�H���_�̓ǂݍ���
outputsample <- read.csv("submission_sample.csv")
#���߂̍s��outputsample�ɂ́A�����Ă���̂Œǉ�
nick <-data.frame(X10278_2014_6="10278_2014_6",X0=0)
outputsample <- rbind(nick,outputsample)

#��o�p�f�[�^�̒���ID�ɍ���ID�̂��̂�\���f�[�^����T��������o�͂���
z<-0

for(k in outputsample$X10278_2014_6)
{
z<-z+1

#�\���f�[�^�̒��ɒ�o�p�f�[�^��ID�����邩�̕���
if(length(DF[DF$ID==k,2])!=0)
{
outputsample[outputsample$X10278_2014_6==k,2] <- DF[DF$ID==k,2]
}else
{
#�Ȃ��Ƃ��́A�Ƃ肠�������ϒl�������Ă���
outputsample[outputsample$X10278_2014_6==k,2] <- 0.21374
}

}

# �����@DF��600101_2014_6���Ȃ��č����Ă�DF[DF$ID=="600241_2014_6",2]�Ŋm�F
z
length(outputsample$X10278_2014_6)

head(outputsample)
#��o�t�@�C���o��
write.table( outputsample, file="outputfile2.csv", sep=",", fileEncoding="UTF-8" )

#######�����܂ŒP��A����




#��o�f�[�^�̔N�ԒʎZ�ŗ��̃q�X�g�O�����̕\��
library(ggplot2)
dev.new()
ggplot(outputsample,aes(x=X0)) +
geom_histogram() +
theme_bw(16) +
ylab("count")


#�w�K�p�f�[�^�̔N�ԒʎZ�ŗ��̃q�X�g�O�����̕\��
dev.new()
ggplot(DF,aes(x=monthdata)) +
geom_histogram() +
theme_bw(16) +
ylab("count")









