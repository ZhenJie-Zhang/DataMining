# �ܼƦW��  �t�N
# Survived  �ͦs���p:�s��(1)�Φ��`(0)
# Pclasss   �y������(1, 2, 3)
# Name      ���ȦW�r
# Sex       ���ȩʧO
# Age       ���Ȧ~��
# SibSp     �b��S�̩n�f��/�t����
# Parch     �b�������/�l�k��
# Ticket    ��s��
# Fare      �����
# Cabin     �y����
# Embarked  �n���f

setwd("E:/DataMining/unit2_data_structure/titanic")
test <- read.csv("E:/DataMining/titanic/test.csv", header = T, stringsAsFactors = F)
test <- read.csv("test.csv", header = T)
train <- read.csv("E:/DataMining/titanic/train.csv", header = T, stringsAsFactors = F)
summary(test)
str(test)
View(test)
is.na(test)
test[test$Cabin == "",]


library(mice)
complete.cases(test)

create_report(test)
create_report(train)

# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic/report
# https://kknews.cc/zh-tw/other/oolj96m.html
# https://zhuanlan.zhihu.com/p/29130905
# https://officeguide.cc/r-logistic-regression-titanic-survival-analysis/
# �[�������]
library('ggplot2') # �i����
library('ggthemes') # �i����
library('scales') # �i����
library('dplyr') # �ƾڳB�z
library('mice') # �ʥ��ȶ��
library('randomForest') # �ؼ�

#��B�[��ƾ�
# �ˬd�ƾ�
str(train)
str(test)
head(train)
head(test)

#�i�H�ݨ�A���FSurvived��줣�P�~�A��L��짡�ۦP�C�X�ְV�m���P���ն��A���U�@�B�ƾڲM�~���ǳ�
full <- bind_rows(train, test)
str(full)
summary(full)

# 1.�ƾڲM�~ ----
# a. �[��m�W�ܶq----
# �q���ȦW�r�������Y��
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# �d�ݫ��өʧO�������Y�μƶq?
table(full$Sex, full$Title)

# ��󨺨ǥX�{�W�v���C���Y�ΦX�֬��@�� 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# ���@�Ǻ٩I�i�歫�s���w�]���t�q�^ �pmlle, ms���p�j, mme ���k�h
full$Title[full$Title =='Mlle']<- 'Miss'
full$Title[full$Title =='Ms'] <- 'Miss'
full$Title[full$Title =='Mme']<- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'
# ���s�d�ݴ����᪺���p
table(full$Sex, full$Title)

# �̫�q���ȩm�W���A�����m��
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
nlevels(factor(full$Surname))

# b.�a�x���p�O�_�|�v�T�ͦs���G?----
# �ثe����ڭ̤w�g�B�z�����ȩm�W�o�@�ܶq�A�ñq�䤤�����F�@�Ƿs���ܶq�C 
# �U�@�B�Ҽ{�l�ͤ@�Ǯa�x�������ܶq 
# �����A�ͦ��a�x�H��family size�o�@�ܶq�C
# �i�H���w���ܶqSubSp�MParch�]����t�q�ѷӤW���^

# �ͦ��a�x�H���ܶq�A�]�A�ۤv�b��
full$Fsize <- full$SibSp + full$Parch + 1
# �ͦ��@�Ӯa�x�ܶq�G�H�m_�a�x�H�� �榡
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# �ϥ� ggplot2 ø�s�a�x�H�ƻP�ͦs���p���������Y
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# https://rstudio-pubs-static.s3.amazonaws.com/329677_8f579b9e46284caeb9d3a72b7fdb7ac3.html
library(ggplot2)
ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar()+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar(position="fill")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))

percentData <- full %>% group_by(Fsize) %>% count(Survived) %>%
  mutate(ratio=scales::percent(n/sum(n)))

ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar(position="fill")+
  geom_text(data=percentData, aes(y=n,label=ratio),
            position=position_fill(vjust=0.5))



# �q�L�ϧΧڭ̥i�H����o�{�H�U�S�I�G
# - 1. �ӤH�W��M�a�x�H��>4�H���a�x���s���H�Ƥp�󦺤`�H��
# - 2. �a�x�H��size�b[2:4]���s���H�ƭn���󦺤`�H��
# �]���ڭ̥i�H�N�a�x�H���ܶq�i����q�X�֡A
# ���㪺�i�H����3�q�G�ӤH�A�p�a�x�A�j�a�x�A�Ѧ��ͦ��s�ܶq�C
ftable(full[1:891,c('Survived','Pclass','Sex')],
       row.vars = c('Sex','Survived'),
       col.vars = 'Pclass')
table(full[1:891,c('Pclass','Sex','Survived')])
mosaicplot(table(full[1:891,c('Pclass','Sex','Survived')]), 
           sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)

# datasets::Titanic----
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

# https://en.wikipedia.org/wiki/Mosaic_plot
# https://commons.wikimedia.org/wiki/File:Mosaic_titanic_independent.png
# Mosaic_plot - wiki, example table
ftable(apply(Titanic, c('Sex','Survived','Class'), sum))
ftable(apply(Titanic, c(2,4,1), sum))

# Mosaic_plot - wiki, example-----
par(mfrow=c(1,2))
# png("mosaic_titanic_independent.png", width=800, height=480)
titanic <- apply(Titanic, c(1,2,4), sum)
# titanic <- apply(Titanic, c('Class','Sex','Survived'), sum)
mosaicplot(titanic, sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)
independent <- outer(outer(apply(titanic, 1, sum), apply(titanic,2, sum)), apply(titanic, 3, sum))/sum(titanic)^2
dimnames(independent) <- dimnames(titanic)
mosaicplot(independent, sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)
# dev.off()

# ������
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize > 1 & full$Fsize <= 4]<- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
# �q�L���ɧJ�ϡ]mosaic plot�^�d�ݮa�x�W�һP�ͦs���p�������Y
mosaicplot(table(full$FsizeD,full$Survived), main='Family Size by Survival', shade=TRUE)


# �q�ϤW�]�i�H��ө������[��X�ӡA�ӤH�P�j�a�x���Q��ͦs�U�ӡA
# �Ӭ۹諸�p�a�x�����ͦs�v�۹����
# c. �յۥͦ���h�ܶq
# �i�H�o�{�b���ȫȿ��ܶqpassenger cabin�]�s�b�@�Ǧ����Ȫ��H���p�ȿ��h��deck
# �i�H�ݥX�o�@�ܶq���ܦh�ʥ���
full$Cabin[1:28]

# �Ĥ@�Ӧr���Y���ȿ��h��.�p:
strsplit(full$Cabin[2], NULL)[[1]]
strsplit(full$Cabin[2], NULL)[[1]][1]

# �إߤ@�Ӽh���ܶq�]Deck�^�ܤƨ��ȱq A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
summary(full$Deck)

# �o�̦��ܦh�i�H�i�@�B�ާ@���a��A
# �p���ǭ��ȦW�U�]�t�ܦh���� (e.g., row 28: "C23 C25 C27"), 
# ���O�Ҽ{��o�@�ܶq�ƭȪ��}���ʡ]sparseness�^�A��1014 �ӯʥ��ȡC 
# �᭱�N���A�i�@�B�Ҽ{�C

# a. �n���f�ʥ��X�X����� ----
summary(full$Embarked)
full[full$Embarked=="", c(-4:-5, -18:-13)]
# ���� 62 and 830 �ʤֵn���f�H���C
full[c(62, 830), 'Embarked']

# �ڦ��p��󦳬ۦP���쵥�š]passenger class�^�M�����]Fare�^������
# �]�\���۬ۦP���n���f��membarkment �C
# �ڭ̥i�H�ݨ�L�̤�I���������O���G$ 80 �M $ 80 
# �P�ɥL�̪����쵥�Ť��O�O�G1 �M 1 �C 
# ����L�̳̦��i��O�b���Ӵ�f�n��O�H
median(full[full$Embarked == "C" & full$Pclass==1,'Fare'])
median(full[full$Embarked == "Q" & full$Pclass==1,'Fare'])
median(full[full$Embarked == "S" & full$Pclass==1,'Fare'])

# �h���ʥ��ȭ��Ȫ�ID
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
# �� ggplot2 ø�sembarkment, passenger class, & median fare �T�����Y��
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80),
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# �ܩ���I
# �q��f ('C')�X�o���Y������I������������Ƭ�80�C
# �]���ڭ̥i�H��ߪ���B���Y�����B�����b$80������62�M830 ���X�o��f�ʥ��ȴ�����'C'�C
# �]���L�̲�����80�B�B���Y����,�]�ӥL�̫ܦ��i�ೣ�O�q��fC�n��C
full$Embarked[c(62, 830)] <- 'C'

# b. �����ʥ� �X�X����� ----
# �o�̵o�{1044�檺���Ȳ������ŭ�
summary(full$Fare)
full[is.na(full$Fare), c(-4:-5, -18:-13)]

# ����1044��ƾ�
full[1044, ]

# �o�O�q��fSouthampton ('S')�X�o���T�������ȡC
# �q�ۦP��f�X�o�B�B��ۦP���쪺���ȼƥج� (n = 494)�C
sum(full$Embarked=="S" & full$Pclass==3)
nrow(full[full$Embarked=="S" & full$Pclass==3,])

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ],
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) + theme_few()

# �q�o�쪺�ϧΤW��,�N�ʥ��ȥΤ���ƶi������O�X�z���C�����ƭȬ�$8.05
# ���X�o��f�M�ȿ����šA���������ʥ���
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, 
                          na.rm = TRUE)

# c. �~�֯ʥ��X�X�w����� ----
# 
# �̫�A���p�ڭ̤��e�[��쪺�A�b�Τ�~�֡]Age�^�����j�q���ʥ��s�b�C 
# �o�̧ڭ̱N���~�֩M��L�ܶq�c�ؤ@�ӹw���ҫ���~�֯ʥ��ȶi��w���C
# S�έp�ʥ��ƶq
sum(is.na(full$Age))
## [1] 263

# �q�`�ڭ̷|�ϥ� rpart (recursive partitioning for regression) �]�Ӱ��ʥ��ȹw�� 
# �b�o�̧ڱN�ϥ� mice �]�i��B�z�C
# ����z�ѡA�A�i�H�q�L�\Ū����
# ����즡��{ Chained Equations��
# �h�����ɪkMultipleImputation�]MICE�^�����eMICE (PDF)�C
# �b�o���e�ڭ̥��n��]�l�ܶq�]factor variables�^�]�l�ơA
# �M��A�i��h�����ɪk�C
colnames(full)
# �Ϧ]�l�ܶq�]�l��
factor_vars <-  c('PassengerId','Pclass','Sex','Embarked', 
                  'Title','Surname','Family','FsizeD')
full[factor_vars] <-  lapply(full[factor_vars],function(x) as.factor(x))
# �]�m�H���ؤl
set.seed(129)
# �簣�@�ǨS����Ϊ��ܶq����A�A����h�����ɪk�A:
mice_mod <-  mice(full[, !names(full) %in% 
                         c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], 
                  method='rf')
# �O�s�����X
mice_output <-  complete(mice_mod)

# ���ڭ̹��ƾڶ�ɫe�P��ɫ᪺�ƾڤ������p�C�T�O�ƾڤ����S�εo�Ͱ���
# ø�s�~�֤�����
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data',
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output',
     col='lightgreen', ylim=c(0,0.04))
# ���G�ݰ_�Ӥ����A����U���i�H��mice�ҫ������G���~�ּƾڶi������C

# MICE�ҫ����G�����~���ܶq.
full$Age <- mice_output$Age
# �ˬd�ʥ��ȬO�_�Q���������F
sum(is.na(full$Age))
## [1] 0

# �{�b�A�ڭ̤w�g�����F��Ҧ����n�ܶq���ʥ��Ȫ������u�@�C 
# ���O�o�@���٨S�����A�ڭ̥i�H��~���ܶq�i�@�B�諸�����C

# 2.�S�x�u�{ ----
# �{�b�ڭ̪��D�C�@�쭼�Ȫ��~�֡A
# ����ڭ̥i�H���~�֥ͦ��@���ܶq�p�ൣ�]Child�^�M ���ˡ]Mother�^�C
# �����зǡG
# - �ൣ �G �~��Age < 18
# - ���� �G 1 �k�ʡF 2 �~�� > 18; 3 �֦��W�L1�Ӥl�k 4 �Y�Τ��O'Miss'�C

# �����ڭ̨Ӭݦ~�ֻP�ͦs���p���������Y
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  # ���ʧO�ӬݡA�]���e���ڭ̪��D �ʧO���ͦs���p�����n�v�T
  facet_grid(.~Sex) +
  theme_few()

# �ͦ��ൣ�]child�^�ܶq, �åB��󦹹����ൣchild�P���Hadult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
# �i�ܹ����H��
table(full$Child, full$Survived)
## 
## 0 1
## Adult 484 274
## Child 65 68

# �q���G�ݡA�ൣ���ͦs�v�n���󦨤H���O�o�ä��N���ۧ@���ൣ�N�@�w�i�H���١C
# ���p�ڭ̷��~�ݡm�K�F�����n�q�v�ɡA�̫����n�D���˩M�ൣ���W��@�ˡC
# �U���ӥͦ����˳o���ܶq�C

# �ͦ������ܶq
full$Mother <- 'NotMother'
full$Mother[  full$Sex =='female'
            & full$Parch > 0 
            & full$Age > 18 
            & full$Title != 'Miss'] <- 'Mother'
# �έp���ƶq
table(full$Mother, full$Survived)
## 
## 0 1
## Mother 16 39
## Not Mother 533 303
# ��s�ͦ�������ܶq�����]�l�ơC
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

# �ܦ��A�Ҧ��ڭ̻ݭn���ܶq���w�g�ͦ��A�åB�䤤�S���ʥ��ȡC 
# ���F�O�I�_���A�ڭ̶i��G���T�{�C

# �o�Ӱ_�줰��@��?
md.pattern(full[1:891,])
library(VIM)
aggr(full[1:891,], col = c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(full), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data", "Pattern"))
summary(full[1:891,])
# �V�m��(Train Data)��Deck��687�ӯʥ���(NaN)�A�]��Cabin���e���ŭ�("")

# �{�b�ڭ̲ש󧹦�����Z���J�ƾڶ��]the Titanic dataset�^���Ҧ����ܶq�ʥ��Ȫ���ɡA
# �ð��즳�ܶq�c�ؤF�@�Ƿs�ܶq�A�Ʊ�o�ǥi�H�b�̲ת��ͦs���p�w���ɰ_�����U�C

# �ҫ��]�w�P�w�� ----
# �b�����W�����u�@����A�ڭ̶i�J��̫�@�B�G�w���K�F�����W���Ȫ��ͦs���p�C 
# �b�o�̧ڭ̨ϥ��H���˪L������k(The RandomForest Classification Algorithm) 
# �ڭ̫e������h�u�@���O���F�o�@�B�A�Ȫ��C

# a. ����V�m���P���ն� ----
# �ڭ̲Ĥ@�B�ݭn�N�ƾ��ܦ^������V�m���P���ն��C
# �N�ƾک�����V�m���P���ն�
train <- full[1:891,]
test <- full[892:1309,]

# b. �إ߼ҫ�
# �ڭ̧Q�ΰV�m���V�m�إ��H���˪L randomForest �ҫ��C
# �]�m�H���ؤl
set.seed(754)
# �إ߼ҫ�l (�`�N: ���O�Ҧ��i���ܶq�����[�J)
rf_model <- randomForest(factor(Survived) ~ 
                           Pclass + Sex + Age + 
                           SibSp + Sex*Parch + Fare + 
                           Embarked + Title + FsizeD + 
                           Child + Mother, 
                         data = train)
# ��ܼҫ��~�t
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# �¦⨺���u���ܡG����~�t�v�]the overall error rate�^�C��20% 
# ����M�����O���ܡG�J���P���٪��~�t�v�ܦ��۹����٨ӻ��A�ڭ̥i�H��ǽT���w���X���`�C


# c.�ܶq���n�� ----
# �q�L�p��Gini�Y�Ʊo������ܶq�����n�ʱƧ�
# ������n�ʫY��
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# ��󭫭n�ʫY�ƱƦC�ܶq
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
#�q�L ggplot2 ø�s�������n���ܶq��
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour= 'red') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()

# �ڭ̱q�ϤW�i�H�ݥX�����ܶq�~�O��ڭ̹w���̭��n���ܶq 
# �q�ϤW���Y�ΩM�ʧO���ͦs���p�v�T�̤j�A�䦸�O�����M�~�֡C
# �Ӭ��������ȿ���ƲĤ��C 
# �ӳ̥X�G�ڷN�ƪ��O���˩M�Ĥl���ͦs�P�_���v�T�̤p�Ʀb11�M10. �o�ӧڤp�ɭԬ��K�F�������L�H�ۮt�ƻ��C

# d.�w��
# �U����F�̫�@�B�F----�w�����G�I 
# �b�o�̥i�H���~�إߪ��ҫ��������Φb���ն��W�C 
# # �����ն��i��w��

prediction <- predict(rf_model, test)
# �N���G�O�s���ƾڮءA����Kaggle������ɪ��榡�n�D�C[��C�GPassengerId and Survived (prediction)]
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
# �N���G�g�J���
# write.csv(solution, file = 'rf_mod_Solution1.csv', row.names = F)
# �����F�F��̨Ϊ��w�����G�A�ڭ̤]�i�H���s�c�ؤ��P���ҫ��A�Ϊ̥Τ��P���ܶq�i��զX�C

test$Survived <- as.numeric(as.character(prediction))
full <- bind_rows(test, train)

y <- train$Survived
y_hat <- as.numeric(as.character(predict(rf_model, train)))
confusionMatrix <- table(y, y_hat)
# TP <- confusionMatrix[2,2]
# TN <- confusionMatrix[1,1]
# accuracy <- (TP+TN)/sum(confusionMatrix)
accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)
cat("Accuracy(train)=",round(accuracy*100,2),"%\n")