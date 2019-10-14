# ���s�s�X ----
# �ǳƸ�� ----
data("cars")
str(cars)
summary(cars)

# �N�t�פ���3�� ----
# �Ĥ@��speed<12; �ĤG��12 < speed < 15; �ĤT��speed >= 15; 
cars$speed
x1 = cars$speed
new_cars_band = 1*(x1<12) + 2*(x1>=12&x1<15) + 3*(x1>=15)
new_cars_band
# �N�Ʀr�����ഫ����r ----
label = c('�C', '��', '��')
new_cars_band = label[new_cars_band]
new_cars_band

# �A�N�t�׼�����X�A�ܦ����ؼ��ҡA�ϥ�%in% ----
# '�C', '��'�ন'�@���⨮'; '��'�ন'�]��'
# �ϥ�%in%
car_categ = c('�@���⨮', '�]��')
new_cars_band1 = 1*(new_cars_band %in% c('�C', '��')) + 2*(new_cars_band %in% c('��'))
new_cars_band1 = car_categ[new_cars_band1]
new_cars_band1

# �A�N���ؼ�����X�A�ܦ����ؼ��ҡA�ϥ�ifelse ----

# within�N���O SQL �y�k���� Case When ----
# �N�t�פ��� 3 �� , �C speed<12 ; �� speed <15 ; �� speed >= 15
new_cars <- cars
new_cars <- within(new_cars,
                   {
                     speed_level <- NA
                     speed_level[cars$speed<12] <- "�C"
                     speed_level[cars$speed>=12 & cars$speed<15] <- "��"
                     speed_level[cars$speed>=15] <- "��"
                   }
                  )
head(new_cars,5)