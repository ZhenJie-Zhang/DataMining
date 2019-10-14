# factor(因素向量)特色
# ‧ R語言中預設讀入的資料集會將非連續型變數以Factor的格式儲存
# ‧ Factor的類型有兩種，
# 分別是名目型(Nominal，如鄉鎮市)與順序型(Ordinal，如分數等級A、B、C)
# ‧ 可以把Factor視為多了一些資訊的Vector，譬如字串向量，但是多了levels資訊

# 名目型Factor ----
speed <- c('慢','快','極快','中')
speed
speedFactor <- factor(speed)
speedFactor
speedFactor[1]
speedFactor[2]
speedFactor[1] > speedFactor[2]
levels(speedFactor)
class(speedFactor)

# 順序型Factor ----
#需增加兩個參數，其中levels是給定順序，順序是由小排到大
speedFactor1 <- factor(speed, order=TRUE, levels=c('慢','中','快','極快') )
speedFactor1
speedFactor1[1]
speedFactor1[2]
speedFactor1[1] > speedFactor1[2]
class(speedFactor1)
