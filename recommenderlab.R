#recommenderlab R语言推荐算法库
######### recommender包的数据结构和数据操作
m <- matrix(sample(c(as.numeric(0:5), NA), 30, replace = TRUE, 
     prob = c(rep(0.5/6, 6), 0.5)), ncol = 6, 
     dimnames = list(user = paste("u", 1:5, sep = ""), 
     item = paste("i", 1:6, sep = "")))

#没评分的值都是NA
m
#把m转化为realRatingMatrix
library(recommenderlab)
m.real <- as(m, "realRatingMatrix")
m.real
str(m.real)
rating <- m.real@data
rating
as.matrix(rating)
# NA没有了，这种转换是不合适的。需要这样做
as(m.real, "matrix")
# 转化为列表
as(m.real, "list")
# 转化为数据框
head( as(m.real, "data.frame") )
# 标准化
n.real <- normalize(m.real)
n.real
# 标准化前后的比较，像素图
image(m.real, main = "Raw rating")
image(n.real, main = "Normalized rating")

##########协同过滤（Collaborative Flitering）方法
data("MovieLense")
# 可视化原始数据
image(MovieLense)
# 获取评分
ratings.movie <- data.frame(ratings = getRatings(MovieLense))
summary(ratings.movie$ratings)
library(ggplot2)
ggplot(ratings.movie, aes(x = ratings)) + 
  geom_histogram(fill = "beige", color = "black", binwidth = 1, alpha = 0.7) + 
  xlab("rating") + ylab("count")

# 标准化去除用户评分的偏差
ratings.movie1 <- data.frame(ratings = getRatings(normalize(MovieLense, method = "Z-score")))
summary(ratings.movie1$ratings)
ggplot(ratings.movie1, aes(x = ratings)) + geom_histogram(fill = "beige", color = "black",alpha = 0.7) + xlab("rating") + ylab("count")

# 用户的电影点评数
movie.count <- data.frame(count = rowCounts(MovieLense))
ggplot(movie.count, aes(x = count)) + 
  geom_histogram(fill = "beige", color = "black",alpha = 0.7) + 
  xlab("counts of users") + ylab("counts of movies rated")

# 电影的平均评分
rating.mean <- data.frame(rating = colMeans(MovieLense))
ggplot(rating.mean, aes(x = rating)) + 
  geom_histogram(fill = "beige", color = "black",alpha = 0.7) + 
  xlab("rating") + 
  ylab("counts of movies ")

#推荐算法
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#利用前940位用户建立推荐模型
m.recomm <- Recommender(MovieLense[1:940], method = "IBCF")
m.recomm
#对后三位用户进行推荐预测，使用predict()函数，默认是topN推荐，这里取n=3
ml.predict <- predict(m.recomm, MovieLense[941:943], n = 3)
str(ml.predict)
as(ml.predict, "list")

#用户对item的评分预测
ml.predict2 <- predict(m.recomm, MovieLense[941:943], type = "ratings")
as(ml.predict2, "matrix")

#模型的评估 RMSE（均平方根误差）
rmse <- function(actuals, predicts)
{
  sqrt(mean((actuals - predicts)^2, na.rm = T))
}

