# clean environment
rm(list = ls())

# load data for food, service and price
service <- read.csv("service.csv")
food <- read.csv("food.csv")
price <- read.csv("price.csv")

# sum of positive for food
sum(food$positive)
# sum of negative for food
sum(food$negative)
# user sentiment for food
sum(food$positive)/(sum(food$positive)+sum(food$negative))
# correlation between user sentiment and stars for food
cor(food$proportion,food$stars)

# sum of positive for service
sum(service$positive)
# sum of negative for service
sum(service$negative)
# user sentiment for service
sum(service$positive)/(sum(service$positive)+sum(service$negative))
# correlation between user sentiment and stars for service
cor(service$proportion,service$stars,use = "complete.obs")

# sum of positive for price
sum(price$positive)
# sum of negative for price
sum(price$negative)
# user sentiment for price
sum(price$positive)/(sum(price$positive)+sum(price$negative))
# correlation between user sentiment and stars for price
cor(price$proportion,price$stars,use = "complete.obs")

# do chisq test
mat.1 <- matrix(c(24047,1907,15458,3162,7869,802),ncol = 2,nrow = 3,byrow = T)
colnames(mat.1) <- c("positive","negative")
rownames(mat.1) <- c("food","service","price")
print(mat.1)
chisq.test(mat.1)

# load data for food related nouns
lobster <- read.csv("lobster.csv")
crab <- read.csv("crab.csv")
shrimp <- read.csv("shrimp.csv")
oyster <- read.csv("oyster.csv")
fish <- read.csv("fish.csv")
clam <- read.csv("clam.csv")

# sum of positive for lobster
sum(lobster$positive)
# sum of negative for lobster
sum(lobster$negative)
# user sentiment for lobster
sum(lobster$positive)/(sum(lobster$positive)+sum(lobster$negative))
# correlation between user sentiment and stars for lobster
cor(lobster$proportion,lobster$stars,use = "complete.obs")

# sum of positive for crab
sum(crab$positive)
# sum of negative for crab
sum(crab$negative)
# user sentiment for crab
sum(crab$positive)/(sum(crab$positive)+sum(crab$negative))
# correlation between user sentiment and stars for crab
cor(crab$proportion,crab$stars,use = "complete.obs")

# sum of positive for shrimp
sum(shrimp$positive)
# sum of negative for shrimp
sum(shrimp$negative)
# user sentiment for shrimp
sum(shrimp$positive)/(sum(shrimp$positive)+sum(shrimp$negative))
# correlation between user sentiment and stars for shrimp
cor(shrimp$proportion,shrimp$stars,use = "complete.obs")

# sum of positive for oyster
sum(oyster$positive)
# sum of negative for oyster
sum(oyster$negative)
# user sentiment for oyster
sum(oyster$positive)/(sum(oyster$positive)+sum(oyster$negative))
# correlation between user sentiment and stars for oyster
cor(oyster$proportion,oyster$stars,use = "complete.obs")

# sum of positive for fish
sum(fish$positive)
# sum of negative for fish
sum(fish$negative)
# user sentiment for fish
sum(fish$positive)/(sum(fish$positive)+sum(fish$negative))
# correlation between user sentiment and stars for food
cor(fish$proportion,fish$stars,use = "complete.obs")

# sum of positive for clam
sum(clam$positive)
# sum of negative for clam
sum(clam$negative)
# user sentiment for clam
sum(clam$positive)/(sum(clam$positive)+sum(clam$negative))
# correlation between user sentiment and stars for clam
cor(clam$proportion,clam$stars,use = "complete.obs")

# do chisq test
mat.2 <- matrix(c(6022,374,3146,479,4379,265,802,58,6646,558,1154,100),ncol = 2,nrow = 6,byrow = T)
colnames(mat.2) <- c("positive","negative")
rownames(mat.2) <- c("lobster","crab","shrimp","oyster","fish","clam")
print(mat.2)
chisq.test(mat.2)

# load data for service related nouns
waiter <- read.csv("waiter.csv")
waitress <- read.csv("waitress.csv")
chef <- read.csv("chef.csv")
manager <- read.csv("manager.csv")

# sum of positive for waiter
sum(waiter$positive)
# sum of negative for waiter
sum(waiter$negative)
# user sentiment for waiter
sum(waiter$positive)/(sum(waiter$positive)+sum(waiter$negative))
# correlation between user sentiment and stars for waiter
cor(waiter$proportion,waiter$stars,use = "complete.obs")

# sum of positive for waitress
sum(waitress$positive)
# sum of negative for waitress
sum(waitress$negative)
# user sentiment for waitress
sum(waitress$positive)/(sum(waitress$positive)+sum(waitress$negative))
# correlation between user sentiment and stars for waitress
cor(waitress$proportion,waitress$stars,use = "complete.obs")

# sum of positive for chef
sum(chef$positive)
# sum of negative for chef
sum(chef$negative)
# user sentiment for chef
sum(chef$positive)/(sum(chef$positive)+sum(chef$negative))
# correlation between user sentiment and stars for chef
cor(chef$proportion,chef$stars,use = "complete.obs")

# sum of positive for manager
sum(manager$positive)
# sum of negative for manager
sum(manager$negative)
# user sentiment for manager
sum(manager$positive)/(sum(manager$positive)+sum(manager$negative))
# correlation between user sentiment and stars for manager
cor(manager$proportion,manager$stars,use = "complete.obs")

# do chisq test
mat.3 <- matrix(c(1468,267,1424,284,818,48,586,263),ncol = 2,nrow = 6,byrow = T)
colnames(mat.3) <- c("positive","negative")
rownames(mat.3) <- c("lobster","crab","shrimp","oyster","fish","clam")
print(mat.3)
chisq.test(mat.3)
