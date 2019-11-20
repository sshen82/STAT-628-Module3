library(shiny)
library(ggplot2)
library(tm)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)

CatchError = function(expr){
  err = NULL
  tryCatch(expr, error = function(e){
    err <<- e
    NULL})
  return (err)
}
names = list.files("./data/")
namesr = c()
for (i in 1:length(names)){
  ttt = strsplit(names, "[.]")
  namesr[i] = ttt[[i]][1]
}

j = 2
data1 = read.csv(paste("./data/", "chef.csv", sep = ""), header=T)
colnames(data1) = c("business_id", "positive_chef", "negative_chef", "proportion_chef", "stars")
for (i in names[2:12]){
  temp = read.csv(paste("./data/", i, sep = ""), header=T)[, c(2, 3, 4)]
  colnames(temp) = c(paste("positive_", namesr[j], sep = ""), paste("negative_", namesr[j], sep = ""), 
                     paste("proportion_", namesr[j], sep = ""))
  data1 <- cbind(data1, temp)
  j = j + 1
}

temp = data1[1, ]
nm = strsplit(colnames(temp), "_")
c2 = c()
for (i in 1:length(nm)){
  if (nm[[i]][1] == "proportion"){
    c2[i] = i
  }else(
    c2[i] = 0
  )
}

totalprop = data1[, c2]
colnames(totalprop) = namesr
totalf = totalprop[, namesr[c(2, 3, 4, 5, 7, 10)]]
tf = rowMeans(totalf, na.rm = TRUE)
totals = totalprop[, namesr[c(1, 6, 9, 11, 12)]]
ts = rowMeans(totals, na.rm = TRUE)
totalp = totalprop[, namesr[8]]
tp = totalp
dats = data.frame(data = c(tf, ts, tp), xx = c(rep("Food", 433), 
                                               rep("Service", 433),
                                               rep("Price", 433)))

require('leaps')
data=read.csv("business_attributes.csv")
# clean data
data_new=data
data_new$X=NULL
data_new$GoodForMeal=NULL
data_new$Ambience=NULL
data_new$BusinessParking=NULL
bid = data_new$business_id
data_new$business_id = NULL

#use R squared to select 
leaps<-regsubsets(stars ~ ., data=data_new, nbest=4)
model2=lm(stars~BusinessAcceptsCreditCards+WiFi+NoiseLevel+RestaurantsAttire+Alcohol+RestaurantsGoodForGroups+GoodForKids,data_new)
test_x = data_new[1, c(3, 6, 7, 8, 9, 14, 17)]
dattt = data_new[, c(3, 6, 7, 8, 9, 14, 17)]
predict(model2, test_x)

server = shinyServer(function(input, output){
  pred <- reactive({
    validate(
      need(input$bus %in% as.character(data1$business_id), "Please input a valid business id.")
    )
    pre = input$bus
    pre
  })
  
  output$ref = renderTable({
    if (is.null(CatchError(pred())) == TRUE){
      temp = data1[as.character(data1$business_id) %in% input$bus, ]
      nm = strsplit(colnames(temp), "_")
      c2 = c()
      for (i in 1:length(nm)){
        if (nm[[i]][1] == "proportion"){
          c2[i] = i
        }else(
          c2[i] = 0
        )
      }
      props = temp[, c2]
      colnames(props) = namesr
      food = mean(as.numeric(props[, namesr[c(2, 3, 4, 5, 7, 10)]]), na.rm = TRUE)
      service = mean(as.numeric(props[, namesr[c(1, 6, 9, 11, 12)]]), na.rm = TRUE)
      price = props[, namesr[8]]
      all = c(food, service, price)
      if (any(is.na(all))){
        all[!is.na(all)] = rank(all[!is.na(all)])
        all[is.na(all)] = "No Value"
        names(all) = c("Food", "Service", "Price")
        result = as.data.frame(t(all))
        rownames(result) = ""
        result
      }else{
        all = as.character(rank(all))
        names(all) = c("Food", "Service", "Price")
        result = as.data.frame(t(all))
        rownames(result) = ""
        result
      }
    }else{
      data.frame(Food = NA, Service = NA, Price = NA)
    }
  })
  
  output$plotfood = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      temp = data1[as.character(data1$business_id) %in% input$bus, ]
      nm = strsplit(colnames(temp), "_")
      c2 = c()
      for (i in 1:length(nm)){
        if (nm[[i]][1] == "proportion"){
          c2[i] = i
        }else(
          c2[i] = 0
        )
      }
      props = temp[, c2]
      colnames(props) = namesr
      food = props[, namesr[c(2, 3, 4, 5, 7, 10)]]
      if (sum(is.na(food)) == 6){
        paste("")
      }else{
      paste("We recommend that you offer", "<font color=\"#FF0000\"><b>", names(food)[which.max(food)],  "</b></font>",
            ", and drop", "<font color=\"#FF0000\"><b>", names(food)[which.min(food)], "</b></font>", "people don't like it.", sep = " ")
      }
    }else{
      paste("")
    }
  })
  
  output$plotservice = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      temp = data1[as.character(data1$business_id) %in% input$bus, ]
      nm = strsplit(colnames(temp), "_")
      c2 = c()
      for (i in 1:length(nm)){
        if (nm[[i]][1] == "proportion"){
          c2[i] = i
        }else(
          c2[i] = 0
        )
      }
      props = temp[, c2]
      colnames(props) = namesr
      service = props[, namesr[c(1, 6, 11, 12)]]
      if (sum(is.na(service)) == 4){
        paste("")
      }else{
        paste("You can rise the salary of", "<font color=\"#FF0000\"><b>", names(service)[which.max(service)], "</b></font>",
              ", and 'punish'", "<font color=\"#FF0000\"><b>", names(service)[which.min(service)], "</b></font>", sep = " ")
      }
    }else{
      paste("")
    }
  })
  
  output$conclusion = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      temp = data1[as.character(data1$business_id) %in% input$bus, ]
      nm = strsplit(colnames(temp), "_")
      c2 = c()
      for (i in 1:length(nm)){
        if (nm[[i]][1] == "proportion"){
          c2[i] = i
        }else(
          c2[i] = 0
        )
      }
      props = temp[, c2]
      colnames(props) = namesr
      food = mean(as.numeric(props[, namesr[c(2, 3, 4, 5, 7, 10)]]), na.rm = TRUE)
      service = mean(as.numeric(props[, namesr[c(1, 6, 9, 11, 12)]]), na.rm = TRUE)
      price = props[, namesr[8]]
      all = c(food, service, price)
      if (any(is.na(all))){
        all[!is.na(all)] = rank(all[!is.na(all)])
        all[is.na(all)] = "No Value"
        names(all) = c("Food", "Service", "Price")
        result = as.data.frame(t(all))
        rownames(result) = ""
        result
      }else{
        all = as.character(rank(all))
        names(all) = c("Food", "Service", "Price")
        result = as.data.frame(t(all))
        rownames(result) = ""
        result
      }
      paste("You need to enhance your", "<font color=\"#FF0000\"><b>", names(result)[result == "1"], "</b></font>", ".", sep = " ")
    }else{
      paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "The restaurant you input is not included.")
    }
  })
  
  output$plot = renderPlot({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      ggplot(dats, aes(x = xx, y = data)) + geom_boxplot() +
        coord_flip() + xlab("") + ylab("") + theme_classic(base_size = 15) +
        geom_point(data = data.frame(x = factor(c("Food", "Service", "Price")), y = c(dats[idx, 1], dats[idx + 433, 1], dats[idx + 433 * 2, 1])), 
                   aes(x = x, y = y), color = "red", size = 4)
    }
    else{
      ggplot(dats, aes(x = xx, y = data)) + geom_boxplot() +
        coord_flip() + xlab("") + ylab("") + theme_classic(base_size = 15)
    }
  })
  
  output$att1 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[1] == "True"){
        paste("")
      }else{
        org = predict(model2, he)
        he[1] = "True"
        new = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "accept Credit Cards", "</b></font>", ", your star will increase by", round(new - org, 2), ".")
      }
    }
    else{
      paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "The restaurant you input is not included.")
    }
  })
  
  output$att2 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[2] == "'free'"){
        paste("")
      }else{
        org = predict(model2, he)
        he[2] = "'free'"
        new = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "have free WiFi", "</b></font>", ", your star will increase by", round(new - org, 2), ".")
      }
    }
    else{
      paste("")
    }
    })
  
  output$att3 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[3] == "'quiet'"){
        paste("")
      }else{
        org = predict(model2, he)
        he[3] = "'quiet'"
        new = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "have quiet environment", "</b></font>", ", your star will increase by", round(new - org, 2), ".")
      }
    }
    else{
      paste("")
    }
  })
  
  output$att4 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[4] == "'dressy'"){
        paste("")
      }else{
        org = predict(model2, he)
        he[4] = "'dressy'"
        new = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "require customers to dress formally", "</b></font>", ", your star will increase by", round(new - org, 2), ".")
      }
    }
    else{
      paste("")
    }
  })
  
  output$att5 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[5] == "'beer_and_wine'"){
        paste("")
      }else{
        org5 = predict(model2, he)
        he[5] = "'beer_and_wine'"
        new5 = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "offer beer and wine", "</b></font>", ", your star will increase by", round(new5 - org5, 2), ".")
      }
    }
    else{
      paste("")
    }
  })
  
  output$att6 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[6] == "False"){
        paste("")
      }else{
        org6 = predict(model2, he)
        he[6] = "False"
        new6 = predict(model2, he)
        paste("If you", "<font color=\"#FF0000\"><b>", "only allow small groups", "</b></font>", ", your star will increase by", round(new6 - org6, 2), ".")
      }
    }
    else{
      paste("")
    }
  })
  
  output$att7 = renderText({
    if (is.null(CatchError(pred())) == TRUE){
      idx = which(pred() == data1$business_id)
      he = dattt[idx, ]
      if (he[7] == "False"){
        paste("")
      }else{
        org7 = predict(model2, he)
        he[7] = "False"
        new7 = predict(model2, he)
        paste("If the restaurant", "<font color=\"#FF0000\"><b>", "isn't suitable for kids", "</b></font>", ", your star will increase by", round(new7 - org7, 2), ".")
      }
    }
    else{
      paste("")
    }
  })
})



