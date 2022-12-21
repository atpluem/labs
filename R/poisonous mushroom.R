library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)

mushroom <- read.csv("mushrooms.csv")
mushroom %>% 
  ggplot(aes(x = class, fill = class)) + 
  geom_bar()

# ring.number - class
mushroom %>%
  ggplot(aes(x = ring.number, fill = class)) +
  geom_bar(position = "fill")  
cross_tab <- table(mushroom$ring.number, mushroom$class)
cross_tab
chisq.test(cross_tab)

# ring.type - class
mushroom %>%
  ggplot(aes(x = ring.type, fill = class)) +
  geom_bar(position = "fill")  
cross_tab <- table(mushroom$ring.type, mushroom$class)
cross_tab
chisq.test(cross_tab)

# veil.color - class
mushroom %>%
  ggplot(aes(x = veil.color, fill = class)) +
  geom_bar(position = "fill")  
cross_tab <- table(mushroom$veil.color, mushroom$class)
cross_tab
chisq.test(cross_tab)

# veil.type - class
mushroom %>%
  ggplot(aes(x = veil.type, fill = class)) +
  geom_bar(position = "fill")  
cross_tab <- table(mushroom$veil.type, mushroom$class)
cross_tab
chisq.test(cross_tab)

# ---------- Dicision tree ----------
set.seed(555)
test_ind <- sample(nrow(mushroom),
                   0.3*nrow(mushroom))
mushroom_training <- mushroom[-test_ind,] 
mushroom_testing <- mushroom[test_ind,]
summary(mushroom_testing)

# Without tuning
tree <- rpart(class ~ ., data = mushroom_training)
rpart.plot(tree)
tree$variable.importance
res <- predict(tree, mushroom_testing, type = "class")
head(res)
confusionMatrix(res,
                mushroom_testing$class,
                mode = "prec_recall",
                positive = "p")

# Tuning comfortable attribute 
# -(odor, spore.print.color, habitat, population)
tree_tuning1 <- rpart(class ~ cap.shape + cap.surface + cap.color + 
                       bruises + gill.attachment + gill.spacing + gill.size + gill.color + 
                       stalk.shape + stalk.surface.above.ring + stalk.surface.below.ring +
                       stalk.color.above.ring + stalk.color.below.ring +
                       veil.type + veil.color + ring.number + ring.type,
                     data = mushroom_testing)
rpart.plot(tree_tuning1)
res_turning1 <- predict(tree_tuning1, mushroom_testing, type = "class")
head(res_turning1)
confusionMatrix(res_turning1,
                mushroom_testing$class,
                mode = "prec_recall",
                positive = "p")

# Tuning CP by k-fold CV
train_control <- trainControl(method = "cv",
                              number = 500)
model <- train(class ~ ., data = mushroom_training,
               trControl = train_control,
               method = "rpart")

# Tuning by lift analysis
# Lift calculation (caret)
res.p1 <- predict(tree, mushroom_testing)[,"p"]
lift_result <- data.frame(prob = res.p1,
                          class = mushroom_testing$class)
lift_obj <- lift(class ~ prob, 
                 data = lift_result,
                 class = "p") 
plot(lift_obj, values = 60)
# Lift chart (ROCR)
res.p2 <- prediction(res.p1, mushroom_testing$class,
                     label.ordering = c("p","e"))
pref_lift <- performance(res.p2, "lift", "rpp")
plot(pref_lift)
