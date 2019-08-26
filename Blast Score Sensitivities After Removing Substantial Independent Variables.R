library(readxl)
library(rjags)
library(dplyr)
library(ggplot2)
library(caret)
library(WVPlots)
player_blast_data<-data.frame(read_excel("D:/CAPE COD/player_Blast_data_caret.xlsx",col_names=TRUE))
colnames(player_blast_data)

split <- round(nrow(player_blast_data) * .80)

# Create train
player_blast_train <- player_blast_data[1:split, ]

# Create test
player_blast_test <- player_blast_data[(split+1):nrow(player_blast_data), ]


# Set seed
set.seed(42)
# connection score LM model utilizing Cross-Validation
connection_score_lm_model <- {train(
  Connection_Score~.,
  data = player_blast_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
}

# Set Seed
set.seed(43)
# Plane Score LM model utilizing Cross-Validation
plane_score_lm_model <- {train(
  Plane_Score~.,
  data = player_blast_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
}

# Set Seed
set.seed(44)
# Plane Score LM model utilizing Cross-Validation
rotation_score_lm_model <- {train(
  Rotation_Score~.,
  data = player_blast_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
}

# Set Seed
set.seed(45)
# Plane Score LM model utilizing Cross-Validation with fewer independent variables
plane_score_lm_model_2 <- {train(
  Plane_Score~Connection_Score +
    Rotation_Score +
    Bat_Speed_mph +
    Rotational_Acceleration +
    Attack_Angle_Degrees +
    Early_Connection_Degrees +
    Connection_At_Impact_Degrees +
    Vertical_Bat_Angle_Degrees +
    Power_kW +
    Time_To_Contact_Seconds +
    Peak_Hand_Speed_mph,
  data = player_blast_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
}
plane_score_lm_model_2
summary(plane_score_lm_model_2)
plane_score_lm_pred<-predict(plane_score_lm_model_2, player_blast_test)
plane_score_lm_pred_df<-data.frame(actual = player_blast_test$Plane_Score, predicted = plane_score_lm_pred, 
                                   residuals = plane_score_lm_pred_df$actual - plane_score_lm_pred_df$predicted)


# Set Seed
set.seed(46)
# Connection Score LM model utilizing Cross-Validation with fewer independent variables
connection_score_lm_model_2 <- {train(
  Connection_Score~Plane_Score +
    Rotation_Score +
    Bat_Speed_mph +
    Rotational_Acceleration +
    Attack_Angle_Degrees +
    Vertical_Bat_Angle_Degrees +
    Power_kW +
    Peak_Hand_Speed_mph +
    On_Plane_Efficiency_Percent,
  data = player_blast_train,
  method = "lm",
  trcontrol = trainControl(
    method ="cv", number = 10
  )
)
}
connection_score_lm_model_2
summary(connection_score_lm_model_2)
connection_score_lm_pred<-predict(connection_score_lm_model_2, player_blast_test)
connection_score_lm_pred_df<-data.frame(actual = player_blast_test$Connection_Score, predicted = connection_score_lm_pred,
                                        residuals = connection_score_lm_pred_df$actual - connection_score_lm_pred_df$predicted)


# Set Seed
set.seed(47)
# Rotation Score LM model utilizing Cross-Validation with fewer independent variables
rotation_score_lm_model_2 <-{ train(
  Rotation_Score~Connection_Score +
    Plane_Score +
    Bat_Speed_mph +
    Attack_Angle_Degrees +
    Early_Connection_Degrees +
    Connection_At_Impact_Degrees +
    Vertical_Bat_Angle_Degrees +
    On_Plane_Efficiency_Percent +
    Peak_Hand_Speed_mph,
  data = player_blast_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
}
rotation_score_lm_model_2
summary(rotation_score_lm_model_2)
rotation_score_lm_pred <- predict(rotation_score_lm_model_2, player_blast_test)
rotation_score_lm_pred_df<-data.frame(actual = player_blast_test$Rotation_Score, predicted = rotation_score_lm_pred, 
                                      residuals = rotation_score_lm_pred_df$actual - rotation_score_lm_pred_df$predicted)



# Set Seed
set.seed(48)
# Plane Score RF model utilizing Cross-Validation with fewer independent variables
plane_score_rf_model <- {train(
  Plane_Score~Connection_Score +
    Rotation_Score +
    Bat_Speed_mph +
    Rotational_Acceleration +
    Attack_Angle_Degrees +
    Early_Connection_Degrees +
    Connection_At_Impact_Degrees +
    Vertical_Bat_Angle_Degrees +
    Power_kW +
    Time_To_Contact_Seconds +
    Peak_Hand_Speed_mph,
  tuneLength = 10,
  data = player_blast_train, method = "ranger",
  importance = "permutation",
  trControl = trainControl(method = "cv", number = 10)
)}
plane_score_rf_model
summary(plane_score_rf_model)
plane_score_rf_pred_df<-data.frame(actual = player_blast_test$Plane_Score, predicted = plane_score_rf_pred,
                                   residuals = plane_score_rf_pred_df$actual - plane_score_rf_pred_df$predicted)



# Set Seed
set.seed(50)
# Rotation Score RF model utilizing Cross-Validation with fewer independent variables
rotation_score_rf_model <- {train(
  Rotation_Score~Connection_Score +
    Plane_Score +
    Bat_Speed_mph +
    Attack_Angle_Degrees +
    Early_Connection_Degrees +
    Connection_At_Impact_Degrees +
    On_Plane_Efficiency_Percent +
    Vertical_Bat_Angle_Degrees +
    Peak_Hand_Speed_mph,
  tuneLength = 3,
  data = player_blast_train,
  method = "ranger",
  importance = "impurity",
  trControl = trainControl(method = "cv", number = 10)
)}

rotation_score_rf_model
summary(rotation_score_rf_model)
rotation_score_rf_pred <- predict(rotation_score_rf_model, player_blast_test)
rotation_score_rf_pred_df<-data.frame(actual = player_blast_test$Rotation_Score, predicted = rotation_score_rf_pred,
                                      residuals = rotation_score_rf_pred_df$actual - rotation_score_rf_pred_df$predicted)


# Set Seed
set.seed(49)
# Connection Score RF model utilizing Cross-Validation with fewer independent variables
connection_score_rf_model <- {train(
  Connection_Score~Plane_Score +
    Rotation_Score +
    Bat_Speed_mph +
    Rotational_Acceleration +
    Attack_Angle_Degrees +
    Vertical_Bat_Angle_Degrees +
    Power_kW +
    Peak_Hand_Speed_mph +
    On_Plane_Efficiency_Percent,
  tuneLength = 3,
  data= player_blast_train,
  importance = "impurity",
  method = "ranger",
  trControl = trainControl(method ="cv", number = 10)
)}
connection_score_rf_model
summary(connection_score_rf_model)
connection_score_rf_pred <- predict(connection_score_rf_model, player_blast_test)
connection_score_rf_pred_df<-data.frame(actual = player_blast_test$Connection_Score, predicted = connection_score_rf_pred,
                                        residuals = connection_score_rf_pred_df$actual - connection_score_rf_pred_df$predicted)


# Establish TrainControl and Custom Grid for GLM Models
glm_control <- trainControl(
  method = "cv",
  number = 10
)
glm_grid <- expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20)
)

# Set Seed
set.seed(51)
plane_score_glm_model <- {train(Plane_Score~Connection_Score +
                                 Rotation_Score +
                                 Bat_Speed_mph +
                                 Rotational_Acceleration +
                                 Attack_Angle_Degrees +
                                 Early_Connection_Degrees +
                                 Connection_At_Impact_Degrees +
                                 Vertical_Bat_Angle_Degrees +
                                 Power_kW +
                                 Time_To_Contact_Seconds +
                                 Peak_Hand_Speed_mph, 
                                data = player_blast_train, method = "glmnet", trControl = glm_control, tuneGrid = glm_grid)}
plot(plane_score_glm_model)
summary(plane_score_glm_model)
plane_score_glm_model
plane_score_glm_pred <- predict(plane_score_glm_model, player_blast_test)
plane_score_glm_pred_df<-data.frame(actual = player_blast_test$Plane_Score, predicted = plane_score_glm_pred,
                                    residuals = plane_score_glm_pred_df$actual - plane_score_glm_pred_df$predicted)


# Set Seed
set.seed(52)
rotation_score_glm_model <- {
  train(
    Rotation_Score ~ Connection_Score +
      Plane_Score +
      Bat_Speed_mph +
      Attack_Angle_Degrees +
      Early_Connection_Degrees +
      Connection_At_Impact_Degrees +
      Vertical_Bat_Angle_Degrees +
      On_Plane_Efficiency_Percent +
      Peak_Hand_Speed_mph,
    data = player_blast_train,
    method = "glmnet",
    trControl = glm_control,
    tuneGrid = glm_grid
  )
}
rotation_score_glm_model
summary(rotation_score_glm_model)
rotation_score_glm_pred <- predict(rotation_score_glm_model, player_blast_test)
rotation_score_glm_pred_df<-data.frame(actual = player_blast_test$Rotation_Score, predicted = rotation_score_glm_pred,
                                       residuals = rotation_score_glm_pred_df$actual - rotation_score_glm_pred_df$predicted)



# Set Seed
set.seed(53)
connection_score_glm_model <- {train(
  Connection_Score ~ Plane_Score +
    Rotation_Score +
    Bat_Speed_mph +
    Rotational_Acceleration +
    Attack_Angle_Degrees +
    Vertical_Bat_Angle_Degrees +
    Power_kW +
    Peak_Hand_Speed_mph +
    On_Plane_Efficiency_Percent, 
  data = player_blast_train,
  method = "glmnet",
  trControl = glm_control,
  tuneGrid = glm_grid
)
}
connection_score_glm_model
summary(connection_score_glm_model)
connection_score_glm_pred <- predict(connection_score_glm_model, player_blast_test)
connection_score_glm_pred_df<-data.frame(actual = player_blast_test$Connection_Score, predicted = connection_score_glm_pred,
                                         residuals = connection_score_glm_pred_df$actual - connection_score_glm_pred_df$predicted)

# Connection Score Plots
plot(varImp(connection_score_glm_model, scale = TRUE), top = 10, main = "Connection Score glmnet")
GainCurvePlot(connection_score_glm_pred_df, "predicted", "actual", "Connection Score GLM Gain Plot")
ggplot(connection_score_glm_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin=0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Connection Score GLM Residuals")

plot(varImp(connection_score_rf_model, scale = TRUE), top = 8, main = "Connection Score random forest")
GainCurvePlot(connection_score_rf_pred_df, "predicted", "actual", "Connection Score Random Forest Gain Plot")
ggplot(connection_score_rf_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin=0, ymax=residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Connection Score Random Forest Residuals")

plot(varImp(connection_score_lm_model_2 , scale = TRUE), top = 10, main = "Connection Score lm")
GainCurvePlot(connection_score_lm_pred_df, "predicted", "actual", "Connection Score LM Gain Plot")
ggplot(connection_score_lm_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin=0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Conection Score LM Residuals")


# Plane Score Plots
plot(varImp(plane_score_glm_model, scale = FALSE), top = 10, main = "Plane Score glmnet")
GainCurvePlot(plane_score_glm_pred_df, "predicted", "actual", "Plane Score GLM Gain Plot")
ggplot(plane_score_glm_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Plane Score GLM Residuals")


plot(varImp(plane_score_rf_model, scale = FALSE), top = 10, main = "Plane Score random forest")
GainCurvePlot(plane_score_rf_pred_df, "predicted", "actual", "Plane Score Random Forest Gain Plot")
ggplot(plane_score_rf_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Plane Score Random Forest Residuals")

plot(varImp(plane_score_lm_model_2, scale = FALSE), top = 10, main = "Plane Score lm")
GainCurvePlot(plane_score_lm_pred_df, "predicted", "actual", "Plane Score LM Gain Plot")
ggplot(plane_score_rf_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Plane Score LM Residuals")


# Rotation Score Plots

plot(varImp(rotation_score_glm_model, scale = FALSE), top = 10, main = "Rotation Score glmnet")
GainCurvePlot(rotation_score_glm_pred_df, "predicted", "actual", "Rotation Score GLM Gain Plot")
ggplot(rotation_score_glm_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept=0, linetype=3) +
  ggtitle("Rotation Score GLM Residuals")


plot(varImp(rotation_score_rf_model, scale = FALSE), top = 10, main = "Rotation Score random forest")
GainCurvePlot(rotation_score_rf_pred_df, "predicted", "actual", "Rotation Score Random Forest Gain Plot")
ggplot(rotation_score_rf_pred_df, aes(x = predicted, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept=0, linetype = 3) +
  ggtitle("Rotation Score Random Forest Residuals")


plot(varImp(rotation_score_lm_model_2, scale = FALSE), top = 10, main = "Rotation Score lm")
GainCurvePlot(rotation_score_lm_pred_df, "predicted", "actual", "Rotation Score LM Gain Plot")
ggplot(rotation_score_lm_pred_df, aes(x = predicted, y = residuals)) + 
  geom_hline(yintercept=0, linetype = 3) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) +
  ggtitle("Rotation Score LM Model Residuals")

