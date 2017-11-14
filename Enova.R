library(dplyr)
training <- read.csv("/Users/qingjinf/Downloads/Participant Files/training.csv", stringsAsFactors = F)

validation<-read.csv("/Users/qingjinf/Downloads/Participant Files/validation.csv", stringsAsFactors = F)

subset<-training[,c(1,2,9,10,11,12,13,14,15,16,20)]


subset$damage<-0
subset$damage<-ifelse(nchar(subset$damage_code)>0, 1, 0)
subset<-subset[,-11]
subset$poprate<-(subset$population_5_years_ago - subset$current_population)/subset$current_population

subset$build_date<-as.Date(subset$build_date, "%Y-%m-%d")
subset$remodel_date<-as.Date(subset$remodel_date,"%Y-%m-%d")
#subset<-subset[,-14]

subset$today<-as.Date("2017-11-13","%Y-%m-%d")
subset$age<-subset$today - subset$build_date
subset[is.na(subset$remodel_date),]$remodel_date<-subset[is.na(subset$remodel_date),]$build_date
subset$remodel_age<-subset$today - subset$remodel_date

subset<-subset[,-13] #delete today
subset<-subset[,-c(4,5)]  #delete dates
subset<-subset[,-1] #delete id

school<-subset %>% group_by(area_type) %>% summarise(count = sum(na.omit(schools_in_area)))
number<-subset %>% group_by(area_type) %>% summarise(count = n())
school$count/number$count

subset[subset$area_type == 'rural' & is.na(subset$schools_in_area),]$schools_in_area <- 2.274071
subset[subset$area_type == 'suburban' & is.na(subset$schools_in_area),]$schools_in_area <- 3.986186
subset[subset$area_type == 'urban' & is.na(subset$schools_in_area),]$schools_in_area <-8.546827

public<-subset %>% group_by(area_type) %>% summarise(count = sum(na.omit(public_transit_score)))
number<-subset %>% group_by(area_type) %>% summarise(count = n())
public$count/number$count

subset[subset$area_type == 'rural' & is.na(subset$public_transit_score),]$public_transit_score <-  0.8643504
subset[subset$area_type == 'suburban' & is.na(subset$public_transit_score),]$public_transit_score <- 3.7491723
subset[subset$area_type == 'urban' & is.na(subset$public_transit_score),]$public_transit_score <-7.3816467
subset<-subset[,-8] #delete damage
subset<-subset[,-5] #delete pop_5


#####Emma's
training$profit <- training$final_price- training$initial_price -training$investment
na <- apply(training,2,is.na)
num.NA <- apply(na,2,sum)
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(training)[1])] # trainT = train
training <- training[, remain.col]

training$profit <- training$final_price- training$initial_price -training$investment

avg.imp <- function(a,avg){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- avg
  return(imputed)
}

mcv.imp<-function  (a,modevalue){
  missing    <-is.na(a)
  n.missing<-sum(missing)
  a.obs<-a[!missing]
  imputed <-a
  imputed[missing] <-modevalue
  return (imputed)}

random.imp<-function (a){
  missing    <-is.na(a)      
  n.missing<-sum(missing)   
  a.obs<-a[!missing]
  imputed    <-a
  imputed[missing]    <-sample (a.obs,    n.missing,    replace=TRUE)
  return    (imputed)}

Mode    <-function(x)   {
  x<-x[!is.na(x)]
  ux<-unique(x)  
  ux[which.max(tabulate(match(x,    ux)))] 
}    

Mode.fib = Mode(training$floors_in_building)

training$floors_in_building <- mcv.imp(training$floors_in_building,Mode.fib)
training$floors_in_unit <- mcv.imp(training$floors_in_unit,Mode(training$floors_in_unit))
training$sqft <- avg.imp(training$sqft,mean(na.omit(training$sqft)))
training$floors_of_unit <- mcv.imp(training$floors_of_unit,Mode(training$floors_of_unit))
training$basement <-random.imp(training$basement)
training$overall_inspector_score <- avg.imp(training$overall_inspector_score,mean(na.omit(training$overall_inspector_score)))


####Combine
trainingset<-training[,c(18:38)]
final<-cbind(subset,trainingset)
final<-final[,-c(25,26,27,28)]
final<-final[,-c(23)]  #no exterior color
final<-final[,-c(23)]  #no exterior material

######change grade to 0 and 1
final$structural_quality_grade <- ifelse(final$structural_quality_grade == c("A","B"),1,0)
final$exterior_condition_grade <- ifelse(final$exterior_condition_grade == c("A","B"),1,0)
final$interior_condition_grade <- ifelse(final$interior_condition_grade == c("A","B"),1,0)
final$utilities_grade<- ifelse(final$utilities_grade == c("A","B"),1,0)
final$damage_and_issue_grade<- ifelse(final$damage_and_issue_grade == c("A","B"),1,0)

final<-final[,-10]
final<-final[-which(final$profit == 0),]  ##delete 2 profit = 0

set <-final[-which(is.na(final$age)),]
set<-set[-which(is.na(final$remodel_age)),]
final<-set

final$profitinverse<-log(final$profit + 1 -min(final$profit))

final<-final[-5850,]
final<-final[-1432,]
###Model
fit1 <- lm(profit~.,data = final )
summary(fit1)

step(fit1,direction="both")



fit2<-lm(formula = profit ~ zone + area_type + current_population + 
           public_transit_score + poprate + remodel_age + structural_quality_grade + 
           damage_and_issue_grade + overall_inspector_score + sqft + 
           floors_in_unit + parking + central_hvac + initial_value, 
         data = final)
summary(fit2)

plot(fit2)



fit3 <- lm(profitinverse~. - profit -final_price, data = final )
summary(fit3)
step(fit3,direction="both")

#Final
fit3_1 <- lm(profit ~. - profitinverse -final_price, data = final )
summary(fit3_1)
step(fit3_1,direction="both")

fit4_1<-lm(formula = profit ~ zone + area_type + current_population + 
             schools_in_area + public_transit_score + poprate + age + 
             remodel_age + structural_quality_grade + damage_and_issue_grade + 
             overall_inspector_score + sqft + floors_in_unit + parking + 
             basement + central_hvac, data = final)
summary(fit4_1)
plot(fit4_1)


###Final
fit5<-lm(final_price~.-profit -profitinverse,data=final)
summary(fit5)
step(fit5)

fit6<-lm(formula = final_price ~ zone + area_type + current_population + 
           schools_in_area + public_transit_score + poprate + age + 
           remodel_age + structural_quality_grade + exterior_condition_grade + 
           interior_condition_grade + damage_and_issue_grade + floors_in_building + 
           floors_in_unit + parking + basement + central_hvac, data = final)

summary(fit6)
plot(fit6)
###Predict


validation$poprate<-(validation$population_5_years_ago - validation$current_population)/validation$current_population

validation$today<-as.Date("2017-11-13","%Y-%m-%d")
validation$build_date<-as.Date(validation$build_date, "%Y-%m-%d")
validation$remodel_date<-as.Date(validation$remodel_date,"%Y-%m-%d")

validation$age<-validation$today - validation$build_date
validation[is.na(validation$remodel_date),]$remodel_date<-validation[is.na(validation$remodel_date),]$build_date
validation$remodel_age<-validation$today - validation$remodel_date

school<-validation %>% group_by(area_type) %>% summarise(count = sum(na.omit(schools_in_area)))
number<-validation %>% group_by(area_type) %>% summarise(count = n())
school$count/number$count

validation[validation$area_type == 'rural' & is.na(validation$schools_in_area),]$schools_in_area <- 2.167539
validation[validation$area_type == 'suburban' & is.na(validation$schools_in_area),]$schools_in_area <- 3.883785
validation[validation$area_type == 'urban' & is.na(validation$schools_in_area),]$schools_in_area <-8.444818

public<-validation %>% group_by(area_type) %>% summarise(count = sum(na.omit(public_transit_score)))
number<-validation %>% group_by(area_type) %>% summarise(count = n())
public$count/number$count

validation[validation$area_type == 'rural' & is.na(validation$public_transit_score),]$public_transit_score <-  0.828721
validation[validation$area_type == 'suburban' & is.na(validation$public_transit_score),]$public_transit_score <- 3.719339
validation[validation$area_type == 'urban' & is.na(validation$public_transit_score),]$public_transit_score <-7.331090


validation$structural_quality_grade <- ifelse(validation$structural_quality_grade == c("A","B"),1,0)

validation$damage_and_issue_grade<- ifelse(validation$damage_and_issue_grade == c("A","B"),1,0)

validation$exterior_condition_grade <- ifelse(validation$exterior_condition_grade == c("A","B"),1,0)
validation$interior_condition_grade <- ifelse(validation$interior_condition_grade == c("A","B"),1,0)
validation$utilities_grade<- ifelse(validation$utilities_grade == c("A","B"),1,0)


validation$floors_in_building <- mcv.imp(validation$floors_in_building,Mode(validation$floors_in_building))
validation$floors_in_unit <- mcv.imp(validation$floors_in_unit,Mode(validation$floors_in_unit))


validation$overall_inspector_score <- avg.imp(validation$overall_inspector_score,mean(na.omit(validation$overall_inspector_score)))

validation$sqft <- avg.imp(validation$sqft,mean(na.omit(validation$sqft)))

validation$floors_in_unit <- mcv.imp(validation$floors_in_unit,Mode(validation$floors_in_unit))

validation$floors_of_unit <- mcv.imp(validation$floors_of_unit,Mode(validation$floors_of_unit))


validation$profitinverse<-log(validation$profit + 1 -min(validation$profit))


profit ~ zone + area_type + current_population + 
  schools_in_area + public_transit_score + poprate + age + 
  remodel_age + structural_quality_grade + damage_and_issue_grade + 
  overall_inspector_score + sqft + floors_in_unit + parking + 
  basement + central_hvac

variable_pro<-validation[,c('zone', 'area_type', 'current_population','schools_in_area',
                          'public_transit_score','poprate','age',
                        'remodel_age','structural_quality_grade',
                          'damage_and_issue_grade', 'overall_inspector_score','sqft','parking',
                          'floors_in_unit' ,'basement' ,'central_hvac' )]
validation$profit <- predict(fit4_1,variable_pro)



variable_fp<-validation[,c('zone', 'area_type', 'current_population','schools_in_area',
                            'public_transit_score','poprate','age',
                            'remodel_age','structural_quality_grade',
                            'exterior_condition_grade', 'interior_condition_grade','damage_and_issue_grade','parking',
                            'floors_in_unit' ,'basement' ,'central_hvac','floors_in_building' )]
fit6<-lm(formula = final_price ~ zone + area_type + current_population + 
           schools_in_area + public_transit_score + poprate + age + 
           remodel_age + structural_quality_grade + exterior_condition_grade + 
           interior_condition_grade + damage_and_issue_grade + floors_in_building + 
           floors_in_unit + parking + basement + central_hvac, data = final)
validation$final_price <- predict(fit6,newdata = variable_fp)
validation$investment <- validation$final_price -validation$initial_price- validation$profit 





dt<-validation[with(validation,order(- validation$profit)),]

dt$purchase_decision[1:641] = 1

dt$purchase_decision[642:nrow(dt)] = 0
validation <- dt

write.csv(validation,'validation_newbee.csv')

sum(validation$purchase_decision)
sum(dt$final_price[1:641]) < 4*10^8

validation[validation$property_id == dt$pr]
sort(validation$profit,decreasing=T)
