source('contrast_sim_utility.R')



library(doParallel)
detectCores()
registerDoParallel(4)



n=2000
r=1000
FUN=DGP10

result111 = simulation_v12(FUN, n= n,r = r,Right_Y_model = TRUE,Right_E_model = TRUE)
print('one done')
write.csv(as.data.frame(result111), paste('res/',n,'NewEQ/result111.csv', sep = ''))

result101 = simulation_v12(FUN, n= n,r = r,Right_Y_model = TRUE,Right_E_model = FALSE)
print('two done')
write.csv(as.data.frame(result101), paste('res/',n,'NewEQ/result101.csv', sep = ''))

result011 = simulation_v12(FUN, n= n,r = r,Right_Y_model = FALSE,Right_E_model = TRUE)
print('three done')
write.csv(as.data.frame(result011), paste('res/',n,'NewEQ/result011.csv', sep = ''))

result001 = simulation_v12(FUN, n= n,r = r,Right_Y_model = FALSE,Right_E_model = FALSE)
print('four done')
write.csv(as.data.frame(result001), paste('res/',n,'NewEQ/result001.csv', sep = ''))


result110 = simulation_v12(FUN, n= n,r = r,Right_Y_model = TRUE,Right_E_model = TRUE, Right_I_model = FALSE)
print('five done')
write.csv(as.data.frame(result110), paste('res/',n,'NewEQ/result110.csv', sep = ''))

result100 = simulation_v12(FUN, n= n,r = r,Right_Y_model = TRUE,Right_E_model = FALSE, Right_I_model = FALSE)
print('six done')
write.csv(as.data.frame(result100), paste('res/',n,'NewEQ/result100.csv', sep = ''))

result010 = simulation_v12(FUN, n= n,r = r,Right_Y_model = FALSE,Right_E_model = TRUE, Right_I_model = FALSE)
print('seven done')
write.csv(as.data.frame(result010), paste('res/',n,'NewEQ/result010.csv', sep = ''))

result000 = simulation_v12(FUN, n= n,r = r,Right_Y_model = FALSE,Right_E_model = FALSE, Right_I_model = FALSE)
print('eight done')
write.csv(as.data.frame(result000), paste('res/',n,'NewEQ/result000.csv', sep = ''))
