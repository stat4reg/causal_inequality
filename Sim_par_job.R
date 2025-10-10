source('contrast_sim_utility.R')


library(doParallel)
detectCores()
registerDoParallel(5)



n=1000
r=1000
FUN=DGP10
Path = paste('res/',n,'NewEQflexNM/', sep = '')

result = simulation_v12(FUN, n= n,r = r)

result2 = as.data.frame(result)

ALt = c('11100', '10100', '01100', '00111', '00011')

for (bin in ALt) {
  resultsel <- result2[result$ALt == bin,]
  write.csv(resultsel, paste(Path, 'result', bin ,'.csv', sep = ''))
}

