source('contrast_sim_utility.R')
var <- .1
sd <- sqrt(var)
beta <- .75

require(doParallel)
detectCores()
registerDoParallel(5)

n <- 1000
r <- 1000
FUN <- DGP10
Path <- paste('res/n',n,'sd',round(sd,digits = 1),'beta',beta,'/', sep = '')

result <- simulation_v12(FUN, n= n,r = r, n_param = 10000000, sd=sd, beta=beta)

result2 <- as.data.frame(result)

ALt <- c('11100', '10100', '01100', '00111', '00011')

for (bin in ALt) {
  resultsel <- result2[result$ALt == bin,]
  write.csv(resultsel, paste(Path, 'result', bin ,'.csv', sep = ''))
}

