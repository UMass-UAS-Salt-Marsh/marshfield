# Gather RTK GPS points for UMass UAS 2026


path <- 'C:/Work/saltmarsh/data/uas2026/field/RTK'


x <- list.files(path)
z <- NULL

for(i in x) {
   f <- read.csv(file.path(path, i))
   if(is.null(f$Tilt.angle))
      f <- cbind(f, Tilt.angle = NA)
   
   if(is.null(z))
      z <- f
   
   else
      z <- rbind(z, f)
}
