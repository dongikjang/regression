scaled <- function(model, type="standardized")
  UseMethod("scaled")

scaled.lm <- function(model, type="standardized"){
  switch(type,
         studentized = rstandard(model),
         
         rstudent = rstudent(model),
         
         standardized = residuals(model)/summary(model)$sigma
  )
}
