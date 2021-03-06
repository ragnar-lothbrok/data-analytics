#
# Example R code for generating an H2O Scoring POJO.
#

rm(list = ls(all.names = T))
# "Safe" system.  Error checks process exit status code.  stop() if it failed.
safeSystem <- function(x) {
  print(sprintf("+ CMD: %s", x))
  res <- system(x)
  print(res)
  if (res != 0) {
    msg <- sprintf("SYSTEM COMMAND FAILED (exit status %d)", res)
    stop(msg)
  }
}

library(h2o)

cat("Starting H2O\n")
myIP <- "localhost"
myPort <- 54321
h <- h2o.init(ip = myIP, port = myPort, startH2O = TRUE)

cat("Building GBM model\n")
df <- h2o.importFile(path = normalizePath("/home/raghunandangupta/Downloads/splits/splitaa"));
y <- "click"
x <- c("id","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
gbm.h2o.fit <- h2o.gbm(training_frame = df, y = y, x = x, model_id = "GBMPojo", ntrees = 10)

cat("Downloading Java prediction model code from H2O\n")
model_id <- gbm.h2o.fit@model_id

tmpdir_name <- "generated_model"
cmd <- sprintf("rm -fr %s", tmpdir_name)
safeSystem(cmd)
cmd <- sprintf("mkdir %s", tmpdir_name)
safeSystem(cmd)

h2o.download_pojo(gbm.h2o.fit, "./generated_model/")

cat("Note: H2O will shut down automatically if it was started by this R script and the script exits\n")
