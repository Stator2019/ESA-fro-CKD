
# enviroment --------------------------------------------------------------

  library(stringr)
  len = length(str_split(getwd(), pattern = '/', simplify = T))
  source(
    file = 
      paste(
        sep = '/',
        str_c(str_split(getwd(), pattern = '/', simplify = T)[,1:(len-1)],
              collapse = '/'),
        'RPackages.R'
      ),
    encoding = 'UTF-8'
  )
  
  db_hospital <- 
    odbcConnect("DB_Hospital",
                uid = 'jerry',
                pwd = 'jerry@gracelab')
  
  ckd_fracture_Grace <- 
    odbcConnect("ckd_fracture_Grace",
                uid = 'jerry',
                pwd = 'jerry@gracelab')
  
  ckd_epo_jerry <- 
    odbcConnect(
      "ckd_epo_jerry",
      uid = 'jerry',
      pwd = 'jerry@gracelab'
    )


# functions ---------------------------------------------------------------
  
  
  write_to_db <- 
    function(data, tablename){
      sqlSave(
        ckd_epo_jerry,
        data,
        tablename = paste0('dbo.', tablename),
        rownames = F,
        append = T
      )
    }
  