
# 環境設定及連結資料庫 --------------------------------------------------------------
  source(
    'code/functions.R',
    encoding = 'UTF-8'
  )
  
  # odbcClose(db_hospital)
  
# 產生 long-formt cohort  ---------------------------------------------------
  source(
    'code/create_long_format_cohort.R',
    encoding = 'UTF-8'
  )

# 計算 cohort 總人數 --------------------------------------------------------------------
  
  count(distinct(result_longer[between(drug_date, 1040101, 1061231), ],
                 chr_no, hospital),
        hospital)

  count(distinct(oie_longer_cohort[between(drug_date, 1040101, 1061231), ],
                 chr_no, hospital),
        hospital)
    
  write_to_db(result_longer, 'result_longer')

# 建立 cohort set -----------------------------------------------------------
  
  chr_no_cohort <-
    distinct(result_longer[between(drug_date, 1040101, 1061231), ],
             chr_no, hospital)

  chr_no_cohort <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.chr_no_cohort;'
    ) %>% 
    setDT(key = c('hospital', 'chr_no'))
  

# 產生 opd, ipd, epd long-format cohort -------------------------------------
  source(
    'code/create_opd_ipd_epd_long_format_cohort.R',
    encoding = 'UTF-8'
  )
  
  # write_to_db(
  #   oie_longer_cohort,
  #   'oie_longer_cohort'
  # )
  
# 第一次發生 CKD ----------------------------------------------------------
  ckd_first_date <- 
    oie_longer_cohort[(icd_type == 'icd9_code' &
                         grepl(icd_code, pattern = '58[56].*')) |
                        (icd_type == 'icd10_code' &
                           grepl(icd_code, pattern = '(^N18.[4569]$)|(^N19$)')), ] %>% 
    group_by(chr_no, hospital) %>% 
    summarise(
      ckd_start_date = min(drug_date)
    ) %>% 
    setDT(key = c('hospital', 'chr_no'))
  
  # 第一次發生 CKD 後發生骨折的時間 -----------------------------------------------------------
    source(
      'code/fracture_after_first_ckd.R',
      encoding = 'UTF-8'
    )      

  # 抓取洗腎病人 ------------------------------------------------------------------
    source(
      'code/dialysis_after_first_ckd.R',
      encoding = 'UTF-8'
    )
  
  ptm <- proc.time()  
  oie_longer_cohort <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.oie_longer_cohort;'
    ) %>% 
    setDT(key = c('chr_no', 'hospital', 'icd_code', 'drug_date'))
  proc.time() - ptm
  # med_basic ---------------------------------------------------------------
    source(
      'code/med_basic.R',
      encoding = 'UTF-8'
    )
  
  write_to_db(
    opd_med_atc,
    'opd_med_atc'
  )
  write_to_db(
    ipd_med_atc,
    'ipd_med_atc'
  )

# 按人抓取第一次及最後一次領藥時間 --------------------------------------------------------
  
  trans_date <- 
      function(var_date){
        var_date <- 
          as.character(var_date + 19110000)
        
        result_date <- 
          fastPOSIXct(
            paste(sep = '/',
                  str_sub(var_date, 1, 4),
                  str_sub(var_date, 5, 6),
                  str_sub(var_date, 7, 8))
          ) %>% 
          as.Date()
        
        return(result_date)
      }
    
  find_the_min_max_drug_time_opd <- 
    opd_med_atc[str_length(ATC_CODE) > 0, ] %>% 
      mutate(
        OPD_DATE = trans_date(OPD_DATE)
      ) %>% 
      group_by(data_source, CHR_NO, ATC_CODE) %>% 
      summarise(
        first_drug_date = 
          min(OPD_DATE),
        last_drug_date = 
          max(OPD_DATE),
        drug_freq = 
          sum(!is.na(FEE_NO))
      ) %>% 
      arrange(desc(drug_freq))

  find_the_min_max_drug_time_ipd <- 
    ipd_med_atc[str_length(ATC_CODE) > 0, ] %>% 
    mutate(
      BEGIN_DATE = trans_date(BEGIN_DATE),
      DC_DATE = trans_date(DC_DATE)
    ) %>% 
    group_by(CHR_NO, data_source, ATC_CODE) %>% 
    summarise(
      first_drug_date = 
        min(BEGIN_DATE),
      last_drug_date = 
        max(DC_DATE),
      drug_freq = 
        sum(!is.na(FEE_NO))
    ) %>% 
    arrange(desc(drug_freq))

# 計算門急診領藥次數 ---------------------------------------------------------------
  opd_drug_freq <- 
    find_the_min_max_drug_time_opd %>% 
    group_by(.drop=T) %>% 
    mutate(
      atc_5 = str_sub(ATC_CODE, 1, 5)
    ) %>% 
    distinct(data_source, CHR_NO, atc_5) %>% 
    count(atc_5) %>% 
    arrange(desc(n))
  
  ipd_drug_freq <- 
    find_the_min_max_drug_time_ipd %>% 
    group_by(.drop=T) %>% 
    mutate(
      atc_5 = str_sub(ATC_CODE, 1, 5)
    ) %>% 
    distinct(data_source, CHR_NO, atc_5) %>% 
    count(atc_5) %>% 
    arrange(desc(n))
  
  drug_freq <- 
    rbind(
      find_the_min_max_drug_time_opd %>% 
        group_by(.drop=T) %>% 
        mutate(
          atc_5 = str_sub(ATC_CODE, 1, 5)
        ) %>% 
        distinct(data_source, CHR_NO, atc_5),
      find_the_min_max_drug_time_ipd %>% 
        group_by(.drop=T) %>% 
        mutate(
          atc_5 = str_sub(ATC_CODE, 1, 5)
        ) %>% 
        distinct(data_source, CHR_NO, atc_5)
    ) %>% 
    distinct() %>% 
    count(atc_5) %>% 
    mutate(
      percent_n = n/14527
    ) %>% 
    arrange(desc(n))
  View(drug_freq)

# 檢驗檢查 --------------------------------------------------------------------
  # 萬芳 exper_sign -----------------------------------------------------------
ptm <- proc.time()   
    exper_sign <- 
      sqlQuery(
        db_hospital,
        paste0(
          "select *
            from DB_Hospital.dbo.v_exper_sign_w
            where CHR_NO in ( ",
          str_c(
            "'",
            chr_no_cohort[hospital == 'w', ]$chr_no,
            "'",
            collapse = ', '
          ),
          ") ;"
        )
      )
  

  # 附醫與雙和 -------------------------------------------------------------------

  opd_exper <- 
    map_dfr(
      c('s', 't'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select *
              from DB_Hospital.dbo.v_opd_exper_",
            x,
            " where CHR_NO in ( ",
            str_c(
              "'",
              chr_no_cohort[hospital == x, ]$chr_no,
              "'",
              collapse = ', '
            ),
            " ) ;"
          )
        ) %>% 
          mutate(data_source = x)
      }
    )
proc.time() - ptm 

      
  

