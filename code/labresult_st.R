ptm <- proc.time()
  # opd_exper <- 
  #   map_dfr(
  #     c('s', 't'),
  #     function(x){
  #       sqlQuery(
  #         db_hospital,
  #         paste0(
  #           "select *
  #             from DB_Hospital.dbo.v_opd_exper_",
  #           x,
  #           " where CHR_NO in ( ",
  #           str_c(
  #             "'",
  #             chr_no_cohort[hospital == x, ]$chr_no,
  #             "'",
  #             collapse = ', '
  #           ),
  #           " ) ;"
  #         )
  #       ) %>% 
  #         mutate(hospital = x)
  #     }
  #   ) %>%
  #   setDT(key = c('FEE_NO', 'CODE_NO', 'ITEM_NO',
  #                 'CHR_NO', 'hospital', 'SEQ_NO'))
  # 
  # names(opd_exper) <- names(opd_exper) %>% tolower()
  
  labresult <- 
    map_dfr(
      c('s', 't'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select *
              from DB_Hospital.dbo.v_labresult_",
            x,
            " where CHR_NO in (",
            str_c(
              "'",
              chr_no_cohort[hospital == x, ]$chr_no,
              "'",
              collapse = ', '
            ),
            " );"
          )
        ) %>% 
          mutate(hospital = x,
                 KIND = as.character(KIND),
                 R_TIME = as.character(R_TIME),
                 C_OPER = as.character(C_OPER))
      }
    ) %>% 
    setDT(key = c('O_ITEM', 'R_ITEM', 'FEE_NO',
                  'CHR_NO', 'hospital', 'TUBE_NO'))
  
  names(labresult) <- names(labresult) %>% tolower()
  
  experiment <- 
    map_dfr(
      c('s', 't'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select distinct t2.chr_no,
                    t2.tube_no, t2.b_date, t2.b_time,
                    t2.r_item, t1.fee_code
              from (select code_no, item_no, seq_no, fee_code
                      from DB_Hospital.dbo.v_experiment_", x,
            ") as t1 inner join (select chr_no, tube_no, kind, o_item, 
            b_date, b_time, r_item from DB_Hospital.dbo.v_labresult_",
            x,
            " where chr_no in (",
            str_c(
              "'",
              chr_no_cohort[hospital == x, ]$chr_no,
              "'",
              collapse = ', '),
            ")",
            ") as t2 on t1.code_no = t2.kind and 
                 t1.item_no = t2.o_item ;"
          )
        ) %>% 
          mutate(
            hospital = x
          )
      }
    )

# 取得 r_item_name(檢驗名字) ----------------------------------------------------

  # exper_join_labresult <- 
  #   inner_join(
  #     opd_exper %>% 
  #       mutate(tube_no = as.character(tube_no)) %>% 
  #       select(fee_no, tube_no, chr_no, hospital) %>% 
  #       distinct(),
  #     labresult %>% 
  #       select(chr_no, hospital, fee_no,
  #              tube_no, r_item) %>% 
  #       distinct(),
  #     by = c('fee_no' = 'fee_no',
  #            'tube_no' = 'tube_no',
  #            'chr_no' = 'chr_no',
  #            'hospital' = 'hospital')
  #   )
  
  exp_item <- 
    map_dfr(
      c('s', 't'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select distinct r_item, r_item_name
                from DB_Hospital.dbo.v_exp_item_",
            x,
            "; "
          )
        ) %>% 
          mutate(hospital = x)
      }
    )

# 取得 fee_code(檢驗批價碼) ------------------------------------------------------
  
  experiment_join_exp_item <- 
    inner_join(
      experiment,
      exp_item,
      by = c('hospital' = 'hospital',
             'r_item' = 'r_item')
    ) %>% 
    setDT(key = c('chr_no', 'hospital', 
                  'fee_code', 'r_item_name'))

# 合併 fee_code 及 r_item_name -----------------------------------------------
# 
#   labresult_st <- 
#     inner_join(
#       opd_exper_join_experiment %>% 
#         mutate(tube_no = as.character(tube_no)) %>% 
#         distinct(fee_no, tube_no, chr_no, hospital,
#                  fee_code),
#       exper_join_labresult_join_exp_item %>% 
#         select(-r_item) %>% 
#         distinct()
#     )

# labresult 分類 ------------------------------------------------------------

print('tmp1')
  labresult_st <- 
    bind_rows(
      experiment_join_exp_item[fee_code %chin% c('F090380', 'F09038C') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = '体液') &
                                 !grepl(r_item_name, pattern = '其他'),] %>% 
        mutate(exam_item = 'alb'),
      experiment_join_exp_item[fee_code %chin% c('F090020', 'F09002B', 'F09002B') &
                                 grepl(toupper(r_item_name), pattern = 'BUN') &
                                 !grepl(r_item_name, pattern = '後'),] %>% 
        mutate(exam_item = 'bun'),
      experiment_join_exp_item[fee_code %chin% c('F090060', 'F09006B') &
                                 grepl(r_item_name, pattern = 'HbA1c') ,] %>% 
        mutate(exam_item = 'HbA1'),
      experiment_join_exp_item[fee_code %chin% c('F09040C') &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = '其它檢體') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = '24') &
                                 !grepl(r_item_name, pattern = '檢體類別'),] %>% 
        mutate(exam_item = 'tp'),
      experiment_join_exp_item[fee_code %chin% c('F090230', 'F09023C') &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = '其它檢體') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = 'Capillary'),] %>% 
        mutate(exam_item = 'cl'),
      experiment_join_exp_item[fee_code %chin% c('F090270', 'F09027B') ,] %>% 
        mutate(exam_item = 'alp'),
      experiment_join_exp_item[fee_code %chin% c('F090290', 'F09029C') &
                                 !grepl(r_item_name, pattern = 'Micro') ,] %>% 
        mutate(exam_item = 'tbil'),
      experiment_join_exp_item[fee_code %chin% c('F090010', 'F09001C') &
                                 !grepl(r_item_name, pattern = '膽固醇'),] %>% 
        mutate(exam_item = 'chol'),
      experiment_join_exp_item[fee_code %chin% c('F090040', 'F09004C') &
                                 !grepl(r_item_name, pattern = 'dialysate') &
                                 !grepl(r_item_name, pattern = 'other') ,] %>% 
        mutate(exam_item = 'trig'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 (grepl(r_item_name, pattern = 'normoblast') |
                                    grepl(r_item_name, pattern = '芽')),] %>% 
        mutate(exam_item = 'nrbc'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 (grepl(r_item_name, pattern = 'Baso') |
                                    grepl(r_item_name, pattern = '嗜鹼')),] %>% 
        mutate(exam_item = 'basophil'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 (grepl(r_item_name, pattern = 'Mono') |
                                    grepl(r_item_name, pattern = '單核球')),] %>% 
        mutate(exam_item = 'monocyte'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 grepl(r_item_name, pattern = 'Neut'),] %>% 
        mutate(exam_item = 'neutrophil'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 (grepl(r_item_name, pattern = 'Lymp') |
                                    grepl(r_item_name, pattern = '淋巴球')) &
                                 !grepl(r_item_name, pattern = 'Atypical') &
                                 !grepl(r_item_name, pattern = 'ATYPLYMPH'),] %>% 
        mutate(exam_item = 'lymphocyte'),
      experiment_join_exp_item[fee_code %chin% c('F08013C') &
                                 (grepl(r_item_name, pattern = 'Eosi') |
                                    grepl(r_item_name, pattern = '嗜酸')),] %>% 
        mutate(exam_item = 'esoinophil'),
      experiment_join_exp_item[fee_code %chin% c('F090110', 'F09011B') &
                                 (grepl(r_item_name, pattern = 'ca') |
                                    grepl(r_item_name, pattern = '鈣')) &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = '其它檢體') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = 'random'),] %>% 
        mutate(exam_item = 'ca'),
      experiment_join_exp_item[fee_code %chin% c('F090150', 'F09015B') &
                                 !grepl(r_item_name, pattern = '後') &
                                 !grepl(r_item_name, pattern = 'eGFR'),] %>% 
        mutate(exam_item = 'creatinine'),
      experiment_join_exp_item[fee_code %chin% c('F080060', 'F08006C', 'F080110', 'F08011C') &
                                 grepl(r_item_name, pattern = 'pdw'),] %>% 
        mutate(exam_item = 'pdw'),
      experiment_join_exp_item[fee_code %chin% c('F080010', 'F08001C', 'F080110', 'F08011C', 
                                                 'F080120', 'F08012C', 'F080140', 'F08014C',
                                                 'F080820', 'F08082C') &
                                 grepl(r_item_name, pattern = 'rdw') &
                                 !grepl(r_item_name, pattern = 'sd'),] %>% 
        mutate(exam_item = 'r_cv'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080110', 'F08011C',
                                                 'F080120', 'F08012C', 'F08083C') &
                                 (grepl(r_item_name, pattern = 'mch') |
                                    grepl(r_item_name, pattern = '血色素')) &
                                 !grepl(r_item_name, pattern = 'MCHC') &
                                 !grepl(r_item_name, pattern = 'hgb'),] %>% 
        mutate(exam_item = 'mch'),
      experiment_join_exp_item[fee_code %chin% c('F090050', 'F09005C', 'F09005Z') &
                                 (grepl(r_item_name, pattern = 'glu') |
                                    grepl(r_item_name, pattern = 'ac')) &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = 'dialysate') &
                                 !grepl(r_item_name, pattern = '洗腎') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = 'random') &
                                 !grepl(r_item_name, pattern = 'hr') &
                                 !grepl(r_item_name, pattern = '後') &
                                 !grepl(r_item_name, pattern = 'pc'),] %>% 
        mutate(exam_item = 'glu_ac'),
      experiment_join_exp_item[fee_code %chin% c('F090210', 'F09021C') &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = 'dialysate') &
                                 !grepl(r_item_name, pattern = '洗腎') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = 'random') &
                                 !grepl(r_item_name, pattern = 'hr') &
                                 !grepl(r_item_name, pattern = '後') &
                                 !grepl(r_item_name, pattern = 'capillary'),] %>% 
        mutate(exam_item = 'na'),
      experiment_join_exp_item[fee_code %chin% c('F090220', 'F09022C') &
                                 !grepl(r_item_name, pattern = 'CSF') &
                                 !grepl(r_item_name, pattern = 'Pleural') &
                                 !grepl(r_item_name, pattern = '尿') &
                                 !grepl(r_item_name, pattern = 'Urine') &
                                 !grepl(r_item_name, pattern = 'Ascites') &
                                 !grepl(r_item_name, pattern = 'dialysate') &
                                 !grepl(r_item_name, pattern = '洗腎') &
                                 !grepl(r_item_name, pattern = 'other') &
                                 !grepl(r_item_name, pattern = 'random') &
                                 !grepl(r_item_name, pattern = 'hr') &
                                 !grepl(r_item_name, pattern = '後') &
                                 !grepl(r_item_name, pattern = 'capillary'),] %>% 
        mutate(exam_item = 'k'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080110', 'F08011C',
                                                 'F080120', 'F08012C', 'F08084C') &
                                 (grepl(r_item_name, pattern = 'mch') |
                                    grepl(r_item_name, pattern = '血色素')) &
                                 !grepl(r_item_name, pattern = 'hgb'),] %>% 
        mutate(exam_item = 'mchc'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080110', 'F08011C', 'F080120',
                                                 'F08012C', 'F080820', 'F08082C', 'F08127C') &
                                 grepl(r_item_name, pattern = 'mcv')] %>% 
        mutate(exam_item = 'mcv'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080010', 'F08001C', 'F080110',
                                                 'F08011C', 'F080120', 'F08012C', 'F080140',
                                                 'F08014C', 'F080820', 'F08082C') &
                                 grepl(r_item_name, pattern = 'rbc'),] %>% 
        mutate(exam_item = 'rbc'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080060', 'F08006C', 'F080110', 
                                                 'F08011C') &
                                 grepl(r_item_name, pattern = 'plt'),] %>% 
        mutate(exam_item = 'plt'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080020', 'F08002C', 'F080110', 
                                                 'F08011C', 'F080120', 'F08012C', 'F080820', 
                                                 'F08082C') &
                                 grepl(r_item_name, pattern = 'wbc'),] %>% 
        mutate(exam_item = 'wbc'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080040', 'F08004C', 'F08004Y', 
                                                 'F08011C', 'F08012C', 'F08012C', 'F080820', 
                                                 'F08082C') &
                                 (grepl(r_item_name, pattern = 'hct') |
                                    grepl(r_item_name, pattern = '血比容')),] %>% 
        mutate(exam_item = 'hct'),
      experiment_join_exp_item[fee_code %chin% c('F121160', 'F12116A') &
                                 grepl(r_item_name, pattern = 'ferritin'),] %>% 
        mutate(exam_item = 'ferritin'),
      experiment_join_exp_item[fee_code %chin% c('F08001C', 'F080030', 'F08003C', 'F080110', 
                                                 'F08011C', 'F080120', 'F08012C', 'F080820', 
                                                 'F08082C') &
                                 (grepl(r_item_name, pattern = 'hg') |
                                    grepl(r_item_name, pattern = '血色素')),] %>% 
        mutate(exam_item = 'hgb')
    ) %>% 
    setDT(key = c('chr_no', 'hospital', 'exam_item'))
  
  labresult_st <- 
    experiment_join_exp_item %>% 
    mutate(
      exam_item =
        case_when(
          fee_code %chin% c('F090380', 'F09038C') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = '体液') &
            !grepl(r_item_name, pattern = '其他') ~ 'alb',
          fee_code %chin% c('F090020', 'F09002B', 'F09002B') &
            grepl(toupper(r_item_name), pattern = 'BUN') &
            !grepl(r_item_name, pattern = '後') ~ 'bun',
          fee_code %chin% c('F090060', 'F09006B') &
            grepl(r_item_name, pattern = 'HbA1c') ~ 'HbA1',
          fee_code %chin% c('F09040C') &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = '其它檢體') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = '24') &
            !grepl(r_item_name, pattern = '檢體類別') ~ 'tp',
          fee_code %chin% c('F090230', 'F09023C') &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = '其它檢體') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = 'Capillary') ~ 'cl',
          fee_code %chin% c('F090270', 'F09027B') ~ 'alp',
          fee_code %chin% c('F090290', 'F09029C') &
            !grepl(r_item_name, pattern = 'Micro') ~ 'tbil',
          fee_code %chin% c('F090010', 'F09001C') &
            !grepl(r_item_name, pattern = '膽固醇') ~ 'chol',
          fee_code %chin% c('F090040', 'F09004C') &
            !grepl(r_item_name, pattern = 'dialysate') &
            !grepl(r_item_name, pattern = 'other') ~ 'trig',
          fee_code %chin% c('F08013C') &
            (grepl(r_item_name, pattern = 'normoblast') |
            grepl(r_item_name, pattern = '芽')) ~ 'nrbc',
          fee_code %chin% c('F08013C') &
            (grepl(r_item_name, pattern = 'Baso') |
            grepl(r_item_name, pattern = '嗜鹼')) ~ 'basophil',
          fee_code %chin% c('F08013C') &
            (grepl(r_item_name, pattern = 'Mono') |
            grepl(r_item_name, pattern = '單核球'))~ 'monocyte',
          fee_code %chin% c('F08013C') &
            grepl(r_item_name, pattern = 'Neut') ~ 'neutrophil',
          fee_code %chin% c('F08013C') &
            (grepl(r_item_name, pattern = 'Lymp') |
            grepl(r_item_name, pattern = '淋巴球')) &
            !grepl(r_item_name, pattern = 'Atypical') &
            !grepl(r_item_name, pattern = 'ATYPLYMPH') ~ 'lymphocyte',
          fee_code %chin% c('F08013C') &
            (grepl(r_item_name, pattern = 'Eosi') |
            grepl(r_item_name, pattern = '嗜酸')) ~ 'esoinophil',
          fee_code %chin% c('F090110', 'F09011B') &
            (grepl(r_item_name, pattern = 'ca') |
            grepl(r_item_name, pattern = '鈣')) &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = '其它檢體') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = 'random') ~ 'ca',
          fee_code %chin% c('F090050', 'F09005C', 'F09005Z') &
            (grepl(r_item_name, pattern = 'glu') |
            grepl(r_item_name, pattern = 'ac')) &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = 'dialysate') &
            !grepl(r_item_name, pattern = '洗腎') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = 'random') &
            !grepl(r_item_name, pattern = 'hr') &
            !grepl(r_item_name, pattern = '後') &
            !grepl(r_item_name, pattern = 'pc') ~ 'glu_ac',
          fee_code %chin% c('F090210', 'F09021C') &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = 'dialysate') &
            !grepl(r_item_name, pattern = '洗腎') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = 'random') &
            !grepl(r_item_name, pattern = 'hr') &
            !grepl(r_item_name, pattern = '後') &
            !grepl(r_item_name, pattern = 'capillary') ~ 'na',
          fee_code %chin% c('F090220', 'F09022C') &
            !grepl(r_item_name, pattern = 'CSF') &
            !grepl(r_item_name, pattern = 'Pleural') &
            !grepl(r_item_name, pattern = '尿') &
            !grepl(r_item_name, pattern = 'Urine') &
            !grepl(r_item_name, pattern = 'Ascites') &
            !grepl(r_item_name, pattern = 'dialysate') &
            !grepl(r_item_name, pattern = '洗腎') &
            !grepl(r_item_name, pattern = 'other') &
            !grepl(r_item_name, pattern = 'random') &
            !grepl(r_item_name, pattern = 'hr') &
            !grepl(r_item_name, pattern = '後') &
            !grepl(r_item_name, pattern = 'capillary') ~ 'k',
          fee_code %chin% c('F090150', 'F09015B') &
            !grepl(r_item_name, pattern = '後') &
            !grepl(r_item_name, pattern = 'eGFR') ~ 'creatinine',
          fee_code %chin% c('F080060', 'F08006C', 'F080110', 'F08011C') &
            grepl(r_item_name, pattern = 'pdw') ~ 'pdw',
          fee_code %chin% c('F080010', 'F08001C', 'F080110', 'F08011C', 
                            'F080120', 'F08012C', 'F080140', 'F08014C',
                            'F080820', 'F08082C') &
            grepl(r_item_name, pattern = 'rdw') &
            !grepl(r_item_name, pattern = 'sd') ~ 'r_cv',
          fee_code %chin% c('F08001C', 'F080110', 'F08011C',
                            'F080120', 'F08012C', 'F08083C') &
            (grepl(r_item_name, pattern = 'mch') |
            grepl(r_item_name, pattern = '血色素')) &
            !grepl(r_item_name, pattern = 'MCHC') &
            !grepl(r_item_name, pattern = 'hgb') ~ 'mch',
          fee_code %chin% c('F08001C', 'F080110', 'F08011C',
                            'F080120', 'F08012C', 'F08084C') &
            (grepl(r_item_name, pattern = 'mch') |
            grepl(r_item_name, pattern = '血色素')) &
            !grepl(r_item_name, pattern = 'hgb') ~ 'mchc',
          fee_code %chin% c('F08001C', 'F080110', 'F08011C', 'F080120',
                            'F08012C', 'F080820', 'F08082C', 'F08127C') &
            grepl(r_item_name, pattern = 'mcv') ~ 'mcv',
          fee_code %chin% c('F08001C', 'F080010', 'F08001C', 'F080110',
                            'F08011C', 'F080120', 'F08012C', 'F080140',
                            'F08014C', 'F080820', 'F08082C') &
            grepl(r_item_name, pattern = 'rbc') ~ 'rbc',
          fee_code %chin% c('F08001C', 'F080060', 'F08006C', 'F080110', 
                            'F08011C') &
            grepl(r_item_name, pattern = 'plt') ~ 'plt',
          fee_code %chin% c('F08001C', 'F080020', 'F08002C', 'F080110', 
                            'F08011C', 'F080120', 'F08012C', 'F080820', 
                            'F08082C') &
            grepl(r_item_name, pattern = 'wbc') ~ 'wbc',
          fee_code %chin% c('F08001C', 'F080040', 'F08004C', 'F08004Y', 
                            'F08011C', 'F08012C', 'F08012C', 'F080820', 
                            'F08082C') &
            (grepl(r_item_name, pattern = 'hct') |
            grepl(r_item_name, pattern = '血比容'))~ 'hct',
          fee_code %chin% c('F121160', 'F12116A') &
            grepl(r_item_name, pattern = 'ferritin') ~ 'ferritin',
          fee_code %chin% c('F08001C', 'F080030', 'F08003C', 'F080110', 
                            'F08011C', 'F080120', 'F08012C', 'F080820', 
                            'F08082C') &
            (grepl(r_item_name, pattern = 'hg') |
            grepl(r_item_name, pattern = '血色素')) ~ 'hgb',
          T ~ 'undefined'
        )
    ) %>% 
    setDT(key = c('chr_no', 'hospital', 'exam_item'))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  