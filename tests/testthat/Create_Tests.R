
data("simdat_intensive_longitudinal")
try(dat <- readRDS("C:\\Users\\pascku\\OneDrive\\scripts\\01 R-Packages\\within-between-correlations\\test_data_real_factors.rds"))
try(dat <- readRDS("C:\\Users\\kueng\\OneDrive\\scripts\\01 R-Packages\\within-between-correlations\\test_data_real_factors.rds"))

create_files <- function(data, cluster, type) {
  # spearman
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'spearman',
  )

  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_spearman_',type,'.rds'))
  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_spearman_',type,'.rds'))

  # pearson

  cors <- wbCorr(data,
                 cluster = cluster
  )

  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_pearson_',type,'.rds'))
  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_pearson_',type,'.rds'))

  # Jackknife
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'spearman-jackknife',
  )


  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_jackknife_',type,'.rds'))
  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_jackknife_',type,'.rds'))

  # boot spearman
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'spearman',
                 boot = TRUE
  )

  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_spearman_boot_',type,'.rds'))

  # boot auto
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'auto',
                 boot = TRUE
  )

  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_auto_boot_',type,'.rds'))

  # boot auto with 99% CI
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'auto',
                 boot = TRUE,
                 confidence_level = 0.99
  )

  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_auto99_boot_',type,'.rds'))

}

create_files(simdat_intensive_longitudinal,
             'participantID',
             'simdat')
create_files(dat,
             'CoupleID',
             'real')

print("done")
