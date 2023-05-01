
data("simdat_intensive_longitudinal")
try(dat <- readRDS("C:\\Users\\pascku\\OneDrive\\scripts\\01 R-Packages\\within-between-correlations\\test_data_real_factors.rds"))

create_files <- function(data, cluster, type) {
  # spearman
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'spearman',
  )

  get_table(cors)
  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_spearman_',type,'.rds'))


  summary(cors)

  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_spearman_',type,'.rds'))

  # pearson

  cors <- wbCorr(data,
                 cluster = cluster
  )

  get_table(cors)
  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_pearson_',type,'.rds'))


  summary(cors)

  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_pearson_',type,'.rds'))

  # Jackknife
  cors <- wbCorr(data,
                 cluster = cluster,
                 method = 'spearman-jackknife',
  )

  get_table(cors)
  saveRDS(get_table(cors), file = paste0('tests/testthat/testdata/exp_tables_jackknife_',type,'.rds'))


  summary(cors)
  saveRDS(summary(cors), file = paste0('tests/testthat/testdata/exp_matrices_jackknife_',type,'.rds'))

}

create_files(simdat_intensive_longitudinal,
             'participantID',
             'simdat')
create_files(dat,
             'CoupleID',
             'real')

print("done")
