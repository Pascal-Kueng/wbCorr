
data("simdat_intensive_longitudinal")


# spearman
cors <- wbCorr(simdat_intensive_longitudinal,
               cluster = 'participantID',
               method = 'spearman',
)

get_table(cors)
saveRDS(get_table(cors), file = 'tests/testthat/testdata/exp_tables_spearman.rds')


summary(cors)

saveRDS(summary(cors), file = 'tests/testthat/testdata/exp_matrices_spearman.rds')

# pearson

cors <- wbCorr(simdat_intensive_longitudinal,
               cluster = 'participantID'
)

get_table(cors)
saveRDS(get_table(cors), file = 'tests/testthat/testdata/exp_tables_pearson.rds')


summary(cors)

saveRDS(summary(cors), file = 'tests/testthat/testdata/exp_matrices_pearson.rds')
