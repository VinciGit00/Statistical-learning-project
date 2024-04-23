- Seed fixed for every script (set.seed(22))
- The dataset should be standardized (df <- df %>% mutate_all(~(scale(.) %>% as.vector)))
- Validation 70-30
- Performance parameter: MSE full/significative and MAE
- Residuals should be normal (kolgomorov-smirnov) with 0 mean
- At the end of each process there should be present the parameter significance test
- Not significative parameters should be removed

