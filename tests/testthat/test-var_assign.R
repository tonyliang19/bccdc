cnames0 <- c("antigen", "HSDA", "students", "refusal_all.n", "refusal_all.percent", "no_imm_record.n", "no_imm_record.percent",
             "utd_all.n", "utd_all.percent", "utd.n", "utd.percent", "immunity.n", "immunity.percent", "partial_contra.n", "partial_contra.percent",
             "partial_refusal.n", "partial_refusal.percent", "partial_other.n", "partial_other.percent", "unimm_contra.n", "unimm_contra.percent",
             "unimm_refusal.n", "unimm_refusal.percent", "unimm_other.n", "unimm_other.percent") # n = 26

Disease0 = c("1UptoDate", #1
             "D_T_aP",    #2
             "D_T_aP_IPV",#3
             "HepB",      #4
             "HPV",       #5
             "HPV Series Initiation but not Completion",#6
             "HPV-Female",#7
             "HPV-Male",  #8
             "Measles",   #9
             "Measles - 1 Dose Only",#10
             "Meningo",   #11
             "Mumps",     #12
             "Pertussis", #13
             "Polio",     #14
             "Rubella",   #15
             "Td",        #16
             "Varicella", #17
             "xNo Information",#18
             "yRefusalstoAll&NoImms") # n = 19

test_that("`var_assign` returns a list of variabels in the length of 4", {
  expect_error(var_assign(0, cnames0, Disease0))
})

test_that("`var_assign` returns a list of variabels in the length of 4", {
  expect_error(var_assign("18gr", cnames0, Disease0))
})

test_that("`var_assign` returns a list of variabels in the length of 4", {
  expect_equal(length(var_assign("7yr", cnames0, Disease0)),
               4)
})

test_that("`var_assign` returns a list of variabels in the length of 4", {
  expect_equal(length(var_assign("7yr", cnames0, Disease0)),
               4)
  expect_equal(var_assign("7yr", cnames0, Disease0)[[1]],
               "1. 7y_BCCDC" )
  expect_equal(var_assign("7yr", cnames0, Disease0)[[2]],
               cnames0)
  expect_equal(var_assign("7yr", cnames0, Disease0)[[3]],
               Disease0[-c(5,6,7,8,13, 16)])
  expect_equal(var_assign("7yr", cnames0, Disease0)[[4]],
               Disease0[-c(1,3,5,6,7,8,10,13,16,18,19)])
})
