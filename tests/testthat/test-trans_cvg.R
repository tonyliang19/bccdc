test_that("`group_name` is a string and `df` is a data frame",{
  vch_df0 = data.frame(
    antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men",
                "hpv", "measle","td", "poilo"),
    HSDA = c("Vancouver", "Richmond", "VCH", "Richmond", "Vancouver",
             "VCH", "VCH", "Vancouver", "Richmond"))
  expect_error(trans_cvg(7, df = vch_df0))
  expect_error(trans_cvg("7yr", df = "vch_df0"))
})

test_that("`trans_cvg` returns a list in length of 9", {

  vch_df0 = data.frame(
    antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men",
                "hpv", "measle","td", "poilo"),
    HSDA = c("Vancouver", "Richmond", "VCH", "Richmond", "Vancouver",
             "VCH", "VCH", "Vancouver", "Richmond"))

  expect_identical(trans_cvg("7yr", df = vch_df0)[[1]],
                   data.frame(antigen = c("hpv_F","hpv","measle"),
                              HSDA = c("VCH","VCH","VCH")))
  expect_identical(trans_cvg("7yr", df = vch_df0)[[2]],
                   data.frame(antigen = c("dtap","dtap_p","hpv_M","men","poilo"),
                              HSDA = c("Vancouver","Richmond","Richmond","Vancouver","Richmond")))
  expect_equal(trans_cvg("7yr", df = vch_df0)[[3]], "measles")
  expect_equal(trans_cvg("7yr", df = vch_df0)[[4]], 5)
  expect_equal(trans_cvg("7yr", df = vch_df0)[[5]], "measles - 1 Dose Only")
  expect_equal(trans_cvg("7yr", df = vch_df0)[[6]], NA)
  expect_equal(trans_cvg("7yr", df = vch_df0)[[7]], NA)
  expect_identical(trans_cvg("7yr", df = vch_df0)[[8]],
                   data.frame(antigen = c("hpv_F","hpv","measle"),
                              HSDA = c("VCH","VCH","VCH")))
  expect_identical(trans_cvg("7yr", df = vch_df0)[[9]],
                   data.frame(antigen = c("dtap","hpv_M","men","poilo"),
                              HSDA = c("Vancouver","Richmond","Vancouver","Richmond")))
})
