test_that("`atg_ex` should return a tibble", {
  expect_error(atg_ex("test",
                      TRUE,
                      c("dtap", "dtap_p", "hpv_F", "hpv_M"),
                      "7yr"))
})

df_y_test <- data.frame(
  antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv",
              "measle", "td", "poilo"),
  HSDA = c("Vancouver", "Richmond", "VCH", "Richmond", "Vancouver", "VCH",
           "VCH", "Vancouver", "Richmond"))

test_that("`atg_ex` should return a tibble", {
  expect_equal(nrow(atg_ex(df_y_test,
                           TRUE,
                           c("dtap", "dtap_p", "hpv_M", "men","td", "poilo"),
                           "7yr")),
               3)
  expect_equal(ncol(atg_ex(df_y_test,
                           TRUE,
                           c("dtap", "dtap_p", "hpv_M", "men","td", "poilo"),
                           "7yr")),
               2)
})


df_g_test <- data.frame(
  grade = c("06","06","07","08", "09", "10",
            "11", "12", "07"),
  antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv",
              "measle", "td", "poilo"),
  HSDA = c("Vancouver", "Richmond", "VCH", "Richmond", "Vancouver", "VCH",
           "VCH", "Vancouver", "Richmond"))
test_that("`atg_ex` should return a tibble", {
  expect_identical(atg_ex(df_g_test,
                          FALSE,
                          c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv",
                            "measle", "td"),
                          "7gr",
                          "07"),
                   data.frame(antigen = c("poilo"), HSDA = "Richmond"))
})
