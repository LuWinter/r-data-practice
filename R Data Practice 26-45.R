library(data.table)
library(tidyverse)
library(microbenchmark)
stock_data <- readRDS("data/stock-market-data.rds")
head(stock_data)

# 26. 每个行业每天成交额最大的5只股票和成交额总和是多少？ ------------------------------------------

stock_data %>%
  arrange(desc(amount)) %>%
  group_by(industry, date) %>%
  summarise(Top5 = paste(symbol[1:5], collapse = " "),
            amount = sum(amount[1:5]))
data[order(-amount), .(symbol = symbol[1:5], amount = sum(amount[1:5])),
     keyby = .(date, industry)
     ][1:5]

# 27. 每个行业每天成交额超过该行业中股票成交额80%分位数的股票的平均收益率是多少？ -----------------------------

stock_data %>%
  mutate(ret = close/pre_close - 1) %>%
  group_by(date, industry) %>%
  summarise(aver_ret = mean(ret[amount > quantile(amount, 0.8)]))
stock_data[, .(symbol = symbol, ret = close/pre_close - 1, amount = amount, industry, date)
     ][, .SD[amount > quantile(amount, 0.8)], keyby = .(date, industry)
     ][, .(aver_ret = mean(ret)), keyby = .(date, industry)
     ][1:5]

# 28. 每天成交额最大的10%的股票的平均收益率和成交额最小的10%的股票的平均收益率的相关系数是多少？ --------------------

fun_dplyr <- function(){
  df28 <- stock_data %>%
    mutate(ret = close/pre_close - 1) %>%
    group_by(date) %>%
    summarise(max_10 = mean(ret[amount > quantile(amount, 0.9)]),
              min_10 = mean(ret[amount < quantile(amount, 0.1)]))
  cor(df28$min_10, df28$max_10, use = "complete.obs")
}
microbenchmark(fun_dplyr(), times = 10)

microbenchmark(
stock_data[, .(symbol = symbol, ret = close/pre_close - 1, amount = amount, date)
     ][, .(ret_aver = mean(ret)), keyby = .(date, tag = ifelse(amount > quantile(amount, 0.9), "max10%", ifelse(amount < quantile(amount, 0.1), "min10%", "others")))
     ][, .SD[-3], keyby = .(date)
     ][1:5], times = 10)

# 29. 每天哪些行业的平均成交额高于全市场平均成交额？ ---------------------------------------------

microbenchmark(
stock_data[, .(amount_mkt_aver = mean(amount), industry = industry, amount = amount), keyby = .(date)
     ][, .(amount_ind_aver = mean(amount), amount_mkt_aver), keyby = .(date, industry)
     ][amount_ind_aver > amount_mkt_aver, unique(.SD[, 1:2])
     ][1:15], times = 10)

microbenchmark(
stock_data %>%
  group_by(date) %>%
  mutate(amount_mkt_aver = mean(amount)) %>%
  group_by(date, industry) %>%
  summarise(
    amount_ind_aver = mean(amount),
    amount_mkt_aver = mean(amount_mkt_aver)
    ) %>%
  filter(amount_ind_aver > amount_mkt_aver), times = 10)

# 30. 每天每个股票对市场的超额收益率是多少? -------------------------------------------------

microbenchmark(
stock_data %>%
  mutate(stkcd_ret = close/pre_close - 1) %>%
  group_by(date) %>%
  mutate(weight = capt / sum(capt),
         mkt_ret = sum(weight*stkcd_ret)) %>%
  group_by(symbol) %>%
  mutate(alpha = coef(lm(stkcd_ret ~ mkt_ret))[1],
         beta = coef(lm(stkcd_ret ~ mkt_ret))[2]) %>%
  group_by(date, symbol) %>%
  mutate(abnr_ret = stkcd_ret - alpha - beta * mkt_ret), times = 5)

microbenchmark(
stock_data[, .(stkcd_ret = close/pre_close - 1, weight = capt/sum(capt), symbol), keyby = date
  ][, .(mkt_ret = sum(weight*stkcd_ret), stkcd_ret, symbol, weight), keyby = date
  ][, .(alpha = coef(lm(stkcd_ret ~ mkt_ret))[1], beta = coef(lm(stkcd_ret ~ mkt_ret))[2], mkt_ret, stkcd_ret, date), by = symbol
  ][, .(abnr_ret = stkcd_ret - alpha - beta * mkt_ret, alpha, beta, symbol, stkcd_ret, mkt_ret), keyby = .(date, symbol)
  ][1:5], times = 5)


# 31. 每天每个股票对市场去除自身的超额收益率是多少? ---------------------------------------------

stock_data[, .(stkcd_ret = close/pre_close - 1, symbol, mkt_capt_outself =  sum(capt) - capt, capt), keyby = date
  ][, .(mkt_ret_outself = {
    a <- vector()
    for (i in 1:.N) {
      a[i] <- sum((capt[-i]/mkt_capt_outself[i]) * stkcd_ret[-i])
    }
    a
  }, stkcd_ret, symbol),
  keyby = date
  ][, .(alpha = coef(lm(stkcd_ret ~ mkt_ret_outself))[1], beta = coef(lm(stkcd_ret ~ mkt_ret_outself))[2], mkt_ret_outself, stkcd_ret, date), by = symbol
  ][, .(abnr_ret = stkcd_ret - alpha - beta * mkt_ret_outself), keyby = .(date, symbol)
  ][1:5]

microbenchmark(
stock_data %>%
  mutate(stkcd_ret = close / pre_close - 1) %>%
  group_by(date) %>%
  mutate(mkt_capt_outself = sum(capt) - capt) %>%
  mutate(mkt_ret_outself = {
           a <- vector()
           for (i in 1:length(mkt_capt_outself)) {a[i] <- sum((capt[-i]/mkt_capt_outself[i]) * stkcd_ret[-i])}
           a}) # 以下步骤如上题，不列
, times = 10) # mean = 8.30sec

microbenchmark(
  stock_data[, .(stkcd_ret = close/pre_close - 1, symbol, mkt_capt_outself =  sum(capt) - capt, capt), keyby = date
  ][, .(mkt_ret_outself = {
    a <- vector()
    for (i in 1:.N) {
      a[i] <- sum((capt[-i]/mkt_capt_outself[i]) * stkcd_ret[-i])
    }
    a
  }, stkcd_ret, symbol),
  keyby = date
  ], times = 10
) # mean = 8.09sec



# 32. 每天每个股票对行业的超额收益率是多少? -------------------------------------------------

# 同第30题



# 33. 每天每个股票对行业去除自身的超额收益率是多少？ ---------------------------------------------

# 同第31题


# 34. 每个股票每天对市场的超额收益率与对行业的超额收益率的相关系数如何？ -----------------------------------

mkt <- stock_data[, .(stkcd_ret = close/pre_close - 1, weight = capt/sum(capt), symbol), keyby = date
  ][, .(mkt_ret = sum(weight/100*stkcd_ret), stkcd_ret, symbol), keyby = date
  ][, .(alpha = coef(lm(stkcd_ret ~ mkt_ret))[1], beta = coef(lm(stkcd_ret ~ mkt_ret))[2], mkt_ret, stkcd_ret, symbol, date)
  ][, .(abnr_mkt_ret = stkcd_ret - alpha - beta * mkt_ret), keyby = .(date, symbol)]
ind <- stock_data[, .(stkcd_ret = close/pre_close - 1, symbol, weight = capt/sum(capt)), keyby = .(industry,date)
  ][, .(ind_ret = sum(weight/100*stkcd_ret), stkcd_ret, symbol), keyby = .(industry, date)
  ][, .(alpha = coef(lm(stkcd_ret ~ ind_ret))[1], beta = coef(lm(stkcd_ret ~ ind_ret))[2], ind_ret, stkcd_ret, symbol, date), keyby = .(industry)
  ][, .(abnr_ind_ret = stkcd_ret - alpha - beta * ind_ret),
    keyby = .(date, symbol)]

mkt[ind, on = .(date, symbol)
  ][, cor(abnr_mkt_ret, abnr_ind_ret)]
rm(mkt, ind)


# 35. 每天有哪些行业的平均收益率超过市场平均收益率？ ---------------------------------------------

stock_data %>%
  mutate(stkcd_ret = close/pre_close - 1) %>%
  group_by(date, industry) %>%
  mutate(ind_weight = capt/sum(capt),
         ind_ret = sum(ind_weight * stkcd_ret)) %>%
  group_by(date) %>%
  mutate(mkt_weight = capt/sum(capt),
         mkt_ret = sum(mkt_weight * stkcd_ret)) %>%
  group_by(date, industry) %>%
  filter(ind_ret > mkt_ret) %>%
  count()

stock_data[, .(stkcd_ret = close/pre_close - 1, symbol, ind_weight = capt/sum(capt), capt), keyby = .(industry,date)
  ][, .(ind_ret = sum(ind_weight*stkcd_ret), stkcd_ret, symbol, capt), keyby = .(industry, date)
  ][, .(ind_ret, mkt_weight = capt/sum(capt), stkcd_ret, industry), keyby = date
  ][, .(ind_ret, mkt_ret = sum(mkt_weight*stkcd_ret), industry), keyby = date
  ][ind_ret > mkt_ret, unique(.SD)
  ]


# 36. 每天每个行业对市场的超额收益率是多少？ -------------------------------------------------

# 同上



# 37. 每天每个行业对去除本行业后的市场超额收益是多少？ --------------------------------------------

# 同上



# 38. 每天分别有多少股票是最近连续3个交易日上涨、下跌的？ ------------------------------------------

stock_data[, .(stkcd_ret = close/pre_close - 1), keyby = .(symbol, date)
  ][, {
    l <- list()
    b1 <- stkcd_ret > 0
    b2 <- stkcd_ret < 0
    for (t in 1:.N) {
      l[[t+3]] <- list(r3day_up = mean(b1[t:(t+2)]), r3day_dn = mean(b2[t:(t+2)]), date = date[t+3])
    }
    rbindlist(l)
  }
  , keyby = .(symbol)
  ][!is.na(date), .(stkcd_amount = uniqueN(symbol)), keyby = .(date, tag = ifelse(r3day_up == 1, "r3day_up", ifelse(r3day_dn == 1, "r3day_dn", "others")))
  ][tag == "r3day_dn"|tag == "r3day_up"
  ]












