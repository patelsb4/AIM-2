
source('Code/dlnm_bs_func.R')
source('Code/dlnm_ns_func.R')
source('Code/casecrossv2.R')

#using bs


all_knots_50bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.50),
  plot_name = "all_knots_50bs.png"
)

all_knots_75bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.75),
  plot_name = "all_knots_75bs.png"
)

all_knots_90bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.90),
  plot_name = "all_knots_90bs.png"
)

all_knots_2575bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.25,0.75),
  plot_name = "all_knots_2575bs.png"
)

all_knots_255075bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.25,0.50,0.75),
  plot_name = "all_knots_255075bs.png"
) 

all_knots_3366bs <- run_dlnm_casecross_bs(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.33,0.66),
  plot_name = "all_knots_3366bs.png")


## using ns

all_knots_50ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.50),
  plot_name = "all_knots_50ns.png"
)

all_knots_75ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.75),
  plot_name = "all_knots_75ns.png"
)

all_knots_90ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.90),
  plot_name = "all_knots_90ns.png"
)

all_knots_2575ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.25,0.75),
  plot_name = "all_knots_2575ns.png"
)


all_knots_255075ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.25,0.50,0.75),
  plot_name = "all_knots_255075ns.png"
) 

all_knots_3366ns <- run_dlnm_casecross_ns(
  data = pregpp_cc,
  temp_var = "temp.mean",
  lag_days = 7,
  exp_probs = c(0.33,0.66),
  plot_name = "all_knots_3366ns.png")
