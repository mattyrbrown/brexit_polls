I analyse the potential to use poll data to accurately estimate the outcome of the 2016 brexit vote

download-data.R - downloads brexit poll data, actual brexit vote results and packages required to perform poll analysis

spread-estimate-without-pollster-variability.R - takes results from the polls performed in the week before voting to produce an estimate for the actual spread. The model does not take into consideration pollster variability or general bias

spread-estimate-with-pollster-variability.R - takes the results from each pollsters final poll before voting to porduce an estimate for the actual spread. The model incoporates pollster variability but does not take into consideration general bias

hierarchical-spread-estimate.R - takes the results from each pollsters final poll before voting to produce an estimate for the actual spread. The model incoporates both pollster variability and general bias

poll-and-combined-spread-estimates - produces a figure showing the estimated spreads for each pollster's final poll before voting as well as the combined estimated spreads calculated in spread-estimate-without-pollster-variability.R, spread-estimate-with-pollster-variability.R and hierarchical-spread-estimate.R