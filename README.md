
### Description
The paper is an observation study that analyzes the impact (treatment effect) of UN peacekeeping interventions on the countries’ lenient peacebuilding success 2 years (*pbs2l*) and 5 years (*pbs5l*) after the war, with *untype* as the treatment and any countries that received any type of interventions (*untype* different from *None* or 0) belong to treatment group and control group otherwise.

First, I replicated figure 8 in https://gking.harvard.edu/files/counterf.pdf to show the model dependence of regression without matching, by comparing the original regression model with the augmented one (after adding an interaction term between variables). The figure shows that there are differences in the results obtained by the two models.

Therefore, matching was applied to increase accuracy by decreasing the dissimilarities between the two study group. The results show that UN interventions did increase the peace building success in the given countries by 0.2% after 2 years and 0.26% after 5 years. The result is plausible because there is not much initial difference between the control and treatment groups that can cause the treatment effect, indicated by the big p-values 0.15 and 0.2. 

The full report can be accessed here: https://github.com/trangnguyenvn1398/Causal_Inference/blob/master/Causal_Inference_UN.pdf
