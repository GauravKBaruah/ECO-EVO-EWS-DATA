**PLEASE email : gaurav.baruah@ieu.uzh.ch/ piyushgkb@gmail.com if you find any errors !**

*THERE ARE README.md FILES IN EACH OF THE FOLDERS. PLEASE GO THROUGH THEM TOO WHEN LOOKING AT THE DATA AND THE SCRIPTS.*




*In this folder there are other folders, R scripts and loads of simulation data for population collapse.*


# DATASETS INCLUDED IN THIS FOLDER:



1. It contains population collapse data going by the names as *EWS./factor/value_of_the_factor/.RData*
Where /factor/ is genetic variation; plasticity, and reproduction. 


2. This folder also contains *genvar.RData ; plasticity.RData ; reproductive_rate.RData* which are Kendall's Tau values of population collapse of abundance and trait-based signals for different levels of genetic variation, plasticity and reproductive rate.


3. This folder also contains false positive data created by the method of Dakos et al 2012. Particularly,
**False_positives_constant_timeseries_genvar.RData** ,
**False_positives_constant_timeseries_plasticity.RData**,
**False_positives_constant_timeseries_R0.RData**,
 
These data sets contains false positive rates from for different levels of genetic variation, plasticity and R0, for constant timeseries of 30 time points.

4. Also contains :
**False.positives.variable_timeseries_genvar.RData** ,
**False.positives.variable_timeseries_R0.RData**,
**False.positives.variable_timeseries_plasticity.Rdata**
 these data sets are false positive rates for different levels of genetic variation, plasticity , R0 for variable timeseries length.



# R SCRIPTS INCLUDED IN THIS FOLDER :

1. **Functions_J_A_Ecology.R**  is the R script that has majority of the functions needed for EWS analysis. Specifically modified EWS code, trait-based EWS code etc.

2. **01_Abundance-based-ews-script-analysis.R**  is the R script that is used for abundance-based EWSs .

3. **02_trait-based-ews-script-analysis.R**  is the R script for trait-based EWSs analysis.

4. **03_Effective_Potential_and_return_timeplot.R** the R script that plots the potential curve and return times.

5. **04_plotting_False_positives_with_surrogate_timeseries.R**  plots rate of false positives

6. **S1_Trait_Abundance_EWS_variable_timeseries_script_analysis.R**  abundance and trait-based EWS analysis for variable time series length.

7. **S2_False_positives_surogateseries_variable_timeseries_length_analysis.R** for analysing and creating surrogate timeseries and estimating rate of false positives.


8. **S3_False_positives_surogateseries_constant_timeseries_length-main-text_analysis.R** this scripts is for generating false positive rates from surrogate timeseries for the main-text,








