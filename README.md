# About 

The Cancer Intervention and Surveillance Modeling Network (CISNET) Tobacco Control Policy (TCP) Model is a deterministic compartmental Markov 
modeling framework developed for each of the 50 US states and District of Columbia using state-specific estimates of smoking initiation and cessation 
probabilities by age, gender, and birth-cohort. It has been adapted from a previous US model, known as the CISNET Smoking History Generator Population Model (Tam et al., 2021).
The TCP Model estimates the long-term potential impacts of policies on smoking prevalence and mortality outcomes. 

The accompanying code evaluates the impacts of Tobacco 21 (T21) policies, which raise the minimum legal age of cigarette purchase to 21. For more details, see:
*	US Tobacco 21 policies and potential mortality reductions by state (pending publication)


# Methodology 

Birth cohorts are simulated from 1908 to 2100. Individuals are born as never smoking and may transition to current smoking in each subsequent year; once smoking, 
individuals may transition to former smoking in each subsequent year. Consistent with the original model, exit from the population occurs through mortality probabilities
by smoking status and years since quitting, or upon reaching age 99. Mortality probabilities and life expectancies are specific to age, gender, smoking status, and birth cohort. 
Among people who have quit smoking, mortality probabilities vary by years since quitting. The TCP Model outputs smoking prevalence by age group and mortality outcomes 
in the form of smoking-attributable deaths averted and life-years gained. 

Application to T21 policies
Policy scenarios are modeled based on the governance tier of T21 policies at the local, state, and federal levels. Four policy scenarios are modeled: 
1.	baseline (no T21);
2.	local T21; 
3.	state & local T21; 
4.	combined local, state, & federal T21. 

Policy effects estimates are based on state-level findings from Hansen et. al, which translate to a 34% reduction to smoking initiation among persons ages 18-20 (95% CI: 15%-53%). 
For each policy scenario, three policy effect sizes are modeled: 15%, 34%, and 53% relative reductions to initiation probabilities. 
The upper and lower confidence intervals were modeled as sensitivity analysis to gauge outcomes under "optimistic" and "pessimistic" conditions. 

# Model Usage

The TCP modeling framework is comprised of six R script files, numbered in the order in which they should be run. 

**01_model_inputs.R** 
*	loads the necessary packages, loads global datasets, and sets model inputs. The beginning of the file contains two inputs that need to be set based on the user's desired outputs.
The variables are:
  
> *	**policy_decay** - (0/1) This variable determines whether or not scenario policy effects should be applied with indefinate efficacy (value=0) or
  with a 20% exponential decay rate beginning in the year 2030 (value=1).
 	
> *	**make_tcp_out** - (0/1) This variable determines whether or not to generate and output the .csv files used to populate figures for
the [tobacco control policy (TCP) tool](https://tobaccopolicyeffects.org). A value of 1 will generate the .csv files.
File generation is not necessary unless you are working on TCP tool website development. 


**02_main_analysis.R**
*	runs the TCP model, loops through every state and saves the prevalence and mortality outputs.

**03_model_functions.R**
*	contains all TCP model functions; generate_prevs(), calculate_mort(), runstates(), and generate_TCPoutput() functions.
 
**04_visualization.R**
*	Contains the functions to plot the figures included in the manuscript.

**05_generate_US_csv.R**
*	Generates the USA total .csv files needed for the TCP tool by aggregating the mortality and smoking prevalence outputs for each of the states.
This file does not need to be run if you are not in need of the TCP tool data files.

The R folder also contains 00_state_process.R, a script which performs all of the initial data processing and cleaning needed to run the TCP model. 
This script does not need to be run, as all the raw data has been processed and saved in the "data" folder. 
The script is included in the repository for those interested. The 00_state_process.R script loads the data in "data-raw" folder, reformats and cleans, 
and saves the state specific data as inputs for population model. The data processed include state specific life expectancy,
census population data, mortality probabilities, and smoking initiation/cessation probabilities.

# Model Naming Conventions
Where applicable, objects are named according the following format:
**DataType_Gender.Description_Format**

The naming conventions in this model are consistent with those outlined in the DARTH modeling framework (Alarid-Escudero, Fernando, et al., 2019).
Objects are named with prefixes that describe their data type:



| Tables   |      Are      |
|----------|:-------------:|
| v_ |  vector | 
| l_ |   list  | 
| m_ | matrix | 
| df_ | data frame| 
| a_ | array | 


Gender specifications are abbreviated as ‘M’, ‘F’, or ‘B’ to represent Male, Female, and Both. 
Data format abbreviations are included where applicable to indicate if the data is age-period (AP) or age-cohort (AC).

Further information about the DARTH framework can be found here:

* Alarid-Escudero, Fernando, et al. “A need for change! A coding framework for improving transparency in decision modeling.” 
PharmacoEconomics, vol. 37, no. 11, 24 Sept. 2019, pp. 1329–1339, https://doi.org/10.1007/s40273-019-00837-x. 


# Authors and acknowledgments
Alyssa Crippen<sup>1</sup>  \
Jamie Tam<sup>1</sup>   \
Rafael Meza<sup>2</sup>   \
Jihyoun Jeon<sup>3</sup> 


### Author Affiliations:

1.	Department of Health Policy and Management, Yale School of Public Health
2.	Integrative Oncology, BC Cancer Research Centre
3.	Department of Epidemiology, University of Michigan School of Public Health
   
Model code is based on the Cancer Intervention and Surveillance Modeling Network (CISNET) Smoking History Generator Population Model
used for modeling the effects of graphic health warnings (Tam et al., 2021). See corresponding GitHub repository [here](https://github.com/mezarafael/SHG_PopModel_GHW).

### References:

Tam J, Jeon J, Thrasher JF, et al. Estimated Prevalence of Smoking and Smoking-Attributable Mortality Associated With Graphic Health Warnings
on Cigarette Packages in the US From 2022 to 2100. JAMA Health Forum. 2021;2(9):e212852. [doi:10.1001/jamahealthforum.2021.2852](https://jamanetwork.com/journals/jama-health-forum/fullarticle/2784492)


