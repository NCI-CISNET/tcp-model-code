# About 

The Cancer Intervention and Surveillance Modeling Network (CISNET) Tobacco Control Policy (TCP) Model is a deterministic compartmental Markov 
modeling framework developed for each of the 50 US states and District of Columbia using state-specific estimates of smoking initiation and cessation 
probabilities by age, gender, and birth-cohort. It has been adapted from a previous US model, known as the CISNET Smoking History Generator Population Model (Tam et al., 2021).
The TCP Model estimates the long-term potential impacts of policies on smoking prevalence and mortality outcomes. 

The accompanying code evaluates the impacts of three tobacco control policies: cigarette taxation, smoke-free air laws, and tobacco control expenditures.
It simulates the effects of each policy at the state level.


# Methodology 

Birth cohorts are simulated from 1908 to 2100. Individuals are born as never smoking and may transition to current smoking in each subsequent year; once smoking, 
individuals may transition to former smoking in each subsequent year. Consistent with the original model, exit from the population occurs through mortality probabilities
by smoking status and years since quitting, or upon reaching age 99. Mortality probabilities and life expectancies are specific to age, gender, smoking status, and birth cohort. 
Among people who have quit smoking, mortality probabilities vary by years since quitting. The TCP Model outputs smoking prevalence by age group and mortality outcomes 
in the form of smoking-attributable deaths averted and life-years gained. 

**1.Cigarette Taxation**
Cigarette tax policies are modeled as price increases fully passed to consumers, influencing initiation and cessation through age-specific elasticities. 
Tax effects are applied through arc elasticity and implemented via modifiers to individual-level transition probabilities. 
Simulations span tax increases from $1 to $3 above current state price levels, beginning in 2025 and running through 2100.


**2.Smoke-Free Air Laws**
Modeled as venue-specific interventions (workplaces, restaurants, bars), smoke-free air laws modify initiation and cessation probabilities based on the scope of venue coverage.
Effects are scaled by existing coverage rates and simulated across incremental increases up to 100% coverage in each venue.


**3.Tobacco Control Programme Expenditures**
Effects of programme expenditures are based on literature and expert consensus, assuming a 10% reduction in initiation and 12.5% increase in cessation when funding reaches 100% of CDC recommendations. 
A nonlinear response function reflects increasing and diminishing returns.
States are modeled from their current funding levels in 10-percentage-point increments up to 100%.


# Model Usage

The TCP modeling framework is comprised of seven R script files, numbered in the order in which they should be run. 

**01_model_inputs.R** 
*	loads the necessary packages, loads global datasets, and sets model inputs. The beginning of the file contains two inputs that need to be set based on the user's desired outputs.
The variables are:
  
> *	**policy_decay** - (0/1) This variable determines whether or not scenario policy effects should be applied with indefinate efficacy (value=0) or
  with a 20% exponential decay rate beginning in the year 2030 (value=1).
 	
> *	**make_tcp_out** - (0/1) This variable determines whether or not to generate and output the .csv files used to populate figures for
the [tobacco control policy (TCP) tool](https://tobaccopolicyeffects.org). A value of 1 will generate the .csv files.
File generation is not necessary unless you are working on TCP tool website development. 

**02_model_functions.R**
*	contains all TCP model functions; generate_prevs(), calculate_mort(), runstates(), and generate_TCPoutput() functions.
  
**03_main_analysis.R**
*	runs the TCP model, loops through every state and saves the prevalence and mortality outputs.
 
**04_visualization_policy_specific.R**
*	functions to visualize the policy-specific impacts on life-years gained, deaths avoided, smoking prevalence, and prevalence reduction, based on each policy’s own input parameters.

**05_visualization_within_policy_comparison.R**
* functions to generate within-policy comparisons for each tobacco control policy, illustrating the impact of varying implementation levels on key outcomes.

**06_visualization_across_policy_comparison.R**
* functions to compare the impacts of different tobacco control policies across states by generating:
1. A US map and accompanying table displaying baseline state-level smoking prevalence in 2025  
2. Estimates of deaths averted per state under full implementation of all policies  
3. A 3x3 grid for each state visualizing the effects of tax increases, smoke-free air laws, and expenditure increases on:
   - Smoking prevalence reduction  
   - Life-years gained  
   - Deaths averted  
4. A summary table of baseline policy parameters by state  

**07_generate_US_csv.R**
*	Generates the USA total .csv files needed for the TCP tool by aggregating the mortality and smoking prevalence outputs for each of the states.
This file does not need to be run if you are not in need of the TCP tool data files.

The R folder also contains 00_state_process.R, a script which performs all of the initial data processing and cleaning needed to run the TCP model. 
This script does not need to be run, as all the raw data has been processed and saved in the "data" folder. 

The script is included in the repository for those interested. The 00_state_process.R script loads the data in "data-raw" folder, reformats and cleans, and saves the state specific data as inputs for population model. The data processed include state specific life expectancy, census population data, mortality probabilities, and smoking initiation/cessation probabilities.

Note: The large datasets "params_022422_1.csv" and "params_022422_2.csv" are not included in the data-raw folder due to GitHub's file size limitations (over 100MB). Interested users may contact the authors directly to request access to these files for data processing purposes.

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
Mengyao Wang<sup>1</sup> \
Alyssa Crippen, PhD (2022-2024) \
Sarah Skolnick, MPH, PhD<sup>2</sup> \
Rafael Meza, PhD<sup>3</sup> \
Jamie Tam, MPH, PhD<sup>2,4</sup>

### Author Affiliations:

1.	Department of Biostatistics, Yale Graduate School of Arts and Sciences
2.	Rutgers Institute for Nicotine and Tobacco Studies
3.	BC Cancer Research Center
4.	Department of Health Behavior Society and Policy, Rutgers School of Public Health
   
Model code is based on the Cancer Intervention and Surveillance Modeling Network (CISNET) Smoking History Generator Population Model
used for modeling the effects of graphic health warnings (Tam et al., 2021). See corresponding GitHub repository [here](https://github.com/mezarafael/SHG_PopModel_GHW).

### References:

Tam J, Levy DT, Jeon J, Clarke J, Gilkeson S, Hall T, Feuer EJ, Holford TR, Meza R. Projecting the effects of tobacco control policies in the USA through microsimulation: a study protocol. BMJ Open. 2018 Mar 23;8(3):e019169. doi: 10.1136/bmjopen-2017-019169. PMID: 29574440; PMCID: PMC5875683.
