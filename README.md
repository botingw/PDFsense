# PDFsense
This program is for studying the strength of constraints on Parton Distribution Functions (PDFs) imposed by experimental data. 
The approach, sensitivity mapping, is encoding the sensitivity as the correlation between Hessian error ensembles of experimental residuals and PDFs f(x, &mu;), where (x, &mu;) indicates the structure of nucleon. It is published in ArXiv: 1803.02777. 


This README introduces the knowledge for running this code, including some introduction and examples in following sections:
1. How to run the code
2. generating the data for plots
3. python GUI example 
4. script/notebook example

The section 1 is the introduction of generating correlation and sensitivity plots with the code.

section 3 is an instruction of the GUI for light users. It is easy to use the GUI to make sensitivity plots but it only include datasets in arXiv:1803.02777. 
If users want to run our code in the terminal, use our package to plot the sensitivities for his own datasets, or modify our code to do the further customization, 
they need to read other parts.   

The section 4 includes the instruction of an example. The example is for users using the script (terminal) and Mathematica .nb files. 
Users can run the example and read the section 4 to learn the detail of the running steps and arguments in configure files 

section 2 is for users who want to plot sensitivities for his own datasets (the default datasets (in ./quick_data/) are datasets in Table 1, 2, 3 of arXiv:1803.02777)

#################################
# How to run the code           #
#################################

One may choose to run the GUI interface, the Mathematica scripts (.m executables), or Mathematica notebooks (.nb executables)  
to implement PDFsense. the GUI interface provides a user-friendly way to run the package. The script and notebook
files enable users to run the package more flexibly and to generate more results in a given run.

Running the GUI interface:
a. ./XQPLOTTER 
b. select the PDF set, experiments, variables to be presented, and the presentation of plots 
(x, mu range, the color palette of the value range, etc)
c. click submit and output plots will be displayed.

Running the script and notebook:

a. setting config1.txt file under ./bin:
The settings for the experimental datasets to input, variables presented in plots, and the presentation of the plots are controlled by config1.txt. Options are turned on (off) by setting their corresponding "Flag" to 1 (0).

In config1.txt, lines start with a "#" symbol are comments and lines with xxx ":" xxx are input variables and their values for the code.
"Job ID (copy from the counter file):", "Job description:", "PDF set:", "Type:", "Flag:", "Expt. ID:", "Expt. Flag:", "Type:", "Flag:" 
are for the setup of the input data.
"xmin,   xmax:", "mumin, mumax:", "zmin, zmax:", "Number of bins:", "Type:", "Mode:", "Mode 1 range:", "Mode 2 range:", "Size:"
are for the setup of how figures look like.

input data are in ./quick_data, output plots will be in ./plots/Jobs,
In the config1.txt, "PDF set:" set the input data (output of fxQsamept_corr_v2.nb or .m)
"Expt. ID:" and "Expt. Flag:" select the experiments (represented by ID numbers) to be plotted,
if a selected experiment is not in the input data, the plots would not contain it's information. 

The "user" option to "Type:" in "#Functions (F)..." paragraph enables users (when it's Flag = 1) to create the customized function Func in 
correlations Corr(ri, Func) and sensitivity Sens(ri, Func). 

"#Axes ranges..." paragraph and the settings below it are for the demonstration of plots 

b. run the Mathematica script/notebook:
first, go to ./bin directory, then 
for the script: go to ./bin directory, then commandline math -script run_v4.m 
for the notebook: open run_v4.nb and implement the code

c. output
The output is in ./plots/Jobs,
The folder name is controlled by "Job ID (copy from the counter file):" in config1.txt  


#################################
# generating the data for plots #
#################################

If one doesn't want to only use the data in our database (data default in ./quick_data), one could also generate the
data from the Mathematica scripts (.m executables), or Mathematica notebooks.
The input data they need is the analyzed data from CTEQ code: .pds file and .dta file 
.pds contains the PDF info and .dta contains analyzed data of experiments and the selected PDF set
The detail way to set the customized database is in III A, D of the tutorial

a. edit the savedata_config.txt
PDF set Dir, PDF method, Expt ID List, datalis file for input setting
Correlation Path & Correlation File for output setting
If output setting is "default", the data would be saved in ./quick_data

b. if Expt ID List in include savedata_config.txt new experiments (new IDs) whose (x,mu) transform formulas are not set in the function selectExptxQv2 in 
./lib/dtareadbotingw2016.nb, modify the function to incorporate the formulas for these Expt IDs, then save dtareadbotingw2016.nb by dtareadbotingw2016.nb and 
dtareadbotingw2016.m (see III A, D in the tutorial) 

c. run fxQsamept_corr_v2.nb (or .m by math -script fxQsamept_corr_v2.m)

d. check whether the new database files are in the ./quick_data/ folder
(dtacentral_samept_data_($PDF_set_Dir).dat, fxQNset_samept_data_($PDF_set_Dir).dat, metadata_samept_data_($PDF_set_Dir).dat, residualNset_samept_data_($PDF_set_Dir).dat)


#################################
# python GUI example            #
#################################

for the running process, please read "Running the GUI interface:" part in "How to run the code " section

The output color palette ranges are defined in "Data color palette" box, which also determines the associated histogram x-axis range

When the "user" box is checked, the script also draws the plots defined in user_func.txt, which enable users to define the customized 
functions as the correlation/sensitivity outputs. Read 3.d and 3.e in "script/notebook example" section to know more information

#################################
# script/notebook example       #
#################################

files under ./example folder are example configure files, a test observable data file (quick_data_example), and a test PDF set (CT14HERA2NNLO).
to run example files, just copy configure files under ./example to the ./bin directory and run scripts (read tutorial III D. SOP of Plotting Figures of New Experiments):

1. run fxQsamept_corr_v2.nb (or .m by math -script fxQsamept_corr_v2.m) (the corresponding configure file is savedata_config.txt)
2. run run_v4.nb (or .m by math -script run_v4.m) (the corresponding configure file is config1.txt)
3. go to ./quick_data/ and ./plots/Jobs/ to see output data files and figure files
  a. make sure dtacentral_samept_data_CT14NNLO_example.dat, fxQNset_samept_data_CT14NNLO_example.dat,
     metadata_samept_data_CT14NNLO_example.dat, residualNset_samept_data_CT14NNLO_example.dat are in the ./quick_data/
  b. make sure the folder ./plots/Jobs/CT14NNLO_example_ExptInCT14Fit exist

4. check and compare the output figures with the setting in this run:
  a. open config1.txt and user_func.txt in ./plots/Jobs/CT14NNLO_example_ExptInCT14Fit. They record the arguments of this run.
  Now let's first read the config1.txt

  b. "Job ID (copy from the counter file):" set up the name of the output folder and "PDF set:" set up the input data.
  The input data is CT14NNLO_example so that the output data listed in 3.a will be the input data. 

  c. in the # Figures to plot...... paragraph, "Flag:" determine the observables in the output figures.
  Here for Type 1-6, Flags are 1, so the 6 kinds of observable will be in the output folder. 
  Let's check it:
  xQbyexpt_xQ.png, expt_error_ratio_xQ+1_samept.png, residual_xQ+1_samept.png, dr_xQ+1_samept.png are corresponding figures of 
  Type 1-4.  
  corrdr_xQ+1_f0_samept.png and corr_xQ+1_f0_samept.png are the Type 5, 6.

  d. in "# Experiments to inlcude......" paragraph, "Expt. Flag:" determin whether the input data contains the corresponding "Expt. ID:"
  open exptname_table.png see whether the IDs with Flag = 1 are contained in the output data list 
 
  e. in the #Functions (F)...... paragraph, "Flag:" indicates that the Flag of anti-d, gluon, and b quark = 1, so they will be in the output.
  The convention of anti-b quark to b quark are from -5 to +5, so corrdr_xQ+1_f0_samept.png is the plot of gluon.
  You will also find corrdr_xQ+1_f-2_samept.png and corrdr_xQ+1_f5_samept.png, which are plots of anti-d and b quarks.
  
  f. You also see the Flag of "user" Type is 1, so the script will run the user-customized functions. They are defined in user_func.txt.
  In user_func.txt, you see there are four functions, so you will see there are four more output correlation/sensitivity plots with
  filename = corrdr_xQ+1_f6_samept.png, ......, corrdr_xQ+1_f9_samept.png

  g. open corrdr_xQ+1_f0_samept.png and compare with #Highlighting...... paragraph of config1.txt. The highlight mode "Mode:" of Type 5 is 1,
  so the highlight range of Type 5 is {{ -5.0, -0.25},{ 0.25, 5.0}}, which is at the up-right side of corrdr_xQ+1_f0_samept.png.
  Also the points with values in the range of -0.25-0.25 are grayed out for they are not highlighted.
  Then open expt_error_ratio_xQ+1_samept.png and compare with #Highlighting...... paragraph of config1.txt. The highlight mode of 
  Type 5 is 0, so the plot does not highlight part of points. In expt_error_ratio_xQ+1_samept.png, all points are colored accordding
  to their values.

  h. See #Axes ranges...... paragraph and expt_error_ratio_xQ+1_samept.png. The scale range "xmin,   xmax:" is auto and "mumin, mumax:" 
  is 1-6000 , check whether the range of the y-axis is from 1-6000.
  Also, the color palette range "zmin, zmax:" is auto, so the color palette at the right-side of the plot is from 0 to the maximum value in points.
  The histogram corresponding to the plot (expt_error_ratio_hist+1_samept.png) also set the x-axis range to be the same as the 
  color palette range.  
    

Under ./quick, there are default 3 plotted data: CT14NNLO, CH14HERA2NNLO, and CT14HERA2_no_jet,
To use them to plot, just set in the config1.txt as their names, e.g.
......
PDF set: CT14HERA2NNLO
......


note:
1. To know how to run, read tutorial III A (PDFsense_tutorial_1.16.pdf) Run Step By Step
2. To know what's the difference among executable files, read tutorial III C. Input And Output Of Executables
3. code_tutorial.nb is used to run tutorial of functions in the program so that users can know how to functions in the package work
4. under ./bin/tools are some tools for advanced analysis,
a. make_info_table_v2.nb and read_info_table_v2_TH_BT_v2.nb could analyze the info files of the outputs of run_v4.nb(.m) (e.g. corr_info-1__samept_info.txt),
the output are tables of statistical quantities for the data corresponding to output plots with the .tex format
b. plotted_data_combine_test.nb could merge data in the database (./quick_data)
5. if the configure files are broken, fix them by copying the default configure files from ./bin/default_config_files to ./bin and change the copied filenames 
in ./bin to config1.txt and savedata_config.txt 
