

The parameters were calculated using GenStats logistic regression function to relate N uptake to DAP for a given yield potential.
There is a summary report which outlines how this was done – sorry can’t remember the name of the report but it’ll be the Quick Test folder in the QTMB or Report folders.
The raw data for each crop is also in the quick test folder so I’d have a look at that first to understand how data for each curve was generated. From memory it was a bit ad hoc as there wasn’t much info for some of the crops.
However, you should be able to generate a curve for a 50t/ha crop fairly easy…

The process is as follows:

  •	Find the appropriate excel file (Summer/Winter cabbage I think)
  •	Generate the N uptake vs DAP data for a 50T/ha yield potential.
  •	Fit a logistic regression to this, either in GenStat or in R (Duncan can assist with this).
  •	Extract required parameters and update QTMB model.
  
Used long cultivar in the MaizeData.xlsx steph put together.
Manually input the potential yield
The grain yield was calculated by a HI of 0.58674 and substrating 0.05 - assumed that this aligned with the AmaizeN tool
The N uptake was estimated by a linear regression - same assumption as note 3
Manually complete the sheet n UPTAKE CURVES
Open Genstat
Import data from excel - make sure all column are labelled
Stats > Regression analysis > standard curves > logistic (s-shape) model
Select the correct variables and run

