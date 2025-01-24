# Setup
1. Execute **ProcessTrainingLog.py** with python to update **traininglog.db**.
   - There are some required libraries needed to run the script.
3. Open **app.R** in RStudio and execute **runGadget** function.
   - There are some required packages needed to install.

# Spreadsheet Format
- The colon comes after the specified rep count. Numbers after the colon represent weight in pounds.  
- The comma separates sets.       
- The dash character is a break from the previous rep count and represents a different rep count and an upcoming set.  
- The semicolon signifies the end of an exercise (all sets) and may or may not have a number after representing the order of the exercise.  
  * If there is only one exercise for that day, the semicolon/order number is optional. But if there is more than one exercise for that day, only a singular exercise may not have an order number.
