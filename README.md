# Setup
1. Requires an installation of Python with **pip**, **venv**, and **sqlite3**. Bash shell is also required to run the script.
2. To display the dashboard and install dependencies. Type ```./runDashboard.sh``` inside the **TrainingLog** folder.
    - The script will install **odfpy** and **pandas** in a virtual environment.
        - odfpy is needed for the spreadsheet. 
    - **ProcessTrainingLog.py** will run and update **traininglog.db**.
    - **app.R** will then run and the dashboard will be in the browser.
        - Neccessary R packages are installed with **pacman**.  
   
# Spreadsheet Format
- The colon comes after the specified rep count. Numbers after the colon represent weight in pounds.  
- The comma separates sets.       
- The dash character is a break from the previous rep count and represents a different rep count and an upcoming set.  
- The semicolon signifies the end of an exercise (all sets) and may or may not have a number after representing the order of the exercise.  
  * If there is only one exercise for that day, the semicolon/order number is optional. But if there is more than one exercise for that day, only a singular exercise may not have an order number.
