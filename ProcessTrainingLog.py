#!/usr/bin/env python
# coding: utf-8

import pandas as pd
from datetime import datetime
import sqlite3

columns = ["Date","Location","Back Squat","Bench Press","Deadlift","Overhead Press","Bent Over Rows","Romanian Deadlift","Hamstring Curl","Leg Extension"]

def processDates(rowNum, row, columns):
    rowDate = datetime.strptime(row[0], "%m/%d/%Y")
    rowLocation = row[1].split("-")
    if rowLocation[0] == "H":
        rowLocation[0] = "Home"
    elif rowLocation[0] == "U":
        rowLocation[0] = "University"
    return {"Id":rowNum, columns[0]:rowDate, "Day":rowDate.strftime("%A"), "Place":rowLocation[0], "City":rowLocation[1]}

def processLifts(rowNum, row, columns=columns):
    metaRow = processDates(rowNum, row, columns)
    #https://stackoverflow.com/a/57001947
    rowList, orderList, numLifts = [], [], 0

    for i in range(2,len(row)): # columns (exclude date and location)
        if pd.isna(row[i]):
            continue # cell empty no data
        else:
            cellDict = {columns[0]:metaRow["Date"], "Lift":columns[i],"Location Id":rowNum} # add date,lift,location id
            cell = row[i]
            numString, sets, reps = "", 1, 0
            for j in range(len(cell)): # characters
                if j < len(cell)-1 and ";" in cell[j]: # could end in ";" - no valuable data
                    cellDict["Order Num"] = int(cell[j+1:])
                    orderList.append(int(cell[j+1:]))
                    cellDict["Set %s" % sets] = int(numString)
                    cellDict["Reps Set %s" % sets] = reps
                    break # reached end
                elif j == len(cell)-1: # no ";", unkown order, end of string
                    if cell[j].isdigit():
                        numString += cell[j]
                    cellDict["Order Num"] = -1
                    cellDict["Reps Set %s" % sets] = reps
                    cellDict["Set %s" % sets] = int(numString)
                elif ":" in cell[j]:
                    reps = int(numString)
                    numString = ""
                elif "," in cell[j] or "-" in cell[j]:
                    cellDict["Set %s" % sets] = int(numString)
                    cellDict["Reps Set %s" % sets] = reps
                    sets += 1
                    numString = ""
                elif cell[j].isdigit():
                    numString += cell[j] # concat numbers
            rowList.append(cellDict)
            numLifts += 1 # every non-empty cell
            
    for row in rowList:
        if row["Order Num"] == -1:
            row["Order Num"] = (set([i for i in range(1, numLifts+1)]) - set(orderList)).pop() #https://stackoverflow.com/a/34045983
    return (rowList, metaRow)

def liftSQL(liftData):
    getReps = ""
    for i in liftData.columns:
        if "Set" in i:
            getReps += ",\n\t[" + i + "] INTEGER NOT NULL"

    createLift = """CREATE TABLE IF NOT EXISTS lifts(
        Date TEXT NOT NULL,
        Lift TEXT NOT NULL,
        [Location Id] INTEGER,
        [Order Num] INTEGER NOT NULL"""
    createLift += getReps + ",FOREIGN KEY ([Location Id]) REFERENCES metadata(Id),PRIMARY KEY (Date,Lift));"
    return createLift

df = pd.read_excel("TrainingLog.ods")
#https://stackoverflow.com/a/55557758
resultList = [processLifts(rowNum,row) for rowNum, row in enumerate(df[columns].to_numpy(),1)]
liftList, metaList = [], []
for row in resultList:
    liftList += row[0]
    metaList.append(row[1])

liftData = pd.DataFrame.from_dict(liftList)  
metaData = pd.DataFrame.from_dict(metaList)

sqlCreateTable = []
createMetadata = """CREATE TABLE IF NOT EXISTS metadata(
    Id PRIMARY KEY NOT NULL,
    Date TEXT,
    Day TEXT,
    Place TEXT,
    City TEXT);"""
sqlCreateTable.append(createMetadata)
sqlCreateTable.append(liftSQL(liftData))

connection = sqlite3.connect("traininglog.db")
cur = connection.cursor()
for statement in sqlCreateTable:
    cur.execute(statement)
connection.commit()

liftData.to_sql(name="lifts", con=connection, if_exists='replace', index=False)
metaData.to_sql(name="metadata", con=connection, if_exists='replace', index=False)
connection.close()

