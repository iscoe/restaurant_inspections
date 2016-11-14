
import json
from pprint import pprint
import csv


def getSelfAndChildren(self):

    result = [ self ]
    if self in parentSet:
        result.extend([ childresult for child in parentDict[self] for childresult in getSelfAndChildren(child)])
        return result
    else:
        return result
        

with open('categories.json') as data_file:
    data = json.load(data_file)

    parents = [ str(cat['parents'][0]) if len(cat['parents']) > 0 else '' for cat in data]

    parentSet = set(parents)
    parentDict = { p: [] for p in parentSet }

    for i in range(len(data)):
        parentDict[ parents[i] ].append( str(data[i]['alias']) )

    foodCategories = [ catresult for cat in ("restaurants", "food", "bars", "coffeeshops", "catering") for catresult in getSelfAndChildren(cat)]

    with open("foodCategories.csv", 'w') as foodcsv:
            writer = csv.writer(foodcsv, delimiter=",")
            for cat in foodCategories:
                writer.writerow([cat])
    

