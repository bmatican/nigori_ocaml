import glob
import os
import numpy
import matplotlib
import matplotlib.pyplot as plot
import re
import math

def get_data():
  f = open(os.path.join(os.path.dirname(__file__), 'output/server/data'))
  item = 1000
  data = {}
  for line in f.readlines():
    line = line.strip()
    pieces = [float(i) for i in line.split()]
    data[item] = pieces
    item = item * 10
  print data
  return data

def graph():
  data = get_data()
  items = sorted([int(i) for i in data.keys()])
  means = [data[i][0] for i in items]
  stds = [data[i][1] for i in items]

  M = len(items)
  start = [i + 0.5 for i in xrange(M)]

  fig = plot.figure()
  ax = fig.add_subplot(111)
  ax.set_xlim(0, M)
  ax.errorbar(start, means, yerr=stds)

  ax.set_title('Time spent on database query.')
  ax.set_ylabel('Time (ms)')
  ax.set_xlabel('Number of items in database.')
  ax.set_xticks(start)
  ax.set_xticklabels(items)
  plot.show()

graph()
