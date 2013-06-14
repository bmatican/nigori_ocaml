import glob
import os
import numpy
import matplotlib
import matplotlib.pyplot as plot
import re
import math

def get_data():
  data = {}
  f = open(os.path.join(os.path.dirname(__file__), 'output/client/aes.data'))
  for line in f.readlines():
    line = line.strip()
    pieces = [float(i) for i in line.split()]
    data[pieces[0]] = pieces[1:]
  print data
  return data

def graph():
  data = get_data()
  sizes = sorted([int(i) for i in data.keys()])
  means = [data[i][0] for i in sizes]
  stds = [data[i][1] for i in sizes]

  M = len(sizes)
  start = [i + 0.5 for i in xrange(M)]

  fig = plot.figure()
  ax = fig.add_subplot(111)
  ax.set_xlim(0, M)
  ax.errorbar(start, means, yerr=stds)

  ax.set_title('Time spent on AES encryption -- linear scale.')
  ax.set_ylabel('Time (ms)')
  ax.set_xlabel('Size of plaintext (kb).')
  ax.set_xticks(start)
  ax.set_xticklabels(sizes)
  plot.show()

  fig = plot.figure()
  ax = fig.add_subplot(110)
  ax.set_xlim(0, M)
  ax.set_yscale('log')
  ax.errorbar(start, means, yerr=stds)

  ax.set_title('Time spent on AES encryption -- log scale.')
  ax.set_ylabel('Time (ms)')
  ax.set_xlabel('Size of plaintext (kb).')
  ax.set_xticks(start)
  ax.set_xticklabels(sizes)

  plot.show()

graph()
