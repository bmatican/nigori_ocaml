import glob
import os
import numpy
import matplotlib
import matplotlib.pyplot as plot
import re
import math

def mean(arr):
  return sum(arr) / len(arr)

def std_dev(arr):
  m = mean(arr)
  return math.sqrt(sum([ (x - m)**2 for x in arr]) / len(arr)) 

def compute(path, name_in_bytes=True):
  re_client = re.compile('CLIENT: (\d+\.\d*)')
  re_server = re.compile('SERVER: (\d+\.\d*)')
  re_wire = re.compile('WIRE: (\d+\.\d*)')

  directory = os.path.join(os.path.dirname(__file__), 'output/{}'.format(path))

  ret = {}
  for name in glob.glob(os.path.join(directory, 'get.output.nigori.db.*')):
    parts = name.split('.')
    number = int(parts[4][1:].lstrip('0'))
    value = int(parts[5][1:].lstrip('0'))
    if name_in_bytes:
      value = (value / 16 + 1) * 16

    f = open(name)
    tclient = []
    tserver = []
    twire = []
    thttp = []
    for line in f.readlines():
      line = line.strip()
      client = re_client.findall(line)
      if len(client) > 0:
        tclient.append(float(client[0]))
        continue

      server = re_server.findall(line)
      if len(server) > 0:
        tserver.append(float(server[0]))
        continue

      wire = re_wire.findall(line)
      if len(wire) > 0:
        twire.append(float(wire[0]))
        continue

    for i in xrange(len(tserver)):
      thttp.append(twire[i] - tserver[i])

    mult = 1000
    if path == 'medium':
      data = thttp
      values = ret.setdefault(number, {})
      values[value] = [mult * mean(data), mult * std_dev(data)]
    elif path == 'server':
      data = tserver
      str_nr = str(number)
      val = str_nr.strip('0')
      value = (int(val) + 1) * (10 ** (len(str_nr) - len(val)))
      ret[value] = [mult * mean(data), mult * std_dev(data)]

  print ret
  return ret

def compute_medium():
  return compute('medium', False)
def compute_server():
  return compute('server', False)
###############
def graph_of_medium():
  sizes = compute_medium()
  data = sizes[1]
  values = sorted(data.keys())

  M = len(values)

  fig = plot.figure()
  ax = fig.add_subplot(111)
  ax.set_xlim(0, M)
  ax.set_ylim(0, 3)

  start = [i + 0.5 for i in xrange(M)]
  ax.errorbar(start, [data[i][0] for i in values], yerr=[data[i][1] for i in values])

  ax.set_title('Time spent away from either client or server libraries.')
  ax.set_ylabel('Time (ms)')
  ax.set_xlabel('Size of data item (kb).')
  ax.set_xticks(start)
  ax.set_xticklabels(values)
  plot.show()
####################
def graph_of_server():
  data = compute_server()
  items = sorted([int(i) for i in data.keys()])
  means = [data[i][0] for i in items]
  stds = [data[i][1] for i in items]

  M = len(items)
  start = [i + 0.5 for i in xrange(M)]

  fig = plot.figure()
  ax = fig.add_subplot(111)
  ax.set_xlim(0, M)
  ax.set_yscale('log')
  ax.grid(b=True, which='major', color='b', linestyle='-')
  ax.grid(b=True, which='minor', color='r', linestyle='-')
  ax.errorbar(start, means, yerr=stds, ecolor='k', elinewidth=2, capsize=6)

  ax.set_title('Time spent on database query.')
  ax.set_ylabel('Time (ms)')
  ax.set_xlabel('Number of items in database.')
  ax.set_xticks(start)
  ax.set_xticklabels(items)
  plot.show()
#######
graph_of_server()
