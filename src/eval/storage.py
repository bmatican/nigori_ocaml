import glob
import os
import stat
import numpy
import matplotlib
import matplotlib.pyplot as plot


def compute_sizes():
  directory = os.path.join(os.path.dirname(__file__), '..')

  ret = {}
  for name in glob.glob(os.path.join(directory, 'nigori.db.*')):
    parts = name.split('.')
    number = int(parts[4][1:].lstrip('0'))
    value = int(parts[5][1:].lstrip('0'))
    value = (value / 16 + 1) * 16

    s = os.stat(name)
    actual_size = s[stat.ST_SIZE]
    extra = 36 + 68 + number * (128 + 68)
    estimated_size = number * value + extra
    diff = actual_size - estimated_size
    percent = 100 * diff / float(actual_size)

    values = ret.setdefault(number, {})
    values[value / 1024] = [actual_size / 1024, estimated_size / 1024]
    print number, value / 1024, diff / 1024, '{:0.2f}%'.format(percent)
  return ret

def graph_by_numbers():
  sizes = compute_sizes()
  numbers = sorted(sizes.keys())
  values = sorted(sizes[numbers[0]].keys())

  M = len(values)
  N = len(numbers)

  ind = numpy.arange(N)
  width = 1.5 / (2.0 * N)
  colors = ['m', 'y', 'b', 'r', 'g', 'k']
  black = len(colors) - 1

  fig = plot.figure()
  ax = fig.add_subplot(111)
  # ax.set_xscale('log', base=10)
  # ax.set_yscale('log', base=2)
  ax.set_ylim(10**0, 10**6)

  legend_colors = []
  for i in xrange(M):
      actual_data = [sizes[j][values[i]][0] for j in numbers]
      estimated_data = [sizes[j][values[i]][1] for j in numbers]
      actual_rect = ax.bar(ind + (i + 1) * width, actual_data, width, color=colors[black], log=True)
      estimated_rect = ax.bar(ind + (i + 1) * width, estimated_data, width, color=colors[i], log=True)
      if len(legend_colors) == 0:
        legend_colors.append(actual_rect[0])
      legend_colors.append(estimated_rect[0])
  legend_colors = tuple(legend_colors)
  names = ['Difference']
  names.extend(["{:d} kb".format(values[i]) for i in xrange(M)])
  legend_names = tuple(names)
  ax.legend(legend_colors, legend_names, loc=2)

  # ax.set_title('Space by number of items and size of data')
  ax.set_ylabel('Space (kb)')
  ax.set_xticks(ind + 0.5)
  ax.set_xticklabels([str(numbers[i]) for i in xrange(N)])
  plot.show()

def graph_by_values():
  sizes = compute_sizes()
  numbers = sorted(sizes.keys())
  values = sorted(sizes[numbers[0]].keys())

  M = len(values)
  N = len(numbers)

  ind = numpy.arange(M)
  width = 1.5 / (2.0 * M)
  colors = ['m', 'y', 'b', 'r', 'g', 'k']
  black = len(colors) - 1

  fig = plot.figure()
  ax = fig.add_subplot(111)
  # ax.set_xscale('log', base=10)
  # ax.set_yscale('log', base=2)
  ax.set_ylim(10**0, 10**6)

  legend_colors = []
  for i in xrange(N):
      actual_data = [sizes[numbers[i]][j][0] for j in values]
      estimated_data = [sizes[numbers[i]][j][1] for j in values]
      print actual_data, estimated_data
      actual_rect = ax.bar(ind + (i + 1) * width, actual_data, width, color=colors[black], log=True)
      estimated_rect = ax.bar(ind + (i + 1) * width, estimated_data, width, color=colors[i], log=True)
      if len(legend_colors) == 0:
        legend_colors.append(actual_rect[0])
      legend_colors.append(estimated_rect[0])
  legend_colors = tuple(legend_colors)
  names = ['Difference', '1 item']
  names.extend(['{} items'.format(numbers[i]) for i in xrange(1, N)])
  legend_names = tuple(names)
  # ax.legend(legend_colors, legend_names, loc=2, bbox_to_anchor=(1.05, 1))
  ax.legend(legend_colors, legend_names, bbox_to_anchor=(0., 1.02, 1., .102), loc=8, ncol=3, mode="expand", borderaxespad=0.)

  # ax.set_title('Space by number of items and size of data')
  ax.set_ylabel('Space (kb)')
  ax.set_xticks(ind + 0.5)
  ax.set_xticklabels([str(values[i]) + "kb" for i in xrange(M)])
  plot.show()

# graph_by_values()
graph_by_numbers()
