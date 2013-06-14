f = open('suffixes.data', 'w+')

for i in xrange(1, 100000):
  s = str(i)
  base = 'test-val-'
  total = 15
  diff = total - len(base) - len(s)
  zeroes = '0' * diff
  value = zeroes + s + '\n'
  f.write(value)

print 'done'
