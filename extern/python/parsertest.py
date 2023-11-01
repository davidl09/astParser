import PyParser
from matplotlib import pyplot
import math
from numpy import arange
import time

f = PyParser.Expression("sin(x)*e^x")

data = []

for i in arange(-math.pi, math.pi, 0.001):
    data.append(f.eval({"x": i, "e": math.e}))


p = pyplot.plot(arange(-math.pi, math.pi, 0.001), data)
pyplot.show()