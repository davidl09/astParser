import PyParser
from matplotlib import pyplot
import math
from numpy import arange
import time

f = PyParser.Expression("x^2+3x^3-4x-6+cos(x)")

data = []

for i in arange(-math.pi, math.pi, 0.001):
    data.append(f.eval({"x": i}))


p = pyplot.plot(data)
pyplot