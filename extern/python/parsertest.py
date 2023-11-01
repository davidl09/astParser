import PyParser
from matplotlib import pyplot
import math
from numpy import arange
import time

f = PyParser.Expression("-sin(x)*exp(0.05x)")

data = []

width = 100

expression = PyParser.Expression()
expression.init("4+2/8")
print(f"4+2/8 = {expression.eval()}")

for i in arange(-width, width, 0.001):
    data.append(f.eval({"x": i, "e": math.e}))


p = pyplot.plot(arange(-width, width, 0.001), data)
pyplot.show()
