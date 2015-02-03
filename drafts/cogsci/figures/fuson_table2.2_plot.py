
from matplotlib import pyplot as plt
import numpy as np


data = [[((3,6), (3, 11)), 14.17, (4, 29), 6.51], 
        [((4, 4), (4, 5)), 17.18, (10, 39), 8.71], 
        [((4, 6), (4, 11)), 29.59, (12, 100), 28.19], 
        [((5, 0), (5, 5)), 40.19, (11, 100), 25.76], 
        [((5, 6), (5, 11)), 38.17, (13, 90), 22.44]]

pos = range(len(data))
means= [x[1] for x in data]
ranges = [list(x[2]) for x in data]
stds = [x[3] for x in data]

w = width = 0.2
fig = plt.figure()
ax = fig.add_subplot(111)
for i, (label, mean, (lo, hi), std) in enumerate(data):
    y = i + 1
    coords = [[lo, hi], [y, y]]
    line = plt.Line2D(coords[0], coords[1], linewidth=1.5)
    lo_edge = plt.Line2D([lo, lo], [y-w, y+w], linewidth=1.5)
    hi_edge = plt.Line2D([hi, hi], [y-w, y+w], linewidth=1.5)
    mean_line = plt.Line2D([mean, mean], [y-w, y+w], color='k', linewidth=1.5)
    box = plt.Rectangle([mean-std/2, y-w/2.0], std, w, edgecolor='k', facecolor='none') 
    ax.add_line(line)
    ax.add_line(lo_edge)
    ax.add_line(hi_edge)
    ax.add_line(mean_line)
    ax.add_patch(box)

ax.set_xlim(0, 105)
ax.set_ylim(0, 6)
ax.set_yticks(range(1, len(data)+1))
ax.set_yticklabels(["%i;%i - %i;%i"%(x[0][0][0], x[0][0][1], x[0][1][0], x[0][1][1]) for x in data])
ax.set_xticks(range(0, 110, 10))

ax.set_xlabel("Largest number correctly reached.")
ax.set_ylabel("Age range")

           

