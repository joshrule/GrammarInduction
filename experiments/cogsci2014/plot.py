


from matplotlib import pyplot as plt
import numpy as np

import sys
from os.path import join, exists

def plot_count_list_probs(csv_file): 
    assert(exists(csv_file))

    xs = np.loadtxt(csv_file, delimiter=",")
    fig = plt.figure()
    ax = fig.add_subplot(111)
    for i in range(xs.shape[1]):
        ax.plot(range(1, len(xs)+1), 100*xs[:, i], linewidth=2, label=i)

    ax.set_xticks(range(10, 100, 10))
    ax.set_ylim(0, 101)
    ax.set_xlabel("Highest number correctly reached when counting starting at 1.")
    ax.set_ylabel("Probability")
    plt.legend()
