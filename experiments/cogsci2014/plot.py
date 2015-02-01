


from matplotlib import pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np

import sys
from os.path import join, exists

def plot_count_list_probs(file_or_data, ax=None): 
    if isinstance(file_or_data, str):
        csv_file = file_or_data
        assert(exists(csv_file))
        xs = np.loadtxt(csv_file, delimiter=",")
    else: 
        xs = file_or_data
        
    xs = np.exp(np.cumsum(np.log(xs[:-1]), 0))
    if ax is None: 
        fig = plt.figure()
        ax = fig.add_subplot(111)

    for i in range(xs.shape[1]):
        ax.plot(range(1, len(xs)+1), 100*xs[:, i], linewidth=2, label=i)

    ax.set_xticks(range(10, 100, 10))
    ax.set_ylim(0, 101)
    ax.set_xlabel("Highest number correctly reached when counting starting at 1.")
    ax.set_ylabel("Probability")
    # plt.legend()

def create_stages_plot(data_dict):  
    fig = plt.figure()
    gs = gridspec.GridSpec(2, 2)
    for i, (key, xs) in enumerate(data_dict.items()):
        row =  i / 2
        col = i % 2
        ax = fig.add_subplot(gs[row, col])
        plot_count_list_probs(xs, ax=ax)
        ax.set_xlabel("Evidence size: %i" %key)
