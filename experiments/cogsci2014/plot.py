


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
        ax.plot(range(1, len(xs)+1), 100*xs[:, i], linewidth=1.5, label=i)

    ax.set_xticks(range(10, 100, 10))
    ax.set_ylim(0, 101)
    ax.set_xlabel("Highest number correctly reached when counting starting at 1.")
    ax.set_ylabel("Probability")
    # plt.legend()

def create_stages_plot(data_dict):  
    fig = plt.figure()
    gs = gridspec.GridSpec(3, 2)
    for i, (key, xs) in enumerate(data_dict.items()):
        row =  i / 2
        col = i % 2
        ax = fig.add_subplot(gs[row, col])
        plot_count_list_probs(xs, ax=ax)
        ax.set_xlabel("Evidence size: %i" %key)
    return fig


def plot_evidence_histogram(): 

    fig = plt.figure()
    gs = gridspec.GridSpec(3, 2)

    def ys(count): 
        hs = [np.log10(float(count)/i) for i in range(1, 100)]
        hs = [max(h, 0) for h in hs ]
        return hs

    def decades(count):
        xs = [i*10 - 1 for i in range(3, 10)]
        hs = [np.log10(float(count)) for i in xs]
        return (xs, hs)
        

    def mk_bar(count, row, col, decade_count=None):
        ax = fig.add_subplot(gs[row, col])
        hs = ys(count) 
        if not decade_count is None: 
            (xs, ds) = decades(decade_count)
            for i, x in enumerate(xs): 
                hs[x+1] = np.log10(10**hs[x+1] + 10**ds[i])
        ax.bar(range(1, 100), hs, linewidth=0, width=1)
        ax.set_yticks([])
        ax.set_xlim(1, 99)
        ax.set_ylim(0, 5)
        return ax
        
    ax1 = mk_bar(10, 0, 0)
    ax2 = mk_bar(100, 0, 1)
    ax3 = mk_bar(1000, 1, 0)
    ax4 = mk_bar(10000, 1, 1)
    ax5 = mk_bar(1000, 2, 0, 100)
    ax6 = mk_bar(10000, 2, 1, 1000)
    return fig


def succ_distribution_plot(N, data):
    ## data = [[prob, word], [prob, word]..]
    ## N is the number word whose successor is begin asked for
    data.sort()
    data.reverse()
    data = data[0:20]
    data.reverse()

    ws = [x[1] for x in data]
    ps = np.array([x[0] for x in data])
    ps = ps/ps.sum()
    xs = range(len(ps))
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    ax.barh(xs,ps)
    ax.set_yticks([x+0.5 for x in xs])
    ax.set_yticklabels(ws)
    # fig.subplots_adjust(bottom=0.25)

    ax.set_xlabel("Probability")
