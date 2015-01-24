
from matplotlib import pyplot as plt
import pandas as pd
import numpy as np

from sklearn.metrics import roc_curve, auc

def run_roc(labels, scores):
    fpr, tpr, thresholds = roc_curve(labels, scores)
    roc_auc = auc(fpr, tpr)
    print "Area under the ROC curve : %f" % roc_auc
    
    # Plot ROC curve
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc)
    ax.plot([0, 1], [0, 1], 'k--')
    ax.set_xlim([0.0, 1.0])
    ax.set_ylim([0.0, 1.0])
    ax.set_xlabel('False Positive Rate')
    ax.set_ylabel('True Positive Rate')
    ax.set_title('ROC for test data')
    ax.legend(loc="lower right")
    return roc_auc, fpr, tpr, ax

