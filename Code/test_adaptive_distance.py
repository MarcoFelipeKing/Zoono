import logging
import os
import tempfile

import matplotlib.pyplot as pyplot
import numpy as np

import pyabc.visualization

# for debugging
df_logger = logging.getLogger("Distance")
df_logger.setLevel(logging.DEBUG)

# model definition
def model(p):
    return {
        "s1": p["theta"] + 1 + 0.1 * np.random.normal(),
        "s2": 2 + 10 * np.random.normal(),
        "s3": 2 + 0.1 * np.random.normal(),
    }


# true model parameter
theta_true = 3

# observed summary statistics
observation = {"s1": theta_true + 1, "s2": 2, "s3": 5}

# prior distribution
prior = pyabc.Distribution(theta=pyabc.RV("uniform", 0, 10))

# database
db_path = pyabc.create_sqlite_db_id(file_="adaptive_distance.db")


def plot_history(history):
    """Plot 1d posteriors over time."""
    fig, ax = pyplot.subplots()
    for t in range(history.max_t + 1):
        df, w = history.get_distribution(m=0, t=t)
        pyabc.visualization.plot_kde_1d(
            df,
            w,
            xmin=0,
            xmax=10,
            x="theta",
            ax=ax,
            label=f"PDF t={t}",
            refval={"theta": theta_true},
            refval_color="grey",
        )
    ax.legend()


distance = pyabc.PNormDistance(p=2)

abc = pyabc.ABCSMC(model, prior, distance)
abc.new(db_path, observation)
h_uni = abc.run(max_nr_populations=7)

plot_history(h_uni)


import pandas as pd

df = pd.DataFrame([2,5,67,2,3,5,23,124])
df.hist()
pyplot.show()