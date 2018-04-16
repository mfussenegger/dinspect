import numpy as np
import json


def _reject_outliers(data, m=5.):
    d = np.abs(data - np.median(data))
    mdev = np.median(d)
    s = d / mdev if mdev else 0.
    return data[s < m]


def reject(input_):
    for line in input_:
        d = json.loads(line)
        print(_reject_outliers(np.array(d)).tolist())
