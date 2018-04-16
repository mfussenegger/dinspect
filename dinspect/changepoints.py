import numpy as np
import json


def _cusum(values):
    """
    >>> _cusum(np.array([10, 12, 14, 11]))
    array([ 0.  , -1.75, -1.5 ,  0.75])
    """
    avg = np.average(values)
    sums = np.zeros(len(values))
    for i in range(len(values) - 1):
        sums[i + 1] = sums[i] + values[i] - avg
    return sums


def _bootstrap(values, num):
    for _ in range(num):
        yield np.random.choice(values, size=values.shape, replace=True)


def _find_changepoints(values):
    vals = np.array(values)
    sums = _cusum(vals)
    sdiff = sums.max() - sums.min()
    num_below = 0
    num_boostrap = 1000
    for resample in _bootstrap(vals, num_boostrap):
        resample_sums = _cusum(resample)
        resample_sdiff = resample_sums.max() - resample_sums.min()
        if resample_sdiff < sdiff:
            num_below += 1
    cl = 100 * (num_below / num_boostrap)
    print('Confidence level that a change occurred: ', cl)


def find_changepoints(lines):
    for line in lines:
        d = json.loads(line)
        _find_changepoints(d)
