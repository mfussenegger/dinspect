import sys
import json
import matplotlib.pyplot as plt
import statistics
import numpy as np
from collections import OrderedDict


def _list_to_dict(items):
    """ Convert a list of dicts to a dict with the keys & values aggregated

    >>> _list_to_dict([
    ...     OrderedDict([('x', 1), ('y', 10)]),
    ...     OrderedDict([('x', 2), ('y', 20)]),
    ...     OrderedDict([('x', 3), ('y', 30)]),
    ... ])
    OrderedDict([('x', [1, 2, 3]), ('y', [10, 20, 30])])
    """
    d = OrderedDict()
    for item in items:
        for k, v in item.items():
            if k not in d:
                d[k] = []
            d[k].append(v)
    return d


def pie(items):
    """Draw a pie chart"""
    items = list(_list_to_dict(items).items())
    labels = [str(i) for i in items[0][1]]
    _, sizes = items[-1]

    plt.pie(sizes, labels=labels, autopct='%1.1f%%')
    plt.show()


def hist(items, num_bins=50):
    plt.hist(items, num_bins, alpha=0.75)
    mean = statistics.mean(items)
    median = statistics.median(items)
    plt.axvline(mean, color='r', linewidth=2, linestyle='dashed', label='mean')
    plt.axvline(median, color='g', linewidth=2, linestyle='dashed', label='median')
    plt.legend()
    plt.show()


def lines(items):
    """Draws lines"""
    first_item = items[0]
    if isinstance(first_item, dict):
        items = list(_list_to_dict(items).items())
        x_label, x_items = items[0]
        plt.xlabel(x_label)
        plt.xticks(range(len(x_items)), x_items, rotation=45)
        for y_label, y_values in items[1:]:
            plt.plot(y_values, label=y_label)
        plt.legend()
    else:
        plt.xlabel('samples')
        plt.plot(items)
    plt.show()


def scatter(items):
    fst = items[0]
    if isinstance(fst, dict):
        items = list(_list_to_dict(items).items())
        x_label, x_items = items[0]
        plt.xlabel(x_label)
        plt.xticks(range(len(x_items)), x_items, rotation=45)
        plt.scatter(x_items, items[1][1])
    else:
        x_items = list(range(len(items)))
        plt.scatter(x_items, items)
    plt.show()


def bar(items):
    """Draw a bar chart"""
    items = list(_list_to_dict(items).items())
    x_label, x_items = items[0]
    y_label, y_values = items[1]
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.bar(range(len(y_values)), y_values)
    plt.xticks(range(len(x_items)), x_items, rotation=45)
    plt.show()


def lineregress(items):
    fst = items[0]
    if isinstance(fst, dict):
        items = list(_list_to_dict(items).items())
        x_label, x_items = items[0]
        y_label, y_items = items[1]
        plt.xlabel(x_label)
        plt.ylabel(y_label)
        fit = np.polyfit(x_items, y_items, 1)
        fit_fn = np.poly1d(fit)
        plt.plot(x_items, y_items, fit_fn(x_items))
    else:
        x_items = list(range(len(items)))
        fit = np.polyfit(x_items, items, 1)
        fit_fn = np.poly1d(fit)
        plt.plot(x_items, items, fit_fn(x_items))
    plt.show()


def boxplot(inputs):
    fst = inputs[0]
    if isinstance(fst, dict):
        items = list(_list_to_dict(inputs).items())
        x_label, labels = items[0]
        plt.xlabel(x_label)
        data = items[1][1]
        plt.boxplot(data, labels=labels)
    else:
        plt.boxplot(inputs)
    plt.show()


modes = {
    'bar': bar,
    'hist': hist,
    'lines': lines,
    'pie': pie,
    'scatter': scatter,
    'lineregress': lineregress,
    'boxplot': boxplot,
}


def parse_input(lines):
    lines = iter(lines)
    for line in lines:
        try:
            data = json.loads(line, object_pairs_hook=OrderedDict)
            if isinstance(data, list):
                yield from data
            else:
                yield data
        except ValueError:
            content = line + ''.join(lines)
            dicts = json.loads(content, object_pairs_hook=OrderedDict)
            if isinstance(dicts, list):
                yield from dicts
            else:
                yield dicts


def plot(mode='lines', *, title=None, verbose=None):
    """Plot JSON received on stdin into a chart

    Inputs:

        List of values:

            [ 10, 20, 30, ... ]


        List of map with >= 2 keys

            [
                { "col1": val, "col2": val },
                ...
            ]

        JSON object per line:

            { "col1": val, "col2": val }
            { "col1": val, "col2": val }

        Single value per line:

            10
            20
            30

    Values of the first column will usually map to the X axis. The other columns
    will map to the Y axis.

    The exact behaviour depends on the mode.
    For example in the pie chart the values of the first column make up the
    labels and the second column determines the size of the pieces.
    """

    if verbose:
        import matplotlib
        print(matplotlib.matplotlib_fname())

    if sys.stdin.isatty():
        raise SystemExit('Expected JSON input via stdin')
    data = list(parse_input(sys.stdin))
    if not data:
        raise SystemExit('Empty input')
    if title:
        plt.title(title)

    if mode in modes:
        return modes[mode](data)
    raise SystemExit('Unsupported mode: ' + mode)
