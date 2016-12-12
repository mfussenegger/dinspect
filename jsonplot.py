#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import argh
import json
import matplotlib.pyplot as plt
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


def hist(items):
    plt.hist(items)
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


@argh.arg('--mode', choices=['lines', 'bar', 'pie', 'hist'])
@argh.arg('--verbose', action='count')
def plot(mode='lines', *, title=None, verbose=None):
    """Plot JSON received on stdin into a chart

    Inputs:

        List of values:

        [
            10,
            20,
            30,
            ...
        ]


    List of map with >= 2 keys

        [
            {
                "col1": val,
                "col2": val
            },
            ...
        ]

    Values of col1 will usually map to the X axis. The other columns will
    map to the Y axis.

    The exact behaviour depends on the mode.
    For example in the pie chart the values of the first column make up the
    labels and the second column determines the size of the pieces.
    """

    if verbose:
        import matplotlib
        print(matplotlib.matplotlib_fname())

    data = json.load(sys.stdin, object_pairs_hook=OrderedDict)
    if not data:
        raise SystemExit('Empty input')
    if title:
        plt.title(title)

    g = globals()
    if mode in g:
        return g[mode](data)
    raise SystemExit('Unsupported mode: ' + mode)


def main():
    p = argh.ArghParser()
    p.set_default_command(plot)
    p.dispatch()


if __name__ == "__main__":
    main()
