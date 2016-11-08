#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import argh
import json
import matplotlib.pyplot as plt
from collections import OrderedDict


def pie(data):
    """Draw a pie chart

    Input format:
        [
            {
                "col1": val,
                "col2": val
            },
            ...
        ]

    Each value in col1 results in a piece of the cake. The size depends on the
    value in col2.
    """
    items = list(data.items())
    labels = [str(i) for i in items[0][1]]
    _, sizes = items[-1]

    plt.pie(sizes, labels=labels, autopct='%1.1f%%')
    plt.show()


def lines(data):
    """Draws lines

    data format:
    [
        {
            "col1": val,
            "col2": val,
            ...
        },
        {
            "col1": val,
            "col2": val,
            ...
        },
        ...
    ]

    The first column will generate the X axis, then per other column a line on
    the Y axis is drawn.
    """
    items = list(data.items())
    x_label, x_items = items[0]
    plt.xlabel(x_label)
    plt.xticks(range(len(x_items)), x_items, rotation=45)
    for y_label, y_values in items[1:]:
        plt.plot(y_values, label=y_label)
    plt.legend()
    plt.show()


def bar(data):
    """Draw a bar chart

    Input format:
        [
            {
                "col1": val,
                "col2". val
            }
        }

    Values in col1 map to the X axis, values in col2 to the Y axis.
    """
    items = list(data.items())
    x_label, x_items = items[0]
    y_label, y_values = items[1]
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.bar(range(len(y_values)), y_values)
    plt.xticks(range(len(x_items)), x_items, rotation=45)
    plt.show()


def _list_to_dict(items):
    d = OrderedDict()
    for item in items:
        for k, v in item.items():
            if k not in d:
                d[k] = []
            d[k].append(v)
    return d


@argh.arg('--mode', choices=['lines', 'bar', 'pie'])
@argh.arg('--verbose', action='count')
def plot(mode='lines', *, title=None, verbose=None):
    """Plot JSON received on stdin into a chart"""

    if verbose:
        import matplotlib
        print(matplotlib.matplotlib_fname())

    data = json.load(sys.stdin, object_pairs_hook=OrderedDict)
    if isinstance(data, list):
        data = _list_to_dict(data)

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
