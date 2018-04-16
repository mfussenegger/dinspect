#!/usr/bin/env python3

from dinspect import plot
import doctest


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(plot))
    return tests
