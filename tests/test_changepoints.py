#!/usr/bin/env python

from dinspect import changepoints
import doctest


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(changepoints))
    return tests
