#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup


setup(
    name='jsonplot',
    install_requires=[
        'argh',
        'matplotlib'
    ],
    entry_points={
        'console_scripts': [
            'jsonplot = jsonplot:main',
        ]
    }
)
