#!/usr/bin/env python3

from setuptools import setup


setup(
    name='dinspect',
    install_requires=[
        'matplotlib',
        'numpy'
    ],
    packages=['dinspect'],
    python_requires='>=3.6',
    entry_points={
        'console_scripts': [
            'dinspect = dinspect.__main__:main',
        ]
    }
)
