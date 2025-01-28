#!/usr/bin/env python3

from setuptools import setup, find_packages

setup(
    name='paw',
    version='0.1',
    packages=find_packages(),
    install_requires=[
        'Flask',
        'flask-cors',
        'requests',
        'nltk',
        'mecab-python3',  # or 'janome' if MeCab is not available
        'unidic-lite',
    ],
    entry_points={
        'console_scripts': [
            'paw=paw.cli:main',
        ],
    },
)
