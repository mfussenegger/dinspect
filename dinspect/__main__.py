import sys
from argparse import ArgumentParser, FileType
from dinspect import plot, outliers, changepoints


def add_changepoints(subparsers):
    p = subparsers.add_parser('changepoints')
    p.set_defaults(func=changepoints.find_changepoints)
    p.add_argument(
        '--input', dest='lines', type=FileType('r'), default=sys.stdin)


def add_outliers(subparsers):
    reject = subparsers.add_parser('outliers-reject')
    reject.set_defaults(func=outliers.reject)
    reject.add_argument(
        '--input', dest='input_', type=FileType('r'), default=sys.stdin)


def add_plot(subparsers):
    plot_parser = subparsers.add_parser('plot')
    plot_parser.set_defaults(func=plot.plot)
    plot_parser.add_argument(
        '--mode',
        choices=plot.modes.keys(),
    )
    plot_parser.add_argument('--verbose', action='count')
    plot_parser.add_argument('--title', type=str)


def main():
    parser = ArgumentParser()
    subparsers = parser.add_subparsers()
    add_plot(subparsers)
    add_outliers(subparsers)
    add_changepoints(subparsers)

    parsed_args = parser.parse_args()
    args = {k: v for k, v in vars(parsed_args).items() if v and k != 'func'}
    parsed_args.func(**args)
