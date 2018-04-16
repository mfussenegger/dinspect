from argparse import ArgumentParser
from dinspect import plot


def main():
    parser = ArgumentParser()
    subparsers = parser.add_subparsers()
    plot_parser = subparsers.add_parser('plot')
    plot_parser.set_defaults(func=plot.plot)
    plot_parser.add_argument(
        '--mode',
        choices=('lines', 'bar', 'pie', 'hist', 'scatter'),
    )
    plot_parser.add_argument('--verbose', action='count')
    plot_parser.add_argument('--title', type=str)

    parsed_args = parser.parse_args()
    args = {k: v for k, v in vars(parsed_args).items() if v and k != 'func'}
    parsed_args.func(**args)
