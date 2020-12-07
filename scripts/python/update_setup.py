import argparse
import subprocess
import collections
import tempfile

try:
    import yaml
except ImportError:
    print('Failed to import yaml. Please install PyYAML (https://pyyaml.org) first.')

# Preserve order of dictionary keys when loading and then saving yaml
def dict_representer(dumper, data):
    return dumper.represent_mapping(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data.items())
def dict_constructor(loader, node):
    return collections.OrderedDict(loader.construct_pairs(node))
yaml.add_representer(collections.OrderedDict, dict_representer)
yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, dict_constructor, Loader=yaml.SafeLoader)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', help='Path to existing gotm.yaml configuration')
    parser.add_argument('outfile', nargs='?', help='Path to updated gotm.yaml configuration file that will be written. Defaults to <in>.', default=None)
    parser.add_argument('--gotm', help='Path to GOTM executable. This will be called to add documentation to the yaml file.')
    parser.add_argument('--detail', choices=('minimal', 'default', 'full'), help='Level of detail of updated yaml file (which settings to include): minimal, default or full.', default='default')
    args = parser.parse_args()
    if args.outfile is None:
        print('No path for output file provided. %s will be updated in-place.' % args.infile)
        args.outfile = args.infile

    with open(args.infile) as f:
        settings = yaml.safe_load(f)
    fd, path = tempfile.mkstemp()
    with tempfile.NamedTemporaryFile('w', delete=False) as f:
        yaml.dump(settings, f, default_flow_style=False, indent=2)

    if args.gotm is not None:
        subprocess.check_call([args.gotm, args.infile, '--write_yaml', args.outfile, '--detail', args.detail])
