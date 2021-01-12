import argparse
import subprocess
import collections
import tempfile
import shutil
import copy
import os

try:
    import yaml
except ImportError:
    print('Failed to import yaml. Please install PyYAML (https://pyyaml.org) first.')

# Work around fortran-yaml's support for (and GOTM's common use of) unquoted * in yaml,
# which in the YAML specification defines an alias.
yaml.scanner.Scanner.fetch_alias = yaml.scanner.Scanner.fetch_plain
del yaml.loader.Loader.yaml_implicit_resolvers['*']
del yaml.loader.Loader.yaml_implicit_resolvers['o']
del yaml.loader.Loader.yaml_implicit_resolvers['O']
analyze_scalar_old = yaml.emitter.Emitter.analyze_scalar
def analyze_scalar(self, scalar):
    analysis = analyze_scalar_old(self, scalar)
    if scalar == '*':
        analysis.allow_flow_plain = analysis.allow_block_plain = True
    return analysis
yaml.emitter.Emitter.analyze_scalar = analyze_scalar

# Preserve order of dictionary keys when loading and then saving yaml
def dict_representer(dumper, data):
    return dumper.represent_mapping(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data.items())
def dict_constructor(loader, node):
    return collections.OrderedDict(loader.construct_pairs(node))
yaml.add_representer(collections.OrderedDict, dict_representer)
yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, dict_constructor, Loader=yaml.SafeLoader)

# Represent null as empty string for compatibility with fortran-yaml
def none_representer(self, data):
    return self.represent_scalar(u'tag:yaml.org,2002:null', u'')
yaml.add_representer(type(None), none_representer)

def resolve_node(root, path, create=False):
    components = path.split('/')
    name = components.pop()
    node = root
    for component in components:
        if component not in node:
            if not create:
                return {name: None}, name
            node[component] = collections.OrderedDict()
        node = node[component]
    return node, name

def get_node(root, path):
    node, name = resolve_node(root, path)
    return node[name]

def del_node(root, path):
    node, name = resolve_node(root, path)
    del node[name]

def update_node(root, path, value):
    node, name = resolve_node(root, path, create=True)
    if name in node and isinstance(node[name], dict):
        node[name].update(value)
    else:
        node[name] = value

def move_node(root, path, newpath):
    node, name = resolve_node(root, path)
    value = node[name]
    if value is not None:
        update_node(root, newpath, value)
    del node[name]

def update_yaml(oldroot):
    root = copy.deepcopy(oldroot)
    version = root.get('version', 5)
    if version == 5:
        move_node(root, 'temperature/analytical/z_t1', 'temperature/two_layer/z_s')
        move_node(root, 'temperature/analytical/t_1', 'temperature/two_layer/t_s')
        move_node(root, 'temperature/analytical/z_t2', 'temperature/two_layer/z_b')
        move_node(root, 'temperature/analytical/t_2', 'temperature/two_layer/t_b')
        move_node(root, 'temperature/analytical/obs_NN', 'temperature/NN')
        if get_node(root, 'temperature/method') == 1:
            update_node(root, 'temperature/method', 10 + get_node(root, 'temperature/analytical/method'))
        del_node(root, 'temperature/analytical')
        move_node(root, 'salinity/analytical/z_s1', 'salinity/two_layer/z_s')
        move_node(root, 'salinity/analytical/s_1', 'salinity/two_layer/s_s')
        move_node(root, 'salinity/analytical/z_s2', 'salinity/two_layer/z_b')
        move_node(root, 'salinity/analytical/s_2', 'salinity/two_layer/s_b')
        move_node(root, 'salinity/analytical/obs_NN', 'salinity/NN')
        if get_node(root, 'salinity/method') == 1:
            update_node(root, 'salinity/method', 10 + get_node(root, 'salinity/analytical/method'))
        del_node(root, 'salinity/analytical')
        move_node(root, 'surface/meteo', 'surface')
        move_node(root, 'bottom/MaxItz0b', 'bottom/max_it_z0b')
        move_node(root, 'turbulence/iw/model', 'turbulence/iw/method')
        move_node(root, 'mimic_3d/ext_pressure/mode', 'mimic_3d/ext_pressure/type')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/AmpM', 'mimic_3d/ext_pressure/dpdx/amp_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/PhaseM', 'mimic_3d/ext_pressure/dpdx/phase_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/AmpS', 'mimic_3d/ext_pressure/dpdx/amp_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/PhaseS', 'mimic_3d/ext_pressure/dpdx/phase_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/AmpM', 'mimic_3d/ext_pressure/dpdy/amp_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/PhaseM', 'mimic_3d/ext_pressure/dpdy/phase_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/AmpS', 'mimic_3d/ext_pressure/dpdy/amp_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/PhaseS', 'mimic_3d/ext_pressure/dpdy/phase_2')
        move_node(root, 'mimic_3d/ext_pressure/PeriodM', 'mimic_3d/ext_pressure/period_1')
        move_node(root, 'mimic_3d/ext_pressure/PeriodS', 'mimic_3d/ext_pressure/period_2')
        move_node(root, 'eq_state/method', 'eq_state/form')
        move_node(root, 'eq_state/mode', 'eq_state/method')
        move_node(root, 'eq_state/T0', 'eq_state/linear/T0')
        move_node(root, 'eq_state/S0', 'eq_state/linear/S0')
        move_node(root, 'eq_state/p0', 'eq_state/linear/p0')
        move_node(root, 'eq_state/dtr0', 'eq_state/linear/dtr0')
        move_node(root, 'eq_state/dsr0', 'eq_state/linear/dsr0')
    root['version'] = 6
    root.move_to_end('version', last=False)
    return root

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', nargs='?', help='Path to existing gotm.yaml configuration', default='gotm.yaml')
    parser.add_argument('--out', help='Path to updated gotm.yaml configuration file that will be written. Defaults to <in>.', default=None)
    parser.add_argument('--gotm', help='Path to GOTM executable. This will be called to add documentation to the yaml file.')
    parser.add_argument('--detail', choices=('minimal', 'default', 'full'), help='Level of detail of updated yaml file (which settings to include): minimal, default or full.', default='default')
    args = parser.parse_args()
    if args.out is None:
        print('No path for output file provided. %s will be updated in-place.' % args.infile)
        args.out = args.infile

    with open(args.infile) as f:
        settings = yaml.safe_load(f)

    settings = update_yaml(settings)
    with tempfile.NamedTemporaryFile('w', delete=False) as f:
        path = f.name
        yaml.dump(settings, f, default_flow_style=False, indent=2)

    if args.gotm is not None:
        subprocess.check_call([args.gotm, path, '--write_yaml', path, '--detail', args.detail], cwd=os.path.dirname(path))

    print('Writing updated %s...' % args.out, end='')
    shutil.copyfile(path, args.out)
    print(' Done.')