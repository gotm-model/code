#!/usr/bin/env python

import argparse
import subprocess
import collections
import tempfile
import shutil
import copy
import os
import glob
import sys

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

def get_node(root, path, default=None):
    node, name = resolve_node(root, path)
    return node.get(name, default)

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
    value = node.get(name, None)
    if value is not None:
        update_node(root, newpath, value)
        del node[name]

def update_yaml(oldroot):
    root = copy.deepcopy(oldroot)
    version = root.get('version', 5)
    if version == 5:
        update_node(root, 'temperature/constant_value', get_node(root, 'temperature/analytical/t_1', default=0.))
        update_node(root, 'salinity/constant_value', get_node(root, 'salinity/analytical/s_1', default=0.))
        move_node(root, 'temperature/analytical/z_t1', 'temperature/two_layer/z_s')
        move_node(root, 'temperature/analytical/t_1', 'temperature/two_layer/t_s')
        move_node(root, 'temperature/analytical/z_t2', 'temperature/two_layer/z_b')
        move_node(root, 'temperature/analytical/t_2', 'temperature/two_layer/t_b')
        move_node(root, 'temperature/analytical/obs_NN', 'temperature/NN')
        if get_node(root, 'temperature/method') == 1:
            update_node(root, 'temperature/method', 10 + get_node(root, 'temperature/analytical/method', default=1))
        del_node(root, 'temperature/analytical')
        move_node(root, 'salinity/analytical/z_s1', 'salinity/two_layer/z_s')
        move_node(root, 'salinity/analytical/s_1', 'salinity/two_layer/s_s')
        move_node(root, 'salinity/analytical/z_s2', 'salinity/two_layer/z_b')
        move_node(root, 'salinity/analytical/s_2', 'salinity/two_layer/s_b')
        move_node(root, 'salinity/analytical/obs_NN', 'salinity/NN')
        if get_node(root, 'salinity/method') == 1:
            update_node(root, 'salinity/method', 10 + get_node(root, 'salinity/analytical/method', default=1))
        del_node(root, 'salinity/analytical')
        move_node(root, 'surface/meteo', 'surface')
        move_node(root, 'surface/waves', 'waves')
        move_node(root, 'bottom/MaxItz0b', 'bottom/max_it_z0b')
        move_node(root, 'turbulence/iw/model', 'turbulence/iw/method')
        move_node(root, 'mimic_3d/ext_pressure/mode', 'mimic_3d/ext_pressure/type')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/AmpM', 'mimic_3d/ext_pressure/dpdx/tidal/amp_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/PhaseM', 'mimic_3d/ext_pressure/dpdx/tidal/phase_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/AmpS', 'mimic_3d/ext_pressure/dpdx/tidal/amp_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdx/PhaseS', 'mimic_3d/ext_pressure/dpdx/tidal/phase_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/AmpM', 'mimic_3d/ext_pressure/dpdy/tidal/amp_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/PhaseM', 'mimic_3d/ext_pressure/dpdy/tidal/phase_1')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/AmpS', 'mimic_3d/ext_pressure/dpdy/tidal/amp_2')
        move_node(root, 'mimic_3d/ext_pressure/dpdy/PhaseS', 'mimic_3d/ext_pressure/dpdy/tidal/phase_2')
        move_node(root, 'mimic_3d/ext_pressure/PeriodM', 'mimic_3d/ext_pressure/period_1')
        move_node(root, 'mimic_3d/ext_pressure/PeriodS', 'mimic_3d/ext_pressure/period_2')
        move_node(root, 'mimic_3d/zeta/period_1', 'mimic_3d/zeta/tidal/period_1')
        move_node(root, 'mimic_3d/zeta/amp_1', 'mimic_3d/zeta/tidal/amp_1')
        move_node(root, 'mimic_3d/zeta/phase_1', 'mimic_3d/zeta/tidal/phase_1')
        move_node(root, 'mimic_3d/zeta/period_2', 'mimic_3d/zeta/tidal/period_2')
        move_node(root, 'mimic_3d/zeta/amp_2', 'mimic_3d/zeta/tidal/amp_2')
        move_node(root, 'mimic_3d/zeta/phase_2', 'mimic_3d/zeta/tidal/phase_2')
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

def process_file(infile, outfile=None, gotm=None, detail='default'):
    if outfile is None:
        outfile = infile

    with open(infile) as f:
        settings = yaml.safe_load(f)

    print('Processing %s...' % infile)
    print('- Updating configuration to latest version...', end='', flush=True)
    settings = update_yaml(settings)
    print(' Done.')

    with tempfile.TemporaryDirectory() as tmpdir:
        path = os.path.join(tmpdir, 'gotm.yaml')
        with open(path, 'w') as f:
            yaml.dump(settings, f, default_flow_style=False, indent=2)

        fabm_yaml = os.path.join(os.path.dirname(infile), 'fabm.yaml')
        if os.path.isfile(fabm_yaml):
            shutil.copyfile(fabm_yaml, os.path.join(tmpdir, 'fabm.yaml'))

        if gotm is not None:
            print('- Calling GOTM to clean-up yaml file...', end='', flush=True)
            try:
                subprocess.check_output([gotm, '--write_yaml', path, '--detail', detail], cwd=tmpdir, stderr=subprocess.STDOUT, universal_newlines=True)
                print(' Done.')
            except subprocess.CalledProcessError as e:
                print('FAILED:\n%s' % e.stdout)
                return False

        print('- Writing updated %s...' % outfile, end='', flush=True)
        shutil.copyfile(path, outfile)
        print(' Done.')
        return True

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', nargs='+', help='Path to existing gotm.yaml configuration', default='gotm.yaml')
    parser.add_argument('--out', help='Path to updated gotm.yaml configuration file that will be written. Defaults to <in>.', default=None)
    parser.add_argument('--gotm', help='Path to GOTM executable. This will be called to add documentation to the yaml file.')
    parser.add_argument('--detail', choices=('minimal', 'default', 'full'), help='Level of detail of updated yaml file (which settings to include): minimal, default or full.', default='default')
    args = parser.parse_args()
    infiles = []
    for name in args.infile:
        infiles.extend(glob.glob(name))
    if args.out is None:
        print('No path for output file provided (--out). Configurations will be updated in-place.')
    elif len(infiles) > 1:
        print('Path for output file cannot be provided if called in batch mode (multiple input files).')
        sys.exit(2)

    for infile in infiles:
        process_file(infile, outfile=args.out, gotm=args.gotm, detail=args.detail)