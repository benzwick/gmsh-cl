#!/usr/bin/env python3
"""generate.py — Generate CFFI bindings and idiomatic CL wrappers for gmsh.

Reads the gmsh API definition from _reference/gmsh/api/gen.py (via GenApi.py)
and emits:
  src/generated/packages.lisp   — CL package definitions
  src/generated/bindings.lisp   — cffi:defcfun for all C functions
  src/generated/*.lisp          — per-package user-facing wrappers

Usage:
    python generate.py
"""

import sys
import os
import re
import textwrap

# ---------------------------------------------------------------------------
# Import the gmsh API definition
# ---------------------------------------------------------------------------

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '_reference', 'gmsh', 'api'))

# Stub out the writer methods so gen.py doesn't try to write C++/Python/etc.
from GenApi import API, Module, arg
for m in ['write_cpp', 'write_c', 'write_python', 'write_julia', 'write_fortran', 'write_texi']:
    setattr(API, m, lambda self: None)

# We need to provide a CMakeLists.txt for gen.py to read version from
# Create a minimal one if needed
cmake_path = os.path.join(os.path.dirname(__file__), '_reference', 'gmsh', 'CMakeLists.txt')
if not os.path.exists(cmake_path):
    os.makedirs(os.path.dirname(cmake_path), exist_ok=True)
    with open(cmake_path, 'w') as f:
        f.write('set(GMSH_MAJOR_VERSION 5)\nset(GMSH_MINOR_VERSION 0)\nset(GMSH_PATCH_VERSION 0)\n')

import gen
api = gen.api

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

OUTDIR = os.path.join(os.path.dirname(__file__), 'src', 'generated')
os.makedirs(OUTDIR, exist_ok=True)

# Module path → CL package mapping
# The module path is built by walking the module tree.
MODULE_TO_PACKAGE = {
    'gmsh':                   'gmsh',
    'gmsh/model':             'gmsh',
    'gmsh/model/geo':         'gmsh/geo',
    'gmsh/model/geo/mesh':    'gmsh/geo',
    'gmsh/model/occ':         'gmsh/occ',
    'gmsh/model/occ/mesh':    'gmsh/occ',
    'gmsh/model/mesh':        'gmsh/mesh',
    'gmsh/model/mesh/field':  'gmsh/mesh',
    'gmsh/option':            'gmsh/option',
    'gmsh/view':              'gmsh/view',
    'gmsh/view/option':       'gmsh/view',
    'gmsh/plugin':            'gmsh/plugin',
    'gmsh/graphics':          'gmsh',
    'gmsh/fltk':              'gmsh/fltk',
    'gmsh/onelab':            'gmsh/onelab',
    'gmsh/logger':            'gmsh/logger',
    'gmsh/parser':            'gmsh/parser',
    'gmsh/algorithm':         'gmsh/algorithm',
}

# Package nickname mapping
PACKAGE_NICKNAMES = {
    'gmsh':           [],
    'gmsh/geo':       ['geo'],
    'gmsh/occ':       ['occ'],
    'gmsh/mesh':      ['mesh'],
    'gmsh/view':      ['view'],
    'gmsh/option':    ['opt'],
    'gmsh/plugin':    ['plugin'],
    'gmsh/fltk':      ['fltk'],
    'gmsh/onelab':    ['onelab'],
    'gmsh/logger':    ['logger'],
    'gmsh/parser':    ['parser'],
    'gmsh/algorithm': ['algorithm'],
}

# Module path → output file mapping
PACKAGE_TO_FILE = {
    'gmsh':           'gmsh-functions.lisp',
    'gmsh/geo':       'geo-functions.lisp',
    'gmsh/occ':       'occ-functions.lisp',
    'gmsh/mesh':      'mesh-functions.lisp',
    'gmsh/view':      'view-functions.lisp',
    'gmsh/option':    'option-functions.lisp',
    'gmsh/plugin':    'plugin-functions.lisp',
    'gmsh/fltk':      'fltk-functions.lisp',
    'gmsh/onelab':    'onelab-functions.lisp',
    'gmsh/logger':    'logger-functions.lisp',
    'gmsh/parser':    'parser-functions.lisp',
    'gmsh/algorithm': 'algorithm-functions.lisp',
}

# Submodule prefixes for merged modules
SUBMODULE_PREFIXES = {
    'gmsh/model/geo/mesh':    'mesh-',
    'gmsh/model/occ/mesh':    'mesh-',
    'gmsh/model/mesh/field':  'field-',
    'gmsh/view/option':       'option-',
}

# Merged output files for sub-submodules
SUBMODULE_FILES = {
    'gmsh/model/geo/mesh':    'geo-mesh.lisp',
    'gmsh/model/occ/mesh':    'occ-mesh.lisp',
}

# Modules where "add" prefix is stripped (CAD constructors)
ADD_STRIP_MODULES = {
    'gmsh/model/geo',
    'gmsh/model/occ',
}

# ---------------------------------------------------------------------------
# Naming helpers
# ---------------------------------------------------------------------------

def camel_to_kebab(name):
    """Convert CamelCase/camelCase to kebab-case."""
    # Insert hyphens before uppercase letters that follow lowercase letters or digits
    s = re.sub(r'([a-z0-9])([A-Z])', r'\1-\2', name)
    # Insert hyphens between consecutive uppercase letters followed by lowercase
    s = re.sub(r'([A-Z]+)([A-Z][a-z])', r'\1-\2', s)
    # Handle transitions like "2D" -> keep as-is, "BSpline" -> "b-spline"
    return s.lower()


def cl_name(func_name, module_path):
    """Compute the CL symbol name for a function."""
    kebab = camel_to_kebab(func_name)

    # Strip "add-" prefix for constructors in geo/occ modules
    if module_path in ADD_STRIP_MODULES and kebab.startswith('add-'):
        kebab = kebab[4:]

    # Apply submodule prefix
    prefix = SUBMODULE_PREFIXES.get(module_path, '')
    if prefix:
        return prefix + kebab

    return kebab


def c_func_name(func_name, module_path):
    """Compute the C function name from the module path and function name.
    e.g., gmsh/model/geo + addPoint -> gmshModelGeoAddPoint"""
    parts = module_path.split('/')
    c_ns = parts[0]
    for p in parts[1:]:
        c_ns += p[0].upper() + p[1:]
    return c_ns + func_name[0].upper() + func_name[1:]


def internal_name(func_name, module_path):
    """Compute the %name for the internal cffi:defcfun."""
    parts = module_path.split('/')
    # Skip 'gmsh' prefix, use rest
    kebab_parts = [camel_to_kebab(p) for p in parts[1:]]
    fn_kebab = camel_to_kebab(func_name)
    if kebab_parts:
        return '%' + '-'.join(kebab_parts) + '-' + fn_kebab
    else:
        return '%' + fn_kebab

# ---------------------------------------------------------------------------
# Type classification helpers
# ---------------------------------------------------------------------------

def arg_type_name(a):
    """Return a string identifying the type of an arg object."""
    # Check the class name or the c_type to determine the arg type
    cls = type(a).__name__

    # Output types are classes
    if cls == 'oint':
        return 'oint'
    elif cls == 'osize':
        return 'osize'
    elif cls == 'odouble':
        return 'odouble'

    # Input types are created by factory functions, but we can identify them
    # by their c_type string
    c_type = a.c if hasattr(a, 'c') else ''

    # iargcargv special case
    if 'argc' in c_type and 'argv' in c_type:
        return 'iargcargv'

    # isizefun special case
    if 'double (*' in c_type and 'dim' in c_type:
        return 'isizefun'

    # Check if it's an output type by the a.out flag
    if a.out:
        # Output types identified by c signature
        if 'char ***' in c_type:
            return 'ovectorstring'
        elif 'int ***' in c_type and 'size_t **' in c_type:
            # Could be ovectorvectorint or ovectorvectorpair
            if 'pair' in a.texi_type:
                return 'ovectorvectorpair'
            else:
                return 'ovectorvectorint'
        elif 'size_t ***' in c_type:
            return 'ovectorvectorsize'
        elif 'double ***' in c_type:
            return 'ovectorvectordouble'
        elif 'char **' in c_type:
            return 'ostring'
        elif 'int **' in c_type:
            # ovectorint or ovectorpair
            if 'pair' in a.texi_type:
                return 'ovectorpair'
            else:
                return 'ovectorint'
        elif 'size_t **' in c_type:
            return 'ovectorsize'
        elif 'double **' in c_type:
            return 'ovectordouble'
        elif 'int *' in c_type:
            return 'oint'
        elif 'size_t *' in c_type:
            return 'osize'
        elif 'double *' in c_type:
            return 'odouble'
        else:
            return 'unknown_output'

    # Input types
    if 'const char * const *' in c_type and 'size_t' in c_type:
        if 'const char * const *' in c_type:
            return 'ivectorstring'
    if 'const int * const *' in c_type or ('const int *' in c_type and 'const size_t *' in c_type and 'const size_t ' in c_type and c_type.count('size_t') >= 2):
        # ivectorvectorint - has ptr, sizes, count
        if c_type.count(',') >= 2 and 'const int * const *' in c_type:
            return 'ivectorvectorint'
    if 'const size_t * const *' in c_type and c_type.count(',') >= 2:
        return 'ivectorvectorsize'
    if 'const double * const *' in c_type and c_type.count(',') >= 2:
        return 'ivectorvectordouble'
    if 'void *' in c_type:
        return 'ivoidstar'

    # Use texi_type for reliable identification
    texi = a.texi_type if hasattr(a, 'texi_type') else ''
    if texi == 'boolean':
        return 'ibool'
    elif texi == 'integer':
        return 'iint'
    elif texi == 'size':
        return 'isize'
    elif texi == 'double':
        return 'idouble'
    elif texi == 'string':
        return 'istring'
    elif texi == 'vector of integers':
        return 'ivectorint'
    elif texi == 'vector of sizes':
        return 'ivectorsize'
    elif texi == 'vector of doubles':
        return 'ivectordouble'
    elif texi == 'vector of strings':
        return 'ivectorstring'
    elif texi == 'vector of pairs of integers':
        return 'ivectorpair'
    elif texi == 'vector of vectors of integers':
        return 'ivectorvectorint'
    elif texi == 'vector of vectors of integers (size)':
        return 'ivectorvectorsize'
    elif texi == 'vector of vectors of doubles':
        return 'ivectorvectordouble'
    elif texi == 'pointer':
        return 'ivoidstar'
    elif texi == 'command line arguments':
        return 'iargcargv'
    elif texi == 'vector of vectors of pairs of integers':
        return 'ovectorvectorpair'

    return 'unknown'


def is_input(a):
    """True if the arg is an input parameter."""
    return not a.out


def is_output(a):
    """True if the arg is an output parameter."""
    return a.out


def has_default(a):
    """True if the arg has a default value."""
    return a.value is not None and a.value != ''

# ---------------------------------------------------------------------------
# CFFI type mapping for defcfun
# ---------------------------------------------------------------------------

def cffi_c_args(a, atype):
    """Return a list of (name, cffi-type) pairs for the C function signature."""
    name = camel_to_kebab(a.name) if a.name else a.name

    if atype == 'iargcargv':
        return [('argc', ':int'), ('argv', ':pointer')]
    elif atype == 'isizefun':
        return [(name, ':pointer'), (name + '-data', ':pointer')]
    elif atype in ('iint', 'ibool'):
        return [(name, ':int')]
    elif atype == 'isize':
        return [(name, ':unsigned-long')]
    elif atype == 'idouble':
        return [(name, ':double')]
    elif atype == 'istring':
        return [(name, ':string')]
    elif atype == 'ivoidstar':
        return [(name, ':pointer')]
    elif atype in ('ivectorint', 'ivectorpair'):
        return [(name, ':pointer'), (name + '-n', ':unsigned-long')]
    elif atype == 'ivectorsize':
        return [(name, ':pointer'), (name + '-n', ':unsigned-long')]
    elif atype == 'ivectordouble':
        return [(name, ':pointer'), (name + '-n', ':unsigned-long')]
    elif atype == 'ivectorstring':
        return [(name, ':pointer'), (name + '-n', ':unsigned-long')]
    elif atype in ('ivectorvectorint', 'ivectorvectorsize', 'ivectorvectordouble'):
        return [(name, ':pointer'), (name + '-n', ':pointer'), (name + '-nn', ':unsigned-long')]
    elif atype == 'oint':
        return [(name, '(:pointer :int)')]
    elif atype == 'osize':
        return [(name, '(:pointer :unsigned-long)')]
    elif atype == 'odouble':
        return [(name, '(:pointer :double)')]
    elif atype == 'ostring':
        return [(name, '(:pointer :pointer)')]
    elif atype in ('ovectorint', 'ovectorpair'):
        return [(name, '(:pointer :pointer)'), (name + '-n', '(:pointer :unsigned-long)')]
    elif atype == 'ovectorsize':
        return [(name, '(:pointer :pointer)'), (name + '-n', '(:pointer :unsigned-long)')]
    elif atype == 'ovectordouble':
        return [(name, '(:pointer :pointer)'), (name + '-n', '(:pointer :unsigned-long)')]
    elif atype == 'ovectorstring':
        return [(name, '(:pointer :pointer)'), (name + '-n', '(:pointer :unsigned-long)')]
    elif atype in ('ovectorvectorint', 'ovectorvectorsize', 'ovectorvectordouble', 'ovectorvectorpair'):
        return [(name, '(:pointer :pointer)'), (name + '-n', '(:pointer :pointer)'), (name + '-nn', '(:pointer :unsigned-long)')]
    else:
        return [(name, ':pointer')]  # fallback


def cffi_return_type(rtype):
    """Return the CFFI return type string."""
    if rtype is None:
        return ':void'
    cls = rtype.__name__ if isinstance(rtype, type) else type(rtype).__name__
    if cls == 'oint':
        return ':int'
    elif cls == 'osize':
        return ':unsigned-long'
    elif cls == 'odouble':
        return ':double'
    return ':void'

# ---------------------------------------------------------------------------
# Wrapper code generation
# ---------------------------------------------------------------------------

def gen_wrapper(func_name, module_path, rtype, args, doc, cl_sym):
    """Generate the CL wrapper function code."""
    atypes = [(a, arg_type_name(a)) for a in args]

    # Separate input and output args
    input_args = [(a, at) for a, at in atypes if is_input(a)]
    output_args = [(a, at) for a, at in atypes if is_output(a)]

    # Build CL parameter list
    required_params = []
    keyword_params = []

    for a, at in input_args:
        if at == 'iargcargv':
            continue  # We hardcode argc=0, argv=null
        name = camel_to_kebab(a.name)
        if has_default(a):
            default = default_value(a, at)
            keyword_params.append((name, default, a, at))
        else:
            required_params.append((name, a, at))

    # Build parameter string
    params = []
    for name, a, at in required_params:
        params.append(name)

    if keyword_params:
        params.append('&key')
        for name, default, a, at in keyword_params:
            params.append(f'({name} {default})')

    param_str = ' '.join(params)

    # Build the internal function call name
    int_name = internal_name(func_name, module_path)

    # Determine if this function returns a value from the C return type
    has_return = rtype is not None

    # Generate the body
    lines = []
    indent = '  '

    # Docstring
    doc_str = doc.replace('"', '\\"') if doc else ''
    # Wrap doc nicely
    doc_lines = textwrap.wrap(doc_str, 72)
    doc_formatted = '\\n'.join(doc_lines) if doc_lines else ''

    # Collect all the wrapping constructs we need
    # We'll build a nested structure of let/with-foreign-object forms

    # Pre-call bindings and forms
    pre_forms = []   # (indent_delta, open_form, close_needed)
    call_args = []   # strings for each C arg
    post_forms = []  # forms to execute after call
    return_values = []  # forms that produce return values

    for a, at in atypes:
        name = camel_to_kebab(a.name) if a.name else ''

        if at == 'iargcargv':
            call_args.append('0')
            call_args.append('(cffi:null-pointer)')

        elif at == 'isizefun':
            call_args.append(name)
            call_args.append('(cffi:null-pointer)')

        elif at == 'ibool':
            if is_input(a):
                call_args.append(f'(if {name} 1 0)')

        elif at == 'iint':
            call_args.append(name)

        elif at == 'isize':
            call_args.append(name)

        elif at == 'idouble':
            call_args.append(f'(to-double {name})')

        elif at == 'istring':
            call_args.append(name)

        elif at == 'ivoidstar':
            call_args.append(name)

        elif at == 'ivectorint':
            ptr_var = f'{name}-ptr'
            n_var = f'{name}-n'
            pre_forms.append(('with-foreign-array', ptr_var, n_var, name, ':int'))
            call_args.append(ptr_var)
            call_args.append(n_var)

        elif at == 'ivectorsize':
            ptr_var = f'{name}-ptr'
            n_var = f'{name}-n'
            pre_forms.append(('with-foreign-array', ptr_var, n_var, name, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)

        elif at == 'ivectordouble':
            ptr_var = f'{name}-ptr'
            n_var = f'{name}-n'
            pre_forms.append(('with-foreign-array', ptr_var, n_var, name, ':double'))
            call_args.append(ptr_var)
            call_args.append(n_var)

        elif at == 'ivectorstring':
            ptr_var = f'{name}-ptr'
            n_var = f'{name}-n'
            pre_forms.append(('with-string-array', ptr_var, n_var, name))
            call_args.append(ptr_var)
            call_args.append(n_var)

        elif at == 'ivectorpair':
            ptr_var = f'{name}-ptr'
            n_var = f'{name}-n'
            pre_forms.append(('with-pairs-array', ptr_var, n_var, name))
            call_args.append(ptr_var)
            call_args.append(n_var)

        elif at == 'ivectorvectorint':
            ptr_var = f'{name}-ptrs'
            sizes_var = f'{name}-sizes'
            nn_var = f'{name}-nn'
            pre_forms.append(('with-vector-vector-int', ptr_var, sizes_var, nn_var, name))
            call_args.append(ptr_var)
            call_args.append(sizes_var)
            call_args.append(nn_var)

        elif at == 'ivectorvectorsize':
            ptr_var = f'{name}-ptrs'
            sizes_var = f'{name}-sizes'
            nn_var = f'{name}-nn'
            pre_forms.append(('with-vector-vector-size', ptr_var, sizes_var, nn_var, name))
            call_args.append(ptr_var)
            call_args.append(sizes_var)
            call_args.append(nn_var)

        elif at == 'ivectorvectordouble':
            ptr_var = f'{name}-ptrs'
            sizes_var = f'{name}-sizes'
            nn_var = f'{name}-nn'
            pre_forms.append(('with-vector-vector-double', ptr_var, sizes_var, nn_var, name))
            call_args.append(ptr_var)
            call_args.append(sizes_var)
            call_args.append(nn_var)

        elif at == 'oint':
            # Output int - allocated inside with-ierr via cffi:with-foreign-objects
            # But for the return type, the C function returns it
            if not has_return or a.name != func_name:
                # This is an output parameter (not the C return value)
                var = f'{name}-out'
                pre_forms.append(('foreign-object', var, ':int'))
                call_args.append(var)
                return_values.append(f'(cffi:mem-ref {var} :int)')

        elif at == 'osize':
            var = f'{name}-out'
            pre_forms.append(('foreign-object', var, ':unsigned-long'))
            call_args.append(var)
            return_values.append(f'(cffi:mem-ref {var} :unsigned-long)')

        elif at == 'odouble':
            var = f'{name}-out'
            pre_forms.append(('foreign-object', var, ':double'))
            call_args.append(var)
            return_values.append(f'(cffi:mem-ref {var} :double)')

        elif at == 'ostring':
            var = f'{name}-out'
            pre_forms.append(('foreign-object', var, ':pointer'))
            call_args.append(var)
            return_values.append(f'(foreign-string-result (cffi:mem-ref {var} :pointer))')

        elif at == 'ovectorint':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            return_values.append(
                f'(foreign-array-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :unsigned-long) :int)')

        elif at == 'ovectorsize':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            return_values.append(
                f'(foreign-array-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :unsigned-long) :unsigned-long)')

        elif at == 'ovectordouble':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            return_values.append(
                f'(foreign-array-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :unsigned-long) :double)')

        elif at == 'ovectorstring':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            return_values.append(
                f'(foreign-string-array-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :unsigned-long))')

        elif at == 'ovectorpair':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            return_values.append(
                f'(dim-tags-to-pairs (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :unsigned-long))')

        elif at == 'ovectorvectorint':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            nn_var = f'{name}-nn-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':pointer'))
            pre_forms.append(('foreign-object', nn_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            call_args.append(nn_var)
            return_values.append(
                f'(foreign-vectors-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :pointer) (cffi:mem-ref {nn_var} :unsigned-long) :int)')

        elif at == 'ovectorvectorsize':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            nn_var = f'{name}-nn-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':pointer'))
            pre_forms.append(('foreign-object', nn_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            call_args.append(nn_var)
            return_values.append(
                f'(foreign-vectors-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :pointer) (cffi:mem-ref {nn_var} :unsigned-long) :unsigned-long)')

        elif at == 'ovectorvectordouble':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            nn_var = f'{name}-nn-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':pointer'))
            pre_forms.append(('foreign-object', nn_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            call_args.append(nn_var)
            return_values.append(
                f'(foreign-vectors-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :pointer) (cffi:mem-ref {nn_var} :unsigned-long) :double)')

        elif at == 'ovectorvectorpair':
            ptr_var = f'{name}-out'
            n_var = f'{name}-n-out'
            nn_var = f'{name}-nn-out'
            pre_forms.append(('foreign-object', ptr_var, ':pointer'))
            pre_forms.append(('foreign-object', n_var, ':pointer'))
            pre_forms.append(('foreign-object', nn_var, ':unsigned-long'))
            call_args.append(ptr_var)
            call_args.append(n_var)
            call_args.append(nn_var)
            return_values.append(
                f'(foreign-vector-pairs-to-list (cffi:mem-ref {ptr_var} :pointer) (cffi:mem-ref {n_var} :pointer) (cffi:mem-ref {nn_var} :unsigned-long))')

    # Now generate the actual code
    code = []
    code.append(f'(defun {cl_sym} ({param_str})')

    # Docstring
    if doc:
        safe_doc = doc.replace('\\', '\\\\').replace('"', '\\"')
        wrapped = textwrap.fill(safe_doc, 70)
        code.append(f'  "{wrapped}"')

    # Build the nested body
    # We need to nest: with-foreign-array forms, then with-ierr, then the call

    # Separate the pre_forms into macro-wrapping and foreign-object forms
    macro_forms = [f for f in pre_forms if f[0] in (
        'with-foreign-array', 'with-pairs-array', 'with-string-array',
        'with-vector-vector-int', 'with-vector-vector-double', 'with-vector-vector-size')]
    foreign_objects = [f for f in pre_forms if f[0] == 'foreign-object']

    # Build call expression
    call_args_str = ' '.join(call_args)
    call_expr = f'(gmsh/internal::{int_name} {call_args_str} ierr)'

    # Build the innermost body
    if has_return:
        # C function returns a value
        inner_body = f'(with-ierr (ierr)\n      {call_expr})'
    elif return_values:
        # Multiple output values
        inner_body = f'(with-ierr (ierr)\n      {call_expr})'
        if len(return_values) == 1:
            inner_body += f'\n    {return_values[0]}'
        else:
            inner_body += f'\n    (values {" ".join(return_values)})'
    else:
        # void, no outputs
        inner_body = f'(with-ierr (ierr)\n      {call_expr})'

    # Wrap with foreign-object forms
    if foreign_objects:
        # Use cffi:with-foreign-objects for multiple or cffi:with-foreign-object for single
        fo_bindings = []
        for fo in foreign_objects:
            _, var, ftype = fo
            fo_bindings.append(f'({var} {ftype})')

        if len(fo_bindings) == 1:
            wrapped = f'(cffi:with-foreign-object {fo_bindings[0]}\n      {inner_body}'
            if return_values:
                # Return values need to be computed inside the with-foreign-object
                # Restructure: put call inside, then collect outputs
                pass
            wrapped += ')'
        else:
            bindings_str = ' '.join(fo_bindings)
            wrapped = f'(cffi:with-foreign-objects ({bindings_str})\n      {inner_body})'
        inner_body = wrapped

    # Wrap with macro forms (outermost)
    for mf in reversed(macro_forms):
        if mf[0] == 'with-foreign-array':
            _, ptr, n, seq, etype = mf
            inner_body = f'(with-foreign-array ({ptr} {n} {seq} {etype})\n      {inner_body})'
        elif mf[0] == 'with-pairs-array':
            _, ptr, n, seq = mf
            inner_body = f'(with-pairs-array ({ptr} {n} {seq})\n      {inner_body})'
        elif mf[0] == 'with-string-array':
            _, ptr, n, seq = mf
            inner_body = f'(with-string-array ({ptr} {n} {seq})\n      {inner_body})'
        elif mf[0] == 'with-vector-vector-int':
            _, ptrs, sizes, nn, seq = mf
            inner_body = f'(with-vector-vector-int ({ptrs} {sizes} {nn} {seq})\n      {inner_body})'
        elif mf[0] == 'with-vector-vector-double':
            _, ptrs, sizes, nn, seq = mf
            inner_body = f'(with-vector-vector-double ({ptrs} {sizes} {nn} {seq})\n      {inner_body})'
        elif mf[0] == 'with-vector-vector-size':
            _, ptrs, sizes, nn, seq = mf
            inner_body = f'(with-vector-vector-size ({ptrs} {sizes} {nn} {seq})\n      {inner_body})'

    code.append(f'  {inner_body})')
    return '\n'.join(code)


def default_value(a, atype):
    """Return a CL default value string for an optional arg."""
    val = a.python_value if a.python_value else a.value
    if val is None:
        val = a.value

    if atype == 'ibool':
        if val in ('true', 'True', '.true.'):
            return 't'
        return 'nil'
    elif atype == 'iint':
        return str(val)
    elif atype == 'isize':
        return str(val)
    elif atype == 'idouble':
        v = str(val)
        # Handle simple numeric values first
        if v.endswith('.'):
            v = v + '0'
        try:
            f = float(v)
            # Format as CL double
            if '.' in v:
                return v
            else:
                return v + '.0'
        except ValueError:
            pass
        # Handle mathematical expressions involving pi
        # Use the python_value which has 'pi' instead of 'M_PI'
        v = str(val)
        v = v.replace('M_PI', 'pi')
        # Match patterns like: 2*pi, -pi/2, -M_PI/2
        import re as re2
        # "2*pi" -> (* 2 pi)
        m = re2.match(r'^(-?\d+)\*pi$', v)
        if m:
            return f'(* {m.group(1)} pi)'
        # "-pi/2" or "pi/2"
        m = re2.match(r'^(-?)pi/(\d+)$', v)
        if m:
            sign = m.group(1)
            denom = m.group(2)
            if sign:
                return f'(/ (- pi) {denom})'
            return f'(/ pi {denom})'
        # Just "pi" or "-pi"
        if v == 'pi':
            return 'pi'
        if v == '-pi':
            return '(- pi)'
        # Fallback: return as-is (may need manual fix)
        return v
    elif atype == 'istring':
        s = str(val).strip('"').strip("'")
        return f'"{s}"'
    elif atype in ('ivectorint', 'ivectorsize', 'ivectordouble', 'ivectorpair',
                    'ivectorstring', 'ivectorvectorint', 'ivectorvectorsize',
                    'ivectorvectordouble'):
        return "'()"
    return 'nil'


# ---------------------------------------------------------------------------
# Walk the module tree
# ---------------------------------------------------------------------------

def walk_modules(module, path=''):
    """Recursively walk the module tree, yielding (module, path) pairs."""
    if path:
        current = path + '/' + module.name
    else:
        current = module.name
    yield module, current
    for sub in module.submodules:
        yield from walk_modules(sub, current)


# Collect all functions grouped by CL package and output file
all_functions = {}  # { (cl_package, output_file): [(cl_name, func_data, module_path)] }
all_bindings = []   # [(c_name, internal_name, rtype, args, atypes)]
all_exports = {}    # { cl_package: set of cl_names }

for top_module in api.modules:
    for module, mod_path in walk_modules(top_module):
        cl_pkg = MODULE_TO_PACKAGE.get(mod_path)
        if cl_pkg is None:
            print(f"WARNING: No package mapping for module path: {mod_path}")
            continue

        # Determine output file
        if mod_path in SUBMODULE_FILES:
            out_file = SUBMODULE_FILES[mod_path]
        else:
            out_file = PACKAGE_TO_FILE.get(cl_pkg, f'{cl_pkg.replace("/", "-")}-functions.lisp')

        key = (cl_pkg, out_file)
        if key not in all_functions:
            all_functions[key] = []

        if cl_pkg not in all_exports:
            all_exports[cl_pkg] = set()

        for rtype, fname, args, doc, special in module.fs:
            # Compute names
            cl_sym = cl_name(fname, mod_path)
            c_name_str = c_func_name(fname, mod_path)
            int_name_str = internal_name(fname, mod_path)

            # Compute arg types
            atypes = [(a, arg_type_name(a)) for a in args]

            # Add to bindings
            all_bindings.append((c_name_str, int_name_str, rtype, args, atypes))

            # Add to functions
            all_functions[key].append((cl_sym, fname, mod_path, rtype, args, doc))

            # Add to exports
            all_exports[cl_pkg].add(cl_sym)

# ---------------------------------------------------------------------------
# Emit packages.lisp
# ---------------------------------------------------------------------------

print(f"Writing {os.path.join(OUTDIR, 'packages.lisp')}...")

with open(os.path.join(OUTDIR, 'packages.lisp'), 'w') as f:
    f.write(';;;; packages.lisp — Package definitions for gmsh-cl\n')
    f.write(';;;;\n')
    f.write(';;;; *** DO NOT EDIT — regenerate with: python generate.py ***\n\n')

    # gmsh/internal package first
    f.write('(defpackage :gmsh/internal\n')
    f.write('  (:use :cl)\n')
    # Export utility symbols that the generated wrappers use
    f.write('  (:export\n')
    f.write('   #:with-ierr\n')
    f.write('   #:to-double\n')
    f.write('   #:with-foreign-array\n')
    f.write('   #:with-pairs-array\n')
    f.write('   #:with-string-array\n')
    f.write('   #:with-vector-vector-int\n')
    f.write('   #:with-vector-vector-double\n')
    f.write('   #:with-vector-vector-size\n')
    f.write('   #:pairs-to-foreign\n')
    f.write('   #:foreign-array-to-list\n')
    f.write('   #:dim-tags-to-pairs\n')
    f.write('   #:foreign-string-result\n')
    f.write('   #:foreign-string-array-to-list\n')
    f.write('   #:foreign-vectors-to-list\n')
    f.write('   #:foreign-vector-pairs-to-list\n')
    f.write('   #:gmsh-error\n')
    f.write('   #:gmsh-error-code\n')
    f.write('   #:gmsh-error-message\n')
    f.write('   #:%free\n')
    f.write('   #:%malloc))\n\n')

    # Get the set of CL exported symbol names by asking SBCL
    import subprocess
    result = subprocess.run(
        ['sbcl', '--non-interactive', '--eval',
         '(do-external-symbols (s :cl) (write-line (string-downcase (symbol-name s))))'],
        capture_output=True, text=True)
    cl_symbols = set(result.stdout.strip().split('\n'))

    # Now each user-facing package
    # Order: gmsh first, then the rest alphabetically
    pkg_order = ['gmsh'] + sorted(p for p in all_exports if p != 'gmsh')
    for pkg in pkg_order:
        exports = sorted(all_exports[pkg])
        nicknames = PACKAGE_NICKNAMES.get(pkg, [])
        shadows = sorted(sym for sym in exports if sym in cl_symbols)

        f.write(f'(defpackage :{pkg}\n')
        f.write(f'  (:use :cl :gmsh/internal)\n')
        if nicknames:
            nick_str = ' '.join(f':{n}' for n in nicknames)
            f.write(f'  (:nicknames {nick_str})\n')
        if shadows:
            f.write(f'  (:shadow\n')
            for sym in shadows:
                f.write(f'   #:{sym}\n')
            f.write(f'  )\n')
        f.write(f'  (:export\n')
        for sym in exports:
            f.write(f'   #:{sym}\n')
        f.write(f'  ))\n\n')

# ---------------------------------------------------------------------------
# Emit bindings.lisp
# ---------------------------------------------------------------------------

print(f"Writing {os.path.join(OUTDIR, 'bindings.lisp')}...")

with open(os.path.join(OUTDIR, 'bindings.lisp'), 'w') as f:
    f.write(';;;; bindings.lisp — CFFI defcfun for all gmsh C functions\n')
    f.write(';;;;\n')
    f.write(';;;; *** DO NOT EDIT — regenerate with: python generate.py ***\n\n')
    f.write('(in-package :gmsh/internal)\n\n')

    for c_name_str, int_name_str, rtype, args, atypes in all_bindings:
        # Determine return type
        if rtype is not None:
            ret_type = cffi_return_type(rtype)
        else:
            ret_type = ':void'

        # Build arg list
        c_args = []
        for a, at in atypes:
            c_args.extend(cffi_c_args(a, at))
        # Always add ierr at the end
        c_args.append(('ierr', '(:pointer :int)'))

        f.write(f'(cffi:defcfun ("{c_name_str}" {int_name_str}) {ret_type}\n')
        for aname, atype in c_args:
            f.write(f'  ({aname} {atype})\n')
        f.write(')\n\n')

# ---------------------------------------------------------------------------
# Emit per-package wrapper files
# ---------------------------------------------------------------------------

for (cl_pkg, out_file), funcs in sorted(all_functions.items()):
    filepath = os.path.join(OUTDIR, out_file)
    print(f"Writing {filepath}...")

    with open(filepath, 'w') as f:
        f.write(f';;;; {out_file} — Generated wrappers for {cl_pkg}\n')
        f.write(';;;;\n')
        f.write(';;;; *** DO NOT EDIT — regenerate with: python generate.py ***\n\n')
        f.write(f'(in-package :{cl_pkg})\n\n')

        for cl_sym, fname, mod_path, rtype, args, doc in funcs:
            wrapper_code = gen_wrapper(fname, mod_path, rtype, args, doc, cl_sym)
            f.write(wrapper_code)
            f.write('\n\n')

# ---------------------------------------------------------------------------
# Write empty files for any missing output files
# ---------------------------------------------------------------------------
expected_files = set(PACKAGE_TO_FILE.values()) | set(SUBMODULE_FILES.values()) | {'graphics-functions.lisp'}
written_files = set(out_file for (_, out_file) in all_functions.keys())

for ef in expected_files:
    if ef not in written_files:
        filepath = os.path.join(OUTDIR, ef)
        if not os.path.exists(filepath):
            pkg = None
            for p, pf in PACKAGE_TO_FILE.items():
                if pf == ef:
                    pkg = p
                    break
            if pkg is None:
                pkg = 'gmsh'
            print(f"Writing empty {filepath}...")
            with open(filepath, 'w') as f:
                f.write(f';;;; {ef} — Generated wrappers\n')
                f.write(';;;;\n')
                f.write(';;;; *** DO NOT EDIT — regenerate with: python generate.py ***\n\n')
                f.write(f'(in-package :{pkg})\n')

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

n_bindings = len(all_bindings)
n_wrappers = sum(len(funcs) for funcs in all_functions.values())
n_packages = len(all_exports)

print(f"\nDone! Generated:")
print(f"  {n_bindings} CFFI bindings")
print(f"  {n_wrappers} wrapper functions")
print(f"  {n_packages} packages")
print(f"  Output directory: {OUTDIR}")
