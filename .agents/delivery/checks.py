#!/usr/bin/env python3
"""Run delivery regression tests only when their code changes, and always in full verification."""
import argparse
import ast
import json
import pathlib
import subprocess
import sys

sys.dont_write_bytecode = True
HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
sys.path.insert(0, str(HERE))
import runtime


def changed_delivery_files(root, reference):
    runtime.git(root, 'rev-parse', '--verify', reference + '^{commit}')
    # The index is the candidate tested by integration-checks, not an old HEAD.
    output = runtime.git(root, 'diff', '--cached', '--name-only', reference, '--', '.agents/delivery')
    return output.splitlines()


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['focused-runtime', 'full-runtime'])
    args = parser.parse_args(argv)
    policy = runtime.validate_policy(json.loads((HERE / 'policy.json').read_text()))
    for path in HERE.glob('*.py'):
        ast.parse(path.read_text(), filename=str(path))
    changed = changed_delivery_files(ROOT, policy['main_ref']) if args.command == 'focused-runtime' else None
    if changed == []:
        print('Delivery code matches integrated main; its regression suite remains in independent full verification.')
        return 0
    print('Delivery regression tests: ' + ('complete full profile' if changed is None else ', '.join(changed)))
    return subprocess.run([sys.executable, '-B', '-m', 'unittest', 'discover', '-s', str(HERE), '-p', 'test_*.py'], cwd=ROOT).returncode


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (runtime.DeliveryError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
