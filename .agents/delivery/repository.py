#!/usr/bin/env python3
"""Validate delivery files; do not claim coverage for application changes."""
import argparse
import ast
import json
import os
import pathlib
import subprocess
import sys

sys.dont_write_bytecode = True
ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import runtime


def workflow():
    for path in pathlib.Path(__file__).resolve().parent.glob('*.py'):
        ast.parse(path.read_text(), filename=str(path))
    policy = runtime.validate_policy(json.loads((ROOT / '.agents/delivery/policy.json').read_text()))
    result = subprocess.run(['git', 'diff', '--check'], cwd=ROOT)
    if result.returncode:
        return result.returncode
    result = subprocess.run(['git', 'diff', '--cached', '--check'], cwd=ROOT)
    if result.returncode:
        return result.returncode
    print('Delivery syntax and policy valid for ' + policy['repository'] + '. This does not verify application behavior.')
    return 0


def choreography():
    import tempfile
    with tempfile.TemporaryDirectory(prefix='choreography-check-') as directory:
        target = pathlib.Path(directory) / 'choreography-test'
        subprocess.run(['clang++', '-std=c++11', '-Wall', '-Wextra', '-Werror',
                        '-fsanitize=address,undefined', 'tests/choreography_test.cpp', '-o', str(target)], cwd=ROOT, check=True)
        subprocess.run([str(target)], cwd=ROOT, check=True)
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['workflow', 'choreography', 'polarizacia-firmware'])
    args = parser.parse_args()
    if args.command == 'workflow':
        return workflow()
    if args.command == 'choreography':
        return choreography()
    library = pathlib.Path(os.environ.get('POLARIZACIA_FASTLED_PATH', ''))
    if not os.environ.get('POLARIZACIA_FASTLED_PATH') or not (library / 'library.properties').is_file():
        raise runtime.DeliveryError('Install FastLED 3.3.0 and set POLARIZACIA_FASTLED_PATH to its directory')
    if 'version=3.3.0' not in (library / 'library.properties').read_text().splitlines():
        raise runtime.DeliveryError('Full firmware verification requires FastLED 3.3.0')
    subprocess.run(['arduino-cli', 'compile', '--fqbn', 'arduino:avr:mega:cpu=atmega2560',
                    '--libraries', str(library), 'firmware/PolarizaciaCerberos'], cwd=ROOT, check=True)
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (runtime.DeliveryError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
