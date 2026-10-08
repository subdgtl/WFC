#!/usr/bin/env python3
"""Prepare supported peer-cloud requests without treating subagents as machines."""
import argparse
import json
import pathlib
import re
import subprocess
import sys

sys.dont_write_bytecode = True
ROOT = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))
import runtime


def cloud_request(request, provider, branch, environment=None):
    runtime.require(provider in ('codex', 'claude'), 'Unknown cloud provider')
    plan = request['plan']
    runtime.require(request['job'] == runtime.digest(plan), 'Invalid job request')
    runtime.require(re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*', plan['repository']),
                    'Cloud dispatch requires a GitHub repository name')
    runtime.require(branch and not branch.startswith('-') and not any(c.isspace() for c in branch),
                    'A published branch is required')
    runtime.require(provider != 'codex' or environment, 'Codex Cloud requires a configured environment identifier')
    payload = json.dumps(request, sort_keys=True)
    prompt = (
        'Run independent full verification only. Do not implement, merge, tag, deploy, '
        'change the check inventory, or silently skip a check. This is a new peer cloud task; '
        'subagents and processes inside its machine are not additional machines. '
        'Read the repository instructions and .agents/delivery/controller.md. '
        'Record the actual provider session/task identifier and provider environment evidence. '
        'Save the following JSON unchanged outside the source checkout as request.json:\n' + payload + '\n'
        'Fetch and create a clean detached verification checkout of exact commit ' + plan['commit'] + '. '
        'Run the committed .agents/delivery/runtime.py run-job with that request, platform '
        + request['platform'] + ', a real environment.json and a new external result directory. '
        'environment.json must record provider=' + provider + ', execution=peer-cloud, '
        'platform, worker_id, machine_id and evidence for this actual separate session. '
        'Use the runtime machine_fingerprint function for machine_id. An environment identifier '
        'is not a machine fingerprint. Record unavailable tools as failures. '
        'Return the ENTIRE result directory, including receipt, every hashed log, environment '
        'and artifact, through the provider supported artifact or branch interface. '
        'If using a return branch, copy only the evidence into delivery-results/'
        + request['job'] + '/' + request['platform'] + '/ on the session outcome branch, '
        'commit and push that branch, and report its exact commit and directory. '
        'Never put evidence on main. Report tested commit, outcome, session identifier, '
        'return branch/commit or artifact location. A final text summary is not gate evidence.'
    )
    if provider == 'codex':
        return {'schema': 1, 'provider': provider, 'request': request,
                'argv': ['codex', 'cloud', 'exec', '--env', environment, '--branch', branch, prompt],
                'prompt': prompt, 'model_selection': 'Confirm actual cloud model; local -c is not a verified cloud model selector',
                'status_argv': ['codex', 'cloud', 'status', '<returned-task-id>'],
                'return': 'Fetch the evidence branch and pass its entire result directory to accept-result'}
    return {'schema': 1, 'provider': provider, 'request': request,
            'connector': 'claude-code-remote', 'tool': 'create_session',
            'arguments': {'title': 'Verify ' + plan['repository'] + ' ' + plan['commit'][:12],
                          'source_url': 'https://github.com/' + plan['repository'],
                          'source_revision': branch,
                          'outcome_branch': 'verification-' + request['job'][:12] + '-' + request['platform'],
                          'model': 'claude-opus-5-5', 'prompt': prompt,
                          **({'environment_id': environment} if environment else {})},
            'cli_argv': ['claude', '--cloud', prompt],
            'cli_requirement': 'Run from a clean published repository branch; CLI may attach interactively',
            'status_tools': ['get_session', 'list_events'],
            'return': 'Fetch the evidence branch and pass its entire result directory to accept-result'}


def verify_source(source, descriptor, branch):
    plan = descriptor['request']['plan']
    status = runtime.git(source, 'status', '--porcelain', '--untracked-files=all')
    runtime.require(not status, 'Dispatch source must be clean; do not upload local edits')
    remote = runtime.git(source, 'remote', 'get-url', 'origin')
    parsed = re.fullmatch(r'(?:https://github\.com/|git@github\.com:)([^/]+/[^/]+?)(?:\.git)?/?', remote)
    runtime.require(parsed and parsed.group(1).lower() == plan['repository'].lower(), 'Repository remote differs from the requested repository')
    runtime.require(runtime.git(source, 'symbolic-ref', '--short', 'HEAD') == branch,
                    'Cloud command must run from the selected published branch')
    local_head = runtime.git(source, 'rev-parse', 'HEAD')
    advertised = runtime.git(source, 'ls-remote', '--exit-code', 'origin', 'refs/heads/' + branch).split()
    runtime.require(advertised and advertised[0] == local_head, 'Push this branch before cloud dispatch')


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--request', required=True)
    parser.add_argument('--provider', required=True, choices=['codex', 'claude'])
    parser.add_argument('--branch', required=True)
    parser.add_argument('--environment')
    parser.add_argument('--output', required=True)
    parser.add_argument('--submit-codex', action='store_true', help='Actually create a Codex cloud peer after explicit delivery start')
    parser.add_argument('--source', help='Clean published checkout required for submission')
    args = parser.parse_args(argv)
    request = runtime.read_json(args.request)
    descriptor = cloud_request(request, args.provider, args.branch, args.environment)
    runtime.write_json(args.output, descriptor)
    if args.submit_codex:
        runtime.require(args.provider == 'codex', 'Claude cloud creation uses the exposed connector; no invented CLI result adapter')
        runtime.require(args.source, 'A source checkout is required')
        verify_source(args.source, descriptor, args.branch)
        auth = subprocess.run(['codex', 'login', 'status'], capture_output=True, text=True, timeout=30)
        runtime.require(auth.returncode == 0 and 'ChatGPT' in auth.stdout + auth.stderr,
                        'Codex subscription sign-in is required; API keys are not a substitute')
        result = subprocess.run(descriptor['argv'], cwd=args.source, capture_output=True, text=True, timeout=120)
        descriptor['submission'] = {'returncode': result.returncode, 'stdout': result.stdout, 'stderr': result.stderr}
        runtime.write_json(args.output, descriptor)
        runtime.require(result.returncode == 0, 'Cloud creation failed; inspect retained submission evidence')
        print('Cloud submission accepted; retrieve its task identifier and machine evidence before accepting a gate result.')
    else:
        print('Prepared ' + args.provider + ' peer request. No cloud session or schedule was started.')
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (runtime.DeliveryError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
