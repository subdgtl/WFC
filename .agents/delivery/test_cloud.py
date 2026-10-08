import contextlib
import io
import json
import pathlib
import tempfile
import unittest
from unittest.mock import patch

import cloud
import runtime


class CloudRequests(unittest.TestCase):
    def setUp(self):
        self.plan = {'repository': 'owner/repository', 'commit': 'a' * 40}
        self.request = {'plan': self.plan, 'job': runtime.digest(self.plan), 'platform': 'linux'}

    def test_codex_uses_new_cloud_task_and_requires_environment(self):
        with self.assertRaises(runtime.DeliveryError):
            cloud.cloud_request(self.request, 'codex', 'main')
        command = cloud.cloud_request(self.request, 'codex', 'main', 'configured-environment')
        self.assertEqual(command['argv'][:7], ['codex', 'cloud', 'exec', '--env', 'configured-environment', '--branch', 'main'])
        self.assertIn(self.plan['commit'], command['prompt'])
        self.assertIn('ENTIRE result directory', command['prompt'])

    def test_claude_uses_observed_peer_connector_without_credential_copy(self):
        descriptor = cloud.cloud_request(self.request, 'claude', 'main', 'configured-environment')
        args = descriptor['arguments']
        self.assertEqual(descriptor['tool'], 'create_session')
        self.assertEqual(args['model'], 'claude-opus-5-5')
        self.assertEqual(args['source_revision'], 'main')
        self.assertEqual(args['source_url'], 'https://github.com/owner/repository')
        self.assertFalse(any('token' in key or 'key' in key for key in args))

    def test_mutated_job_or_repository_or_branch_is_refused(self):
        invalid = dict(self.request, job='wrong')
        with self.assertRaises(runtime.DeliveryError):
            cloud.cloud_request(invalid, 'claude', 'main')
        for repo in ['../repository', 'owner/repository/extra', 'https://github.com/owner/repository']:
            plan = dict(self.plan, repository=repo)
            request = dict(self.request, plan=plan, job=runtime.digest(plan))
            with self.assertRaises(runtime.DeliveryError):
                cloud.cloud_request(request, 'claude', 'main')
        for branch in ['--help', '', 'main\n--bad']:
            with self.assertRaises(runtime.DeliveryError):
                cloud.cloud_request(self.request, 'claude', branch)

    def test_prepare_does_not_launch_any_provider(self):
        with tempfile.TemporaryDirectory() as directory:
            request = pathlib.Path(directory) / 'request.json'
            output = pathlib.Path(directory) / 'dispatch.json'
            request.write_text(json.dumps(self.request))
            with patch.object(cloud.subprocess, 'run', side_effect=AssertionError('provider launched')):
                with contextlib.redirect_stdout(io.StringIO()):
                    status = cloud.main(['--request', str(request), '--provider', 'claude', '--branch', 'main', '--output', str(output)])
            self.assertEqual(status, 0)
            self.assertEqual(json.loads(output.read_text())['provider'], 'claude')


if __name__ == '__main__':
    unittest.main()
