#!/usr/bin/env python3
"""Synthetic regressions; no provider login, external service, or product data."""
import copy
import json
import os
from pathlib import Path
import signal
import platform
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
import runtime as delivery


class RuntimeTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(dir=Path(__file__).parent)
        self.root = Path(self.temporary.name)
        self.repo = self.root / "repository"
        self.repo.mkdir()
        self.git("init", "-b", "main")
        self.git("config", "user.name", "Synthetic Test")
        self.git("config", "user.email", "synthetic@example.invalid")
        self.git("config", "commit.gpgsign", "false")
        self.git("config", "core.hooksPath", "/dev/null")
        (self.repo / ".agents/delivery").mkdir(parents=True)
        (self.repo / ".gitignore").write_text("build/\n")
        (self.repo / "payload.txt").write_text("expected\n")
        (self.repo / "check.py").write_text(
            "from pathlib import Path\n"
            "assert Path('payload.txt').read_text() == 'expected\\n', 'payload differs'\n"
            "Path('build').mkdir(exist_ok=True)\n"
            "Path('build/output.txt').write_text(Path('payload.txt').read_text().upper())\n")
        self.policy = {
            "schema": 1, "repository": "synthetic/repository", "main_ref": "refs/heads/main",
            "inventory": ["check.py", "payload.txt"],
            "integration": {"budget_seconds": 300, "checks": [self.command("syntax")]},
            "verification": {"require_independent_machine": False, "platforms": {
                "linux": {"system": platform.system(), "checks": [self.command("full")], "artifacts": ["build/output.txt"],
                          "environment_probes": [{"id": "python", "argv": [sys.executable, "--version"]}]}}}}
        self.save_policy()
        self.commit()
        self.state = delivery.State(self.root / "state")
        self.environment = {"platform": "linux", "worker_id": "synthetic-worker", "machine_id": "synthetic-machine",
                            "execution": "local-worker"}

    def tearDown(self):
        self.state.db.close()
        self.temporary.cleanup()

    def git(self, *args):
        return delivery.git(self.repo, *args)

    def command(self, name, code=None):
        return {"id": name, "argv": [sys.executable, "check.py"] if code is None else [sys.executable, "-c", code]}

    def save_policy(self):
        delivery.write_json(self.repo / delivery.POLICY, self.policy)

    def commit(self, text="Synthetic fixture"):
        self.git("add", ".")
        self.git("commit", "-m", text)
        return self.git("rev-parse", "HEAD")

    def request(self, platform="linux"):
        self.state.enqueue(self.repo)
        return self.state.claim("synthetic/repository", platform)

    def run_and_accept(self, request=None, suffix="result", environment=None):
        request = request or self.request()
        output = self.root / suffix
        receipt = delivery.run_job(request, self.repo, environment or self.environment, output)
        accepted = self.state.accept(output)
        return receipt, accepted, output

    def test_real_success_receipt_binds_bytes_and_exact_commit(self):
        receipt, accepted, output = self.run_and_accept()
        self.assertEqual(accepted["status"], "passed")
        self.assertEqual((output / "artifacts/build/output.txt").read_text(), "EXPECTED\n")
        released = delivery.release_check(self.state, self.repo, "HEAD")
        self.assertEqual(released["tree"], self.git("rev-parse", "HEAD^{tree}"))
        self.assertEqual(self.state.accept(output)["status"], "unchanged")
        (self.repo / "new.txt").write_text("new source\n")
        self.commit()
        with self.assertRaisesRegex(delivery.DeliveryError, "Exact commit"):
            delivery.release_check(self.state, self.repo, "HEAD")

    def test_artifact_mutation_rejects_release(self):
        receipt, _, _ = self.run_and_accept()
        stored = self.state.directory / "results" / receipt["request"]["token"]
        (stored / "artifacts/build/output.txt").write_text("corrupted\n")
        with self.assertRaisesRegex(delivery.DeliveryError, "Artifact bytes differ"):
            delivery.release_check(self.state, self.repo, "HEAD")

    def test_symbolic_link_in_results_is_rejected_before_copying(self):
        request = self.request()
        output = self.root / "linked-result"
        delivery.run_job(request, self.repo, self.environment, output)
        (output / "external-directory").symlink_to(self.repo, target_is_directory=True)
        with self.assertRaisesRegex(delivery.DeliveryError, "symbolic links"):
            self.state.accept(output)

    def test_passed_commit_removed_from_main_cannot_release(self):
        passed = self.git("rev-parse", "HEAD")
        self.run_and_accept()
        self.git("checkout", "--orphan", "replacement-main")
        replacement = self.commit("Replacement main history")
        self.assertNotEqual(replacement, passed)
        self.git("update-ref", "refs/heads/main", self.git("rev-parse", "HEAD"))
        with self.assertRaisesRegex(delivery.DeliveryError, "main history"):
            delivery.release_check(self.state, self.repo, passed)

    def test_failed_control_creates_one_repair_per_check_across_commits(self):
        (self.repo / "payload.txt").write_text("broken\n")
        self.commit()
        _, accepted, _ = self.run_and_accept()
        self.assertEqual(accepted["status"], "failed")
        rows = self.state.db.execute("SELECT * FROM repairs").fetchall()
        self.assertEqual({row["check_id"] for row in rows}, {"syntax", "full"})
        self.state.db.execute("UPDATE repairs SET status='linked',issue='https://example.invalid/issues/1'")
        (self.repo / "extra.txt").write_text("next commit\n")
        self.commit()
        self.run_and_accept(suffix="second-result")
        rows = self.state.db.execute("SELECT * FROM repairs").fetchall()
        self.assertEqual(len(rows), 2)
        self.assertTrue(all(len(json.loads(row["occurrences"])) == 2 for row in rows))
        self.assertTrue(all(row["status"] == "linked" for row in rows))
        with self.assertRaises(delivery.DeliveryError):
            delivery.release_check(self.state, self.repo, "HEAD")

    def test_queue_finishes_current_and_coalesces_only_pending(self):
        first = self.request()
        (self.repo / "next.txt").write_text("second\n")
        second = self.commit()
        queued_second = self.state.enqueue(self.repo)
        (self.repo / "next.txt").write_text("third\n")
        third = self.commit()
        queued_third = self.state.enqueue(self.repo)
        status = self.state.db.execute("SELECT * FROM repositories").fetchone()
        self.assertEqual(status["running"], first["job"])
        self.assertEqual(status["pending"], queued_third["job"])
        self.assertEqual(self.state.db.execute("SELECT status FROM jobs WHERE id=?", (queued_second["job"],)).fetchone()[0], "superseded")
        self.git("checkout", "--detach", first["plan"]["commit"])
        self.run_and_accept(request=first)
        next_request = self.state.claim("synthetic/repository", "linux")
        self.assertEqual(next_request["plan"]["commit"], third)
        self.assertNotEqual(next_request["plan"]["commit"], second)

    def test_stale_and_divergent_main_do_not_replace_newer_pending(self):
        original = self.git("rev-parse", "HEAD")
        (self.repo / "next.txt").write_text("new\n")
        newest = self.commit()
        queued = self.state.enqueue(self.repo)
        self.git("update-ref", "refs/heads/main", original)
        self.assertEqual(self.state.enqueue(self.repo)["job"], queued["job"])
        self.git("reset", "--hard", original)
        (self.repo / "different.txt").write_text("divergent\n")
        self.commit()
        with self.assertRaisesRegex(delivery.DeliveryError, "diverged"):
            self.state.enqueue(self.repo)

    def test_all_platforms_required_and_workers_can_differ(self):
        self.policy["verification"]["platforms"]["macos"] = copy.deepcopy(self.policy["verification"]["platforms"]["linux"])
        self.save_policy()
        self.commit()
        request = self.request()
        _, accepted, _ = self.run_and_accept(request)
        self.assertEqual(accepted["status"], "running")
        with self.assertRaises(delivery.DeliveryError):
            delivery.release_check(self.state, self.repo, "HEAD")
        second = self.state.claim("synthetic/repository", "macos")
        self.run_and_accept(second, "other-platform", dict(self.environment, platform="macos", worker_id="other-worker"))
        self.assertEqual(delivery.release_check(self.state, self.repo, "HEAD")["status"], "passed")

    def test_shared_deadline_and_retry_do_not_reset_budget(self):
        self.policy["integration"]["checks"] = [self.command("first"), self.command("second")]
        self.save_policy()
        self.commit()
        clock = [0.0]
        deadlines = []
        def controlled_execute(check, checkout, log, deadline):
            deadlines.append(deadline)
            available = deadline - clock[0]
            elapsed = min(200, max(0, available))
            clock[0] += elapsed
            log.write_text("Synthetic executor boundary\n")
            return {"id": check["id"], "argv": check["argv"], "cwd": ".", "log": log.name,
                    "status": "passed" if elapsed == 200 else "timed-out",
                    "returncode": 0 if elapsed == 200 else None, "seconds": elapsed}
        with patch.object(delivery.time, "monotonic", side_effect=lambda: clock[0]), \
             patch.object(delivery, "execute", side_effect=controlled_execute):
            report = delivery.integration_checks(self.state, self.repo, "pull-1", "linux", self.root / "first")
            retried = delivery.integration_checks(self.state, self.repo, "pull-1", "linux", self.root / "retry")
        self.assertEqual(deadlines, [300, 300, 300])
        self.assertEqual([r["status"] for r in report["checks"]], ["passed", "timed-out"])
        self.assertEqual(report["status"], "incomplete")
        self.assertEqual(retried["checks"][0]["status"], "timed-out")
        self.assertEqual(retried["remaining_seconds"], 0)

    def test_real_timeout_stops_sleeping_command(self):
        command = self.command("timeout", "import time; time.sleep(60)")
        started = time.monotonic()
        result = delivery.execute(command, self.repo, self.root / "timeout.log", started + 3)
        self.assertEqual(result["status"], "timed-out")
        self.assertLess(time.monotonic() - started, 15)  # Deliberate runner timer contract.

    def test_split_reservation_requires_published_checkpoint_and_rejects_replay(self):
        reserved = delivery.reserve_integration(self.state, self.repo, "split", "linux")
        token = reserved["token"]
        with self.assertRaisesRegex(delivery.DeliveryError, "durably published"):
            delivery.integration_checks(self.state, self.repo, "split", "linux", self.root / "not-published", reservation=token)
        exported = self.root / "split-checkpoint"
        delivery.checkpoint_export(self.state, exported)
        delivery.confirm_reservation(self.state, token, exported, "Synthetic durable store receipt")
        result = delivery.integration_checks(self.state, self.repo, "split", "linux", self.root / "split-result", reservation=token)
        self.assertEqual(result["status"], "passed")
        with self.assertRaisesRegex(delivery.DeliveryError, "already been consumed"):
            delivery.integration_checks(self.state, self.repo, "split", "linux", self.root / "split-replay", reservation=token)

    def test_restored_split_reservation_is_consumed_even_before_first_launch(self):
        reserved = delivery.reserve_integration(self.state, self.repo, "lost-machine", "linux")
        exported = self.root / "reserved-checkpoint"
        delivery.checkpoint_export(self.state, exported)
        imported = delivery.State(self.root / "restored-reservation")
        try:
            delivery.checkpoint_import(imported, exported)
            with self.assertRaisesRegex(delivery.DeliveryError, "restored"):
                delivery.integration_checks(imported, self.repo, "lost-machine", "linux", self.root / "unsafe-retry", reservation=reserved["token"])
            with self.assertRaisesRegex(delivery.DeliveryError, "interrupted"):
                imported.reserve("synthetic/repository", "lost-machine", 300)
            with self.assertRaisesRegex(delivery.DeliveryError, "awaiting publication"):
                delivery.confirm_reservation(imported, reserved["token"], exported, "Late confirmation")
        finally:
            imported.db.close()

    def test_reservation_waiting_time_and_plan_binding(self):
        with patch.object(delivery.time, "monotonic", return_value=10):
            reserved = delivery.reserve_integration(self.state, self.repo, "waiting", "linux")
        exported = self.root / "waiting-checkpoint"
        delivery.checkpoint_export(self.state, exported)
        delivery.confirm_reservation(self.state, reserved["token"], exported, "Synthetic publication")
        with patch.object(delivery.time, "monotonic", return_value=311):
            result = delivery.integration_checks(self.state, self.repo, "waiting", "linux", self.root / "expired", reservation=reserved["token"])
        self.assertEqual(result["status"], "incomplete")
        self.assertEqual(result["remaining_seconds"], 0)

        other = delivery.reserve_integration(self.state, self.repo, "candidate-change", "linux")
        second_export = self.root / "candidate-checkpoint"
        delivery.checkpoint_export(self.state, second_export)
        delivery.confirm_reservation(self.state, other["token"], second_export, "Synthetic publication")
        (self.repo / "added.txt").write_text("different candidate\n")
        self.git("add", ".")
        with self.assertRaisesRegex(delivery.DeliveryError, "differs from candidate"):
            delivery.integration_checks(self.state, self.repo, "candidate-change", "linux", self.root / "changed-plan", reservation=other["token"])

    def test_budget_cap_and_interrupted_reservation(self):
        invalid = copy.deepcopy(self.policy)
        invalid["integration"]["budget_seconds"] = 301
        with self.assertRaises(delivery.DeliveryError):
            delivery.validate_policy(invalid)
        self.state.reserve("synthetic/repository", "interrupted", 300)
        reopened = delivery.State(self.state.directory)
        try:
            with self.assertRaisesRegex(delivery.DeliveryError, "interrupted"):
                reopened.reserve("synthetic/repository", "interrupted", 300)
        finally:
            reopened.db.close()

    def test_staged_candidate_is_bound_to_resulting_commit(self):
        (self.repo / "additional.txt").write_text("staged\n")
        self.git("add", ".")
        report_path = self.root / "integration"
        report = delivery.integration_checks(self.state, self.repo, "pull-candidate", "linux", report_path)
        self.assertEqual(report["status"], "passed")
        self.commit()
        self.assertEqual(delivery.integration_receipt_check(self.repo, "HEAD", report_path / "integration.json")["status"], "passed")
        (self.repo / "additional.txt").write_text("changed after verification\n")
        self.commit()
        with self.assertRaisesRegex(delivery.DeliveryError, "Committed tree differs"):
            delivery.integration_receipt_check(self.repo, "HEAD", report_path / "integration.json")

    def test_unstaged_candidate_and_missing_reviewed_plan_are_rejected(self):
        (self.repo / "payload.txt").write_text("unstaged\n")
        with self.assertRaisesRegex(delivery.DeliveryError, "Stage every"):
            delivery.integration_checks(self.state, self.repo, "pull-dirty", "linux", self.root / "dirty")
        self.git("checkout", "--", "payload.txt")
        self.policy["integration"]["requires_reviewed_plan"] = True
        self.save_policy()
        self.git("add", ".")
        with self.assertRaisesRegex(delivery.DeliveryError, "explicit reviewed"):
            delivery.integration_checks(self.state, self.repo, "pull-plan", "linux", self.root / "missing")
        selected = {"commit": self.git("rev-parse", "HEAD"), "tree": self.git("write-tree"),
                    "review_evidence": "Synthetic review", "checks": [self.command("selected")]}
        report = delivery.integration_checks(self.state, self.repo, "pull-plan", "linux", self.root / "selected", check_plan=selected)
        self.assertEqual(report["status"], "passed")
        self.assertEqual([check["id"] for check in report["checks"]], ["syntax", "selected"])
        selected["checks"] = [self.command("syntax", "print('replacement')")]
        with self.assertRaisesRegex(delivery.DeliveryError, "mandatory"):
            delivery.integration_checks(self.state, self.repo, "pull-plan", "linux", self.root / "replaced", check_plan=selected)

    def test_wrong_operating_system_is_rejected(self):
        self.policy["verification"]["platforms"]["linux"]["system"] = "Windows"
        self.save_policy()
        self.commit()
        with self.assertRaisesRegex(delivery.DeliveryError, "operating system"):
            delivery.run_job(self.request(), self.repo, self.environment, self.root / "wrong-system")

    def test_explicit_unavailable_exit_code_remains_nonpassing(self):
        command = self.command("unavailable", "import sys; sys.exit(3)")
        command["unavailable_exit_codes"] = [3]
        outcome = delivery.execute(command, self.repo, self.root / "unavailable.log", time.monotonic() + 180)
        self.assertEqual(outcome["status"], "unavailable")
        self.assertEqual(outcome["returncode"], 3)

    def test_source_mutation_during_full_verification_cannot_pass(self):
        self.policy["verification"]["platforms"]["linux"]["checks"] = [self.command("mutate", "from pathlib import Path; Path('payload.txt').write_text('changed')")]
        self.save_policy()
        self.commit()
        receipt, accepted, _ = self.run_and_accept()
        self.assertEqual(accepted["status"], "failed")
        self.assertIn("Tracked source files changed", receipt["errors"])
        self.assertIsNotNone(self.state.db.execute("SELECT 1 FROM repairs WHERE check_id='verification-integrity'").fetchone())

    def test_skip_output_is_failed_even_with_zero_exit(self):
        check = self.command("skipped", "print('SKIP: missing native dependency')")
        check["reject_output_patterns"] = [r"^SKIP:"]
        result = delivery.execute(check, self.repo, self.root / "skip.log", time.monotonic() + 180)
        self.assertEqual(result["returncode"], 0)
        self.assertEqual(result["status"], "failed")

    def test_child_process_left_running_is_detected_and_stopped(self):
        pid_path = self.root / "child.pid"
        code = ("import subprocess,sys; from pathlib import Path; "
                "p=subprocess.Popen([sys.executable,'-c','import time; time.sleep(20)']); "
                f"Path({str(pid_path)!r}).write_text(str(p.pid))")
        result = delivery.execute(self.command("leak", code), self.repo, self.root / "leak.log", time.monotonic() + 180)
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["error"], "Check left subprocesses running")
        child = int(pid_path.read_text())
        status = subprocess.run(["ps", "-p", str(child), "-o", "stat="], capture_output=True, text=True).stdout.strip()
        self.assertTrue(not status or status.startswith("Z"), status)

    def test_missing_inventory_empty_check_and_blocked_coverage_reject_pass(self):
        invalid = copy.deepcopy(self.policy)
        invalid["verification"]["platforms"]["linux"]["checks"] = []
        with self.assertRaises(delivery.DeliveryError):
            delivery.validate_policy(invalid)
        self.policy["verification"] = {"blocked_reason": "Native acceptance has no automated command", "platforms": {}}
        self.save_policy()
        self.commit()
        self.state.enqueue(self.repo)
        with self.assertRaisesRegex(delivery.DeliveryError, "blocked"):
            self.state.claim("synthetic/repository", "linux")
        report = delivery.integration_checks(self.state, self.repo, "pull-blocked", "linux", self.root / "blocked-short")
        self.assertEqual(report["status"], "passed")
        with self.assertRaises(delivery.DeliveryError):
            delivery.release_check(self.state, self.repo, "HEAD")

    def test_same_machine_is_rejected_when_independence_required(self):
        self.policy["verification"]["require_independent_machine"] = True
        self.save_policy()
        self.commit()
        request = self.request()
        with self.assertRaisesRegex(delivery.DeliveryError, "different machine"):
            delivery.run_job(request, self.repo, self.environment, self.root / "same-machine")
        with patch.object(delivery, "machine_fingerprint", return_value="synthetic-independent-machine"):
            receipt = delivery.run_job(request, self.repo, self.environment, self.root / "independent")
        self.assertTrue(receipt["environment"]["independent_machine"])

    def test_unsupported_dispatch_fails_without_launch(self):
        request = self.request()
        marker = self.root / "dispatch-called"
        capability = {"enabled": False, "provider": "codex", "fresh_machine": True,
                      "evidence": "Synthetic", "argv": [sys.executable, "-c", f"open({str(marker)!r},'w').close()"]}
        with self.assertRaisesRegex(delivery.DeliveryError, "disabled"):
            delivery.dispatch(request, capability, self.root / "dispatch")
        self.assertFalse(marker.exists())

    def test_fake_passing_receipt_missing_check_is_rejected(self):
        request = self.request()
        output = self.root / "forged"
        receipt = delivery.run_job(request, self.repo, self.environment, output)
        receipt["checks"].pop()
        receipt["receipt_hash"] = delivery.digest({k: v for k, v in receipt.items() if k != "receipt_hash"})
        delivery.write_json(output / "receipt.json", receipt)
        with self.assertRaisesRegex(delivery.DeliveryError, "coverage"):
            self.state.accept(output)

    def test_checkpoint_transports_receipts_and_reserved_budget(self):
        self.run_and_accept()
        self.state.reserve("synthetic/repository", "reserved", 300)
        exported = self.root / "checkpoint"
        delivery.checkpoint_export(self.state, exported)
        imported = delivery.State(self.root / "restored")
        try:
            delivery.checkpoint_import(imported, exported)
            self.assertEqual(delivery.release_check(imported, self.repo, "HEAD")["status"], "passed")
            with self.assertRaisesRegex(delivery.DeliveryError, "interrupted"):
                imported.reserve("synthetic/repository", "reserved", 300)
            with self.assertRaisesRegex(delivery.DeliveryError, "empty"):
                delivery.checkpoint_import(imported, exported)
        finally:
            imported.db.close()

    def test_abandoned_result_cannot_be_accepted(self):
        request = self.request()
        delivery.run_job(request, self.repo, self.environment, self.root / "abandoned-result")
        self.state.abandon(request["job"], "Synthetic worker recovery")
        with self.assertRaisesRegex(delivery.DeliveryError, "no longer running"):
            self.state.accept(self.root / "abandoned-result")

    def test_worker_clone_executes_without_mutating_source(self):
        self.state.enqueue(self.repo)
        before = self.git("status", "--porcelain")
        result = delivery.worker_once(self.state, self.repo, "synthetic/repository", "linux", self.environment, self.root / "work")
        self.assertEqual(result["status"], "passed")
        self.assertEqual(self.git("status", "--porcelain"), before)
        self.assertEqual(delivery.worker_once(self.state, self.repo, "synthetic/repository", "linux", self.environment, self.root / "work")["status"], "idle")


if __name__ == "__main__":
    unittest.main()
