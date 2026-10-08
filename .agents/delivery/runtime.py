#!/usr/bin/env python3
"""Durable verification beside an existing delivery controller. Python 3.10+."""
from __future__ import annotations

import argparse
import contextlib
import fnmatch
import hashlib
import json
import math
import os
from pathlib import Path
import platform as system_platform
import re
import shutil
import signal
import sqlite3
import subprocess
import sys
import time
import uuid

SCHEMA = 1
MAX_BUDGET = 300.0
POLICY = ".agents/delivery/policy.json"
NAME = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,99}$")


class DeliveryError(Exception):
    """An operation could not establish its required evidence."""


def require(condition, message):
    if not condition:
        raise DeliveryError(message)


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()


def digest(value):
    return hashlib.sha256(canonical(value)).hexdigest()


def file_hash(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def write_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + "." + str(uuid.uuid4()) + ".tmp")
    with temporary.open("xb") as stream:
        stream.write(canonical(value) + b"\n")
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)


def relative(value):
    require(isinstance(value, str) and value and "\\" not in value,
            "Paths must be nonempty repository-relative POSIX paths")
    path = Path(value)
    require(not path.is_absolute() and ".." not in path.parts, "Path escapes its root")
    return value


def inside(root, value):
    root = Path(root).resolve()
    path = root / relative(value)
    require(path.resolve().is_relative_to(root), "Path resolves outside its root")
    return path


def git(repository, *arguments):
    result = subprocess.run(["git", "-C", str(repository), *arguments],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
    require(result.returncode == 0, result.stderr.decode(errors="replace").strip())
    return result.stdout.decode("utf-8", errors="strict").strip()


def ancestor(repository, older, newer):
    result = subprocess.run(["git", "-C", str(repository), "merge-base", "--is-ancestor",
                             older, newer], capture_output=True, check=False)
    require(result.returncode in (0, 1), result.stderr.decode(errors="replace"))
    return result.returncode == 0


def number(value, maximum=None):
    require(isinstance(value, (int, float)) and not isinstance(value, bool)
            and math.isfinite(value) and value > 0, "Time limits must be finite and positive")
    require(maximum is None or value <= maximum, "Integration budget exceeds 300 seconds")
    return float(value)


def check_definition(check):
    require(isinstance(check, dict) and NAME.fullmatch(check.get("id", "")), "Invalid check id")
    require(isinstance(check.get("argv"), list) and check["argv"]
            and all(isinstance(x, str) and x and "\x00" not in x for x in check["argv"]),
            "Checks require nonempty argv arrays")
    relative(check.get("cwd", "."))
    if "timeout_seconds" in check:
        number(check["timeout_seconds"])
    for pattern in check.get("reject_output_patterns", []):
        require(isinstance(pattern, str) and pattern, "Output rejection patterns must be nonempty strings")
        re.compile(pattern)
    require(all(isinstance(code, int) and not isinstance(code, bool) and code != 0
                for code in check.get("unavailable_exit_codes", [])), "Unavailable exit codes must be nonzero integers")


def validate_policy(policy):
    require(policy.get("schema") == SCHEMA, "Unsupported policy schema")
    require(isinstance(policy.get("repository"), str) and policy["repository"], "Missing repository")
    require(isinstance(policy.get("main_ref"), str) and policy["main_ref"].startswith("refs/"),
            "main_ref must be a full Git reference")
    require(isinstance(policy.get("inventory"), list) and policy["inventory"], "Inventory is empty")
    for path in policy["inventory"]:
        relative(path)
    integration = policy.get("integration", {})
    number(integration.get("budget_seconds"), MAX_BUDGET)
    require(isinstance(integration.get("checks"), list), "Missing integration checks")
    platforms = policy.get("verification", {}).get("platforms", {})
    blocked = policy.get("verification", {}).get("blocked_reason")
    require(isinstance(platforms, dict) and (platforms or isinstance(blocked, str) and blocked.strip()),
            "Full verification needs platform coverage or an explicit blocked_reason")
    for name, settings in platforms.items():
        require(NAME.fullmatch(name), "Invalid platform name")
        require(isinstance(settings.get("checks"), list) and (settings["checks"] or settings.get("blocked_reason")),
                "Each platform requires explicit full checks")
        checks = checks_for(policy, name)
        require(len({c["id"] for c in checks}) == len(checks), "Duplicate check id")
        for check in checks + settings.get("environment_probes", []):
            check_definition(check)
        for pattern in settings.get("artifacts", []):
            relative(pattern)
    for check in integration["checks"]:
        check_definition(check)
        if "platforms" in check:
            require(isinstance(check["platforms"], list) and check["platforms"]
                    and (blocked or set(check["platforms"]) <= set(platforms)), "Unknown check platform")
    return policy


def checks_for(policy, platform, full=True):
    checks = [c for c in policy["integration"]["checks"]
              if platform in c.get("platforms", [platform])]
    return checks + (policy["verification"]["platforms"][platform]["checks"] if full else [])


def plan(repository, commit="HEAD", policy_path=POLICY, candidate=False):
    relative(policy_path)
    commit = git(repository, "rev-parse", "--verify", commit + "^{commit}")
    tree = git(repository, "write-tree") if candidate else git(repository, "rev-parse", commit + "^{tree}")
    policy = validate_policy(json.loads(git(repository, "show", tree + ":" + policy_path)))
    entries = []
    for line in git(repository, "ls-tree", "-r", tree).splitlines():
        metadata, path = line.split("\t", 1)
        mode, kind, object_id = metadata.split()
        entries.append({"path": path, "mode": mode, "kind": kind, "object": object_id})
    selected = {}
    for pattern in [policy_path, *policy["inventory"]]:
        matches = [entry for entry in entries if fnmatch.fnmatchcase(entry["path"], pattern)]
        require(matches, "Inventory pattern matched no tracked files: " + pattern)
        selected.update({entry["path"]: entry for entry in matches})
    inventory = [selected[path] for path in sorted(selected)]
    return {"schema": SCHEMA, "repository": policy["repository"], "commit": commit,
            "tree": tree, "policy_path": policy_path, "policy": policy,
            "policy_hash": digest(policy), "inventory": inventory,
            "inventory_hash": digest(inventory)}


class State:
    def __init__(self, directory):
        self.directory = Path(directory).resolve()
        self.directory.mkdir(parents=True, exist_ok=True)
        self.db = sqlite3.connect(self.directory / "state.sqlite3", timeout=30, isolation_level=None)
        self.db.row_factory = sqlite3.Row
        self.db.execute("PRAGMA journal_mode=WAL")
        self.db.execute("PRAGMA synchronous=FULL")
        self.db.executescript("""
          CREATE TABLE IF NOT EXISTS repositories(name TEXT PRIMARY KEY, running TEXT, pending TEXT, latest TEXT);
          CREATE TABLE IF NOT EXISTS jobs(id TEXT PRIMARY KEY, repository TEXT, plan TEXT, status TEXT);
          CREATE TABLE IF NOT EXISTS claims(token TEXT PRIMARY KEY, job TEXT, platform TEXT,
            request TEXT, result TEXT, status TEXT, UNIQUE(job, platform));
          CREATE TABLE IF NOT EXISTS budgets(repository TEXT, integration TEXT, remaining REAL,
            active TEXT, PRIMARY KEY(repository,integration));
          CREATE TABLE IF NOT EXISTS repairs(key TEXT PRIMARY KEY, repository TEXT, check_id TEXT,
            platform TEXT, status TEXT, issue TEXT, occurrences TEXT);
          CREATE TABLE IF NOT EXISTS metadata(key TEXT PRIMARY KEY,value TEXT);
          CREATE TABLE IF NOT EXISTS reservations(token TEXT PRIMARY KEY,payload TEXT,status TEXT,
            checkpoint_hash TEXT,publication_evidence TEXT);
        """)
        self.db.execute("INSERT OR IGNORE INTO metadata VALUES ('controller_machine',?)", (machine_fingerprint(),))

    @contextlib.contextmanager
    def transaction(self):
        self.db.execute("BEGIN IMMEDIATE")
        try:
            yield
            self.db.execute("COMMIT")
        except BaseException:
            self.db.execute("ROLLBACK")
            raise

    def enqueue(self, repository, policy_path=POLICY):
        preliminary = validate_policy(read_json(Path(repository) / relative(policy_path)))
        candidate = plan(repository, preliminary["main_ref"], policy_path)
        require(candidate["policy"]["main_ref"] == preliminary["main_ref"],
                "Working policy main_ref differs from committed policy")
        name = candidate["repository"]
        with self.transaction():
            self.db.execute("INSERT OR IGNORE INTO repositories(name) VALUES (?)", (name,))
            row = self.db.execute("SELECT * FROM repositories WHERE name=?", (name,)).fetchone()
            latest_id = row["pending"] or row["running"] or row["latest"]
            if latest_id:
                latest = json.loads(self.db.execute("SELECT plan FROM jobs WHERE id=?", (latest_id,)).fetchone()[0])
                if ancestor(repository, candidate["commit"], latest["commit"]):
                    return {"status": "unchanged", "job": latest_id}
                require(ancestor(repository, latest["commit"], candidate["commit"]),
                        "Main history diverged; reconcile the queue explicitly")
            identity = digest(candidate)
            prior = self.db.execute("SELECT status FROM jobs WHERE id=?", (identity,)).fetchone()
            if prior:
                return {"status": prior["status"], "job": identity}
            if row["pending"]:
                self.db.execute("UPDATE jobs SET status='superseded' WHERE id=?", (row["pending"],))
            self.db.execute("INSERT INTO jobs VALUES (?,?,?,?)", (identity, name, canonical(candidate).decode(), "pending"))
            self.db.execute("UPDATE repositories SET pending=?,latest=? WHERE name=?", (identity, identity, name))
            return {"status": "pending", "job": identity, "commit": candidate["commit"]}

    def claim(self, repository, platform):
        with self.transaction():
            row = self.db.execute("SELECT * FROM repositories WHERE name=?", (repository,)).fetchone()
            require(row is not None, "Repository has no queued verification")
            job_id = row["running"] or row["pending"]
            if not job_id:
                return None
            job = self.db.execute("SELECT * FROM jobs WHERE id=?", (job_id,)).fetchone()
            snapshot = json.loads(job["plan"])
            require(not snapshot["policy"]["verification"].get("blocked_reason"),
                    "Verification is blocked: " + str(snapshot["policy"]["verification"].get("blocked_reason")))
            require(platform in snapshot["policy"]["verification"]["platforms"], "Platform is not required")
            require(not snapshot["policy"]["verification"]["platforms"][platform].get("blocked_reason"),
                    "Verification is blocked for this platform")
            if self.db.execute("SELECT 1 FROM claims WHERE job=? AND platform=?", (job_id, platform)).fetchone():
                return None
            if not row["running"]:
                self.db.execute("UPDATE repositories SET running=?,pending=NULL WHERE name=?", (job_id, repository))
                self.db.execute("UPDATE jobs SET status='running' WHERE id=?", (job_id,))
            token = str(uuid.uuid4())
            controller = self.db.execute("SELECT value FROM metadata WHERE key='controller_machine'").fetchone()[0]
            request = {"schema": SCHEMA, "job": job_id, "token": token, "platform": platform,
                       "plan": snapshot, "controller_machine": controller}
            self.db.execute("INSERT INTO claims VALUES (?,?,?,?,NULL,'running')",
                            (token, job_id, platform, canonical(request).decode()))
            return request

    def reserve(self, repository, integration, limit):
        with self.transaction():
            self.db.execute("INSERT OR IGNORE INTO budgets VALUES (?,?,?,NULL)", (repository, integration, limit))
            row = self.db.execute("SELECT * FROM budgets WHERE repository=? AND integration=?", (repository, integration)).fetchone()
            require(row["active"] is None, "Integration check is running or interrupted; budget remains reserved")
            available = min(row["remaining"], limit)
            token = str(uuid.uuid4())
            self.db.execute("UPDATE budgets SET remaining=0,active=? WHERE repository=? AND integration=?",
                            (token, repository, integration))
            return token, available

    def settle(self, repository, integration, token, remaining):
        with self.transaction():
            cursor = self.db.execute("UPDATE budgets SET remaining=?,active=NULL WHERE repository=? AND integration=? AND active=?",
                                     (max(0, remaining), repository, integration, token))
            require(cursor.rowcount == 1, "Budget reservation changed")

    def accept(self, result_directory):
        receipt = validate_result(result_directory)
        token = receipt["request"]["token"]
        with self.transaction():
            claim = self.db.execute("SELECT * FROM claims WHERE token=?", (token,)).fetchone()
            require(claim is not None, "Unknown verification claim")
            require(json.loads(claim["request"]) == receipt["request"], "Result does not match its immutable request")
            if claim["status"] == "complete":
                require(json.loads(claim["result"])["receipt_hash"] == receipt["receipt_hash"], "Conflicting repeated result")
                return {"status": "unchanged", "job": claim["job"]}
            require(claim["status"] == "running", "Claim is no longer running")
            destination = self.directory / "results" / token
            if destination.exists():
                require(validate_result(destination)["receipt_hash"] == receipt["receipt_hash"],
                        "Interrupted acceptance has conflicting artifact bytes")
            else:
                shutil.copytree(result_directory, destination)
            validate_result(destination)
            stored = dict(receipt, result_directory=str(destination))
            self.db.execute("UPDATE claims SET result=?,status='complete' WHERE token=?", (canonical(stored).decode(), token))
            for result in receipt["checks"]:
                if result["status"] != "passed":
                    self.record_repair(receipt, result)
            if receipt["status"] != "passed" and all(c["status"] == "passed" for c in receipt["checks"]):
                self.record_repair(receipt, {"id": "verification-integrity", "status": "failed"})
            job = self.db.execute("SELECT * FROM jobs WHERE id=?", (claim["job"],)).fetchone()
            snapshot = json.loads(job["plan"])
            claims = self.db.execute("SELECT * FROM claims WHERE job=?", (claim["job"],)).fetchall()
            required = set(snapshot["policy"]["verification"]["platforms"])
            completed = {c["platform"] for c in claims if c["status"] == "complete"}
            status = "running"
            if completed == required:
                status = "passed" if all(json.loads(c["result"])["status"] == "passed" for c in claims) else "failed"
                self.db.execute("UPDATE jobs SET status=? WHERE id=?", (status, claim["job"]))
                self.db.execute("UPDATE repositories SET running=NULL WHERE running=?", (claim["job"],))
            return {"status": status, "job": claim["job"]}

    def record_repair(self, receipt, result):
        snapshot = receipt["request"]["plan"]
        platform = receipt["request"]["platform"]
        key = digest([snapshot["repository"], platform, result["id"]])
        occurrence = {"job": receipt["request"]["job"], "commit": snapshot["commit"],
                      "receipt": receipt["receipt_hash"], "status": result["status"]}
        prior = self.db.execute("SELECT * FROM repairs WHERE key=?", (key,)).fetchone()
        occurrences = json.loads(prior["occurrences"]) if prior else []
        if occurrence not in occurrences:
            occurrences.append(occurrence)
        status = prior["status"] if prior and prior["status"] != "resolved" else "pending"
        issue = prior["issue"] if prior and prior["status"] != "resolved" else None
        self.db.execute("INSERT OR REPLACE INTO repairs VALUES (?,?,?,?,?,?,?)",
                        (key, snapshot["repository"], result["id"], platform, status, issue, canonical(occurrences).decode()))

    def abandon(self, job_id, reason):
        require(reason.strip(), "Recovery requires a reason")
        with self.transaction():
            row = self.db.execute("SELECT * FROM jobs WHERE id=?", (job_id,)).fetchone()
            require(row is not None and row["status"] == "running", "Job is not running")
            self.db.execute("UPDATE jobs SET status='abandoned' WHERE id=?", (job_id,))
            self.db.execute("UPDATE claims SET status='abandoned' WHERE job=? AND status='running'", (job_id,))
            self.db.execute("UPDATE repositories SET running=NULL WHERE running=?", (job_id,))
            write_json(self.directory / "recovery" / (job_id + ".json"), {"job": job_id, "reason": reason})


def clean_checkout(checkout, snapshot):
    require(git(checkout, "rev-parse", "HEAD") == snapshot["commit"], "Checkout is not at the requested commit")
    require(git(checkout, "rev-parse", "HEAD^{tree}") == snapshot["tree"], "Checkout tree differs")
    require(not git(checkout, "status", "--porcelain", "--untracked-files=no"), "Tracked source files changed")
    submodules = git(checkout, "submodule", "status", "--recursive")
    require(all(not line or line[0] not in "-+U" for line in submodules.splitlines()),
            "Submodule checkout is missing or differs")


def clean_candidate(checkout, snapshot):
    require(git(checkout, "rev-parse", "HEAD") == snapshot["commit"], "Candidate parent commit changed")
    require(git(checkout, "write-tree") == snapshot["tree"], "Staged candidate tree changed")
    require(not git(checkout, "diff", "--name-only"), "Stage every tracked change before integration checks")
    require(not git(checkout, "ls-files", "--others", "--exclude-standard"),
            "Stage or ignore untracked files before integration checks")


def machine_fingerprint():
    """Identify a running machine without exposing its raw host or boot identifiers."""
    boot = Path("/proc/sys/kernel/random/boot_id")
    if boot.exists():
        identity = boot.read_text().strip()
    elif sys.platform == "darwin":
        result = subprocess.run(["sysctl", "-n", "kern.bootsessionuuid"], capture_output=True, text=True)
        require(result.returncode == 0, "Cannot establish machine identity")
        identity = result.stdout.strip()
    else:
        raise DeliveryError("Machine identity currently supports Linux and macOS workers")
    require(identity, "Empty machine identity")
    return digest([system_platform.node(), identity])


def command_guard(status_path):
    """Keep the process-group leader alive until its controller collects every result."""
    invocation = json.loads(sys.stdin.read())
    try:
        child = subprocess.Popen(invocation["argv"], cwd=invocation["cwd"], stdin=subprocess.DEVNULL)
        result = {"returncode": child.wait()}
    except OSError as error:
        result = {"returncode": None, "error": str(error)}
    write_json(status_path, result)
    while True:
        signal.pause()


def group_children(leader, deadline):
    listing = subprocess.run(["ps", "-ax", "-o", "pid=,pgid="], capture_output=True, text=True,
                             timeout=max(0, deadline - time.monotonic()))
    require(listing.returncode == 0, "Cannot inspect check subprocesses")
    return [int(parts[0]) for line in listing.stdout.splitlines()
            if len(parts := line.split()) == 2 and int(parts[1]) == leader and int(parts[0]) != leader]


def execute(check, checkout, log_path, deadline):
    """All subprocess work, including an adapter wrapper, shares its caller's deadline."""
    require(os.name == "posix", "Process-tree enforcement currently supports POSIX workers only")
    log_path = Path(log_path).resolve()
    started = time.monotonic()
    limit = min(deadline, started + check.get("timeout_seconds", float("inf")))
    result = {"id": check["id"], "argv": check["argv"], "cwd": check.get("cwd", "."),
              "status": "timed-out", "returncode": None, "seconds": 0.0, "log": str(log_path.name)}
    if started >= limit:
        log_path.write_text("No execution time remained.\n")
        return result
    cwd = inside(checkout, check.get("cwd", "."))
    status_path = log_path.with_suffix(".status.json")
    with log_path.open("wb") as log:
        process = subprocess.Popen([sys.executable, str(Path(__file__).resolve()), "_command-guard", str(status_path)],
                                   stdin=subprocess.PIPE, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            process.stdin.write(canonical({"argv": check["argv"], "cwd": str(cwd)}))
            process.stdin.close()
            while time.monotonic() < limit:
                if status_path.exists():
                    outcome = read_json(status_path)
                    result["returncode"] = outcome["returncode"]
                    result["status"] = "passed" if outcome["returncode"] == 0 else "failed"
                    if outcome.get("error"):
                        log.write(outcome["error"].encode())
                    # The guard has not been collected and still owns this process group.
                    try:
                        children = group_children(process.pid, limit)
                    except subprocess.TimeoutExpired:
                        result["status"] = "timed-out"
                        break
                    if children:
                        result["status"] = "failed"
                        result["error"] = "Check left subprocesses running"
                    break
                time.sleep(min(0.01, max(0, limit - time.monotonic())))
        finally:
            # Do not collect the guard before signalling its group: its live identity
            # prevents signalling a reused process-group id.
            with contextlib.suppress(ProcessLookupError):
                os.killpg(process.pid, signal.SIGKILL)
            process.wait()
            status_path.unlink(missing_ok=True)
    if result["returncode"] in check.get("unavailable_exit_codes", []) and result["status"] == "failed":
        result["status"] = "unavailable"
    if result["status"] == "passed":
        patterns = [re.compile(p) for p in check.get("reject_output_patterns", [])]
        with log_path.open(errors="replace") as log:
            if any(pattern.search(line) for line in log for pattern in patterns):
                result["status"] = "failed"
                result["error"] = "Command output matched a rejected result pattern"
    result["seconds"] = time.monotonic() - started
    return result


def prepare_integration(repository, platform, policy_path, check_plan):
    snapshot = plan(repository, "HEAD", policy_path, candidate=True)
    require(platform in snapshot["policy"]["verification"]["platforms"]
            or snapshot["policy"]["verification"].get("blocked_reason"), "Unknown platform")
    require(check_plan or not snapshot["policy"]["integration"].get("requires_reviewed_plan"),
            "This repository requires an explicit reviewed integration check plan")
    checks = checks_for(snapshot["policy"], platform, full=False)
    if check_plan is not None:
        require(check_plan.get("commit") == snapshot["commit"] and check_plan.get("tree") == snapshot["tree"]
                and check_plan.get("review_evidence"), "Reviewed plan must bind the exact staged candidate and its review")
        selected = check_plan.get("checks", [])
        for check in selected:
            check_definition(check)
        mandatory = {check["id"]: check for check in checks}
        require(len({check["id"] for check in selected}) == len(selected), "Duplicate focused check id")
        for check in selected:
            require(check["id"] not in mandatory or check == mandatory[check["id"]],
                    "Reviewed plan cannot replace a mandatory integration check")
        checks = checks + [check for check in selected if check["id"] not in mandatory]
    require(checks, "No integration checks are configured for this platform")
    clean_candidate(repository, snapshot)
    return snapshot, checks


def reserve_integration(state, repository, integration, platform, policy_path=POLICY, check_plan=None):
    started = time.monotonic()
    snapshot, checks = prepare_integration(repository, platform, policy_path, check_plan)
    token, allowance = state.reserve(snapshot["repository"], integration,
                                     snapshot["policy"]["integration"]["budget_seconds"])
    payload = {"token": token, "repository": snapshot["repository"], "integration": integration,
               "platform": platform, "snapshot_hash": digest(snapshot), "checks_hash": digest(checks),
               "machine": machine_fingerprint(), "started_monotonic": started,
               "allowance": allowance, "deadline_monotonic": started + allowance}
    state.db.execute("INSERT INTO reservations VALUES (?,?,'reserved',NULL,NULL)",
                     (token, canonical(payload).decode()))
    return payload


def confirm_reservation(state, token, checkpoint_directory, publication_evidence):
    require(publication_evidence.strip(), "Durable checkpoint publication evidence is required")
    checkpoint = read_json(Path(checkpoint_directory) / "checkpoint.json")
    require(digest({k: v for k, v in checkpoint.items() if k != "checkpoint_hash"}) == checkpoint["checkpoint_hash"],
            "Reservation checkpoint integrity differs")
    with state.transaction():
        row = state.db.execute("SELECT * FROM reservations WHERE token=?", (token,)).fetchone()
        require(row is not None and row["status"] == "reserved", "Reservation is not awaiting publication")
        payload = json.loads(row["payload"])
        require(payload["machine"] == machine_fingerprint(), "Reservation belongs to another machine")
        exported = [r for r in checkpoint["tables"]["reservations"] if r["token"] == token]
        require(len(exported) == 1 and exported[0]["payload"] == row["payload"]
                and exported[0]["status"] == "reserved", "Checkpoint does not contain this unspent reservation")
        exported_budget = [b for b in checkpoint["tables"]["budgets"] if b["active"] == token]
        require(len(exported_budget) == 1 and exported_budget[0]["remaining"] == 0,
                "Checkpoint does not preserve the conservative budget reservation")
        state.db.execute("UPDATE reservations SET status='published',checkpoint_hash=?,publication_evidence=? WHERE token=?",
                         (checkpoint["checkpoint_hash"], publication_evidence, token))
    return {"status": "published", "token": token, "checkpoint_hash": checkpoint["checkpoint_hash"]}


def consume_reservation(state, token, snapshot, checks, integration, platform):
    with state.transaction():
        row = state.db.execute("SELECT * FROM reservations WHERE token=?", (token,)).fetchone()
        require(row is not None and row["status"] == "published",
                "Reservation was not durably published, was restored, or has already been consumed")
        payload = json.loads(row["payload"])
        require(payload["machine"] == machine_fingerprint(), "Reservation belongs to another machine")
        require(payload["snapshot_hash"] == digest(snapshot) and payload["checks_hash"] == digest(checks)
                and payload["integration"] == integration and payload["platform"] == platform,
                "Reservation differs from candidate, checks, integration or platform")
        budget = state.db.execute("SELECT * FROM budgets WHERE repository=? AND integration=?",
                                  (snapshot["repository"], integration)).fetchone()
        require(budget is not None and budget["active"] == token and budget["remaining"] == 0,
                "Reservation budget changed")
        state.db.execute("UPDATE reservations SET status='consumed' WHERE token=?", (token,))
        return payload


def integration_checks(state, repository, integration, platform, output, policy_path=POLICY, check_plan=None, reservation=None):
    started = time.monotonic()
    snapshot, checks = prepare_integration(repository, platform, policy_path, check_plan)
    if reservation is None:
        token, allowance = state.reserve(snapshot["repository"], integration,
                                         snapshot["policy"]["integration"]["budget_seconds"])
        deadline = started + allowance
    else:
        reserved = consume_reservation(state, reservation, snapshot, checks, integration, platform)
        token, allowance = reservation, reserved["allowance"]
        started, deadline = reserved["started_monotonic"], reserved["deadline_monotonic"]
    output = Path(output)
    output.mkdir(parents=True, exist_ok=False)
    results = []
    try:
        for index, check in enumerate(checks):
            result = execute(check, repository, output / (str(index) + ".log"), deadline)
            results.append(result)
            if result["status"] != "passed":
                break
        clean_candidate(repository, snapshot)
    finally:
        elapsed = time.monotonic() - started
        state.settle(snapshot["repository"], integration, token, allowance - elapsed)
    status = "passed" if len(results) == len(checks) and all(r["status"] == "passed" for r in results) else "incomplete"
    if any(r["status"] == "failed" for r in results):
        status = "failed"
    report = {"schema": SCHEMA, "status": status, "integration": integration, "plan": snapshot,
              "checks": results, "reviewed_plan": check_plan,
              "seconds": elapsed, "remaining_seconds": max(0, allowance - elapsed),
              "logs": [{"path": p.name, "sha256": file_hash(p)} for p in sorted(output.glob("*.log"))]}
    report["report_hash"] = digest(report)
    write_json(output / "integration.json", report)
    return report


def run_job(request, checkout, environment, output):
    snapshot = request["plan"]
    platform = request["platform"]
    require(request["job"] == digest(snapshot), "Request plan hash differs")
    require(environment.get("platform") == platform, "Worker platform does not match requested coverage")
    require(environment.get("worker_id") and environment.get("machine_id"), "Worker and machine identity are required")
    require(environment.get("execution") in ("local-worker", "peer-cloud"), "Unsupported worker execution type")
    if environment["execution"] == "peer-cloud":
        require(environment.get("provider") in ("claude", "codex") and environment.get("evidence"),
                "Peer cloud execution requires provider and observed machine evidence")
    validate_policy(snapshot["policy"])
    require(not snapshot["policy"]["verification"].get("blocked_reason"), "Verification is explicitly blocked")
    require(not snapshot["policy"]["verification"]["platforms"][platform].get("blocked_reason"),
            "Platform verification is explicitly blocked")
    actual_machine = machine_fingerprint()
    independent = actual_machine != request["controller_machine"]
    require(independent or not snapshot["policy"]["verification"].get("require_independent_machine", True),
            "Full verification requires a different machine from the controller")
    settings = snapshot["policy"]["verification"]["platforms"][platform]
    required_system = settings.get("system") or next((system for prefix, system in
        (("linux", "Linux"), ("macos", "Darwin"), ("windows", "Windows"))
        if platform.lower().startswith(prefix)), None)
    require(required_system is None or required_system == system_platform.system(),
            "Worker operating system does not satisfy the requested platform")
    require(plan(checkout, snapshot["commit"], snapshot["policy_path"]) == snapshot,
            "Worker source or check inventory differs from request")
    clean_checkout(checkout, snapshot)
    output = Path(output).resolve()
    require(not output.is_relative_to(Path(checkout).resolve()), "Results must be outside the checkout")
    output.mkdir(parents=True, exist_ok=False)
    observed = dict(environment, system=system_platform.system(), release=system_platform.release(),
                    architecture=system_platform.machine(), python=sys.version,
                    runtime_sha256=file_hash(__file__), machine_fingerprint=actual_machine,
                    independent_machine=independent)
    probes = []
    results = []
    errors = []
    for index, probe in enumerate(settings.get("environment_probes", [])):
        result = execute(probe, checkout, output / ("probe-" + str(index) + ".log"),
                         time.monotonic() + probe.get("timeout_seconds", 30))
        probes.append(result)
    for index, check in enumerate(checks_for(snapshot["policy"], platform)):
        result = execute(check, checkout, output / ("check-" + str(index) + ".log"),
                         time.monotonic() + check.get("timeout_seconds", 7200))
        results.append(result)
    try:
        clean_checkout(checkout, snapshot)
    except DeliveryError as error:
        errors.append(str(error))
    for pattern in settings.get("artifacts", []):
        matches = sorted(Path(checkout).glob(pattern))
        files = [p for p in matches if p.is_file()]
        if not files:
            errors.append("Missing required artifact: " + pattern)
        for source in files:
            relative_path = source.relative_to(checkout).as_posix()
            require(not source.is_symlink(), "Artifact is a symbolic link")
            source = inside(checkout, relative_path)
            destination = output / "artifacts" / relative_path
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, destination)
    observed["probes"] = probes
    write_json(output / "environment.json", observed)
    files = [{"path": p.relative_to(output).as_posix(), "sha256": file_hash(p), "bytes": p.stat().st_size}
             for p in sorted(output.rglob("*")) if p.is_file()]
    passed = results and all(r["status"] == "passed" for r in results + probes) and not errors
    receipt = {"schema": SCHEMA, "request": request, "status": "passed" if passed else "failed",
               "checks": results, "environment": observed, "environment_hash": digest(observed),
               "errors": errors, "files": files, "artifact_hash": digest(files)}
    receipt["receipt_hash"] = digest(receipt)
    write_json(output / "receipt.json", receipt)
    return receipt


def validate_result(directory):
    directory = Path(directory).resolve()
    require(not any(path.is_symlink() for path in directory.rglob("*")), "Result contains symbolic links")
    receipt = read_json(directory / "receipt.json")
    require(receipt.get("schema") == SCHEMA, "Unknown receipt schema")
    unhashed = {k: v for k, v in receipt.items() if k != "receipt_hash"}
    require(digest(unhashed) == receipt.get("receipt_hash"), "Receipt hash differs")
    request = receipt["request"]
    snapshot = request["plan"]
    require(request["job"] == digest(snapshot), "Receipt request differs")
    require(digest(snapshot["policy"]) == snapshot["policy_hash"], "Policy hash differs")
    require(digest(snapshot["inventory"]) == snapshot["inventory_hash"], "Inventory hash differs")
    require(digest(receipt["environment"]) == receipt["environment_hash"], "Environment hash differs")
    require(receipt["environment"]["platform"] == request["platform"], "Environment platform differs")
    require(receipt["environment"]["independent_machine"] ==
            (receipt["environment"]["machine_fingerprint"] != request["controller_machine"]),
            "Machine independence evidence contradicts itself")
    require(receipt["environment"]["independent_machine"]
            or not snapshot["policy"]["verification"].get("require_independent_machine", True),
            "Required independent machine coverage is missing")
    expected = checks_for(validate_policy(snapshot["policy"]), request["platform"])
    results = receipt["checks"]
    require(len(expected) == len(results) and expected, "Check coverage is incomplete")
    for check, result in zip(expected, results):
        require(check["id"] == result["id"] and check["argv"] == result["argv"]
                and check.get("cwd", ".") == result["cwd"], "Check execution differs")
        require(result["status"] in ("passed", "failed", "timed-out", "unavailable"), "Unknown check outcome")
        require(result["status"] != "passed" or result["returncode"] == 0, "A nonzero check was marked passing")
    manifest = receipt["files"]
    require(manifest and digest(manifest) == receipt["artifact_hash"], "Artifact manifest differs or is empty")
    paths = [entry["path"] for entry in manifest]
    require(len(paths) == len(set(paths)), "Duplicate artifact paths")
    require(set(paths) == {p.relative_to(directory).as_posix() for p in directory.rglob("*")
                           if p.is_file() and p != directory / "receipt.json"}, "Artifact files differ")
    for entry in manifest:
        path = inside(directory, entry["path"])
        require(path.is_file() and not path.is_symlink() and path.stat().st_size == entry["bytes"]
                and file_hash(path) == entry["sha256"], "Artifact bytes differ: " + entry["path"])
    require(read_json(directory / "environment.json") == receipt["environment"], "Environment artifact differs")
    settings = snapshot["policy"]["verification"]["platforms"][request["platform"]]
    required_system = settings.get("system") or next((system for prefix, system in
        (("linux", "Linux"), ("macos", "Darwin"), ("windows", "Windows"))
        if request["platform"].lower().startswith(prefix)), None)
    require(required_system is None or required_system == receipt["environment"]["system"],
            "Receipt operating system does not satisfy required coverage")
    expected_probes = snapshot["policy"]["verification"]["platforms"][request["platform"]].get("environment_probes", [])
    probes = receipt["environment"]["probes"]
    require(len(probes) == len(expected_probes), "Environment probe coverage differs")
    for expected_probe, result in zip(expected_probes, probes):
        require(result["id"] == expected_probe["id"] and result["argv"] == expected_probe["argv"], "Environment probe differs")
    require(all(result["log"] in paths for result in results + probes), "Missing command log")
    passed = all(r["status"] == "passed" and r["returncode"] == 0 for r in results + probes) and not receipt["errors"]
    require(receipt["status"] == ("passed" if passed else "failed"), "Receipt outcome contradicts its evidence")
    return receipt


def release_check(state, repository, commit, policy_path=POLICY):
    snapshot = plan(repository, commit, policy_path)
    require(not snapshot["policy"].get("release", {}).get("blocked_reason"), "Release is explicitly blocked")
    require(not snapshot["policy"]["verification"].get("blocked_reason"), "Full verification is explicitly blocked")
    main_commit = git(repository, "rev-parse", "--verify", snapshot["policy"]["main_ref"] + "^{commit}")
    require(ancestor(repository, snapshot["commit"], main_commit), "Verified commit is no longer in main history")
    identity = digest(snapshot)
    row = state.db.execute("SELECT * FROM jobs WHERE id=?", (identity,)).fetchone()
    require(row is not None and row["status"] == "passed", "Exact commit has not passed full platform coverage")
    claims = state.db.execute("SELECT * FROM claims WHERE job=?", (identity,)).fetchall()
    require({c["platform"] for c in claims} == set(snapshot["policy"]["verification"]["platforms"]),
            "Required platform coverage is missing")
    receipts = []
    for claim in claims:
        stored = json.loads(claim["result"])
        receipt = validate_result(stored["result_directory"])
        require(receipt["receipt_hash"] == stored["receipt_hash"] and receipt["status"] == "passed",
                "Stored passing evidence changed")
        require(receipt["request"]["plan"] == snapshot, "Receipt source or inventory differs")
        receipts.append(receipt["receipt_hash"])
    return {"status": "passed", "commit": snapshot["commit"], "tree": snapshot["tree"],
            "inventory_hash": snapshot["inventory_hash"], "receipts": receipts}


def integration_receipt_check(source, commit, report_path):
    report = read_json(report_path)
    require(digest({k: v for k, v in report.items() if k != "report_hash"}) == report["report_hash"],
            "Integration report hash differs")
    require(report["status"] == "passed" and report["checks"]
            and all(check["status"] == "passed" and check["returncode"] == 0 for check in report["checks"]),
            "Integration checks did not pass")
    actual = plan(source, commit, report["plan"]["policy_path"])
    require(actual["tree"] == report["plan"]["tree"] and actual["policy_hash"] == report["plan"]["policy_hash"]
            and actual["inventory_hash"] == report["plan"]["inventory_hash"], "Committed tree differs from checked candidate")
    for entry in report["logs"]:
        require(file_hash(inside(Path(report_path).parent, entry["path"])) == entry["sha256"],
                "Integration log bytes differ")
    return {"status": "passed", "commit": actual["commit"], "tree": actual["tree"], "report_hash": report["report_hash"]}


TABLES = ("repositories", "jobs", "claims", "budgets", "repairs", "metadata", "reservations")


def checkpoint_export(state, output):
    """The controller stores this directory through its existing durable transport."""
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=False)
    with state.transaction():
        tables = {name: [dict(row) for row in state.db.execute("SELECT * FROM " + name)] for name in TABLES}
        results = state.directory / "results"
        if results.exists():
            shutil.copytree(results, output / "results")
    files = [{"path": path.relative_to(output).as_posix(), "sha256": file_hash(path)}
             for path in sorted(output.rglob("*")) if path.is_file()]
    checkpoint = {"schema": SCHEMA, "tables": tables, "files": files}
    checkpoint["checkpoint_hash"] = digest(checkpoint)
    write_json(output / "checkpoint.json", checkpoint)
    return {"status": "exported", "checkpoint_hash": checkpoint["checkpoint_hash"]}


def checkpoint_import(state, source):
    source = Path(source).resolve()
    require(not any(path.is_symlink() for path in source.rglob("*")), "Checkpoint contains symbolic links")
    checkpoint = read_json(source / "checkpoint.json")
    require(checkpoint.get("schema") == SCHEMA and digest({k: v for k, v in checkpoint.items()
            if k != "checkpoint_hash"}) == checkpoint.get("checkpoint_hash"), "Checkpoint integrity differs")
    require(set(checkpoint["tables"]) == set(TABLES), "Checkpoint tables differ")
    require({entry["path"] for entry in checkpoint["files"]} == {
        path.relative_to(source).as_posix() for path in source.rglob("*")
        if path.is_file() and path != source / "checkpoint.json"}, "Checkpoint artifact inventory differs")
    for entry in checkpoint["files"]:
        require(file_hash(inside(source, entry["path"])) == entry["sha256"], "Checkpoint artifact bytes differ")
    require(all(state.db.execute("SELECT COUNT(*) FROM " + name).fetchone()[0] == 0
                for name in TABLES if name != "metadata"), "Import requires a new empty coordinator state")
    with state.transaction():
        state.db.execute("DELETE FROM metadata")
        for name in TABLES:
            columns = [row[1] for row in state.db.execute("PRAGMA table_info(" + name + ")")]
            for record in checkpoint["tables"][name]:
                require(set(record) == set(columns), "Checkpoint record schema differs")
                if name == "claims" and record["result"]:
                    stored = json.loads(record["result"])
                    stored["result_directory"] = str(state.directory / "results" / record["token"])
                    record = dict(record, result=canonical(stored).decode())
                if name == "reservations":
                    # A restored reservation can never launch work again, even
                    # if the exported snapshot predates the original launch.
                    record = dict(record, status="restored")
                state.db.execute("INSERT INTO " + name + " VALUES (" + ",".join("?" for _ in columns) + ")",
                                 [record[column] for column in columns])
        if (source / "results").exists():
            shutil.copytree(source / "results", state.directory / "results")
        for claim in state.db.execute("SELECT result FROM claims WHERE status='complete'"):
            stored = json.loads(claim[0])
            require(validate_result(stored["result_directory"])["receipt_hash"] == stored["receipt_hash"],
                    "Restored receipt differs")
    return {"status": "imported", "checkpoint_hash": checkpoint["checkpoint_hash"]}


def dispatch(request, capability, output):
    require(capability.get("enabled") is True, "External dispatcher is disabled")
    require(capability.get("provider") in ("claude", "codex"), "Unsupported provider")
    require(capability.get("fresh_machine") is True and capability.get("evidence"),
            "No observed fresh-machine dispatch capability")
    check = {"id": "dispatch", "argv": capability.get("argv", [])}
    check_definition(check)
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=False)
    request_path = output / "request.json"
    write_json(request_path, request)
    # The configured adapter receives paths, not a constructed shell command.
    check["argv"] = [*check["argv"], str(request_path), str(output / "acknowledgement.json")]
    result = execute(check, output, output / "dispatch.log", time.monotonic() + 30)
    require(result["status"] == "passed", "Dispatcher failed; claim remains running for explicit reconciliation")
    acknowledgement = read_json(output / "acknowledgement.json")
    require(acknowledgement.get("token") == request["token"]
            and acknowledgement.get("job") == request["job"]
            and acknowledgement.get("session_id") and acknowledgement.get("machine_id")
            and acknowledgement.get("fresh_machine") is True,
            "Dispatcher did not acknowledge this request on a fresh machine")
    return {"status": "dispatched", "acknowledgement": acknowledgement}


def worker_once(state, source, repository, platform, environment, work_root):
    request = state.claim(repository, platform)
    if request is None:
        return {"status": "idle"}
    root = Path(work_root).resolve() / request["token"]
    root.mkdir(parents=True, exist_ok=False)
    write_json(root / "request.json", request)
    checkout = root / "checkout"
    clone = subprocess.run(["git", "clone", "--no-hardlinks", "--no-checkout", str(Path(source).resolve()), str(checkout)],
                           capture_output=True, check=False)
    require(clone.returncode == 0, clone.stderr.decode(errors="replace"))
    git(checkout, "checkout", "--detach", request["plan"]["commit"])
    # A clone of a local repository advertises its local branches, which may
    # lag behind the fetched main reference used by the coordinator.
    git(checkout, "update-ref", request["plan"]["policy"]["main_ref"], request["plan"]["commit"])
    run_job(request, checkout, environment, root / "result")
    return state.accept(root / "result")


def main(argv=None):
    arguments = sys.argv[1:] if argv is None else argv
    if arguments and arguments[0] == "_command-guard":
        command_guard(arguments[1])
        return 0
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", default=".delivery-state", help="Durable local coordinator directory; never share SQLite over a network")
    sub = parser.add_subparsers(dest="command", required=True)
    for command in ("enqueue", "integration-checks", "reserve-integration", "release-check"):
        child = sub.add_parser(command)
        child.add_argument("--source", required=True)
        child.add_argument("--policy", default=POLICY)
        if command in ("integration-checks", "reserve-integration"):
            child.add_argument("--integration", required=True)
            child.add_argument("--platform", required=True)
            child.add_argument("--check-plan")
            if command == "integration-checks":
                child.add_argument("--output", required=True)
                child.add_argument("--reservation")
        if command == "release-check":
            child.add_argument("--commit", required=True)
    claim_parser = sub.add_parser("claim")
    claim_parser.add_argument("--repository", required=True)
    claim_parser.add_argument("--platform", required=True)
    claim_parser.add_argument("--output", required=True)
    run_parser = sub.add_parser("run-job")
    run_parser.add_argument("--request", required=True)
    run_parser.add_argument("--checkout", required=True)
    run_parser.add_argument("--environment", required=True)
    run_parser.add_argument("--output", required=True)
    accept_parser = sub.add_parser("accept-result")
    accept_parser.add_argument("--result", required=True)
    receipt_parser = sub.add_parser("integration-receipt-check")
    receipt_parser.add_argument("--source", required=True)
    receipt_parser.add_argument("--commit", required=True)
    receipt_parser.add_argument("--report", required=True)
    export_parser = sub.add_parser("checkpoint-export")
    export_parser.add_argument("--output", required=True)
    import_parser = sub.add_parser("checkpoint-import")
    import_parser.add_argument("--input", required=True)
    confirm_parser = sub.add_parser("confirm-reservation")
    confirm_parser.add_argument("--token", required=True)
    confirm_parser.add_argument("--checkpoint", required=True)
    confirm_parser.add_argument("--publication-evidence", required=True)
    dispatch_parser = sub.add_parser("dispatch")
    dispatch_parser.add_argument("--request", required=True)
    dispatch_parser.add_argument("--capability", required=True)
    dispatch_parser.add_argument("--output", required=True)
    for command in ("worker", "serve"):
        child = sub.add_parser(command)
        child.add_argument("--source", required=True)
        child.add_argument("--repository", required=True)
        child.add_argument("--platform", required=True)
        child.add_argument("--environment", required=True)
        child.add_argument("--work-root", required=True)
        if command == "serve":
            child.add_argument("--interval", type=float, default=30)
    repair_parser = sub.add_parser("repair-link")
    repair_parser.add_argument("--key", required=True)
    repair_parser.add_argument("--issue", required=True)
    resolve_parser = sub.add_parser("repair-resolve")
    resolve_parser.add_argument("--key", required=True)
    abandon_parser = sub.add_parser("abandon")
    abandon_parser.add_argument("--job", required=True)
    abandon_parser.add_argument("--reason", required=True)
    sub.add_parser("status")
    sub.add_parser("repairs")
    args = parser.parse_args(arguments)
    try:
        if args.command == "run-job":
            result = run_job(read_json(args.request), args.checkout, read_json(args.environment), args.output)
        elif args.command == "dispatch":
            result = dispatch(read_json(args.request), read_json(args.capability), args.output)
        elif args.command == "integration-receipt-check":
            result = integration_receipt_check(args.source, args.commit, args.report)
        else:
            state = State(args.state)
            if args.command == "enqueue":
                result = state.enqueue(args.source, args.policy)
            elif args.command == "integration-checks":
                result = integration_checks(state, args.source, args.integration, args.platform, args.output, args.policy,
                                            read_json(args.check_plan) if args.check_plan else None, args.reservation)
            elif args.command == "reserve-integration":
                result = reserve_integration(state, args.source, args.integration, args.platform, args.policy,
                                             read_json(args.check_plan) if args.check_plan else None)
            elif args.command == "confirm-reservation":
                result = confirm_reservation(state, args.token, args.checkpoint, args.publication_evidence)
            elif args.command == "release-check":
                result = release_check(state, args.source, args.commit, args.policy)
            elif args.command == "claim":
                result = state.claim(args.repository, args.platform)
                if result:
                    write_json(args.output, result)
                else:
                    result = {"status": "idle"}
            elif args.command == "accept-result":
                result = state.accept(args.result)
            elif args.command == "checkpoint-export":
                result = checkpoint_export(state, args.output)
            elif args.command == "checkpoint-import":
                result = checkpoint_import(state, args.input)
            elif args.command in ("worker", "serve"):
                if args.command == "serve":
                    number(args.interval, 300)
                while True:
                    result = worker_once(state, args.source, args.repository, args.platform,
                                         read_json(args.environment), args.work_root)
                    if args.command == "worker":
                        break
                    print(json.dumps(result), flush=True)
                    time.sleep(args.interval if result["status"] == "idle" else 0)
            elif args.command in ("repair-link", "repair-resolve"):
                with state.transaction():
                    if args.command == "repair-link":
                        cursor = state.db.execute("UPDATE repairs SET status='linked',issue=? WHERE key=?", (args.issue, args.key))
                    else:
                        cursor = state.db.execute("UPDATE repairs SET status='resolved' WHERE key=?", (args.key,))
                    require(cursor.rowcount == 1, "Unknown repair")
                result = {"status": "updated"}
            elif args.command == "abandon":
                state.abandon(args.job, args.reason)
                result = {"status": "abandoned"}
            elif args.command == "repairs":
                result = [dict(row) for row in state.db.execute("SELECT * FROM repairs ORDER BY key")]
            else:
                result = {table: [dict(row) for row in state.db.execute("SELECT * FROM " + table)]
                          for table in ("repositories", "jobs", "budgets")}
        print(json.dumps(result, indent=2, sort_keys=True))
        return 0 if not isinstance(result, dict) or result.get("status") not in ("failed", "incomplete") else 1
    except (DeliveryError, OSError, ValueError, KeyError, sqlite3.Error) as error:
        print(json.dumps({"status": "error", "error": str(error)}), file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
