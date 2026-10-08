# Delivery verification runtime

This extension keeps short integration checks and complete verification separate.
Existing controllers, issue ownership, reviews, build scripts, full suites and
release tools remain authoritative. Read `controller.md` for the controller
procedure and `policy.json` for the repository's commands and platform coverage.
Installing these files starts no process, schedule, provider session or release.

## Requirements and boundaries

Use Python 3.10 or newer, Git, `ps`, and a Linux or macOS worker. On macOS the
runtime also reads `sysctl kern.bootsessionuuid`; on Linux it reads the kernel
boot identifier. The command executor requires permission to inspect processes
and stop its own process groups. A restricted host that denies these operations
cannot provide execution evidence. Native Windows command execution is currently
unsupported and fails explicitly; a Windows platform requirement remains open.
Install the required language runtimes and repository dependencies through the
repository's existing setup instructions before assigning an independent worker.
Any required setup that blocks an integration belongs inside its check plan and
consumes that integration's remaining allowance.

The runtime uses only Python's standard library. Full verification normally
requires a different machine from the controller. Machine identity hashes the
host and operating-system boot identity; an environment configuration name or a
second process is not an independent machine. `require_independent_machine:false`
is an explicit diagnostic policy override, retained in every receipt. Production
policies should keep the default `true`.

Receipts detect changed or incomplete evidence. They are not signatures and do
not authenticate a malicious worker. Keep controller state and request tokens
private, use the existing repository ownership lock, and accept artifacts only
from the claimed provider session. Commands must keep their descendants in their
process group; deliberately daemonized processes that escape that group need a
worker-container boundary. The executor detects ordinary leaked child processes,
fails the check, and stops its still-owned process group. It does not claim to
contain malicious commands.

## Repository policy

Vendor `runtime.py`, `test_runtime.py`, `cloud.py`, `test_cloud.py`, `checks.py`,
`test_scope.py`, `controller.md` and this document under `.agents/delivery/`. Start `policy.json` from
`policy.example.json`, replacing the example commands with actual repository
checks. Do not replace a full gate with syntax checking or omit an unsupported
platform. A command is an argument array, with optional repository-relative
`cwd`, `timeout_seconds`, `reject_output_patterns`, and
`unavailable_exit_codes`. A shell is used only when explicitly named in that
array. Output rejection patterns are regular expressions applied to log lines;
they catch gates that exit successfully after reporting skipped checks.

`inventory` contains tracked path patterns covering the suites, registration,
fixtures and command definitions. `['*']` covers the entire tracked tree. Each
pattern must match. The immutable request contains matching paths, Git object
identifiers, the complete policy, commit and tree. The full plan automatically
includes static integration checks that apply to its platform. Every platform
also needs at least one explicit complete check. Platform names beginning with
`linux`, `macos` or `windows` require the corresponding operating system; `system`
can state an exact operating-system name for another platform name.

Set `verification.blocked_reason` and an empty `platforms` object when no honest
complete automated profile exists. A platform can instead declare its own
`blocked_reason`. These configurations refuse full verification and release;
they are not passing profiles. `release.blocked_reason` allows diagnostic full
verification while preserving a separate release restriction. Artifacts are
required file patterns under the worker checkout, including ignored output
directories. Every pattern must produce at least one file. A source-only profile
may use an empty artifacts list: its actual command logs and environment record
still form hashed verification artifacts.

## One integration allowance

Stage the intended candidate first. `integration-checks` tests that staged tree,
rejects unstaged or untracked source changes, and confirms the candidate did not
change while checks ran. The command does not commit or publish anything.

```sh
python3 .agents/delivery/runtime.py --state /durable/controller-state integration-checks \
  --source /checkout --integration repository-pull-request-identity \
  --platform linux --check-plan /outside-checkout/focused.json \
  --output /outside-checkout/integration-result
```

The optional focused plan contains `commit` (current HEAD), `tree` (`git
write-tree`), `review_evidence`, and `checks` (the selected command definitions).
`integration.requires_reviewed_plan:true` makes it mandatory. Static integration
checks always remain mandatory and execute before additional focused checks;
the plan cannot replace their definitions. Include compilation or type checks,
lint, and behavior tests appropriate to the actual change. The policy and plan
do not themselves prove that the selection is adequate; both reviewers inspect
its coverage.

One stable integration identifier spans pre-commit checks, corrections,
integration, wrappers and retries. It is not a commit identifier. All commands
share the remaining allowance, at most 300 seconds total. The state reserves the
entire remaining allowance before running, then returns unused time only after
a normal controlled result. A crash leaves that reservation consumed and active;
it cannot silently reset the budget. Exhaustion is `incomplete`, unavailable
checks never pass, and failures remain `failed`. The controller must include
every required blocking command here; commands executed outside the runtime
cannot be accounted for by it. Process termination and result recording can add
small cleanup overhead after the execution deadline.

A cloud controller must publish a conservative reservation before it starts the
checks. Use the same source, integration identity, platform and focused plan in
both calls:

```sh
python3 .agents/delivery/runtime.py --state /controller-state reserve-integration \
  --source /checkout --integration repository-pull-request-identity \
  --platform linux --check-plan /outside-checkout/focused.json
python3 .agents/delivery/runtime.py --state /controller-state checkpoint-export \
  --output /outside-checkout/reserved-checkpoint
```

Save the returned reservation token. Publish the entire checkpoint through the
existing durable adapter and retain its actual storage receipt. Only after that
publication succeeds, confirm and execute:

```sh
python3 .agents/delivery/runtime.py --state /controller-state confirm-reservation \
  --token RESERVATION_TOKEN --checkpoint /outside-checkout/reserved-checkpoint \
  --publication-evidence ACTUAL_DURABLE_STORAGE_RECEIPT
python3 .agents/delivery/runtime.py --state /controller-state integration-checks \
  --source /checkout --integration repository-pull-request-identity \
  --platform linux --check-plan /outside-checkout/focused.json \
  --reservation RESERVATION_TOKEN --output /outside-checkout/integration-result
```

Confirmation checks that the exported checkpoint contains the exact reservation
with no available allowance. The trusted controller supplies the successful
publication evidence; the runtime does not invent or authenticate an external
storage service. Failed publication must never be confirmed. The reservation
binds the machine, staged tree, policy, check plan, platform and integration.
The execution deadline starts at reservation, so blocking publication and other
waiting consume the same allowance. Reservations execute at most once and a
restored reservation is never executable, even when its exported checkpoint
predated execution. Export and publish the settled state after the run to retain
unused time. Until that publication succeeds, recovery conservatively loses the
reserved allowance. The synchronous command remains available when the
controller's state storage itself survives its process or machine lifetime.

After committing, verify that the commit contains the tested candidate:

```sh
python3 .agents/delivery/runtime.py integration-receipt-check \
  --source /checkout --commit EXACT_COMMIT --report /outside-checkout/integration-result/integration.json
```

## Immutable full verification

The controller fetches main through its existing Git transport, then enqueues
the current `main_ref`. It keeps implementing, reviewing and merging while full
verification runs. The queue stores one running immutable commit and only the
newest pending descendant per repository; newer merges replace only pending
work. Divergent history requires explicit reconciliation.

```sh
python3 .agents/delivery/runtime.py --state /durable/controller-state enqueue --source /checkout
python3 .agents/delivery/runtime.py --state /durable/controller-state claim \
  --repository owner/repository --platform linux --output /outside-checkout/request.json
```

Each required platform has its own claim for that running commit. Transfer the
request unchanged to the independent worker. `cloud.py` prepares actual
provider commands; generic `dispatch` accepts only an explicitly enabled
external adapter with observed fresh-machine evidence. It passes request and
acknowledgement paths as arguments and requires the acknowledgement to contain
`token`, `job`, `session_id`, `machine_id`, and `fresh_machine:true`. No configured
adapter means no cloud launch. A successful dispatch acknowledgement is not
successful verification.

The worker uses a clean checkout of the requested commit and runs:

```sh
python3 .agents/delivery/runtime.py run-job --request /worker/request.json \
  --checkout /worker/checkout --environment /worker/environment.json --output /worker/result
```

The environment record supplies `platform`, `worker_id`, `machine_id`, and
`execution` (`peer-cloud` or `local-worker`). Peer cloud records also require
`provider` (`claude` or `codex`) and actual session evidence in `evidence`.
The runtime adds observed system, architecture, Python version, runtime hash,
machine fingerprint and tool probe results. It runs every configured full check,
captures real exit codes and logs, verifies source identity after execution,
copies required artifacts, and hashes all resulting bytes. Equivalent workers
can satisfy the same platform; the receipt does not require one particular
worker instance.

Return the complete result directory through the supported provider transport:

```sh
python3 .agents/delivery/runtime.py --state /durable/controller-state accept-result --result /returned/result
```

Incomplete coverage, changed logs, conflicting repeated results, missing
artifacts, source changes or abandoned claims cannot pass. Once every required
platform has returned, the job passes or fails and the newest pending commit can
start. `repairs` lists the deduplicated failure outbox. The existing controller
publishes or updates a bounded repair issue and records it using
`repair-link --key KEY --issue ISSUE_URL`; `repair-resolve --key KEY` requires
actual repair evidence. The runtime itself sends no messages or issues.

## Recovery and unattended local workers

Use one controller state directory on one machine; never share SQLite directly
over a network filesystem. After every important transition,
`checkpoint-export --output NEW_DIRECTORY` copies the queue, claims, conservative
budget reservations, receipts and hashed artifacts. Save that entire directory
through the existing durable repository adapter. A new controller restores it
with `checkpoint-import --input DIRECTORY` into empty state before resuming.
The checkpoint contains active claim tokens and belongs in private storage.
Hash checking detects damage but does not replace trusted transport or the
repository-wide ownership lock. Losing the latest checkpoint loses later state;
checkpoint publication is a required controller responsibility.

`worker --source CLONE --repository ID --platform NAME --environment FILE
--work-root DIRECTORY` processes one claim in a new isolated clone. `serve`
accepts the same arguments plus `--interval SECONDS` and keeps processing the
existing queue. It does not install a schedule or fetch main. Run it only on an
authorized separate worker with restored controller state, or use exported
requests with `run-job`. A same-machine diagnostic override must be explicit.
Controller/provider scheduling and evidence transport remain external extension
points. This preserves existing mature scheduling engines.

Running claims are never stolen automatically. Recover a lost worker only after
establishing it is no longer running, using `abandon --job JOB --reason REASON`.
Late results then fail. Preserve the recovery record and continue with newer
pending work. An interrupted integration reservation remains consumed; no
recovery command grants it another allowance.

## Release evidence

```sh
python3 .agents/delivery/runtime.py --state /durable/controller-state release-check \
  --source /checkout --commit EXACT_COMMIT
```

This checks full passing coverage for that exact commit, tree, policy, suite
inventory, platform and artifact bytes, and confirms the commit remains in main
history. Fetch main first. It does not tag, publish or deploy. The existing
release guard must also pass; release commands must use the returned commit and
tested artifacts, never an advancing main reference. Manual or native acceptance
remains a separate explicit restriction.

## Regression checks

```sh
python3 -m unittest discover -s .agents/delivery -p 'test_*.py' -v
```

Fixtures are synthetic temporary Git repositories. Controls deliberately fail
commands, corrupt source and artifact bytes, remove check evidence, exhaust a
shared deadline, leave a child process running, move main backwards, introduce
divergent history, withhold a platform, request the wrong machine/system, and
attempt an unsupported dispatch. No provider credentials, product data, real
cloud session, issue, release or deployment is used by these tests.
