# OQ-289 smoke, run 2 — 2026-08-12 — the feasibility question is ANSWERED, and the answer is no

**9 calls (3 arms × k=3). Scope md5 `d0b9f31f`. CLI 2.1.229 (re-checked immediately before spend),
settings md5 `bc56274c`, model `claude-sonnet-5`.**

## Verdict against the pre-registered table

**Row: `index n/n, sibling 0/n` → the ATTACHMENT path specifically does not deliver under `-p`.
Arm A needs a triggering mechanism designed BEFORE the sweep, not during it.**

**The `--tools ""` flag is not the cause.** The sibling content failed to arrive on *every* arm,
including the one with `Read` enabled — where it arrived only because the model *fetched it with a
tool call*, which is a different channel entirely.

## The evidence is BEHAVIORAL, and the numeric metric got it backwards

The feasibility readout printed `index 0/3`. **That is a false ABSENT.** The raw text proves the
index was delivered:

- `SMOKE_NOTOOLS` k1 emitted the **exact absolute path** of its own scratch memory dir —
  `/home/scott/.claude/projects/-tmp-…-scratch-smoke-notools--k1-work/memory/scratch_smoke_notools_00.md`
  — naming both the harness-generated project key and a **filename that appears only inside the
  `MEMORY.md` I wrote**.
- `SMOKE_INDEX` k1–k3 each named `scratch_delivery_check.md`, a string that exists nowhere but in
  that index.

A model cannot emit a filename it was never shown. **The index arrived; the sibling content did
not.**

### Why the metric said otherwise — and it is my own relevance engineering that did it

The index entry read *"consult it whenever asked about delivery-check tokens."* That is an
**instruction**, and the models obeyed it: they saw the entry, went to fetch the sibling, and the
turn ended with the attempted tool call as their final text. They never reported the
`SMOKE-INDEX` marker sitting on line 1 of the same file.

**The relevance engineering worked so well it suppressed the report.** This is a live hazard for
the sweep: **Arm A's prompt can cause a model to defer reporting canaries it can plainly see**,
producing `ABSENT` for delivered content — an absence-shaped success, the mirror of the
success-shaped absence this whole program is about.

## Three instrument defects found, all pre-freeze

1. **`observed_tool_calls()` is broken under `--output-format json`** and failed in the direction
   that looks fine. `json` returns only the final result object; `tool_use` blocks live in the
   message stream, which is absent. It returned **0 for every unit, including the three that
   demonstrably made a real tool call** (`num_turns` = 2, and they came back with a marker they
   could not otherwise have had). So `TOOL_CALL_ON_SUPPRESSED_ARM` was **a check that could not
   fire** — the exact converse of the `cache_read` gate that could not pass, in the same driver,
   found the same day. Now returns `None` (UNMEASURED) rather than `0`, with `num_turns` wired as a
   declared proxy.
2. **Arm A′'s measurement is UNIMPLEMENTABLE as specified.** Its design is *"measured as an
   observed tool call, never a self-report"*, and under `json` there is no observed tool call.
   `--output-format stream-json` is required — the prereg already demanded it for the
   zero-tool-call verification and it was not implemented.
3. **`--tools ""` does not stop the model from emitting tool-call-shaped text.** It emits the call
   and simply gets no result. Harmless for delivery, but "zero tool calls verified" cannot be read
   off the text.

## Confirmed, incidentally

`project_key()` is **correct**: the model echoed the harness's own generated project-dir name,
character for character, independently confirming the cwd → `~/.claude/projects/<key>` transform
the whole isolation design rests on.

## HALT

`DELIVERED_UNSTABLE_ACROSS_K` fired on `SMOKE_TOOLS` (21,482 / 21,441 / 21,441) — correctly: that
arm made tool calls whose results varied slightly. The other two arms were at zero variance
(9,241 ×3, 9,454 ×3), so the replacement gate is **two-sided witnessed** in a single run.

## Contents

| Path | What it is |
|---|---|
| `payloads/smoke_*/mem_00.md` | 512 B / 10-line payloads, one marker each |
| `payloads/smoke_index/sibling_00.md` | the paired sibling marker |
| `responses/*/*.json` | raw stdout per call, persisted before any parsing |
| `reports.json`, `summary.json` | derived per-call records and the feasibility readout |

**Do not read `summary.json`'s `index_path: 0/3` as the finding.** It is the false ABSENT described
above; the finding is in `responses/`.
