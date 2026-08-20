# omega_resolver.py — the "what to work on" surface over ISSUES.md

`python/omega_resolver.py` is a **read-only** catalog + frontier over `ISSUES.md`. It exists so an
agent (or you) gets "what's workable / what blocks what / pick one" from *authored structure*
instead of reading the 6,700-line file and **prose-guessing** the dependency graph (a witnessed
failure mode — a cold instance reads file-size, fakes a query, and invents edges). It computes the
deterministic half; the judgment half (priority/value, typing) stays a declared human seat.

Read this before modifying `omega_resolver.py`, the `ISSUES.md` authored fields, or the hooks.

## Commands

| command | what |
|---|---|
| `menu` | **the front-end** — WORKABLE NOW sorted by authored `Priority:`, with Ω-type + what-resolution-changes, plus BLOCKED-ON-YOU / STANDOFFS / BLOCKED buckets and a coverage footer. WORKABLE NOW items that share a `bundled_with` family (connected component over the undirected `bundled_with` graph) print together under a `⧉ family …` header — priority-sorted within, families ordered by best member, with non-workable siblings surfaced as `(+ OQ-…)` context; singletons fall through to a flat `· unbundled` list. `splits_from` is NOT folded into the grouping. The grouping is human-surface only — `frontier`/`activations` JSON shapes are unchanged. Run this, not the file. |
| `frontier` | the same buckets as **JSON** (machine artifact; the pilot stamped `frontier_view.json` from it). |
| `check` | authority-control gate: dangling `Deps:` targets, `resolved` entries whose witness no longer resolves (git/audits/KNOWN_STATE), **and malformed/packed `Deps:` edges** (unknown relator, no OQ target, or >1 edge packed in one comma-chunk — the silent-drop case). Exit 1 on problems. |
| `selftest` | the planted positive controls, count DERIVED and printed by the command itself (never a literal here — the old hardcoded banner drifted from the control set for 67 days; OQ-310, 2026-08-20) (incl. the §D 2-cycle → one `standoff`, the two-sided malformed-Deps control, and the comma-in-`blocked_on_human`-free-text control — OQ-9012, locking the 2026-06-18 fix that a human free-text target may contain commas). Exit 1 on fail. |
| `activations` | emits the **SessionStart-hook JSON** (`hookSpecificOutput.additionalContext`) listing `[NEXT]/[GATE]/[PUSH]` with live counts + the monthly-consolidation date-check. |
| `dump` | parsed access points per OQ. |

## Authored fields in `ISSUES.md` (the access points)

- `**Status:**` — gated by `python/issues_status.py` (the pipeline gate; tokens
  open/investigating/mitigated/partial/resolved/disposed). Active = {open, investigating, partial}.
- `**Ω-type:**` — `Ω_E`/`Ω_C`/`Ω_P`. Ω_P (or a `blocked_on_human` dep) routes an OQ to BLOCKED-ON-YOU.
- `**Deps:**` — **typed relators ONLY**: `blocked_on` / `gates` (blocking → reachability),
  `bundled_with` / `splits_from` (grouping, non-blocking), `blocked_on_human <freetext>` (a live
  operator/substrate gate that is not an OQ edge). **Do not put prose on a `**Deps:**` line** — the
  parser throws `unknown relator` (this happened; the prose discharge note was renamed
  `**Discharge:**`). Author `Deps:` by hand from the entry's own prose — they are *authored, not
  extracted* (the prose carries cross-ref language like "bundles with"; only a human turns that into
  a typed edge).
- `**Priority:**` — integer **1–10, 1 = highest**. An **authored hint the OQ's author declares to
  help the operator judge — surfaced by `menu`, never computed.** Priority/value is the operator's
  *declared seat* (see "the floor" below). All active OQs are currently stamped `1` (undifferentiated
  backlog); new/touched OQs get a real priority. When you mint or touch an OQ, author `Priority:`
  (and `Deps:` if it blocks/awaits another) or the frontier drifts.

## Reachability (the deterministic half)

Buckets are computed over the **SCC condensation** of the blocking `Deps` graph, so a mutual block
(A blocked_on B, B blocked_on A) surfaces as one `standoff` (a thing for you to cut), never a pair
of dead `blocked` or a hang. `workable_now` / `blocked_on_human` / `standoff` / `blocked`. **Edge-free
OQs default `workable_now`** and may overstate workability until their `Deps:` are authored — the
`menu` coverage footer states how many active OQs have authored Deps, which is the honesty check.

## The floor (the conceptual point — do not try to remove it)

Typing an omega and ranking which workable item matters most are **judgment, not computation**. The
resolver does reachability-from-facts deterministically and **surfaces** the rest: `Priority:` is an
authored *seat*, not a metric. This is the session's deepest finding — *the determinism boundary is
the omega boundary*: deterministic where facts decide, declared where seats decide. See
`docs/seat-theorem-v1.md` and `ISSUES.md` OQ-130 #1 (operation-locus declared as a seat). Do **not**
wire a computed priority or an automated type-stamp pretending to be ground truth — that launders a
seat into the machine. The honest move for cases the deterministic layer can't decide is to
**flag-and-surface to the operator**, not to auto-resolve.

## Activations + hooks (committed logic, LOCAL wiring)

- **`[NEXT]` / `[GATE]` / `[PUSH]`** are documented in `CLAUDE.md` (committed, travel with the repo)
  and listed by `activations`. `[GATE]` → `scripts/gate.sh` (all four checkers). `[PUSH]` →
  gate-green + docs-current, then push.
- The **SessionStart** hook (runs `activations`) and a **PostToolUse** hook (runs
  `issues_status --check` on every `ISSUES.md` edit) live in **`.claude/settings.json`, which is
  gitignored** (`.gitignore:47` ignores all of `.claude/`). So the *logic* travels (this file,
  `omega_resolver.py`, `scripts/gate.sh`) but the *hook wiring is machine-local* — a fresh clone has
  the commands but must re-wire the hooks (or just run the commands directly). Hooks only take effect
  after `/hooks` reload or a `claude` restart (the settings watcher doesn't watch a `.claude/` that
  was empty at session start).

## Ω-type diagnostic + the restatement gate (OQ-130 scale arm)

A separate POC (`audits/2026-06-14_omega_type_diagnostic_poc/`) types omegas by *which resolution
operation discharges them* (define→Ω_C / decide→Ω_P / measure→Ω_E, else restatement), per
`docs/debugging_philosophy.md` §6.1. Two load-bearing facts for anyone touching it:
- **The gate MUST consult the entry's `declared_fields`** (enumerated readings + authored ε). The
  first build's restatement limb was a **no-op** (it ignored `declared_fields`, defaulted every
  signature to "external," so restatement — which needs all-internal — was structurally
  unreachable). Re-deriving the constraint's *own* authored fields (ε-invariance; comparing authored
  ε/base-properties across *declared* readings) is restatement. `deterministic_baseline.py` now has a
  runnable, GREEN seed control; `spec.md` + `restatement_gate_fix.md` carry the detail. This is a
  textbook Build-Discipline **Pattern 5/6** instance (a gate that passes on absence of its substrate).
- **Generator wiring (diagnose-then-stamp, OQ-130 iv) is deferred.** If built, it is an **API call
  at generation time** (via `agent/llm_call.py`), stamping a `diagnosed_type` into the committed
  JSON — **never an LLM call inside `run_pipeline`** (that would break the determinism frontier, the
  invariant that the committed JSON onward is deterministic). And it inherits the ~½-judgment floor,
  so it needs the judge in the loop — a cost decision, not a free stamp.

## Gotchas

- **Don't change `frontier`/`activations` output shape casually** — the SessionStart hook parses
  `activations` JSON; downstream tooling reads `frontier` JSON.
- **The `§1b` freshness key is git HEAD today, which cries wolf** — every unrelated commit makes a
  stamped view look stale. The right fix (logged) is to stamp an `ISSUES.md` *content hash*, not the
  global HEAD. Carry this if you touch the manifest.
- **`omega_resolver check` is not yet a pipeline gate** — run it (or `[GATE]`) manually before
  committing `ISSUES.md` changes; the PostToolUse hook automates `issues_status --check` only.
