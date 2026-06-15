# Ω-type diagnostic POC (OQ-130 child) — 2026-06-14

**Question.** `debugging_philosophy.md` §6.1 gives paradox-KIND a diagnostic (*the fix that works
reveals the type*). `omega_variables.md` has none — Ω-type is self-assigned by the generating LLM.
Port the §6.1 discipline: type each omega by **which resolution operation discharges it** (define→Ω_C
/ decide→Ω_P / measure→Ω_E), external-at-its-own-locus or else restatement. Settle, per-omega, whether
the `kernel_reading`/committer family is a legitimate committer frontier (Ω_P, mistyped) or restatement
(artifact) — the open ruling OQ-130 carried from the prior soundness POC.

**Method.** Two-party cross-instance. Adjudicator sealed a hand-diagnosed 14-omega held key
(`adjudicator_held_key.json`, committed `94c7346e` BEFORE the executor ran), anchored to the
**substrate question**, not §6.1. A blind executor subagent built the spec + both classifiers + ran
the 40-omega stratified sample (`spec.md`, `deterministic_baseline.py`, `judge_results.json`,
`executor_writeup.md`), never reading the held key. Read-only over `prolog/testsets_haiku/`.

## Result (one paragraph)

The cross-instance read **disagreed on exactly the on-trial set, and the disagreement is the finding.**
Executor: 82.5% agree-with-authored, family **uniform Ω_C**. Adjudicator: 9/14 agree, **5/14 typed
differently — all 5 in the committer-frame family**, because the executor's gate **collapsed `decide`
and `measure` into `define`, re-stamping the authored `conceptual` label.** Adjudicated per-omega, the
**family SPLITS**: Ω_C (genuine criterion frontiers), Ω_P (committer-position frontiers, e.g. id 28 —
the Seat-Theorem Cor-2b case), Ω_E (observable suppression, id 38), **restatement** (generate-and-
compare artifact, id 20). ⇒ **the prior "uncontested: retype the family Ω_P wholesale" is REFUTED**;
retype **per-omega by mechanism**. Two omegas named `kernel_reading_contest` (19, 38) discharge
differently — **mechanism ≠ name**. The seeded two-sided gate control fired its alarm: the executor's
restatement limb **missed both seeded restatements** (false-negatives) → `restatement-rate=0` is partly
gate-no-op, not measured-empty. "Ω_E is a status" **holds** (1/14 falsifier, rare, directional).

## Five metrics (adjudicated; full reasoning in `adjudication.md`)

| metric | executor | adjudicated | meaning |
|---|---|---|---|
| (a) diagnosed-vs-authored | 82.5% (upper) | ≈72.5% (family corrections) | self-label unreliable **both directions** → diagnostic load-bearing |
| (b) deterministic-`unknown` | 12.5% | boundary ≈53% (unknown + commits-wrong 40%) | lexical classifier insufficient; typing needs judgment |
| (c) restatement rate | 0/40 | ≥1/40; **gate misses seeded restatements** | 0 is partly gate-no-op (Pattern 5/6) — sharpen before any corpus count |
| (d) family per-omega | uniform Ω_C | **SPLITS** Ω_C/Ω_P/Ω_E/restatement | aggregate hides the real-frontier-vs-artifact cut |
| (e) Ω_E-as-status falsifier | 1/14 rare → holds | agree (directional) | empirical label mostly load-bearing |

## Files

- `sampler.py` → `sample_40.json` — deterministic stratified draw (seed 20260614), declared fields carried.
- `adjudicator_held_key.json` — **sealed** 14-omega hand-diagnosis (committed pre-executor `94c7346e`).
- `spec.md`, `deterministic_baseline.py`, `det_results.json`, `build_judge.py`, `judge_results.json`,
  `executor_writeup.md` — blind executor's build + run.
- `score_held.py` — reproduces the held-vs-executor table + two-sided gate control from the JSONs.
- `adjudication.md` — the adjudicator's writeup (this audit's substantive product).

## Scope / honesty

POC bar: roughness expected; the deliverable is **what it teaches**. Held overlap 14 (small-N, not a
calibrated κ). The rate (~27% mistyped) is directional; the **direction** (self-label unreliable both
ways; family splits; gate under-catches restatement) is robust. Five adjudicator-corrections are
settleable for ids 28/38/20, hybrid-refinements for 10/19, **contested+escalated** for 27/31. The
whole result rides the **decider-locus cut**, an operator framework call the POC shows is load-bearing
and that two instances split on — not that one cut is proven right.
