# Release note — `natural_law_without_beneficiary/1` reads the agent-filtered view

**Landed:** 2026-07-25 (OQ-66, ruling 63-A; operator Q1 2026-07-25).
**Change:** `drl_core:natural_law_without_beneficiary/1` third conjunct changes from
`\+ narrative_ontology:constraint_beneficiary(C, _)` to
`\+ narrative_ontology:agent_beneficiary(C, _)`. One token. Everything else in this
directory is the apparatus that checks it.

**Owed and written late.** This note should have preceded the commit. It did not, because the
stop point that was supposed to force it never fired — see *Method note* below.

---

## What changed, stated at the right quantity

**Zero observable diff on six legs. One predicate-truth flip.**

Those are not in tension, and collapsing them into "diff 0 everywhere" is the error this note
exists to correct. The measured quantity was *final classification observables* (`dr_type` ×4
canonical contexts, `maxent_top_type` ×4). The predicate's own truth value is a different
quantity and it did change:

| leg | observable diff | predicate-truth flip |
|---|---|---|
| `testsets` (199) | 0 | none |
| `testsets_haiku` (960) | 0 | none |
| `testsets_flash` (960) | 0 | none |
| `testsets_sonnet` (1001) | 0 | none |
| `testsets_kimi` (1005) | 0 | none |
| `archives/datasets/kernel_v1` (1106) | 0 | **1 — `maxwell_demon_impossibility`** |

`maxwell_demon_impossibility`'s sole beneficiary is `entropic_universe_hypothesis`, a registered
non-agent value. Under the raw view it has a beneficiary (nlwb FALSE); under the filtered view it
has none surviving (nlwb TRUE). The flip is **downstream-invisible only because that constraint
classifies `rope` in both arms** — rope is reached before the snare and tangled_rope blocks that
nlwb gates, so the flipped truth is never consulted on a path that matters for it.

## The no-op is structural on five legs, contingent on the sixth

This is the distinction that governs how the change may be cited going forward.

- **Five live legs — STRUCTURAL.** 10,124 beneficiary facts, `registry_hits=0`: no fact carries
  either registered value. Combined with the filter being *exactly* registry membership (one
  clause, static, no kind inference — now enforced by three tests), `constraint_beneficiary/2`
  and `agent_beneficiary/2` have **identical extensions**. There is nothing for the swap to
  change. This cannot drift at runtime: the registry is static, with no `assertz`/`retract`
  anywhere in the tree.
- **`kernel_v1` — CONTINGENT.** Its zero is not forced. It holds because one constraint's metrics
  land in rope territory. Different metrics on the same beneficiary shape would have produced a
  visible diff.

**Forward statement — use this, not "behaviourally free":**

> No observable change on the checked corpora. **The first live constraint carrying a registered
> non-agent beneficiary with snare-range metrics will classify differently than it would have
> pre-cutover.**

`tests/fixtures/nlwb_controls/nlwb_ctl_nonagent_only` is the standing proof that the engine now
behaves differently under the two views: identical metrics to `nlwb_ctl_agent_only`, opposite
outcome (snare vs. blocked), the only variable being registry membership.

## Consumer surface

The predicate's consumers, re-audited at cutover:

| site | consumer | status |
|---|---|---|
| `drl_core.pl:391` | snare block (`\+ nlwb`) | in scope, exercised by the fixture leg |
| `drl_core.pl:426` | tangled_rope block (`\+ nlwb`) | **DEAD — structurally unfireable; OQ-250** |
| `maxent_classifier.pl:182` | `boolean_spec(snare, …, forbidden)` | in scope, measured via refit on all six legs |
| `maxent_classifier.pl:186` | `boolean_spec(tangled_rope, …, forbidden)` | LIVE; now covered two-sided in the gate |
| `maxent_classifier.pl:201` | `eval_boolean_feature/3` | the read path for both specs |
| `invertibility_analysis.pl:123`, `omega1_audit.pl:128` | diagnostics | on record from the 2026-06-03 ledger; not re-measured |

**The tangled_rope block is OPEN, not a declared residue → OQ-250.** An earlier draft of this note
called it accepted residue. That was wrong: whether the guard can fire *at all* was unanswered, and
the answer turns out to be **no — structurally**. `classify_from_metrics(..., tangled_rope)` requires
`requires_active_enforcement(C)` at `drl_core.pl:435`, and `nlwb` requires its negation, so the
`\+ nlwb` guard at `:426` can never block anything on any corpus. Witnessed with a positive control
(FINDINGS §8): `tr_guard_blocks=0` on every leg while `tr_body_control` runs 952–5809.

"Write the missing fixture" is therefore **not an available disposition** — a fixture would have to
satisfy the contradiction itself. The gap cannot be closed by a control, only by a ruling: delete
the dead guard, or declare it inert and monitor it (OQ-138 pattern). Opposite dispositions, so it is
a tracked question, not an accepted cost.

**Split out and already closed:** the MaxEnt mirror `boolean_spec(tangled_rope, nlwb, forbidden)`
(`maxent_classifier.pl:186`) is **LIVE**, not dead — it evaluates the feature with no enforcement
conjunct gating it (`-8.0 → -12.0` across identical-metric fixtures). That half needed no ruling and
is now covered two-sided in the gate's fixture pass (`agency_maxent_tr_mirror_inert`).

## Method note — the stop point was keyed on the wrong quantity

The plan specced a halt-and-report before landing, triggered by **a non-zero diff on `kernel_v1`**.
What the halt was actually protecting is the operator's seat on *this note* and on the consumer
re-audit scope. That seat is triggered by a **predicate-truth flip**, not by an observable diff.

The flip happened. The trigger did not fire. The filter landed and the note is being written
afterward.

No harm to the ruling — Q1 was "land regardless, a divergence changes the note not the decision" —
but the sequencing property was lost. **Generalization for the next stop point: key it on the
quantity that carries the meaning, not the one the harness happens to emit.** A trigger defined
over the convenient measurement will silently under-fire whenever the meaningful quantity and the
emitted one come apart, which is exactly the case a stop point exists for.

## Enforcement

`_prolog_agency_gate()` in `python/run_pipeline.py`, fourth sequential fail-fast gate in the Prolog
phase. Two swipl processes: the suite over the live corpus, then the planted fixture leg in a fresh
process. **The fixture pass is the load-bearing half** — the live legs' extensional identity means
a revert of `drl_core.pl` keeps the live-corpus suite GREEN. Break control witnessed: reverting
throws `agency_nlwb_set([nlwb_ctl_no_beneficiary])`.

## Open against this change

**OQ-248** — the gate-2 evidence recorded for the `entropic_universe_hypothesis` registry entry
does not re-witness (recorded `0.990 mountain / entropy 0.031`; measured `rope 0.95 / entropy
0.156 / mountain 0.010`). Nothing in this cutover depends on that entry — the diff is zero either
way — but the entry's justification is now open. Re-ruling it is a gate-2 ruling and the
operator's seat.
