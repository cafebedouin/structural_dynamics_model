# Omega-resolver pilot on ISSUES.md (2026-06-14)

Pilot of the omega-resolver memo (`~/.claude/plans/brief-the-omega-glittery-wozniak.md`):
read-only catalog views over the existing `ISSUES.md` prose + one authored `Deps:` field,
no `issues/` migration (Option 1). The pilot tests the **one claim still in doubt**: does
computing over the access points *route the right few from the many* against a ground truth
a human can read by eye (§B)?

## What was built (all read-only over ISSUES.md; never writes it)

`python/omega_resolver.py` — loader (status / Ω-type / references / authored `Deps:`),
authority control (fail-closed, each authority list positive-controlled non-empty),
SCC-condensation frontier view (§D), checker (§3), and a planted-fixture selftest.

## Step results (each witnessed before the next)

1. **§8 landed into OQ-129 OPEN-A**, metrics **re-witnessed** this branch (not transcribed):
   live 16/20 (80%) mirror, haiku 258/358 (72.1%), avg 2.73–2.85 types. Witness:
   `audits/2026-06-14_extraction_blindness_existential_label/`.
2. **Loader positive controls (selftest, 8/8 fired)** — incl. §D planted 2-cycle → exactly one
   `standoff` (not two `blocked`, not a hang); non-cycle leaf NOT in standoff; resolved-dep leaf
   → workable_now; Ω_P leaf → blocked_on_human; **human-gate Ω_E → blocked_on_human, not
   workable_now**; dangling Deps flagged; rotted-witness flagged AND the over-fire negative
   control (one real + one truncated audit dir) NOT flagged. `selftest.txt`.
3. **16 `Deps:` edges authored by hand** from each entry's own prose (§1e — values authored, not
   extracted), `issues_status.py --check` still passes (128 parsed, 0 malformed; pipeline gate
   intact). 1 authored field, no migration.
4. **Frontier view renders all 64 active OQs** (`frontier_view.json`): 50 workable_now /
   11 blocked_on_human / 3 blocked / **0 standoff**. The live blocking graph is acyclic — the §D
   machinery is proven by the planted control, and the plan's hypothesised OQ-50↔OQ-122 cycle is
   moot (OQ-50 resolved). Checker: **0 problems** on real data (measured-clean: the OQ-9007
   positive control proves the probe fires).

## Step-5 §E verdict — the only claim in doubt (`adjudication.txt`)

View vs an independent naive cold-reader baseline (prose-surface; status-blind, Ω-blind):
**57 confirm, 7 contradict, 0 standoff.** Each CONTRADICT — the view overturning the flat read —
is settled by an EXTERNAL fact, not preference:

| OQ | baseline | view | settled by (external fact) |
|---|---|---|---|
| OQ-37 | blocked | workable_now | blocker OQ-90 status=**resolved** |
| OQ-41 | blocked | workable_now | blocker OQ-46 status=**resolved** |
| OQ-03/56/58/69/82 | workable | blocked_on_human | node's own **Ω-type=Ω_P** (routes out to a human) |

Two refinements the resolver adds that a flat read cannot cheaply make:
1. **status-check** drops resolved blockers (OQ-37, OQ-41) a cold reader holds blocked on the
   prose phrase "gated on OQ-90" / "stopgap until OQ-46";
2. **Ω-type routing** splits not-workable into `blocked` vs `blocked_on_human` (Ω_P routes out;
   may never resolve) — the omega taxonomy's whole point, collapsed by a workable/blocked baseline.

**≥1 §A defect-OQ advanced through the view:** OQ-129 (Evaluation-layer §A defect) landed in
step 1 and routes to workable_now; OQ-37 (Wiring-layer "dead feeder" §A defect) is one of the
two status-check wins. **Pilot success criterion met: a checkable confirm/contradict verdict AND
≥1 §A defect-OQ advanced.**

## A model gap the pilot surfaced (the pilot earning its keep)

Three active **Ω_E** entries (OQ-71, OQ-75, OQ-119) are blocked on an **operator-spend-go /
substrate gate** — a live human gate that is *not* an OQ→OQ edge and *not* an Ω_P blocker, so the
plan's OQ→OQ + Ω_P model mis-buckets them as workable_now. Surfaced during step-3 authoring and
fixed with one honest relator, `blocked_on_human <freetext>`, authored from the prose ("gated on
operator go", "blocked on substrate"). Recorded as a refinement to the §2 access-point families.

## Honest limitations (stated, not hidden)

- **§E independence is partial.** A single agent authored both the `Deps:` edges and the baseline,
  so the two-party protocol is not truly independent. The verdicts survive only because each
  CONTRADICT cites an external fact (blocker status / authored Ω-type) that settles it regardless
  of authorship — the checkable half of §E, not the by-eye half.
- **No live standoff** to exercise §D on real data; the §D positive control is the planted
  fixture only. If a future blocking cycle among active OQs appears, the view already handles it.
- The pilot validates the **machinery**; scaling to the 4,430-omega corpus (§C) still needs the
  §8-style "is this omega real?" gate on the authored omegas — a separate gate the pilot's
  human-authored 128 records don't need.

## Files
`adjudicate.py`, `adjudication.txt`, `frontier_view.json`, `selftest.txt`. Apparatus:
`python/omega_resolver.py`.
