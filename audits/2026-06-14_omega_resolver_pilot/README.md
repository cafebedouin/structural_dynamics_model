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
4. **Frontier view renders all 65 active OQs** (`frontier_view.json`, regenerated at HEAD
   7cc79689 so `store_version == HEAD` — the §1b freshness control passing): 50 workable_now /
   12 blocked_on_human / 3 blocked / **0 standoff**. The live blocking graph is acyclic — the §D
   machinery is proven by the planted control, and the plan's hypothesised OQ-50↔OQ-122 cycle is
   moot (OQ-50 resolved). Checker: **0 problems** on real data (measured-clean: the OQ-9007
   positive control proves the probe fires). *(The first-committed artifact read 64, stamped one
   commit before OQ-130 was minted; the manifest exposed the lag, regenerated — see adjudicator
   note below.)*

## Step-5 §E verdict — the only claim in doubt (`adjudication.txt`)

**Corrected headline (do not quote "57/7").** The ablation baseline is status/Ω-blind over the
*same* authored `Deps:` the view reads, so its agreement on the 43 edge-free OQs (both default to
`workable_now`) is a shared default, **not corroboration**. The honest result is **22 meaningful
comparisons = 7 contradict + 15 substantive confirm** (raw confirms 58/65). The 7 contradicts —
the view overturning the flat read — are each settled by an EXTERNAL fact, not preference:

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

## Adjudicator's independent verdict (held baseline — the real independence test)

The `adjudicate.py` baseline above is an *ablation*, not independent. The independent test is the
separate adjudicator's held baseline (a cold instance's prose-only prioritization of `ISSUES.md`,
withheld from the executor per §E). Its two sharpest predictions, adjudicated against the entries:

- **"OQ-44 is a hidden hub behind {35,37,41,36,43}" → correct-but-stale; view earns its keep.**
  OQ-44 is *resolved* (policy ruled 2026-06-11). The authored `Deps:` for OQ-37/41 (`blocked_on
  OQ-90`/`OQ-46`) are faithful to their own prose ("now UNBLOCKED: OQ-90 RESOLVED"; "stopgap until
  OQ-46 (resolved)"); the view routes both `workable_now`, overturning the cold reader's "blocked"
  (read from blocking language without seeing the blockers resolve — Pattern 5 in the baseline).
- **"OQ-56 is the keystone of {53,55,56}" → structure confirmed, reachability corrected.**
  53 and 55 are authored `blocked_on OQ-56` (faithful). But OQ-56 is genuinely **Ω_P** ("the
  taxonomy is the user's to rule, not the engine's"), so the view correctly routes it
  `blocked_on_human`, overturning the baseline's "workable keystone."
- **"OQ-122↔128↔129 SCC, rule together"** is honored as *grouping* relators (not blocking), so no
  blocking cycle — `standoff=0` live, the SCC machinery proven by the planted control.

Every contradiction resolved against an external fact (resolved-status or authored Ω_P), never
preference. **The view earns its keep against the independent baseline too.**

## What the verdict does NOT prove (the ceiling, named so the scale arm isn't over-trusted)

The proven claim is narrow: **the view computes reachability correctly where reachability is a
function of facts** — mechanical propagation of resolved-status and Ω-type that a prose-reader
misses. Every one of the 7 contradictions was of that kind. **Not tested:** where the *judgment*
lives — the *ranking* among workable-now items ("which of 50 to do first"), as opposed to the
bucketing. Standoff is proven by the planted fixture, not live data (0 live). For ISSUES.md the
ranking gap is small (65 readable entries, eyeball-able); **for the 4,430-omega corpus it is the
whole game** — that is where "right" is a priority judgment, most omegas carry no external fact to
settle their order, and the eyeball check is unavailable. **The pilot validated the apparatus,
which was never the doubt for the corpus; the corpus doubt (are the authored omegas real, and can
the routing rank them) is untouched and is OQ-130's burden, not this pilot's result.**

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
