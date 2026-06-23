# OQ-112 Round 2 — Round 0 (re-witness on 92) + the witness-truth control

**Date:** 2026-06-22  **Corpus:** live `testsets`, **LIVE=92** (pinned).
**Code:** working tree on `main`; manifest commit `ab8d1d7` (n=92, code_dirty).
**Status:** Round 0 complete; defer-vs-build seat RULED (B, count-determined, **two
falsifiers**); witness-truth (premature-assertion) verdict = **PASSED ×2 (`:555`,
`:734`) · OPEN ×1 (indexed assert nonexistent — not-yet-constructible)**. This is a
**per-assert-site** verdict, NOT a stage-level pass (operator, 2026-06-22). **Build NOT
started.** Round 0 reviewed by operator; the build opens with the indexed assert as its
first and highest-risk write, gated by its own forced-throw control before anything reads it.

Every verdict cites a pasted probe, not prose. Read-only w.r.t. the engine — the only
mutation anywhere below is a test-local `constraint_claim/2` fact, asserted then
retracted under `setup_call_cleanup` (POST witness confirms restore).

---

## Round 0 — the three witnesses

### W1 — re-pin / confirm 92 (`pinned_corpus.txt`)

Manifest commit is still `ab8d1d7` == Round-1's pin commit → the plan's "re-pin fresh
only if the commit differs" condition does NOT trigger; membership re-emitted anyway.
`LIVE=92`, and the 92-id membership is **byte-identical** to Round 1's pin:

```
diff(Round-1 92, Round-2 92) -> IDENTICAL: Round-2 92 == Round-1 92
```

### W2 — item-4 reachability on 92: throw is REAL but LATENT (not live)

`item4_rewitness_92.txt` (the 62-row sentinel trace re-run on 92) + the corrected
profile-present probe `reachability_and_zero_92.txt`. Key lines:

```
W2a ABSENT_SUPP (6 of 92): [actinide_..._contradictions, digital_money_..._contradictions,
    knowledge_legitimacy_biomedicine_contradictions, performance_..._contradictions,
    polaris_document_status_contradictions, visual_evidentiary_authority_contradictions]
W2b   claim-LESS (excluded from maxent_run discovery): 6 of 6   <-- ALL of them
W2b   claim-BEARING (WOULD enter maxent -> hit sink):   0 of 6
W2b   MAXENT_DISCOVERY_N = 86  (corpus = 92; absorbed-by-exclusion = 6)
W2c ISOLATED sink: gaussian_log_likelihood(unknown,0.5,0.2,_) -> error(type_error(evaluable,unknown/0))
W2d maxent_run(<civ context>) -> success (summary=maxent_summary(86,...))
W2d PROFILE PRESENT (snare,suppression,<ctx>) = params(0.7468..., 0.0605...)
W2d PROFILE-PRESENT sink on actinide_..._contradictions (snare) -> error(type_error(evaluable,unknown/0))
W2d control (present-supp) actinide_..._flat_control -> success (LL=-105.0)   <-- numeric, discriminates
```

**Reading.** The sink genuinely throws `type_error(evaluable, unknown/0)` — witnessed
two ways: **isolated** (W2c) and **profile-present** (W2d, after `maxent_run` populated
`maxent_profile(snare,suppression,·)`; this is the real throw, NOT the `LL=-10.0`
prior-fallback non-witness the original 62-row trace produced when profiles were absent).
The present-suppression control returns a number on the identical goal, so the probe
demonstrably tells throw from value.

**But the throw is NOT reachable through the live driver on 92.** All 6
unknown-suppression constraints are **claim-less**, so maxent's discovery
(`constraint_claim/1` filter) drops exactly those 6 → `N=86` enter, none of which carries
the sentinel. The item-4 hazard is **latent**: it fires only when a *claim-bearing* story
is missing `suppression_requirement` (currently 0 such). This matches the plan's framing
("first claim-bearing story missing `suppression_requirement` triggers it").

### W3 — zero-with-witness count = 0 (`reachability_and_zero_92.txt`)

```
W3 maxent_run_info facts present: [ri(<civ context>, 86)]
W3 ZERO_WITH_WITNESS_COUNT = 0   zeros=[]
```

After exercising both run_info-asserting stages (`maxent_run` :555, `maxent_multi_run`
:734), **no maxent stage on the live 92 emits `maxent_run_info(_, 0, _)`** — every
asserted witness carries N=86. (`maxent_indexed_run` asserts none at all — see the build
caveat below.)

---

## Defer-vs-build ruling (delegated to the count; per operator this turn)

The mechanical rule (plan §"ruling selects only one fork"): **zero zero-with-witness
stages → (B) defer is witnessed-safe; any such stage → (A) is forced.** W3 count = **0**.

**RULING: (B) — Defer under commitment-plus-falsifier.** Ship the witness-bearing gate
(settled, ruling-invariant). Record in OQ-112 — **TWO falsifiers, two probes** (the
single-falsifier wording would let a re-checker miss the path W2 actually surfaced):

> *Assertion:* on the live 92, no maxent stage emits `maxent_run_info(_, 0, _)` **and**
> the only path to a zero-output stage (a claim-bearing story missing
> `suppression_requirement`) is upstream-pruned — so the zero-legal-with-witness
> producer/loader/consumer handshake (fork A) is not built this round.
> *Falsifier 1 (zero-with-witness):* any maxent stage emitting `maxent_run_info(_, 0, _)`
> on a live corpus → fork (A) forced. Re-check via `reachability_and_zero_92.pl` **W3**.
> *Falsifier 2 (latent item-4 hazard):* any **claim-bearing** story lacking
> `suppression_requirement` appears (count currently 0, W2) → the sink fires, the stage
> voids, and `run_info` is **absent** (NOT zero-with-witness — so the gate catches it by
> absence, correctly). Re-check via the **item-4 reachability probe**
> (`probe_reachability_and_zero_92.pl`, W2b), **NOT W3** — W3 counts zero-with-witness
> and is blind to the claim-less→claim-bearing transition.

This is a count-determined fork, not a free seat — reversible the moment either falsifier
fires. The gate form is correct either way (collapses to flat-fail-closed if
zero-with-witness never occurs).

**[EDGE] What defer-B actually rests on (so a later reader doesn't misread it).** Defer-B
is safe on 92 **not because the hazard is absent** but because the only zero-output path is
upstream-pruned by the `constraint_claim/1` discovery filter — and that pruning is itself
the latent hazard item 2 exists to gate. So defer-B's *safety* and the gate's *necessity*
rest on the **same fact** (the 6 unknown-suppression constraints being claim-less). "Defer-B
safe" ≠ "hazard absent": the hazard is **gated and upstream-pruned**, two different claims
with two different falsifiers (above). The gate catches the hazard by absence when the
pruning lapses; the defer rides on the pruning holding.

---

## The witness-truth (premature-assertion) control — PASSED ×2 (:555/:734) · OPEN ×1 (indexed)

**Per-assert-site verdict — NOT a stage-level pass.** "The control passed" would invite an
aggregation the evidence does not support: the witness-truth property is established per
assertion site, and there are three sites with three verdicts — `:555` PASSED, `:734`
PASSED, indexed **OPEN-by-nonexistence** (§"the indexed path" below). Two of three discharged
does not make a passed gate.

**Why this is the DO-FIRST gate, not a verify-among-five.** The gate reads
`maxent_run_info` as "this stage genuinely completed." If that fact were asserted
*before* the per-constraint loop that can throw, a mid-loop throw would have *already*
written its own completion witness → the gate reads it clean → Pattern 6 reconstituted
inside its own fix. So the build may not be trusted until a forced mid-loop throw is shown
to leave `maxent_run_info` ABSENT.

### Two-sided + phase-resolved witness

`witness_truth_control.txt` (driver level) and `witness_truth_phase.txt` (phase + literal
mid-index):

```
CTRL clean: maxent_run -> success ; run_info_count=1            <-- positive control: present IS detectable
PRE: actinide_..._contradictions is claim-less (excluded)      <-- baseline
FORCED: discovery N=87 ; THROWER at index K=1 of N
FORCED: maxent_run -> error(type_error(evaluable,unknown/0)) ; run_info FOR Ctx ABSENT   <-- GOOD
FORCED: total run_info_count=0
FORCED multi_run -> error(type_error(evaluable,unknown/0)) ; run_info ABSENT              <-- GOOD (:734)
POST: actinide_..._contradictions claim-less again -- cleanup OK

PHASE: discovery N=87 ; THROWER (polaris_..._contradictions) at index K=52 (mid-list)
PHASE A maxent_compute_profiles(full list incl thrower) -> success     <-- throw is NOT in the profile phase
PHASE B maxent_classify_all([thrower], profiles present) -> error(type_error(evaluable,unknown/0))
                                                                       <-- throw IS in the per-constraint loop
PHASE C real maxent_run (thrower at K=52) -> error(...) ; run_info ABSENT, count=0   <-- GOOD, literal mid-loop
```

**Conclusion.** `maxent_run_info` is asserted strictly *after* `maxent_classify_all`
(read: `:555`, `:734`; empirical: a throw at the per-constraint loop, isolated by PHASE B,
at literal mid-index K=52 of 87, leaves the witness absent and the count 0). The plan's
DO-FIRST worry — assert positioned ahead of the loop — is **empirically falsified for
`maxent_run` and `maxent_multi_run`**. Their assertion sites do **not** need to be moved;
the gate can sit downstream of the existing asserts **for these two paths only**. The
positive control is what earns the trust: clean run → count=1, thrown run → count=0, so
absence is *discriminating*, not byte-identical-to-never-looked.

### The indexed path — OPEN-by-nonexistence, and the highest-risk write in Round 2

`maxent_indexed_run` (`:870–904`) asserts **no** `maxent_run_info` at all (confirmed by
read + W3 showing only the `maxent_run`/`multi_run` fact). So the indexed stage's
witness-truth is **not "deferred-pending-recheck" and not "passed" — it is
not-yet-constructible**: there is no assert to be downstream of, and the control is
**unrunnable today** because there is nothing to control. Verdict: **OPEN**.

This makes the indexed assert **the single highest-risk line in Round 2** — it is the one
place the build *writes* a completion witness rather than *reads* an existing one, so it is
the one place the premature-assert Pattern-6 reconstitution can be introduced *by this fix*.
The order is therefore **forced and is a hard gate, not a verify-item**:

1. add the assert **after** `maxent_classify_all_indexed` (`:888`);
2. run the **K-of-N forced-throw control against the indexed path specifically**
   (adapt `probe_witness_truth_phase.pl`: inject a claim-bearing unknown-suppression
   constraint into indexed discovery, force the sink throw mid-loop, witness
   `maxent_run_info`/the new indexed completion fact ABSENT, count 0, two-sided clean→present);
3. the gate does **not** trust the indexed path until that paste exists.

The other two sites earned trust by control; this one must earn it the same way, and
cannot until it is built.

---

## STOP / forward step (operator-reviewed 2026-06-22)

Round 0 + ruling + the witness-truth control are done and pasted above; the operator
reviewed them and confirmed the per-assert-site verdict (PASSED ×2 / OPEN ×1) and the
two-falsifier defer. This commit lands the **read/measure pass alone** (separated-pass
discipline — measure before any engine write); the only mutation was a test-local
`constraint_claim/2`, asserted-then-retracted, POST-witnessed restored.

**The build opens with the indexed assert as its FIRST and HIGHEST-RISK write, gated by
its own forced-throw control before anything reads it** (§"the indexed path", the 3-step
hard gate). Downstream of that one control passing: the attempt marker (`json_report.pl:72/76`,
`trajectory_mining.pl:910/912`), the `verdict_join` fail-closed wire-in
(`diagnostic_summary.pl:632`), item 7 (`json_report.pl:428–431`), the regression test, the
AGENTS.md "completion-witness-or-fail-closed" invariant, and the ISSUES.md OQ-112 sync
(carry the two-falsifier defer + the indexed-OPEN into the entry — its current single-
falsifier wording from Round 1 is now superseded by this audit). No engine source edited yet.

## Files

- `pinned_corpus.txt` — W1 self-witnessing 92 membership + manifest.
- `probe_item4_rewitness_92.pl` / `item4_rewitness_92.txt` — 62-row sentinel trace re-run on 92.
- `probe_reachability_and_zero_92.pl` / `reachability_and_zero_92.txt` — W2 profile-present
  reachability (claim-less exclusion, isolated + profile-present throw, control) + W3 zero count.
- `probe_witness_truth_control.pl` / `witness_truth_control.txt` — driver-level premature-assertion
  control (clean positive control + forced throw, run + multi_run).
- `probe_witness_truth_phase.pl` / `witness_truth_phase.txt` — throw-phase resolution + literal
  mid-index (K=52) thrower.
