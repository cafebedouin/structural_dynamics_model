# OQ-293 — RQ-d does not test A3 as designed: the frozen rule returns REFUTED, and an instrument audit shows the measure was SELECTION among ~8 named gaps, not detection

**Executed:** 2026-08-13
**OQ:** OQ-293 (minted at close; the question's prior home was `amnesiac_institution_v0_6.md` §14 RQ-d / `concealment_without_a_concealer_v0_4.md` §11 Prediction 1, tracked in neither ISSUES nor a dated audit until now)
**Verdict:** Under the pre-registered rule the omission-detection Δ (enumeration − recognition) is **−0.07** pooled and **+0.08** on informative items, both below the +0.20 floor, which the frozen rule reads as REFUTED — but a post-hoc instrument audit finds the materials admit ~8 nameable gaps each while scoring credits only the 1 authored, so **the null is uninformative about A3 and no refutation may be carried forward**; the design flaw is asymmetric (it damages nulls more than positives) and that asymmetry is stated rather than claimed away.
**Substrate:** no pipeline run. 60 model units, `claude-sonnet-5`, thinking disabled, `max_tokens=2000`. Materials and driver byte-unchanged from freeze (`87881ddc…`, `1bcd70fd…`).
**Fired:** live — Amendment 1 fired mid-run: a scorer defect that scored FALSE on a bullet flagging the omission verbatim, found by reading real output against the scorer's verdict while all six synthetic controls passed. It generated false negatives and would have biased the headline. A second control fired at close (the instrument audit), changing the result's status from a finding to uninformative.
**Evidence map:**
- `PREREGISTRATION.md` — frozen design, outcome rules §6, pin manifest §7, Amendment 1 §9
- `audit_log.md` — md5s at their positions; the ordering defect (commit postdates 4 pilot units)
- `responses/` — 60 raw units, persisted before parsing; the datum
- `analysis_output.txt` — the frozen analysis, verbatim
- `run_log.txt` — driver output incl. output gate
- `python/audits/rqd_{materials,scorer,driver,analyze}.py` — pinned instruments

---

## 1. What ran

10 authored design documents, each carrying **both** an omission and an error, under two
protocols (recognition: read-and-confirm; enumeration: write the receiver's concrete actions),
3 replicates, 60 units. Both arms closed with a byte-identical `PROBLEMS OR GAPS:` section so
the scored field was the same and the scorer was blind by construction.

`issued=56 skipped=4 failed=0`; output gate GREEN, 60/60 non-empty and parsing; **0/60 protocol
failures** in either arm.

## 2. The result under the frozen rule

```
                    omission            error
ALL ITEMS      rec 0.33 -> enum 0.27   rec 0.57 -> enum 0.43   Δ -0.07 / -0.13
INFORMATIVE    rec 0.33 -> enum 0.42   rec 0.58 -> enum 0.83   Δ +0.08 / +0.25
(n_items=4)
VERDICT [frozen, floor +0.20]: REFUTED HERE
```

6 of 10 items were pre-registered uninformative: 4 at floor (neither arm ever detected), 2 at
ceiling (recognition ≥ 1.00).

**Amendment 1 did not drive this.** The repair added **1** strict hit across all 60 units, to
the *recognition* arm — measured, not assumed, by retaining the pre-amendment scorer verbatim in
the analysis stage.

## 3. Two observations that survive the audit below

**Verbosity is not detection.** Enumeration produced **27% more text** (3,947 vs 3,114 mean
chars; 1,294 vs 997 mean output tokens) and found **fewer** of *both* defect kinds. Whatever the
enumeration prompt did, it did not trade length for catching. This is narrow but it is real, and
it cuts against the naive account that a longer response reviews more.

**Absences were harder than present-but-wrong content, in both arms** — omission 0.33/0.27
versus error 0.57/0.43. That is the direction A3 predicts about the *difficulty asymmetry*, and
it is the one pattern here not produced by the protocol manipulation. **It is an observation, not
a finding:** the two key sets differ in matchability as well as in what they denote, so part of
the gap may be the scorer rather than the subjects.

## 4. The instrument audit — why the null cannot be carried forward

Reading the four floor items, subjects had not failed to find gaps. They found many:

```
gap-flags emitted per response:  recognition mean 8.4   enumeration mean 7.9
TOTAL flags across 60 responses: 488
authored-omission strict hits:    18
```

Verbatim from `denominator` (scored 0.00 in both arms), recognition r1:

> *"Rolling rate is unspecified: no window size, decay function, or update rule is given
> (rolling over last N writeups? time window? all-time with decay?)."*

That is asking for the denominator. It scored zero because the key set says *"how many writeups"*
and the subject wrote *"last N writeups"*.

**The structural problem is larger than any key list.** A ~200-word design document
underspecifies far more than one thing. Subjects named ~8 gaps; the scorer credits 1. So the
measure is **which gaps a subject ranks into its top ~8**, not whether it can detect an absence.
Selection noise of that magnitude plausibly swamps a 0.20 effect.

**The asymmetry, stated rather than claimed away.** It is tempting to say this objection would
equally undermine a positive result, which would make it principled. That is not quite true and
the honest version is weaker: a **positive** result would have remained partially informative —
it would have shown enumeration shifts attention *toward* that class of gap — whereas a **null**
is genuinely ambiguous between "no effect" and "effect drowned in selection noise." The flaw
therefore damages nulls more than positives, which is precisely the direction that lets a
researcher discard an unwelcome result. Two things keep this from being that move: the flaw was
found the same way Amendment 1 was (reading data against the instrument), and it is recorded here
against the pre-registered verdict rather than in place of it. **The frozen verdict stands in the
record as REFUTED; what is withdrawn is its bearing on A3.**

## 5. What the next design must do

The fix converts *selection* into *recall*. Before the run, build an **exhaustive gap inventory**
per artifact — every substantive underspecification, enumerated by an independent pass and frozen
— then score each arm's **recall over that full inventory**, not over one authored target. Both
arms are then measured against the same denominator, ~8 flags become ~8 scoreable events instead
of 1, and per-item power rises about eight-fold at the same call count. Second change: shorter,
tighter artifacts, so the inventory is small enough to be genuinely exhaustive.

Until then, **A3 remains ANALYTIC with its kill condition unrun.** The paper's §0 typing already
says A3 is discharged by premises and refuted by exhibit; this audit does not change that, and
§14 RQ-d should record that the second arm has been attempted, at what cost, and why the first
attempt does not count.

## 6. Residue

- **OQ-293 minted** — carries the redesign in §5 as the next step, and the finding that the
  first attempt was selection-limited.
- **`amnesiac_institution_v0_6.md` §14 RQ-d and `concealment_without_a_concealer_v0_4.md` §11
  Prediction 1** both describe this experiment in the selection-limited form ("give matched
  reviewers the same artifact… omission-detection rates should separate"). Neither says how
  detection is scored, which is the omission this run discovered *in the papers' own
  specification of it* — the same shape as `judged_not_shown`, item 1 of the materials. Flagged
  for the authors; not edited here.
- **Ordering defect recorded** in `audit_log.md`: the freeze commit postdates 4 pilot units. Fix
  is one line — commit the md5 before the first call, pilot included.
- Instruments left in `python/audits/`, pinned by md5, reusable for the recall redesign.
