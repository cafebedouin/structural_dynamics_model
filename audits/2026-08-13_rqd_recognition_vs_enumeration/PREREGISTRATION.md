# PREREGISTRATION — RQ-d / Prediction 1: recognition versus enumeration

**Written:** 2026-08-13 · **Status:** FROZEN before any call · **Spend authorised by:** operator ("Do 3")
**Question:** `amnesiac_institution_v0_6.md` §14 RQ-d, second arm; `concealment_without_a_concealer_v0_4.md` §11 Prediction 1
**Claim under test:** A3 / E1 — *a party holding a compression can perform recognition but not enumeration; absences have no form to recognise.*

---

## 1. Why this one, and why now

A3 is the premise the whole §2 derivation stands on, and it is currently ANALYTIC with a note
saying it is testable. Of the four primary RQs it is the only one runnable from inside this
repository today: RQ1 needs foreign repositories, RQ2 needs a scorer that does not exist and was
mis-priced at "a weekend" against an actual 73-item preregistration, RQ3 needs a second operator.
This needs matched instances, one artifact, two prompts, and a count.

## 2. The manipulation, and what it deliberately does NOT vary

Each subject receives one design document under one of two protocols:

| arm | task demanded |
|---|---|
| **recognition** | read the design and confirm whether it is complete and correct |
| **enumeration** | write the prompt a receiving instance would need to EXECUTE it, as numbered concrete actions |

**Both arms close with a byte-identical required section** (`PROBLEMS OR GAPS:`), so the reported
channel is the same and only the cognitive route differs. This is the single most important
design decision here: if only the enumeration arm were asked for problems, any separation would
be an artifact of the question rather than of the route.

## 3. Materials and their altitude — stated so it cannot be lost downstream

10 authored design documents (`python/audits/rqd_materials.py`), each carrying **both** defects:

- an **OMISSION** — a required element the design never states
- an **ERROR** — a stated element that is wrong

**Within-artifact dissociation.** Both defects live in the same text, seen by the same subject
under the same prompt. This is what controls the effort/verbosity confound: "enumeration just
produces more words" predicts a lift on *both* defect kinds; A3 predicts a lift on OMISSION only.

**Altitude: these are AUTHORED materials, the floor of the discrimination ladder.** They are
lifted off the floor — but not to the top — by drawing every omission SHAPE from a witnessed
case in this repository's record (`source` field per item): judged-not-shown, production-without-
consumption, input-gates-only, a self-check that cannot consume its own format, orphaned
predecessor. **What this design can license is a claim about defect shapes that have really
occurred here, instantiated in fresh text. It cannot license a claim about naturally-arising
defects in the wild.** RQ-d's stronger form needs a naturally-arising corpus and is not this.

## 4. Subjects, scale, cost

`claude-sonnet-5` — the workforce model, not a cheaper stand-in, since the claim is about this
workforce. `max_tokens=2000`; thinking pinned off by `sampling_overrides` so the budget is pure
text. **10 specs × 2 protocols × 3 replicates = 60 calls.** Replicates are k=3 per the house
redraw standard; generation is stochastic and per-item rates inherit that churn (OQ-264), so
per-item rates are reported with their spread and never collapsed to a single pooled bit.

## 5. Scoring — written and controlled BEFORE the spend

`python/audits/rqd_scorer.py`, mechanical, no judge, blind by construction (it reads only the
identically-worded closing section and never sees the protocol label).

**Strict hit (the pre-registered primary):** a concept key for that defect occurs in a SENTENCE
that also contains a gap marker. Two consequences, both intended:

- using the word does not count — "compute the denominator" has not detected that the
  denominator is unspecified;
- **silently SUPPLYING a missing element does not count.** Surfacing the gap to the sender is
  the entire value being measured; a receiver who quietly fills it in leaves the defect in the
  design where it fires on the next receiver.

A **lenient** count (key anywhere, no marker) is reported alongside as a sensitivity check and
never as the headline. Responses lacking the closing section are recorded as `protocol_failure`
and **reported, not dropped**.

Scorer's own two-sided record (`--selftest`, 6 controls, GREEN at freeze): fires on a flagged
omission; declines on a clean response; **declines on the discriminating case** — concept present
but unmarked, i.e. silent supply — with lenient on and strict off; does not cross-score an error
flag as an omission; distinguishes an absent section from an empty one; and confirms the two key
sets are disjoint on all 10 specs, without which the within-artifact dissociation would be
unmeasurable by construction.

## 6. Outcome rules — pinned now, in advance

Rates are over 30 observations per arm (10 specs × 3 reps). **Effect-size floor, not a p-value:**

| # | condition | verdict |
|---|---|---|
| 1 | omission Δ (enum − recog) **≥ +0.20** AND error \|Δ\| **< 0.20** | **SUPPORTED** — dissociation; A3 gets a witnessed instance in this workforce, at the altitude of §3 |
| 2 | omission Δ **< +0.20** | **REFUTED HERE** — enumeration does not surface omissions better on these shapes |
| 3 | omission Δ ≥ +0.20 **AND** error Δ ≥ +0.20 | **CONFOUNDED** — enumeration lifts everything; consistent with effort/verbosity and NOT with the specific claim |
| 4 | omission Δ **≤ −0.20** | **REVERSED** — evidence against A3 |

**Outcome 3 is the one that would most change the papers**, and it is why the error arm exists.
It is recorded as a real result, not a nuisance.

**Ceiling/floor rule.** An item whose recognition-arm omission rate is **> 0.80 or < 0.10** has
no room to move and is flagged uninformative-for-Δ; pooled rates are reported both with and
without such items. Per-item rows are always shown — a pooled bit over heterogeneous items is
the reporting shape this project has already been burned by.

**Kill condition for the instrument, not the hypothesis.** If `protocol_failure` exceeds 10% of
units in either arm, the run is reported as an instrument failure and no Δ is claimed.

## 7. Pin manifest — every artifact whose substitution could change a number

Genre is not the criterion; causal dependence is. Executables are pinned, not just texts —
this is the rule the previous arc's 219 calls bought.

| artifact | md5 at freeze |
|---|---|
| `python/audits/rqd_materials.py` | `87881ddc78f299f29c4976b76dbb5df3` |
| `python/audits/rqd_scorer.py` | `978b4c7c9d44f9ed91ea12ad6e89f702` |
| `python/audits/rqd_driver.py` | `1bcd70fd20779d9803c569b3990c5a45` |

Both prompt templates live inside the pinned driver. `claude-sonnet-5`, `max_tokens=2000`,
thinking disabled. Analysis stage: `rqd_analyze.py`, written before the full run and pinned in
the writeup with its own md5 — it does not exist at freeze and that is declared here rather than
discovered later.

## 9. AMENDMENT 1 — scorer repaired mid-run (declared, not silently applied)

**When:** 2026-08-13, after the pilot and while the full run was in flight.
**Artifact:** `python/audits/rqd_scorer.py` · `978b4c7c9d44f9ed91ea12ad6e89f702` → re-pinned in the
writeup. The raw responses are untouched; only scoring changed, which is possible **only**
because the driver persists the raw datum before parsing.

**What was wrong.** `score_defect` split the section on sentence punctuation
(`(?<=[.!?])\s+`). Real model output breaks that: a bullet reading *"The exact material/prompt
shown to coders (full record vs. summary only, any length limits) is not specified."* splits at
**"vs."**, stranding the concept key in one fragment and the gap marker in the next. It scored
**False** on a bullet that flags the omission in as many words. The unit is now a line/bullet
with a 300-character proximity cap.

**How it was found, and how it was not.** By reading a real pilot response against the scorer's
verdict. **The six synthetic selftest controls passed throughout** — none of them contained an
abbreviation, so the fixtures could not express the failure. This is the *instrument validated in
one role, used in another* shape: the fixtures were authored prose, the input is model prose, and
the error profile belongs to the role rather than to the instrument.

**Direction, and why it matters.** The defect generated FALSE NEGATIVES, so it suppressed
detections in whichever arm writes more parenthetical prose — the enumeration arm already runs
~35% longer. Left in, it would have biased *against* the hypothesis; but a bias of unknown
asymmetry is not a conservative bias, it is an unmeasured one. **The analysis stage therefore
reports, per arm, how many strict hits the amendment added** (`--amendment-delta`), so the
repair's own effect on the result is visible rather than assumed benign.

**Controls added with the repair:** (7) the exact failing bullet, verbatim from pilot output, as a
naturally-arising regression fixture — a real positive from the population rather than an authored
decoy, which is the control the synthetic set could not supply; (8) a proximity-cap decline, so
the amendment cannot have traded a false negative for a false positive.

## 8. Declared limits

- One workforce, one model, one session. No claim of generality across models or parties.
- Authored materials (§3). The naturally-arising version of this experiment is a separate spend.
- Subjects are single-turn and fresh; a receiver in a real handoff carries session context, which
  the paper itself argues makes them catch *less*. If anything this over-estimates recognition.
- 10 items is enough for a floor-sized effect and not for a confident rate.
