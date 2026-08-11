# Verdict grammar amendment + extension conditional (operator, 2026-08-10)

**Status: BINDING pre-registration content.** `PREREGISTRATION.md` incorporates this file
**verbatim** when it is frozen; this file is the canonical location and the prereg does not restate
its thresholds. Written before any model call, before direction-(ii) extraction, and before any
matrix exists.

**Why it exists.** The secondary-class observation (`packets/wu_source/observation_secondary_class_predicts_disagreement.md`)
threatens the pre-registered verdict grammar. If Wu's 10 disagreements trace **genuine multi-class
membership** rather than coding instability, then the frozen rule — *expressible iff ≥2/3 of a
class's unanimous members land in ONE pattern; a two-pattern split is "partial"* — misreads its
most interesting outcome. A class splitting across two of our patterns could mean the mapping is
correctly **one-to-two**, not that expressibility is partial. Under multi-membership, "partial"
conflates a true structural mapping with coder instability, and those need opposite readings.

---

## A. Stratum re-declaration

**The selection rule does NOT change and remains frozen:** the stratum is the units whose catalog
class equals their dataset class, computed mechanically from the two md5-pinned files before any
coding (12 units; the complement is 10). Nothing about *which units are in it* moves. What changes
is the **interpretation** of the stratum and the **grammar applied to each side of it**.

Two declared readings of what the stratum is. Both are recorded; neither is asserted:

| reading | claim | status |
|---|---|---|
| **(i) codeability artifact** | agreement selects incidents both sources found *easy to classify* | the original declared cost, unchanged |
| **(ii) single-class** | agreement selects incidents that are *structurally single-class* | supported by an OBSERVATION with three live defeaters — **not a finding** |

**Status inheritance, stated so it cannot be laundered.** Reading (ii) is why the primary read
moves to the stratum, and reading (ii) rests on a non-blind, post-hoc, n=22 observation. **This
amendment does not upgrade that observation to a finding, and no writeup sentence may cite the
amendment as evidence for it.** The dependency runs one way only.

**Pre-registered consequence if reading (ii) is later disconfirmed** (by the blind test specified
in the observation file, or otherwise): the stratum reverts to "merely easier," the primary
expressibility read on it becomes a codeability-biased number, and **the headline must move back
to the full 22**. Recorded now so that reversal is a pre-committed move rather than a judgement
call made by whoever holds the pen at the time.

**The declared cost, sharpened rather than retired.** The earlier freeze said the stratum's higher
expressibility is partly a codeability artifact and is "never the cleaner number." That still
holds, and gains a second edge: on reading (ii) the stratum is easier **because those incidents are
structurally simpler**, not only because they are better written. So the pre-registered error is
now stated in both directions:

- the stratum's expressibility figure may **not** be presented as *the* expressibility of the
  taxonomy, and
- a full-22 headline may **not** be derived from it.

**This supersedes one earlier ruling and the supersession is named.** `RECON.md` §R2 point 3 froze
the stratum's use as "narrow: a check that a full-22 verdict is not being *driven* by the ambiguous
10 — direction of robustness, never a headline." The operator has now promoted the stratum to
carry the **primary** expressibility read. A cold reader meeting both texts should read this one as
current. The narrow-use clause is not deleted, it is *replaced*: robustness now runs the other way,
with the full 22 reported as the robustness read.

---

## B. Verdict grammar, per stratum

### Single-class stratum (the 12) — PRIMARY expressibility read

| verdict | rule |
|---|---|
| **expressible** | ≥2/3 of the class's unanimous members land in ONE pattern |
| **partial** | unanimous members land in exactly two patterns — **see §C, this is the ambiguous row** |
| **inexpressible** | ≥1/3 of unanimous members land in `other` |

### Multi-membership stratum (the 10) — a two-pattern split is EXPECTED, not degraded

| verdict | rule | reading |
|---|---|---|
| **one-to-two (expected)** | unanimous members land in exactly two patterns | the image of Wu's own dual membership; **not** a degraded result |
| **collapse** | unanimous members land in ONE pattern | **informative in its own right**: our six do not resolve a distinction Wu's two records disagree about |
| **diffuse** | three or more patterns, or ≥1/3 to `other` | no coherent mapping |

`collapse` is pre-registered as a first-class outcome precisely because it is the one that would
otherwise be reported as a *success* ("the class is expressible!") while meaning something closer
to the opposite.

---

## C. The two readings of "partial", named

| name | claim | signature |
|---|---|---|
| **R-map** | Wu's class genuinely instantiates **two** of our patterns; the mapping is one-to-two | split is *between* units, each unit individually stable |
| **R-churn** | the coder could not settle; the split reflects measurement, not structure | split is accompanied by units failing to reach unanimity |

Note that the k=3 unanimity rule already removes *within-unit* instability — a non-unanimous unit
goes to the UNSTABLE row and never enters a cell. So a two-pattern split **among unanimous
members** is already partial evidence for R-map. The tiebreaker below makes that explicit and
measurable rather than leaving it as an inference.

## D. Tiebreaker, fixed now

**Primary tiebreaker — class-level UNSTABLE rate against the direction's overall UNSTABLE rate:**

- **R-map favoured** if the class's UNSTABLE rate is **≤** the direction's overall UNSTABLE rate.
  The units are individually as stable as anything else in the run; the split is between units.
- **R-churn favoured** if the class's UNSTABLE rate is **>** the direction's overall rate. The
  class is where the coder wobbles, and the split is a symptom of that.
- **UNINFORMATIVE — no tiebreak, row ships typed OPEN** — if the class has **fewer than 4 unanimous
  members**. Declared in advance, same shape as R5's uninformative branch, so a thin split is never
  read as a mapping.

**Secondary, corroborating only, explicitly NOT decisive:** which stratum the split appears in — a
split inside the single-class stratum leans R-churn, inside the multi-membership stratum leans
R-map. It is non-decisive **because the stratum's meaning rests on the observation**, and letting
it decide would close the loop from observation to verdict without the blind test.

**Any R-map verdict ships as PROPOSED Ω_C**, per this OQ's own Ω-type declaration (mapping
semantics are Ω_C). It must name **which two patterns**, and it awaits an operator ruling. R-map is
never emitted as a finding by the assembler.

---

## E. Extension conditional — the H5-gate decision is a measurement, not a scope argument

The pinned rule ("the extension changes n and NOTHING else") makes the volume problem look
definitional: at 5,176 KB the full 73 needs a different extraction protocol, and a different
protocol is not "n and nothing else," so the extension would be a new experiment by fiat.

**The overlap units make it testable instead.** If two independent extractors, working the same
source directory, produce units that code the same, then a multi-extractor protocol is
*demonstrably equivalent* to a single-extractor one — the extension then changes n **in effect**,
which is what the rule protects. If they do not, extraction variance is a live confound and the
extension needs its own pre-registration regardless of volume.

**Measurement.** 4 overlap units (2 per extractor), direction (ii) only, each extracted
independently twice and each extraction coded at k=3. Per unit:

- both extractions unanimous and **equal** → **agree**
- both unanimous and **different** → **FLIP**
- either extraction UNSTABLE → **uninformative** (recorded, not counted as agreement)

**Thresholds, fixed now:**

| outcome | condition | consequence at the H5 gate |
|---|---|---|
| **INSIDE FLOOR** | 4/4 agree | extension may be priced as changes-n-only |
| **LIVE** | ≥2 FLIPs | extraction variance is a confound; extension needs its own prereg regardless of volume |
| **INDETERMINATE** | 1 FLIP, or ≥2 uninformative | **fail-closed to "needs its own prereg"** |

The middle band fails closed deliberately: absence of resolution must not license the cheaper path
(Build Discipline Pattern 5 — a gate that passes because its input is missing).

**MDE, declared.** 4 units gives **25% resolution**. This can distinguish *no* extraction churn
from *substantial* extraction churn and nothing finer; a single flip is already the indeterminate
band. Raising overlap to 3 per extractor (6 units, 17%) would buy one more discrimination step —
**not adopted this run**, flagged so the resolution is a known limit rather than a discovered one.

**Scope limit.** This floor is measured on **direction (ii) only**. Direction (i) was extracted
whole by a single extractor, so the floor does **not** license any claim about direction-(i)
extraction variance. Any writeup sentence generalising it across directions is a pre-registered
error.

**Accounting.** Overlap units are quarantined from all matrices but their calls **do** count toward
the driver's expected payload-capture count (the Phase-3 standing check compares captured payloads
against expected calls, and a quarantined call is still a call).

---

## F. Training-exposure leak — the source-identifying ban belongs to the registered confound

The source-identifying terms banned in both directions (`openclaw` / `Wu` / `arxiv` one way,
`OQ-\d+` / `CLAUDE.md` / `deferential realism` the other) are **not a new design element and need
no separate ruling.** They are the direct consequence of the confound already registered.

The same-family confound says: our writeups are Claude-authored and the coder is Claude, so
direction (ii) agreement may be inflated by prose-convention familiarity. The identical mechanism
runs one level deeper — **a Claude coder that recognises Wu's paper can recall his five classes
from training rather than reasoning from the definitions in its prompt.** That is not a leak
*through the payload*, which the grep catches; it is a leak *through the weights*, which nothing
else in this design catches. Filed under the existing confound accordingly.

**The ban reduces the channel and does not close it, stated plainly so nobody reads it as
discharged.** If the model recognises the *incidents themselves* — a production agent runtime
with a distinctive fingerprint, a 60-day sandbox denial, a specific reserved-file self-silencing —
no amount of vocabulary stripping helps. Redaction operates on names; recognition operates on
situations. A clean leak-grep is therefore evidence about the payload channel **only**, and no
writeup sentence may promote it to evidence about training exposure.

**The falsifier is unchanged and now does double duty.** The named tier falsifier for the
same-family confound is a **different-family model re-code**. It falsifies *both* channels at once
— prose familiarity and training exposure — because a different family has neither our conventions
nor, plausibly, the same exposure to Wu's artifacts. That raises its value against its cost, and
the writeup should say so when it records the falsifier as named-but-not-bought this run.

## G. Writeup obligations added by this amendment

1. **`movespeed_tcc_sandbox` is a worked P2 instance inside Wu's own artifact.** One canonical
   labeling became two; no queryable fact says which governs; and the row's own `paper_class_ref`
   contradicts its own `taxonomy_class`. It is the paper's §5.1 headline incident. **Both
   taxonomies are forked** — ours between `CLAUDE.md` and `build_discipline.md` (OQ-278), his
   between catalog and dataset — **both forks were found by an outsider, and neither author
   noticed his own.** The corpus in which our pattern is instantiated has no stake in either
   taxonomy, which is what makes it evidence rather than self-application.

2. **Three dated instrument defects from one arc, written up ONCE as a set, not three times.**
   §6.4's recursion — controls need controls — is currently argued from a *hypothetical* no-op
   harness. Three instances from a single pre-spend arc is a stronger section than the argument it
   replaces, and the set has one shared property that no individual instance shows:

   | # | instrument | what it returned | the wrong object it measured |
   |---|---|---|---|
   | 1 | frame-census control, v2 | assertion passed | asserted every extracted name ends in `.md` — **encoding the very assumption the control existed to test**; false for a nested hit, which yields a subdirectory name |
   | 2 | secondary-class predictor, first pass | **6** rows (truth: 8) | regex over structured YAML under-read a multi-line field — **and 6 agreed with the conclusion 8 supports** |
   | 3 | de-blocking witness | baseline max-run **2** (truth: 8) | `sum(1 for _ in g)` over `groupby` counted the `(key, grouper)` **tuple** — reported a fully blocked baseline as already unblocked |

   **The shared property: each returned a well-formed, plausible number about the wrong object,
   from inside the witness for the claim it supported.** Not one produced an error, an exception,
   an implausible value, or a result pointing the wrong way. #2 is the sharpest — it is §6.3's
   `identical: True because both empty` **inverted**: there, two failed measurements agreed with
   each other; here a **failed measurement agreed with a sound one**, so the agreement was actively
   *reassuring*. The error had no signal anywhere in the loop: not in the output, not in the
   direction, not in the plausibility.

   **The honest limit on detection, recorded beside them.** *All three were caught by hand-checking
   a number that looked fine — none by any control.* The control architecture this project runs
   caught zero of the three defects that occurred inside its own instruments. That is the most
   uncomfortable available datum about the apparatus and it belongs in §6.4 with the instances,
   not softened. It also sharpens what the `Fired:` bit can and cannot measure (OQ-276): a catch
   rate computed over controls does not see catches that arrive by suspicion.
