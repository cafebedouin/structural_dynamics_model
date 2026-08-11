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

---

## H. NO-UNIT row — the census proxy's PRECISION, and the boundary rule that makes it countable

**The hole, named as the operator's own (2026-08-11).** The escape check audits the §4.5 keyword
proxy's **recall** — directories it missed. **Nothing audits its precision** — directories it
admitted whose keywords matched something other than a reported incident. A proxy has two error
rates; one had an instrument and one did not.

**Asymmetric cost, which is why this is not a footnote.** A confirmed escape-check hit relabels
42% as a *lower bound* — a bounded correction in a known direction. NO-UNIT directories attack the
**numerator** directly and can move the point estimate either way depending on how the two rates
compare. **So 42% is currently a figure with one measured error direction and one unmeasured one,
and the writeup must say exactly that rather than reporting the escape check as though it closed
the question.**

### H.1 Boundary rule — fixed NOW, before the remaining units are extracted

A category that will be counted needs its boundary fixed before its members arrive; deciding
per-directory as they show up is how a category silently acquires an extractor's preference — the
same failure the multi-defect directories are already documenting.

> **A directory yields a UNIT if its prose REPORTS a silent-defect incident, anywhere in the
> document, regardless of whether that incident is the directory's subject. A directory is
> NO-UNIT only if its prose DISCUSSES the concept without reporting an instance.**

**Why this line and not "is the incident the subject?"** Because it is the line the census itself
draws. §4.5's numerator counts *incident-bearing directories* by searching **prose**; the claim
being audited is that the prose describes an incident, not that the directory's topic is one.
Adopting a stricter subject-level rule would measure a different quantity than the figure under
audit and the correction would not apply to it.

**Consequence for `2026-06-15_oq131_six_observer`: it is a UNIT**, and its unit stands. Its
incident (a measurement arm silently running against a 57-item substrate where the working set
held 64) is *reported*, not merely discussed; it simply sits in a self-audit subsection rather
than in the headline.

**New mandatory field, applied retroactively to all units: `incident_location`** — `subject` |
`self_audit_subsection` | `incidental`. This costs nothing and buys a second measurement free: if
the 73 are heavily weighted toward `self_audit_subsection`, the numerator counts directories that
noticed their own incidental defects, which supports a *weaker* claim than "73 audits were about
incidents." Report the distribution; do not collapse it.

### H.2 Pre-registered row and thresholds, fixed before the remaining six are extracted

Let **k** = operator-confirmed NO-UNIT directories in the primary sample (n=22).

| outcome | condition | licensed reading |
|---|---|---|
| **null** | k = 0 | **ONLY** "keyword-proxy false-positive rate ≤ **12.7%** at 95% confidence" (1 − 0.05^(1/22)). **NEVER** "the proxy is precise." Deliberately phrased to mirror the escape check's one-sided licence word for word. |
| **fired** | k ≥ 1 | the 73 numerator is an **OVERCOUNT**. Report the corrected figure as a RANGE combining this precision bound with the escape check's recall bound — never as a new point estimate — and mint a re-census OQ, mirroring the escape check's confirmed-hit consequence. |

**Operator confirmation is required per NO-UNIT, exactly as escape-check hits are quarantined.**
The extractor proposing NO-UNIT is the same party whose *thin extraction* is the competing
explanation for finding no incident. Self-certification here would let an extractor convert a hard
directory into a data point. The coder proposes, the operator rules — same rule, opposite
direction.

### H.3 Provenance belongs in the row

State in the row itself: **recall and precision were audited at different times, by different
mechanisms, and the precision audit arrived by accident** — an extractor noticing that one
directory nearly yielded nothing. It was not designed, pre-registered, or budgeted.

That provenance is not a confession, it is the argument: **the census needed a positive control it
never had.** A census whose output is a count of directories cannot distinguish "matched an
incident" from "matched the word" without one — the identical structure the frame control found
from the other direction, where "174 = 73 + 101, partition exact" is an arithmetic identity that
witnesses nothing about whether the census *classifies* correctly. Two independent routes to the
same missing control, one from the denominator and one from the numerator.

---

# Addendum (operator, 2026-08-11) — overlap accounting, and the channel nobody modelled

Written after extractor A's half closed at 13/13, before extractor B started. Incorporated into
`PREREGISTRATION.md` verbatim with the rest of this file.

## I. Overlap accounting — `role` stops encoding two facts

**Ruling: A's 01 and 06 STAY matrix units.** All thirteen of A's units carry `role: "primary"`;
the overlap relationship moves to its own boolean, `overlap_source`, true on the four
floor-participating directories (`2025-05-15_recon_2`, `2026-06-11_oq44_policy_close`,
`2026-06-27_oq124_oq149_committer_convention_control`, `2026-07-11_oq186_oq188_readsite`).
**The driver quarantines on `matrix_unit`, never on `role`** — see §I.2, which corrects this
clause as originally written.

**Why the field split, stated as the defect it removes.** `role` was being asked to encode two
independent facts: whether a unit enters the matrices, and whether it participates in a floor
comparison. Matrix membership is a **sampling** fact, fixed by the seeded draw; floor
participation is a **control** fact, fixed by the overlap design. They vary independently, and one
string cannot say both without ambiguity — which is exactly what produced the inconsistency
(`01` labelled `primary+overlap`, `06` labelled `primary`, both the same kind of thing).

**Why not exclude them.** Excluding the four would drop seeded-sample units from a stratum already
thin at n=22, to protect against a contamination that does not exist. A's extraction of `01` is
not contaminated by B's later independent extraction of the same directory. **The comparison is
quarantined, not the units** — and it is B's versions of the four overlap directories that never
enter cells.

**Cell accounting, so it cannot drift:** each of the 22 sampled directories contributes exactly
ONE unit to the matrices. A contributes 13, B contributes 9, and B's four
overlap-directory extractions are floor-only. 13 + 9 = 22.

### I.1 Pre-registered limit on what the floor licenses

**Four units now have an independently measured extraction error bar. The other nine do not.**
If the floor comes out non-trivial, those four are the only units whose extraction quality is
known.

> **Pre-registered NOW, before the floor is measured:** the four overlap units may NOT be treated
> as representative of extraction quality generally. Any statement about the reliability of the
> unit set is scoped to the four measured directories unless a separate argument for
> representativeness is made and stated. **Four measured units do not license a claim about
> thirteen, or about twenty-two.**

Stated here rather than at close because a limit written after seeing the floor is a limit chosen
against a known result. If the floor is trivial (4/4 INSIDE FLOOR), this clause costs nothing; if
it is not, this clause is the difference between a scoped finding and an implicit claim.

## J. Writeup obligation — the memory system is a leak channel

**The catch.** A commit message landing the two overlap units summarized both incidents in one
line each. Those two directories are extractor B's PRIMARY assignments. A `git log` read by B
would have anchored B's independent extraction on A's selection, converting the extraction-churn
floor into a self-comparison that reports INSIDE FLOOR by construction — and §E rides the
extension decision on that number. Caught by the author before anything was pushed; the message
was amended to state, in place of the content, why the content is absent.

**Why it generalizes, and why it is the sharpest catch of the arc.** Every leak control in this
design targets what reaches the **coder**: payload greps, banned lexicons, redaction, the planted
leak. This one reached the **extractor**, through **version control** — infrastructure the design
treats as neutral substrate rather than as a channel. In an institution whose memory IS a git
repository, the memory system is itself a leak channel, and a blinding protocol has to name it
explicitly, because it is the one surface everyone reads and nobody registers as an input.

**Carry all three surfaces to the writeup, not just the commit message:** `git log`, `git show`
on the landing commits (which prints the unit bodies verbatim), and any sweep that globs the unit
directory and prints what it loads.

**Second-order note for the same section.** This is the second time in this arc that a leak
response was **fixed in advance rather than negotiated at discovery** — the first being the
quarantine rule for escape-check hits. The pre-committed response here is *declare it and VOID
that pair's floor comparison*, never patch and continue: a voided comparison is recoverable, a
silently contaminated one licenses a scope decision on a fabricated basis.

## K. Second directional guess declined by the data

`incident_location` over A's 13: **subject 10 / incidental 2 / self_audit_subsection 1.**

The operator's reading at the interim boundary (A's first 7, which stood at 6 subject / 1
self-audit) raised the possibility that the numerator is weighted toward directories that merely
noticed their own incidental defects — which would support a weaker claim than "73 audits were
about incidents" (§H.1). **The completed half does not show that skew**, and the reading is
recorded as **not held**.

Recorded the same way the pre-registered C/D disagreement guess was recorded as wrong (`RECON.md`
§R2a). **This is the second directional guess of this arc that the data declined.** Both are kept
in the record at the same volume as the confirmations; a design that only remembers its correct
priors is measuring its own memory.


### I.2 Correction to §I, found when B's half landed — `overlap_source` cannot answer the driver's question

§I as written said the driver quarantines on `overlap_source`. **Measured, that yields 18 cells,
not 22:**

```
quarantine on overlap_source alone -> 18 cells  (RULING SAYS 22)
quarantine on matrix_unit         -> 22 cells   (A 13 + B 9)
```

`overlap_source` is a property of the **directory** (is it floor-participating?), so BOTH
extractions of each overlap directory carry it — 8 units, not 4. Quarantining on it drops the
A-side extraction too, and the four overlap directories vanish from the matrices entirely: exactly
the outcome §I ruled against, arrived at by following §I.

**This is the same defect §I diagnosed, one level down.** A boolean was again being asked a
question it cannot answer, because there is a THIRD independent fact: not "does this directory
participate in a floor comparison" (control) and not "was this directory sampled" (sampling), but
**"of the two extractions of this directory, is this the one that enters cells."**

**Fix, implementing the ruling rather than extending it:** a third explicit field, `matrix_unit`,
on all 26 units. `true` for all 13 of A's and for B's 9 non-overlap extractions; `false` for B's
four overlap-directory extractions — which is precisely §I's "B's versions are the ones that never
enter cells," made machine-checkable instead of left as a rule a driver author has to remember.
Checked property, not an intention: **every one of the 22 sampled directories contributes exactly
one cell unit** (verified `set(counts)=={1}` over the 22).

**Operator: overrule if you would rather redefine `overlap_source` to mean the extraction-level
fact instead of adding a field.** Both work; a third field was chosen because collapsing the
directory-level and extraction-level facts back into one boolean is what produced this.

### I.3 Correction to two stated counts

- **A's units carrying `alternatives_not_extracted` is 8 of 13, not 6.** The 6 was written into
  `HANDOFF_EXTRACTOR_B.md` and this OQ's ISSUES entry by the A2 instance, which counted only the
  six it extracted itself and omitted the two recorded by the first A instance before the handoff.
  Corrected wherever stated. Per-unit entry totals: **A 21 entries over 8 units; B 38 over 13.**
- **B's recording-threshold flag stands, at the corrected magnitude.** B recorded every candidate
  it considered and rejected — including ones it judged not to be defects at all (a control working
  as designed, a declared scope limit, a stale comment); A recorded only competing defects. **The
  two halves' `alternatives_not_extracted` counts are therefore NOT comparable as a defect-density
  measure and no outcome row may be built on them.** The divergence is 8/13 vs 13/13, not 6/13 vs
  13/13 — smaller than reported, and real. Flagged by B rather than smoothed, which is the correct
  handling: the convention differed, and a pooled count would have concealed that a convention
  differed at all.

### I.4 Floor asymmetry — pre-registered BEFORE the number exists

B's selection residual is accepted as recorded (three multi-defect directories where the stated
rule left the call close, each loser recorded with its reason). **Two of the three are
floor-participating directories.** So the floor conflates two things: extraction churn (what it is
built to measure) and selection difference (which of several genuine defects an extractor chose).

**The conflation has a SIGN, and that is what makes it usable.** A selection difference inflates
apparent disagreement; it can never deflate it — two extractors who chose different defects from
the same directory cannot thereby agree. Therefore:

| floor result | reading, fixed now |
|---|---|
| **4/4 INSIDE FLOOR** | **clean regardless.** Selection difference could only have pushed away from agreement, so agreement despite it is agreement. |
| **anything below 4/4** | **ambiguous between churn and selection, and must be read as an UPPER BOUND on extraction churn** — never as a churn point estimate. |

This bites directly on §E: the H5 extension conditional uses the floor to decide whether a
multi-extractor protocol is "changes n and nothing else." Under a sub-4/4 floor, §E may not treat
the measured disagreement as extraction variance without first separating the two — so the
conditional fails CLOSED (extension not licensed) rather than resolving against an inflated number.

Pre-registered now, before the floor is computed, because an asymmetry stated after the number
exists is an asymmetry chosen to suit it.

### J.1 The ruling is an artifact under the same discipline — and it needed a positive control

§I was **correct in prose and wrong in machine-checkable form**: following it faithfully produced
the outcome it prohibited (18 cells where it ruled 22). That is spec-vs-implementation drift where
**the spec is the operator's ruling** — a category this project's discipline had not previously
named, because rulings were treated as the thing implementations are checked *against*, not as
artifacts that are themselves checkable.

**The positive control turned out to be the cell count.** Reading the rule does not catch it; the
rule reads correctly. Counting what the rule claims to produce does. Record alongside the git
channel (§J) for the same reason: both are surfaces the design treated as neutral substrate — one
the memory system, one the operator's own instruction — and neither was registered as something
that could carry a defect.

**A rule that yields its own negation when executed is a defect in the rule, not in the execution.**
The check is cheap and general: after any accounting ruling, compute the quantity the ruling names
and compare it to the number the ruling states.

## L. The recurring shape, stated once for the writeup

**A boolean acquires a third question and answers it by silently privileging one reading.** Twice
in two turns: `role` carrying matrix-membership and floor-participation (§I), then
`overlap_source` carrying directory-level and extraction-level participation (§I.2). Both times
the collapse was invisible in the field's own terms — each boolean returned a well-formed answer to
the question it *could* answer — and both times **the tell was an arithmetic check disagreeing
with a stated intent**:

| witness | stated | counted |
|---|---|---|
| cell accounting (§I.2) | 22 matrix units | **18** |
| §4.5 denominator (step 1) | 74 audit directories | **73** after an empty placeholder was excluded |
| de-blocking harness (§G.3) | baseline max-run 8 | **2** |
| frame-census rider (§L.2, 2026-08-11) | census "across all 22 drawn directories" | **n_escape = 8**; there is no 22-directory escape sample |

**Counting the thing the rule claims to produce catches rule defects that reading the rule does
not.** Each of these took seconds and each caught something a careful re-read had already passed.
This belongs in §6.4 next to the controls-need-controls argument: it is the cheap general form of
that recursion, and unlike the recursion it terminates.

### L.1 The propagation instance, recorded because the flag survived by luck

The corrected count (A's units recording alternatives: 8, not 6) **reached two downstream
artifacts — `HANDOFF_EXTRACTOR_B.md` and this OQ's ISSUES entry — before anyone re-derived it**,
and B's recording-threshold flag was built partly on the wrong figure. The flag survived correction
because the divergence was large enough to hold at either magnitude (8/13 vs 13/13 as against
6/13 vs 13/13). **That is luck, not design.** Had the true figure been 12, the flag would have been
a finding about nothing and would have shipped.

Provenance of the error: an instance counted the units *it* had extracted and reported the number
as a property of the whole half, omitting two written by the prior instance before the handoff. It
is the arc's own recurring shape once more — **a plausible number, sourced from a partial view,
reaching consumers with no way to check it** — this time inside the apparatus rather than inside
the corpus it audits.

### L.2 A check that could not return the failure it looked for — and where the vacuity came from

Found while executing the threshold probe's frame census (2026-08-11).

**The rider, and its correction.** The operator's rider directed the census "across all 22 drawn
directories." The escape stratum is **n = 8**; the 22 is the *primary* draw. The number was accepted
uncounted, and **it was wrong in the direction that made the bound look tighter**: with the 2
unseeable directories removed, the escape side's effective n is 6 and its one-sided recall licence
loosens from ~31% to **~39.3%** (1 − 0.05^(1/6)). Moot while six candidates stand — the bound prices
the null and the null is not the live hypothesis — but recorded here with the other stated-vs-counted
witnesses rather than left in the probe's own file. **Self-reported by the party whose number it was.**

**The vacuity, which is the sharper item.** Run as directed, the census over the 22 primary
directories returns **0 unseeable** — and it could not have returned anything else. The primary
stratum is *defined* by a grep over `--include='*.md'`, so every member has an `.md` **by
construction**. The check looks exactly like a passing check and measures nothing: the same shape as
a clean read from a probe that never looked.

**What makes this instance different from the earlier four — the origin.** In this arc's previous
catches the vacuity came from a **coding or harness defect**: something compared an extraction
against its own echo, or a control's own stated numbers made a defect in them unfalsifiable. Those
are things a careful implementer could have written differently. Here **the vacuity is inherited from
the frame's definition**. Nothing was coded wrong; the population was *specified* such that the
question has one reachable answer. That origin is why re-reading the script would never have caught
it — the script is correct — and why the diagnostic has to be applied to the *population*, not the
code: **ask what value of the input would make this line fail, and if the sampling rule already
excludes it, the check is a definition restated.** (Enumeration as the fifth in the arc is the
operator's count, recorded as theirs; the two instances verified in-file here are the §E floor
self-comparison and the `73 + 101 = 174` partition that concealed a positional parse.)

### L.3 The anti-misreading entries, counted because they are invisible by design

**Fourth instance this arc (operator's count, 2026-08-11):** something written down *specifically to
stop a future reader promoting it*. The latest is the threshold probe's item 3 — the blind judge
returned the same verdict §H.1's redacted paragraph asserts, and the coincidence was recorded together
with the reason it is worth almost nothing (the judge returned `extract` on all four, so agreement
there is what a judge extracting everything produces anyway).

**Why the count is worth keeping.** These entries produce nothing citable. They add no finding, move
no number, and a reader encountering one learns only that a tempting inference was already considered
and declined. That is exactly why they are the entries most likely to be dropped for brevity — and
why their presence is the reason the rest of the record can be trusted. Same discipline as recording
predictions that failed: the value is in the asymmetry between what was kept and what could have been
quietly omitted.

## M. §H.2 residue — k = 0 confirms the null AND leaves the mechanism untested

**Operator, 2026-08-11: the null is confirmed and the row stays OPEN.** k = 0 across the full n=22
primary sample (A 0 + B 0). The row licenses **"keyword-proxy false-positive rate ≤ 12.7% at 95%
confidence"** and **nothing more** — never "the proxy is precise."

**Declared residue, recorded explicitly rather than left as an absence:** because zero NO-UNIT
proposals were made, **the operator-confirmation gate never fired**. §H.2's quarantine mechanism —
extractor proposes, operator rules — is therefore **UNTESTED. It has no positive control and we do
not know that it would work.** It has never been exercised end to end even once.

This is the same structure as the escape check's own quarantine, and the same warning applies: an
unfired gate is not a passed gate. If the extension to n=73 runs, the first NO-UNIT proposal will
be the mechanism's first execution as well as its first use — and it will be carrying a decision
at that moment. **Not a closed row; a declared absence with a named graduation step** (exercise the
gate once on a constructed proposal, or accept that its first real firing is also its test).

## N. Pooled `incident_location` — third declined guess, and the observation that survives it

Pooled over all 26 units: **subject 17 / self_audit_subsection 5 / incidental 4.**

**The skew reading is dead at the pooled level too, and is recorded as NOT HELD — the third
directional guess this arc that the data declined** (after the C/D disagreement guess and the
interim-boundary skew reading). Kept at the same volume as the confirmations.

**But the framing question does not vanish with it.** Non-subject counts, both denominators stated
because they differ and the difference is easy to slide over:

| set | n | subject | self_audit_subsection | incidental | **non-subject** |
|---|---|---|---|---|---|
| all extraction units | 26 | 17 | 5 | 4 | **9 of 26** |
| **matrix units** (the ones that enter cells) | 22 | 14 | 5 | 3 | **8 of 22** |

*(Recorded as its own instance of §L: this row was first written as "9 of 22" — the numerator from
the 26-unit set against the 22-unit denominator — and was caught by counting it before the
pre-registration froze, not by re-reading it. The correct figure for the matrix set is 8 of 22.
Same shape as everything else in §L, inside the sentence that was pre-registering the shape.)*

Either way the incident is real and reported in roughly a third of directories without being what
the directory is about. That is enough that §4.5's phrasing still matters: "73 incident-bearing directories" is defensible,
"73 audits *about* incidents" is not, and the two are easy to conflate in a sentence.

**Status: a declared OBSERVATION with its own row, not a finding.** It has no pre-registered
threshold, was not designed for, and the distinction it turns on (subject vs reported-anywhere) is
the §H.1 boundary rule's own — so it can be reported and cannot be scored.

## O. Declared calibration residue — what the anchors do NOT cover, both directions

Fixed before any coding. Both gaps are consequences of anchor-selection rules that were correct
and mechanical; neither is a defect in the rules. They are stated because H3 is a narrow licence
and the two places it does not reach are the two places the experiment most wants to read.

### O.1 Direction (i) has no P6 anchor

The anchor set is **{P1, P2, P5}**. The published P6 exemplar was disqualified because its incident
is the incident an extracted unit already describes (§ anchors, `controls/anchors.json`).

**P6 is the pattern carrying the most conceptual load** — it is the composition-layer pattern, it
is what R5's pre-registered directional expectation concerns, and it is our side of the E↔P6
correspondence the writeup emits as a PROPOSED Ω_C mapping. So:

> **H3 at ≥2/3 over {P1, P2, P5} licenses "the coder is not broken" across three patterns and says
> NOTHING about the coder's handling of P6. Any P6 result in direction (i) is UNCALIBRATED and must
> be reported as such. The E↔P6 row additionally lacks anchor support on OUR side of the mapping,
> on top of already being PROPOSED rather than ruled.**

Fixing it is not available without cost: the only published P6 exemplar strong enough to anchor on
is the one that collides, and constructing a new P6 anchor would mean the extractor assigning a
pattern — which is the boundary the whole design rests on. **Declared, not repaired.**

### O.2 Direction (ii) has no multi-membership anchor — same shape, opposite direction

The direction-(ii) anchors are drawn **exclusively from the agreeing 12**, because "unambiguously
stated by Wu" requires both frozen records to concur; **none comes from the complement 10.**
Verified mechanically, not assumed.

Under the amendment's own naming the agreeing 12 is the single-class stratum — **with §A's status
caveat carried, not laundered**: reading (ii) is an observation with three live defeaters and is
not asserted here. The gap holds under either reading, because it rests only on the mechanical
fact that every anchor sits in the 12 and none in the 10.

> **H3 for direction (ii) is calibrated on single-class incidents only. The multi-membership
> stratum — the one whose verdict grammar §B had to be amended for, where a two-pattern split is
> the EXPECTED result rather than a degraded one — has no anchor. Recovery on unambiguous
> single-class incidents does not license reading the coder's behaviour where a unit legitimately
> carries two labels.**

**The two gaps are the same shape in opposite directions:** in each, the anchor rule selected for
*unambiguity* and unambiguity is exactly what the hard stratum lacks. An anchor set is a sample of
easy cases by construction, and both gaps are that fact showing up where it costs something.

## P. The self-comparison family — one section, not three catches

Three times in this arc a **self-comparison** was caught before it landed, each in a different
instrument, each arriving through a different door. Write them as a family; individually each reads
as a lucky catch, together they show a mechanism with a stable shape.

| # | instrument | the self-comparison it would have produced | door it came through |
|---|---|---|---|
| 1 | extraction-churn floor | two "independent" extractions that are one extraction and its echo — 4/4 INSIDE FLOOR whatever the truth | reading the other extractor's unit "just for the schema" |
| 2 | the same floor, again | same outcome, reached without opening a file | a commit message summarising the unit, read via `git log` |
| 3 | direction-(i) anchors | a unit and its own anchor drawn from ONE incident — recovery measuring whether the coder can match a text against a near-copy of itself in the same run | the published exemplar happening to be an extracted unit's incident |

**The shared shape: an apparatus measuring agreement between two things that are not independent,
and reporting the agreement at full confidence** — because agreement is exactly what a working
version of each instrument produces. None of the three would have errored, and all three would have
produced the reassuring number.

**They differ in the door, and that is the transferable part.** One was a rule everyone knew and
could break by accident; one was infrastructure nobody had registered as a channel (§J); one was a
selection rule that was correct and still selected a duplicate. **A blinding protocol that names
only the first is two-thirds blind.**

### P.1 The anchor redaction rule was a compositional gap

Both redaction rules were written per-direction, and each was locally complete. **An anchor is by
construction the one artifact belonging to both directions at once** — a unit from taxonomy X
interleaved into a run coding taxonomy Y — so it fell between two rules neither of which was wrong.
The fix (redact anchors against BOTH lexicons) is one line; the gap was invisible until an artifact
existed that instantiated both scopes simultaneously.

**That is a composition-layer defect in the control architecture — found while building controls
for a taxonomy that names composition-layer defects.** Worth the line it costs in the writeup: the
architecture reproduced the failure class it was built to detect, at the level where the classes
compose, which is where its own taxonomy says to look.

---

# Addendum (operator, 2026-08-11) — the twins pass: an instrument's error profile belongs to its role

## Q. Control (c)'s selection metric measured a different taxonomy

Full evidence, options and ruling: `controls/redaction_pair_selection_defect.md`. Ruled **option
C** — the declared three pairs are kept and the two the corrected metric picks are added, reported
as two separate sets, with the **corrected set carrying the both-residue row** and the declared set
reported alongside as the pre-declared comparison. Fixed before either number exists, so the row
cannot acquire a choice at writeup time.

### Q.1 The mechanism, which is new and is not a coding bug

The direction-(ii) banned lexicon was built and validated as a **detector**. In that role a false
positive is *conservative*: it fires H2, you investigate, you clear it, nothing is lost — and this
arc already banked exactly that outcome as catch #3 ("permission *class b*y default").

The same matcher was then reused as a **density metric to select on**. In that role the identical
false positive is **silently decisive**, and it ranked first the one sampled directory with nothing
in it to measure (`five_leg_twin_comparison`: 21 of 21 hits are `P1`/`P2`/`P3` used as that
directory's own probe names).

**An instrument's error profile is a property of its ROLE, not of the instrument.** Validating it
in one role licenses nothing about the other. This belongs in §6.4 beside the three worked
examples, and it is the one that is *not* a coding bug — **the matcher was correct; the reuse was
the defect.** The three existing examples all show an instrument being wrong; this shows a right
instrument in a wrong job, which no amount of testing the instrument would have caught.

**The shape it would have produced, stated because it is the reason this is the sharpest defect of
the arc:** a floor of zero, reported as "redaction costs nothing," clearing the both-residue row to
be read as a finding. **The control designed to protect that row would have flattered it.**

### Q.2 The §1 instruction was correct in prose and wrong in force — corrected form

`HANDOFF_TWINS_AND_DRIVER.md` §1 said "do not re-derive them 'to check'." It was written to prevent
**reselection** and it landed as a prohibition on **verification** — two different things collapsed
into one sentence. **Third time in this arc a rule has been correct in prose and wrong in force**
(§I.2 the cell-accounting field; §J.1 the ruling-as-artifact; this).

**Corrected form, adopted:** checking is *permitted and expected*; reselecting is *mechanically
prevented*. `controls/recheck_predeclared_counts.py` is the shape — it recomputes the stated numbers,
reports whether the pre-declared selection is invariant, and **exits non-zero if the selection ever
moves**, which routes the move to the operator instead of letting a script make it.

**General principle, for §J beside the git channel:** *an instruction that forbids checking a
control's stated numbers makes a defect in them unfalsifiable by construction — a strictly worse
failure than the reselection it prevents.* §J already holds two surfaces the design treated as
neutral substrate (the memory system; the operator's own instruction). This is the third: **a
blinding rule that blinds the verifier as well as the coder.**

### Q.3 §L gets a summary line, not a fourth row

The stated-versus-counted table now has four entries (22/18 cells, 74/73 directories, 8/2 max-run,
~15/10 exemplars), plus 3 of 9 direction-(ii) density counts that did not reproduce. Four is enough
that the list is no longer the point. **The summary line: every one of them was found by counting
what a rule claims to produce, and not one by reading the rule.** Re-reading had already passed all
of them.

### Q.4 A direction-(ii) source cites the orphaned fork branch — an OQ-278 datum from outside the fork pass

Pair 1's source names the pattern for its own incident in its own voice:

> "**Verdict: the existing mandatrophy authoring surface is a Build-Discipline Pattern-1 dangling
> wire.**" — `audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144`

That is a source citing the **orphaned fork branch**, and it arrives from *outside* the
fork-residue pass that was built to measure the fork. Flag it separately in the writeup: the
fork-residue row is fed by a pre-registered instrument, and this is unsolicited evidence of the
same kind arriving by another route, which is worth more than a row it did not come from.

### Q.5 Two constructions worth naming, because both close a door this arc found open

**A check must not go green on an empty input.** `controls/verify_redaction_twins.py` reports
`redaction_twins_direction_ii.json exists — NOT WRITTEN YET` as a **FAILURE** rather than passing
over a file that is not there, and asserts **both** directions per pair (unredacted arm must fail
the sweep, redacted arm must pass). One-sided checking would pass a pair whose redacted arm was
never redacted, or one whose unredacted arm restored nothing — and **a floor of zero is exactly
what both one-sided checks report as healthy.** The arc has now caught green-on-empty in three
instruments (`system_gradient`'s `[] → 0.0`; the OQ-66 MaxEnt soft-failure comparing `[no_top,…]`
against itself; the payload-capture count that would clear a leak-grep by writing zero payloads).
Naming the construction, not just the catches: **a gate whose input is absent must be RED, and a
check that cannot distinguish measured-empty from didn't-look is not a check.**

**A row's instrument is assigned before its number exists.** The both-residue row is fed by the
corrected redaction-pair set, fixed in `controls/redaction_pair_selection_defect.md` **before
either floor is measured**. This closes the last place in the design where a choice could be made
with results in hand — the failure mode is not dishonesty but a writer holding two defensible
numbers and no pre-committed rule for which one feeds the row.
