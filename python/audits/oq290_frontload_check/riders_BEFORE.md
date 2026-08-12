---
name: feedback-prereg-review-riders
description: "Merged prereg/gate-design review riders: HALT gates + numeric thresholds, variance/quantization floors, judged-grammar calibration, NULL-composite outcome tables, table totality + two-sided gates, OQ-126 riders, kill-condition cause classification, effect-size floor not alpha, recon gates interactive, failed-metric substrate read, prereg md5 ordering, tolerance split by indicator kind"
metadata:
  node_type: memory
  type: feedback
  consolidated: 2026-08-10 (merged: feedback_halt_gates_earn_their_keep.md feedback_variance_gate_review_bar.md feedback_judged_grammar_calibration_riders.md feedback_prereg_enumeration_covers_null_composites.md feedback_plan_review_totality_and_two_sided_gates.md feedback_plan_review_riders_oq126.md feedback_kill_condition_cause_classification.md feedback_effect_size_floor_not_alpha.md feedback_recon_gates_run_interactively.md feedback_failed_metric_substrate_read.md feedback_prereg_ordering_witnessable.md feedback_tolerance_split_by_indicator_kind.md)
---

# ── from feedback_halt_gates_earn_their_keep.md

The OQ-259 item-1 plan went through two operator review rounds (extends
[[experiment-plan-review-bar]]). The riders they added, all of which proved load-bearing:

- **Payload coverage of discount rules:** the Arm-0 churn floor must cover every
  payload-carrying file (Biopower ×2 AND Cap K ×2, not one file standing in for both).
- **n=1 drift runs are anti-conservative** — n=2 gives a FLOOR on churn, not an
  estimate; say so plainly rather than treating the discount rule as adequate.
- **Pre-registered selection rules for controls:** a control placed after results
  (Arm-2 scrambled-marker) needs its selection rule + RNG seed pinned BEFORE the arm it
  selects on runs, with an explicit skip-as-moot branch.
- **Commit-order blinding:** recorded calls committed FIRST, label→arm mapping SECOND —
  the two hashes are the blind's evidence when executor and scorer are the same agent.
- **Quantified HALT thresholds:** "reassess if drift is bad" is not a gate; "< 2/3
  reproduce-rate in either re-run → HALT before spend" is.
- **Per-file non-independence of vote scales:** a 2+2 read-through survival scale must
  check whether each within-file pair co-occurs in one source section (Cap K's pair
  did → discounted to ~1 vote; Biopower's didn't → 2 votes).
- **Unknown-sign annotation effects:** marker injection is conservative for one half of
  a prediction only; the write-up may not lean on "conservative" globally.

**Why:** every one of these guards against a specific over-claim channel, and the HALT
rule FIRED on first use (2026-08-05): Cap K same-input re-runs reproduced 3/6 then 2/6
baseline readings, and 3 of 4 discriminator-target readings churned at byte-identical
input — Arm-1 spend would have bought uninterpretable results. Without the review-added
gate the experiment would have run to completion and produced a plausible-looking
verdict inside its own noise floor.

**How to apply:** when a design scores per-item presence/absence across LLM redraws,
demand a same-input churn arm with a NUMERIC halt threshold committed before spend, and
treat "the effect is inside the churn floor" as a first-class outcome with pre-committed
semantics (halt + operator ruling), not a failure to be argued around. The operator's
ruling confirmed exactly this: the churn floor was promoted to THE finding (OQ-264,
program-wide k-redraw standard), and the salvage options were ruled dead as "an
anecdote with a pre-registration wrapper" — do not propose n=1 salvage arms after a
churn gate fires. Evidence:
`audits/2026-08-05_oq259_emphasis_discriminator/ARM0_HALT_REPORT.md`, commits
`c4785da7`/`b8a44661`/`70c458f9`/`df0ec7ec`/`07a6c776`.

# ── from feedback_variance_gate_review_bar.md

Operator review bar for any gate scoring the stability of a JUDGED observable over few
draws (minted OQ-264, 2026-08-06; extends [[halt-gates-earn-their-keep]] and
[[experiment-plan-review-bar]]):

- **Gate thresholds must clear the quantization floor.** Print the achievable value
  lattice per draw (share steps = 1/D) and simulate the stable-null-plus-one-scorer-error
  before committing numbers; a band the null can reach (or a modifier that makes the null
  unpassable) is miscalibration to fix in Phase A, never a finding to write up. Witnessed:
  the OQ-264 plan's own rev-1 sensitivity rule yielded P(INDET)=1.0 under every one-error
  null and was recalibrated pre-commitment per the plan's own clause
  (audits/2026-08-06_oq264_kredraw_variance/CALIBRATION.txt).
- **A variance gate must decompose scorer vs generator** or a FAIL is unattributable:
  seeded silent duplicates measure scorer variance directly; mechanical comparator
  observables (denominator, counts, categorical presence) measure generator churn with no
  judgment; compare their ranges to localize.
- **Anchor/holdout split for judged rubrics:** worked examples quoted in the
  pre-registration are anchors; held-out items re-scored blind are the reliability
  instrument — re-scoring anchors measures anchoring recall, not reliability. Label
  holdout agreement as contaminated if the scorer read the original calls in-session.
- **Monotone-range retraction clause:** observed range is non-decreasing in k, so any
  small-k pass is PROVISIONAL by construction; pre-register the retraction path (report
  full-k range AND mean over all k=3 subsets so the numbers are comparable; a retracted
  pass is reported as retraction, never averaged away).
- **Component rule:** a stable ratio over compensating churn is a coincidence at small n
  — report numerator/denominator ranges separately; operationalize D-aware (residuals
  from the pooled share) so honest proportionality under different D is not blocked.
- **A pooled ratio does not escape unit-identity churn if its denominator is built from
  the same unstable units.** Before adopting a pooled repair for an unstable per-item
  observable, decompose the observed range: does the max-pair fall between
  numerator-identical draws (denominator artifact) or numerator-moving draws (judged
  signal)? Run a denominator-convention sensitivity table (per-draw D vs fixed D vs raw
  count), registered as exploratory/non-gating; the table itself can be the minted
  standard. Witnessed: OQ-264's entire 0.25 share range fell between two TAG=3 draws
  (D 6→4), with the perverse direction fewer-readings→higher-share
  (audits/2026-08-06_oq264_kredraw_variance/DENOM_SENSITIVITY.txt).
- **Zero observed failures is a bound, not a zero:** n clean control pairs give a 95%
  upper bound of 1−0.05^(1/n) on the per-item error rate (~39% at n=6); a gate clause
  keyed on "measured-zero" variance must be cited with the interval, especially where
  sensitivity=1 means one error inside the interval flips the verdict.

**Why:** these were the operator's plan-review riders on the OQ-264 design; the
quantization-floor clause fired on first use (as the HALT clause did in OQ-259).
**How to apply:** at plan review for any stability/variance gate over judged calls,
check all five before spend; calibration output is committed WITH the pre-registration.

# ── from feedback_judged_grammar_calibration_riders.md

Plan-approval riders from the OQ-262 severance/intrinsicness plan review (2026-08-08) — apply to ANY judged-tier grammar or verdict instrument:

- **Two-sided calibration (zero-mutation control).** A control gate that only checks "the instrument finds the defect" (CP 3/3 nonzero) is one-directional: a grammar that finds mutation in *everything* passes it. Pre-register at least one expected-negative (zero-mutation / genuine) case, chosen before verdicts exist; if none is credible, declare the negative verdict class UNCALIBRATED rather than letting it fire uncontrolled.
- **Declare grammar-post-recon, don't launder.** If the criterion was authored after reading the substrate inventory, the prereg says so explicitly. Named fire/no-fire control targets specified against pairs already known to satisfy them test IMPLEMENTATION, not discrimination — the prereg states that too. The R2-style freeze fixes the criterion before verdicts, not before substrate knowledge.
- **Pre-commit the interpretive-downgrade branch.** Before the verdict table exists, write the branch: if RULED (in-file-witnessed) rows are a minority, the writeup altitude drops to "a reading of the authored text under a frozen grammar," not an audit result. Writing the branch after seeing the table is the laundering it exists to prevent.
- **No-rate rule vs unanimity gate — state the distinction.** "No rates over the deliverable table" and "k=N unanimity control gate" coexist legitimately only if the prereg/writeup explicitly distinguish them; otherwise the writeup reads as violating its own rule.
- **Blinding leaks through the instrument.** The grammar text handed to a blind judge is itself a channel: verify the literal text the judge receives contains no control-pair names or expected directions before launch.

**Why:** each rider closes a channel by which a frozen-looking prereg silently pre-decides its outcome. See [[falsifier-design-channel-closure]], [[plan-review-riders-oq126]], [[prereg-enumeration-covers-null-composites]].

**How to apply:** at plan-review time for any audit with a judged/LLM-graded tier, walk the five riders as a checklist before spend.

# ── from feedback_prereg_enumeration_covers_null_composites.md

The OQ-261 v2 proposal enumerated Cell-1 outcomes as all-glue / all-obstruct /
glue-obstruct-mixed. The realized shape was obstruct/obstruct/NULL (one variant below
the sparsity floor) — a cell the frozen enumeration never named, forcing the writeup to
report "between the named cells at scoped altitude" and declare the gap as residue.

**Why:** any design with per-arm validity gates (sparsity floors, OQ-51 nulls,
coverage minima) has NULL as a reachable per-arm outcome, so the outcome space is the
product over {decidable-verdicts, NULL} — not just the decidable combinations. An
enumeration that only names decidable composites leaves the realized outcome unlabeled
exactly when a gate fires, which is when precision matters most.

**How to apply:** when pre-registering a multi-arm/multi-variant discriminator where
any arm can read NULL, write the NULL-composite rows into the outcome table explicitly
(e.g. "decidable arms unanimous-obstruct + k NULL ⇒ ..."), and state whether NULL arms
weaken, void, or leave unchanged the composite verdict. Related: [[plan-review-totality-and-two-sided-gates]]
(table totality), [[commit-plus-falsifier]].

# ── from feedback_plan_review_totality_and_two_sided_gates.md

Five review-bar lessons from the OQ-259 items 2–3 plan (three operator reviews before
approval, 2026-08-06):

1. **Interpretation tables must be total over the outcome space** — enumerate every
   reachable cell plus a catch-all "indeterminate, no ruling" row; a gap in the table
   fills post hoc with whatever framing the result invites.
2. **An n=2 rate cannot discipline an n=3 comparison** — either declare the comparison
   qualitative-context-only (no cell's verdict depends on it) or pin the arithmetic
   before the run.
3. **Calibrate gates TWO-SIDED (ceiling AND floor)** — a positive-case rate alone
   cannot show the gate can fail. Witnessed payoff same day: the P2 token match-rate
   measured INVERTED (arsenal floor above the meta-layer ceiling at every strictness);
   a one-sided calibration would have pinned a threshold that fires on the wrong
   class. Related: [[merit-independent-signature-gate-law]],
   [[variance-gate-review-bar]].
4. **A declared residual leak beats a claimed clean blind** — scope the blinding
   honestly (commit-order + md5-pinned withheld mapping; leaks listed in the prereg).
   The B0 unblinded hypothesis was more generous than the blind calls — the declared
   degraded blind caught exactly what it was declared for.
5. **Degenerate selection rules at the available n are formalities** — a "tier
   selection" or "selection rule" that cannot distinguish outcomes at the n actually
   available (n=2 cannot select tiers) must be renamed to what it is (a measurement),
   not carried as a rule.
6. **A new observable entering a verdict grammar needs its churn measured BEFORE it
   carries weight** (operator, post-Part-C review, same day) — P1 entered the
   graduation grammar unmeasured and had to be base-rate-bounded post-verdict; the
   bound's comparator (already-committed AT Fiat k=3 draws) was free on disk the
   whole time. Corollary: before pricing a comparator run, check whether
   already-committed draws measure the base rate for free. Program tally: 4/4
   observables measured for stability came back less stable than the single-draw
   read (item-1 readings, B0 strict flag, Cap K kernel mint, T Framework kernel ids).

**Why:** each of these was a real defect found in review of an already-twice-reviewed
plan; they are cheap to check at plan time and expensive after spend.
**How to apply:** at plan review, walk the outcome space of every table/gate/rule and
ask: total? two-sided? n-consistent? blind honestly scoped? rule non-degenerate at this
n? Cluster: [[experiment-plan-review-bar]], [[halt-gates-earn-their-keep]],
[[audit-plan-discipline]].

# ── from feedback_plan_review_riders_oq126.md

Four riders the operator attached when reviewing the OQ-126 resolution plan (2026-07-02):

1. **The load-bearing recon claim is the first execution witness, not settled recon.** When a
   plan's scoping (lean fix vs heavy conversion) rests on one claim ("all consumers are
   commentary-grade"), execution owes that claim's witness FIRST, as a gate pass with a HALT
   branch — before any write. Subagent/planning-session recon does not discharge it
   (paste-or-untag is per-turn).
2. **A null field must encode "no path exists," never "checked, none found."** A constant
   `null` slot (e.g. `confronted_by: null` before an ingestion path exists) counterfeits a
   clean probe. Ship an explicit sentinel carrying the no-path fact (e.g.
   `confrontation_path: "none_exists"`) — Pattern-6 landing in schema design.
3. **Promoted guidance lands NORMATIVE, not prose.** If a spec requirement (Tier-2 declared
   record-boundary) lands as a paragraph, future users cite the section and skip the
   requirement frictionlessly — the same failure mode the fix targets. State it as MUST so
   omission is a visible gap.
4. **A decoration kill-condition needs a goes-red control.** Additive JSON fields don't prove
   a fix changed a *reading* — the re-rendered report surface before/after diff does; and the
   guarding test must be shown RED with the emission removed (green-with-field-absent is the
   byte-identical clean read).

**Why:** each rider is an instance of [[introduced-instrument-is-a-claim]] /
Build-Discipline Pattern 6 applied at plan altitude: the plan itself is an instrument whose
load-bearing premises need their own controls.

**How to apply:** when writing any plan whose scoping rests on a recon claim, make Pass 0 a
read-only re-witness gate with an explicit HALT; when adding schema fields that are empty
until a future capability lands, encode the absence-of-path explicitly; when promoting OQ
prose into a spec doc, convert requirements to MUSTs; when claiming a surface rewording as a
control, paste the before/after and the red run.

# ── from feedback_kill_condition_cause_classification.md

Operator ruling at OQ-72 R1 (2026-07-04), on the C1 positive-control grading: ratify the
count thresholds (≥2/3 pass / 1/3 marginal / 0/3 kill), but at ANY miss, diagnose the cause
before the verdict fires. A contradiction pair failing to land same-concept has two causes
with opposite responses: granularity artifact (vocabulary drawn too fine → re-draft the
vocabulary, criterion intact — even 0/3 from a globally-too-fine vocab is a re-vocab, not a
criterion death) vs subject mismatch (the pair opposes on a non-subject axis → the
criterion itself is leaking — THAT is the criterion revisit).

**Why:** grading by count alone can over-react (send a fixable artifact to "criterion
revisit") or under-react (a passing 2/3 waves through the exact leak the control was built
to expose). The count is cheap to pre-register; the classification is what the human seat
actually decides on.

**How to apply:** when pre-registering a kill condition or control bar, ask whether the
failure it counts has more than one cause with different correct responses. If yes, write
the cause-classification step into the pre-registration (trigger = count; verdict =
classified cause). Symmetric on pass: a passing count with a suspicious member still
surfaces that member. Related: [[failed-metric-substrate-read]], [[commit-plus-falsifier]].

# ── from feedback_effect_size_floor_not_alpha.md

When a design compares strata of very different sizes, the pass criterion must be a
**pre-committed effect-size magnitude** applied identically at every n — never a
significance threshold. State the small stratum's minimum detectable effect beside the
floor in the pre-registration.

**Why:** at n≈960 every leg rejects a uniform null on trivial deviation while an n=58
cell cannot; a "fires at 960 / doesn't at 58" verdict is then partly a sample-size
artifact wearing a finding's clothes. The α makes the two legs incomparable by
construction.

**How to apply:** pin the floor as a share above uniform expectation before any grid
value is computed; report the small stratum's MDE next to it; if the small stratum's
MDE does not clear the floor, the condition is **not a falsifier** — stop and redesign
before the freeze md5, do not run it and interpret afterward. A statistic added late
(e.g. a paired contrast discovered mid-recon) needs its OWN floor and its OWN
known-negative — paired and unpaired nulls do not behave the same, so inheriting the
floor is a silent error. Related: [[feedback-variance-gate-review-bar]],
[[feedback-probe-positive-controls]].

# ── from feedback_recon_gates_run_interactively.md

When a pre-registration's pre-freeze phase carries stop-and-escalate branches, run it
**interactively** — surface each gate to the operator at fire time, not in the writeup
after.

**Why:** a stop-and-escalate branch that fires and is only reported post-hoc has
already had its decision made by the executor; the escalation becomes narration. The
operator's seat is upstream of the freeze, so a gate discovered after the md5 cannot be
acted on without breaking the pre-registration.

**How to apply:** name the gates in the plan (OQ-78, 2026-08-10: sonnet-leg
reassignment, high cross-leg intersection, kind-check divergence, power failure); run
recon; report each firing with its witness and the pre-committed branch; ask only where
discretion is genuinely the operator's. Also pin, at the same moment, the **conflict
rule between a newly-added statistic and the pre-committed one** (OQ-78 ruling: the
paired result governs the verdict; disagreement is a headline WRITEUP finding, never
arbitrated once both are visible) — writing that rule after the data is visible is
arbitration, not pre-registration. Related:
[[feedback-prereg-ordering-witnessable]], [[feedback-audit-plan-discipline]],
[[feedback-halt-gates-earn-their-keep]].

# ── from feedback_failed_metric_substrate_read.md

When a **locked, pre-registered metric fails**, the honest next move is to
characterize *why* it failed with a **substrate read** (a per-item witness against
the data), NOT to swap in a different coefficient that happens to look better.
(OQ-182 C-gen, 2026-06-26.)

C-gen locked partition-ARI as the cross-generation recovery metric; it failed
(ARI=0.117 < 0.50). A companion number (PRES=0.83) had been observed post-hoc — and
was therefore **contaminated for gating** (you cannot pre-register a bar on a
quantity you've already seen). Promoting PRES to "the thing that passes" would have
been laundering the fail into a pass.

**The operator's ruling (option 2, done right):** treat it as a NEW, separately
pre-registered question on an *unobserved* quantity, and make the **substrate the
witness**. Here: for each same-kernel reading-pair that splits across the two
generation legs, does the split track a real per-reading structural difference
(`fingerprint_shift`) or is it cut-height/tie-break noise? TRACK = 162/162 = 1.000 —
every split backed by real structure, zero artifact. That per-item read, not any
coefficient, is what settled "the ARI failure is generation-expressive, not noise."

**How to apply:**
- A failed locked metric is reported as failed — full stop. Then ask a *new*
  pre-registered question to explain it; do not retro-fit a metric that passes.
- The explanatory test must key on a quantity unobserved at lock time, and should
  be **per-item / substrate-level** (which rows, why) — coefficients summarize, they
  don't witness.
- Report BOTH findings, neither overwriting: "metric X failed AND the failure is
  [characterized cause]." Pre-commit that dual-finding language before running.
- A positive control you pre-registered may itself fail (here PC1: families spanned
  shifts, violating an over-stated assumption). Surface it and show why the verdict
  survives — halt-and-escalate, don't silently amend it into a pass.

Links: [[feedback-audit-plan-discipline]], [[feedback-commit-plus-falsifier]],
[[feedback-validity-vs-shippability]], [[feedback-witness-discriminates-the-confound]].

# ── from feedback_prereg_ordering_witnessable.md

"Pre-registered" is a claim about ORDERING (semantics frozen before results seen),
and a bare file next to results does not witness it — the file could have been
written after. Make the ordering checkable: record the prereg file's md5 into the
audit log BEFORE the run, so the md5 line sits physically above the first result
line in the same log.

**Why:** operator amendment 5 to the OQ-151 plan (2026-08-09). Without the md5-first
convention, prereg discipline rests on trust in session narrative — exactly the
recap-as-witness substitution Build Discipline Pattern 4 forbids.

**How to apply:** sequence = write PREREGISTRATION.md → `md5sum` it into
audit_log.md → run. Template: `audits/2026-08-09_oq151_dual_gauge/audit_log.md`.
If the prereg is amended after a first run, the log shows both md5s with their
positions — that history IS the honest record. Related:
[[experiment-plan-review-bar]], [[chat-witness-vs-substrate-witness]].

# ── from feedback_tolerance_split_by_indicator_kind.md

OQ-93 κ-plausibility gate (2026-06-11): I offered the operator a single tolerance fraction
over all three counterfeit indicators. The operator rejected the menu shape: a flat tolerance
conflates "can a consumer read this story" (per-story, fail-closed, zero tolerance — exclusion
handles it) with "does this process/prompt deserve promotion" (batch-level, and
indicator-dependent: a systemic-by-nature indicator like template-echo gets zero tolerance and
halts the batch; idiosyncratic indicators get a fraction, e.g. ≥2/10 escalates).

**Why:** an aggregate tolerance is a property of the BATCH; safety is a property of the STORY,
and promotion-worthiness is a property of the PROMPT. One number can't price all three — the
same "sufficiency is a property of the question, not the dataset" move as consumer-named-levels
(OQ-93 battery item 4).

**Follow-on (same session): the supplemental-batch number dissolved into a first-contact
structure.** When the small batch passed, the operator converted the one-time gate into a
FIRST-CONTACT gate (every future item audited once, ledgered, before any consumer read) instead
of buying a supplemental N — because the batch certified the VARIANT process while the LIVE
process had zero output under it; a per-item standing gate tests exactly what the one-time pass
could not. Third instance of number-to-rule → structure in this thread (fraction→consumer-named-
levels; tolerance→split-by-kind; supplemental-N→first-contact).

**Correction (2026-06-12): converting a one-time gate to a standing gate changes its
POPULATION — re-derive each indicator's reachability against the new population.** The batch
gate's C-range included "slot count != 32" because the batch ADDENDUM mandated full grids; carried
into the first-contact gate over live opt-in stories, it misfired on the first schema-legal
partial grid and halted the pipeline on another workstream's flip target. "Battery failure" may
only mean shapes the schema/compiler make unreachable for the population actually flowing through
the gate; population-specific mandates (prompt says full grid) are surfaced as notes, never
exclusions.

**How to apply:** when drafting a quality-gate question (AskUserQuestion or prereg), first sort
each indicator by (a) what it certifies (consumer safety vs process quality) and (b) whether a
firing is systemic or idiosyncratic by nature; pose tolerance per bucket, with per-item
fail-closed exclusion as the floor everywhere. Also: a passed small batch is necessary, not
automatically sufficient, for a promotion flip — record a supplemental-batch provision. See
[[feedback-audit-plan-discipline]], [[probe-positive-controls]].

