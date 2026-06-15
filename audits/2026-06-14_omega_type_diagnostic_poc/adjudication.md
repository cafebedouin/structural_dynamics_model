# Ω-type diagnostic POC — adjudication (separate instance)

*Date 2026-06-14. Plan: `~/.claude/plans/brief-the-omega-glittery-wozniak.md`. Two-party cross-instance
protocol: **adjudicator** (this writeup; sealed `adjudicator_held_key.json` committed `94c7346e`
BEFORE the executor ran) + **blind executor** (subagent; `spec.md`, `deterministic_baseline.py` →
`det_results.json`, `judge_results.json`, `executor_writeup.md`; never read the held key). Read-only
over `prolog/testsets_haiku/*.pl`; no `outputs/` written, no `run_pipeline`.*

## What was built

Ported `debugging_philosophy.md` §6.1 ("the fix that works reveals the type") to Ω-variables:
type each omega by **which resolution operation discharges it** — **define**→Ω_C, **decide**→Ω_P,
**measure**→Ω_E — where a fired operation counts only if it is **external at its own locus**
(criterion / DECIDER / fact), else **restatement**. Two orthogonal steps: signature detection (a set)
+ external-vs-restatement gate (classifies). N=40 stratified sample (family 14 / Ω_E 14 / Ω_C 8 / Ω_P
4), seed 20260614. The adjudicator anchored to the **substrate question** ("does resolving require
going outside the declared fields, and to what?") + `omega_variables.md`'s own keys, NOT §6.1, so the
two parties *could* disagree.

## Headline: the disagreement IS the result, and it is not noise

The executor's LLM-judge agreed with the authored label **33/40 (82.5%)** and declared the
kernel_reading family **UNIFORM Ω_C**. The adjudicator's sealed read agrees on **9/14** held omegas
and types **5/14 differently** — and **every one of the 5 falls in the committer-frame family, on the
three loci the plan pre-registered as contested** (`/tmp` recon → plan §"Open/escalated"). The two
instances are each internally coherent; they diverge on **signature detection** (is the committer-frame
operation `define` or `decide`/`measure`?), which is judgment, not lexis. Witnessed table:

| id | name | adjudicator (sealed) | executor (blind) | verdict |
|---|---|---|---|---|
| 1 | medical_materiality_of_sex | Ω_E | Ω_E | agree |
| 9 | committer_frame__biomedical_foreclosure | Ω_C | Ω_C | agree |
| 10 | kernel_reading_foreclosure | hybrid(Ω_C+Ω_P) | Ω_C | **adj adds Ω_P limb** |
| 14 | technological_carbon_removal_feasibility | Ω_E | Ω_E | agree |
| 16 | bodily_autonomy_categorical_vs_prima_facie | hybrid(Ω_C+Ω_P) | Ω_C | **adj CONCEDES → Ω_C** |
| 19 | kernel_reading_contest (climate) | hybrid(Ω_P+Ω_E) mistyped | Ω_C | **DISAGREE** |
| 20 | kernel_reading_alternative_framing (Nicene) | restatement | Ω_C | **DISAGREE** |
| 24 | committer_structure__liturgical_vs_native | Ω_C | Ω_C | agree |
| 25 | kernel_reading_underdetermination | Ω_C | Ω_C | agree |
| 27 | kernel_reading_distinction (ε-invariance) | restatement | Ω_C | **CONTESTED** |
| 28 | reading_committer_frame_dependence | Ω_P mistyped | Ω_C | **DISAGREE** |
| 30 | martens_clause_scope | Ω_C | Ω_C | agree |
| 33 | global_south_development_path | hybrid(Ω_P+Ω_E) | Ω_P | agree |
| 38 | kernel_reading_contest (Hanafi) | Ω_E mistyped | Ω_C | **DISAGREE** |

(Script: `score_held.py` regenerates this from the two committed JSONs.)

## The one mechanism behind every disagreement: define-dominance re-stamps the authored label

On the committer-frame family the executor's gate **collapsed `decide` and `measure` INTO `define`**:
on its own account (executor_writeup §d, "Undecidable") it ruled "the conceptual framework-call the
deciding locus, with the measure/decide limb as evidence-for." Because the family is authored
**conceptual**, reading the deciding operation as `define`→Ω_C **reproduces the authored label** — the
82.5% agreement is inflated on the family by a define-dominant prior. The adjudicator, applying the
substrate question and `omega_variables.md`'s own discriminators, rules per omega:

- **id 28 → Ω_P (settleable).** The omega literally asks "does choosing this reading depend on the
  reader's theological/institutional **POSITION** (committer frame)?" `omega_variables.md` §"Why These
  Categories Resist Collapse": *"If 'correctness' depends on whose values apply, you're facing a
  preference dependency."* Position-dependence = whose-frame ⇒ **Ω_P**. This is the Seat-Theorem Cor-2b
  committer frontier the prior POC named — **real, and mistyped conceptual**, not artifact.
- **id 38 → Ω_E (settleable).** "Is Hanafi coexistent or does ascendance foreclose alternatives?"
  discharged by "are rival texts **available, taught, suppressed**" — an **observable** state of the
  world. The executor conflated the question-shape (a relation) with the operation that discharges it
  (observation). The porting principle (the fix that works reveals the type) ⇒ **measure → Ω_E**.
- **id 20 → restatement (settleable).** Resolution = "**generate** the homoiousios sibling as a
  separate constraint story and **compare** its authored properties"; consequence self-describes:
  *"documents the committer frame; the corpus gains two constraint stories."* It imports nothing
  external and spawns an artifact — **fails Unlockability** ⇒ restatement / not-a-frontier.
- **id 19 → hybrid(Ω_P+Ω_E), mistyped.** "Which reading is structurally defensible" resolves by "(c)
  normative political shift — a reading accumulates political power" (decide, whose-values) **and** "(a)
  empirical falsification — decoupling fails, tech doesn't scale" (measure). Neither is define; the
  executor's pure Ω_C drops both externals.
- **id 10 → hybrid(Ω_C+Ω_P).** "Foreclose **vs irreconcilable normative commitments held by different
  parties**" fires `decide` (whose-commitment) alongside the jurisprudential `define`. Authored
  conceptual is among the fired ⇒ hybrid-aware agreement, but the Ω_P limb is real.
- **id 16 → Ω_C (adjudicator CONCEDES to executor).** "Categorical vs prima-facie right" is a
  conceptual doctrine resolved by philosophical analysis; the adjudicator's sealed decide-limb was
  over-eager. *(Recorded to show the independence is not rigged toward the adjudicator.)*
- **id 27 → CONTESTED, escalate.** The ε-invariance individuation test is genuinely borderline:
  `define` (constraint-individuation criterion, executor) vs `restatement` (re-deriving authored ε
  across declared readings, adjudicator + prior-POC Irreducibility precedent). Recorded as inter-rater
  unsettled (same subfamily: id 31, not held).

**Net (held set): 8 agree, 5 adjudicator-types-differently, 1 contested** — and the independent look
moved **mislabeling UP** (the executor under-counted family mistyping by reading it Ω_C = authored),
the mirror of the prior soundness POC where independence moved the rate DOWN. Independence again
corrected the self-flattering direction.

## The kernel_reading verdict (settles last turn's open question — the unrigged settler)

OQ-130's open ruling: *"(1) uncontested — retype the kernel-contest family Ω_P; (2) settle first —
local frontier vs template stamp."* **Both halves are now answered, and (1) is REFUTED:**

> **The family does NOT retype uniformly Ω_P. It SPLITS per-omega by mechanism:**
> **Ω_C** (genuine term/criterion frontiers: 9, 24, 25; and exec-Ω_C 0,7,11,17 spot-confirmed) ·
> **Ω_P** (committer-position frontiers: 28; hybrid 10, 19) · **Ω_E** (observable suppression: 38) ·
> **restatement** (generate-and-compare artifact: 20; contested ε-invariance: 27, 31).

So "local frontier vs template stamp" is not one verdict for the family — it is **per-omega**: id 28
is a genuine local committer frontier (Ω_P); id 20 is a template stamp (restatement); id 38 is an
empirical question wearing a contest's name. A wholesale Ω_P retype (the prior "uncontested" step)
would be **wrong** — it would stamp 38 (empirical) and 20 (artifact) as preference frontiers. **The
corpus must be retyped per-omega by the discharging operation, not per-family.** Two omegas share the
name `kernel_reading_contest` (19, 38) yet discharge differently (Ω_P+Ω_E vs Ω_E) — **mechanism ≠
name**, so any name-keyed retype is unsound.

## The five metrics, adjudicated

**(a) diagnosed-vs-authored (hybrid-aware).** Executor 33/40 = 82.5% — an **upper bound**, inflated on
the family by define-dominance. Applying the adjudicated family corrections (19, 28, 38, 20 flip from
agree-Ω_C to mistyped/restatement) lowers agreement to **≈29/40 ≈ 72.5%** as the lower-leaning
estimate. **Finding: the authored Ω-type is unreliable, and unreliable in BOTH directions** — authored
**conceptual→empirical** off the family (ids 2,18,22,23,29: cover-story / historical-necessity /
counterfactual questions that actually want observation, adjudicator AGREES with executor here) and
authored **conceptual→{Ω_P,Ω_E,restatement}** on the family. ⇒ **the diagnostic is load-bearing; the
corpus needs re-typing by mechanism** (pre-registered "mislabeling high" branch).

**(b) deterministic-`unknown` rate (boundary).** Executor 5/40 = 12.5%, two-sided commit control both
pass (commits on known-definitional, refuses on genuinely-ambiguous). **But the baseline agrees with
the judge on only 19/40 = 47.5%** — it commits *confidently to wrong answers* ~40% of the time. So the
12.5% `unknown` rate **understates** the determinism boundary; the honest boundary is the
**unknown + commits-wrong** fraction (≈ 53%). **Refined finding: at the omega-typing layer a
transparent lexical classifier is NOT sufficient — the determinism boundary is high, and it is
exactly the signature-detection judgment (define vs decide vs measure) on which the two LLM instances
themselves diverged.** This confirms "the determinism boundary is the omega boundary" at the typing
layer (pre-registered "deterministic-unknown high" branch, in its stronger form).

**(c) restatement rate (validity-critical — the seeded two-sided gate control is the only witness).**
Executor 0/40. The seeded control **fires the alarm it was built for**:

| seeded control | id | executor gate | result |
|---|---|---|---|
| KNOWN_EXTERNAL | 1, 14, 24, 30 | Ω_E/Ω_E/Ω_C/Ω_C | **PASSED external ✓ (4/4)** |
| UNDER_DECLARATION | 25 | Ω_C | **routed external, not mis-restated ✓** |
| KNOWN_RESTATEMENT | 20, 27 | Ω_C, Ω_C | **MISSED — false-negative ✗ (0/2)** |

⇒ the executor's restatement limb **never fired on the whole 40-sample, including on two seeded
restatements**. **`restatement-rate = 0` is partly gate-no-op, not measured-empty** — the exact
Build-Discipline Pattern-5/6 hazard ("absence satisfies the gate"), and the two-sided control is what
distinguished them. At least id 20 is a settleable false-negative. The gate's *external* side is
sound (5/5 + under-declaration); its *restatement* side under-catches and needs sharpening before any
corpus-wide restatement count is trustworthy.

**(d) kernel_reading per-omega verdict.** The split above. **An aggregate family rate would have
hidden the entire finding** — the executor's "uniform Ω_C" is precisely the aggregate that drowns the
cut between real frontiers (28), empirical questions (38), and artifacts (20).

**(e) Ω_E-as-status falsifier.** Among 14 authored-Ω_E, only **1 (id 35, 7%)** routes external via
`define` (a limiting-criterion question) rather than `measure`; 13/14 route via observation.
**RARE ⇒ "Ω_E is a status" HOLDS** on this sample (adjudicator agrees with executor). The authored
empirical label is mostly load-bearing (awaiting-input), not a catch-all. *Directional* (cell n=14,
±~18pp); earns "Ω_E label is mostly load-bearing here," not "everywhere."

**Biotech triple:** reproduces C/E/P — a **spec-implementation check only**, near-tautological, **not**
a positive control (correctly labeled by the executor).

## Honest limitations

- Held overlap is **14 omegas**; 9/14 agreement is a small-N witness, not a calibrated inter-rater κ.
- The 5 adjudicator-corrections are **settleable against the framework text / the omega's own
  consequence / the which-operation-discharges principle** for ids 28, 38, 20; ids 10, 19 are
  hybrid-refinements; **id 27 (+31) is genuinely contested and escalated**, not ruled.
- "Mistyping ≈ 27%" is the adjudicated point on a 40-sample (wide CI); the **direction** (authored
  Ω-type unreliable both ways; family splits) is the robust claim, the *rate* is directional.
- The whole result rides the **decider-locus cut** (decide = external iff the decider is outside the
  declared set, even when options are enumerated). The executor effectively applied an **option-/
  define-dominant** cut instead. **Which cut is correct is the operator's framework call** (plan
  §"Open/escalated" item 2) — the POC measures that the cut is load-bearing and that two instances
  split on it, not that one cut is right.

## Forward (folded into OQ-130, not chat)

1. **Retype the kernel_reading family PER-OMEGA by discharging operation, never wholesale Ω_P** — the
   prior "uncontested Ω_P retype" is refuted. id 28-class → Ω_P; id 38-class → Ω_E; id 20-class →
   restatement/flag; id 24-class → Ω_C.
2. **Sharpen the restatement gate before any corpus-wide restatement count** — its false-negative on
   seeded restatements (ids 20, 27) means a raw 0-count is untrustworthy (fail-closed: carry the
   coverage bit).
3. **The Ω-type-diagnostic is load-bearing** (mislabeling ≈ ¼–⅓, both directions) — candidate to wire
   into the generator as diagnose-then-stamp, but that is a generator change, **deferred and gated on
   the operator's decider-locus ruling**, itself an OQ-130 child.
4. **Escalate** (operator-only): the decider-locus vs define-dominant cut (item-2, plan); the
   ε-invariance define-vs-restatement call (ids 27, 31); whether confirmed "Ω_E-as-status" edits
   `omega_variables.md`.
