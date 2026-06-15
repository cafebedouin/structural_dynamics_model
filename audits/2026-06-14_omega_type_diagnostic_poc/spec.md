# Ω-Type Diagnostic Protocol (port of debugging_philosophy.md §6.1 to Omega Variables)

## Premise

`docs/debugging_philosophy.md` §6.1 diagnoses a paradox's KIND by *which fix discharges
it* (specify-the-index → Type C, freeze-the-frame → Type A, revise-the-axiom → Type B),
NOT by the paradox's self-description. We port that move to Omega Variables
(`docs/omega_variables.md`): classify each omega by **which resolution OPERATION actually
discharges it**, not by its authored `omega_type` label.

- Ω_E (empirical) → resolved by **measure** (import external observation/data).
- Ω_C (conceptual) → resolved by **define** (specify a term / select a framework).
- Ω_P (preference) → resolved by **decide** (a value/party/stakeholder settles the choice).

## Inputs per omega

`(question, approach, consequence)` PLUS the constraint's **declared fields**: the
enumerated reading set (`cs_reading_relation` + `cs_kernel_id`), beneficiaries, victims,
and authored ε across declared readings. The declared-field blob is the locus-test
substrate: it tells us whether the thing an operation needs is already *contained in the
constraint* (RESTATE) or *external to it* (a live frontier).

## The protocol: two orthogonal steps. Neither stamps a type by itself.

### (A) Signature detection — reports WHICH operation signatures fired. Stamps NOTHING.

Detect which of three resolution-operation signatures are present in (question/approach/
consequence). Zero, one, or several may fire. **Stage names are OPERATIONS, not types.**

- **define**: the omega's resolution specifies a term, selects/clarifies a framework, or
  fixes a criterion ("what does 'living' mean", "is X categorical or prima facie", "do the
  readings foreclose or coexist", "ε-invariance test", "is this a reading OF or ABOUT the
  texts").
- **decide**: a value, commitment, stakeholder, or party settles the choice ("should X
  have voice", "which should dominate", "is this legitimate or illegitimate", "whose values
  apply").
- **measure**: the resolution imports an external observation, data, count, historical
  record, or empirical fact ("systematic evidence review", "historical record / count",
  "deployment data by 2030", "ethnography", "post-removal counterfactual observation").

**`measure` firing is NOT "Ω_E status."** That reading is tested separately in step (B) and
in the post-run Ω_E-falsifier metric; the protocol never asserts it.

### (B) External-vs-restatement gate — applied to EACH fired signature at the operation's OWN locus.

A signature that fires only *internally* (the thing it needs is already declared in the
constraint) is a RESTATEMENT, not a frontier. The locus differs per operation:

- **define → criterion-locus.** Is the term/criterion to be specified one the constraint
  leaves genuinely OPEN, or already declared (e.g. already enumerated as a reading, already
  fixed by an authored field)? *External/open term → Ω_C. Re-stating a declared criterion →
  RESTATE.*

- **decide → DECIDER-locus (NOT option-locus).** Is the DECIDER — the value/party/
  stakeholder that settles the choice — contained in the constraint, or external, **EVEN IF
  the options are enumerated**? *External decider → Ω_P. The constraint already contains the
  deciding commitment → RESTATE.* **Enumerated options ≠ restatement.** (This decider-locus
  cut is the framework call under test; apply it as written.)

- **measure → fact-locus.** Is the fact unobserved-external, or re-derivable from declared
  fields (e.g. comparing the constraint's own AUTHORED ε across declared readings)? *External
  unobserved fact → Ω_E-route. Re-derivation of a declared value → RESTATE.*

### Diagnosis rule (no forced order; the gate, not a sequence, classifies)

- exactly **one** signature external → **that type** (Ω_C / Ω_P / Ω_E);
- **≥2** signatures external → genuine **HYBRID** (report which, e.g. `hybrid(define+measure)`);
- **no** signature external (all fire only internally, or none fire) → **RESTATEMENT /
  not-a-frontier**.

There is **NO first-fires-wins ladder.** Report signatures as a SET; the gate classifies.

## Failure-mode notes (load-bearing)

- **Restatement is the failure mode of EACH operation** — an orthogonal gate on every fired
  signature — NOT a fourth terminal kind. An omega "diagnoses as restatement" only when NO
  fired signature passes its locus test.
- **"Ω_E = status" is a POST-RUN interpretation, never stamped here.** A `measure` signature
  passing the fact-locus gate routes Ω_E; whether Ω_E is a genuine *kind* vs. a status is
  decided by the Ω_E-falsifier metric, not by this protocol.
- **Hybrids are a real finding**, per the open question in omega_variables.md ("Do genuine
  hybrids exist?"). We report them, not decompose them away.
- The deterministic baseline must **fail-closed**: return `unknown` on ambiguity, never guess.
  Its `unknown`-rate is the determinism-boundary number — trustworthy only under a two-sided
  commit control (it must commit on a KNOWN-definitional case and refuse on a GENUINELY
  ambiguous case).

## No-op fix (2026-06-14) — the gate MUST consult `declared_fields`

The first run's restatement limb was a **no-op**: it missed both seeded restatements (id20, id27).
Root cause — **the gate ignored `declared_fields`** and so could never tell a *declared* reading
from an *open* term. The judge typed id20 (`generate the homoiousios sibling and compare base
properties / ε`) as Ω_C, reasoning "the alternative reading is the open term" — but `homoiousios`
is in this constraint's `cs_reading_relation`, so the comparison **re-derives declared fields →
restatement**. With every signature defaulting to "external," restatement (which requires ALL
signatures internal) was structurally unreachable.

**The rule, made operative:** for each fired signature, check its target **against the entry's own
`declared_fields`**. An operation that re-derives the constraint's **authored fields** (ε-invariance;
comparing authored ε / base properties / beneficiary–victim sets across readings *named in
`cs_reading_relation`/`cs_kernel_id`*) is **RESTATEMENT**, on whichever signature it fired (define,
measure, *and* an incidental decide). Treating a *declared* sibling as an "open term" is the no-op;
a reading or kernel **not** in the declared set is a real external term (→ Ω_C / Ω_E / Ω_P).
Distinguish from an **open conceptual criterion** over declared readings (does X *foreclose or
coexist with* Y — the criterion "what foreclose requires" is open → Ω_C), which is *not* restatement.

**Witness:** `deterministic_baseline.py` now wires `declared_fields` through the gate
(`re_derives_authored`); its runnable `seed_control()` is **GREEN** (KNOWN_RESTATEMENT id20/id27
caught; KNOWN_EXTERNAL pass; UNDER_DECLARATION routes external; two-sided commit control holds;
exit 1 on RED). The historical `judge_results.json` records the *pre-fix* judge and is left as the
POC's as-run artifact; a full judge re-run under this fixed rule would refresh the headline metrics.
