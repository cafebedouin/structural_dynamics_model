# Gate-2 re-witness: `non_agent_beneficiary(entropic_universe_hypothesis)`

**Question (OQ-248, first question):** did the 2026-06-03 gate-2 read measure what it claimed?
**Answer: YES on substance, and the discriminator has now been RUN.** The three hypotheses are
resolved — it is **engine regime**, and the engine change was deliberate and ruled.

---

## Method — the discriminator the plan said was owed

Extracted the exact commit that introduced the registry entry and its gate-2 note
(`f600599b`, 2026-06-03 22:17, *"feat(agency-gate): FSM + NL-count gate on agent_beneficiary/2"*)
via `git archive` into a scratch tree — no worktree, no checkout of the live repo. Then ran the
maxwell probe under three arms.

**Declared deviation:** `prolog/domain_registry.pl` did not exist at `f600599b` (it is the OQ-96
dangling-module defect, warned at every load 2026-02→2026-06). An **empty stub** was supplied,
which is the faithful reproduction: `domain_category/2` fails and `domain_priors:category_of/2`
falls through to its clauses 2–3 exactly as it did when the module was absent. Confirmed harmless
for this measurement — the probe reports `category_of=unknown_novel` (clause 3), the same value the
absent module would produce. Also: the era engine predates `corpus_constraint/1` (introduced
2026-06-04), so enumeration uses `constraint_claim/2`, which is how the era engine itself did it,
with a `maxwell_present=yes` enumeration control.

## The three arms

| arm | engine | corpus | signature | `dr_type` | shadow | entropy |
|---|---|---|---|---|---|---|
| **A** | HEAD | `kernel_v1` (1106) | `coupling_invariant_rope` | rope | rope‑0.95 | 0.1557 |
| **B** | `f600599b` | era `testsets` (1103) | **`natural_law`** | **mountain** | **mountain‑0.95** | 0.1557 |
| **C** | `f600599b` | `kernel_v1` (1106) | **`natural_law`** | **mountain** | **mountain‑0.95** | 0.1557 |

Non-vacuity control in every arm (`maxent_dist_facts` 4412/4424/4424, all > 0). Non-degenerate-fit
control in A and B (shadow distribution spans all six types).

**A vs C is the single-variable isolation: same corpus, different engine, opposite answer.**
Corpus regime is **REFUTED**. Engine regime is **CONFIRMED**.

## What the recorded numbers got right, and what they got wrong

Recorded on 2026-06-03: *"MaxEnt shadow 0.990 mountain / entropy 0.031."*

- **Substance: CORRECT.** In its own regime maxwell certified `natural_law`, classified `mountain`
  at all four canonical contexts, and carried a dominant mountain shadow. The gate-2 read was a
  real measurement of a real state.
- **Digits: NOT REPRODUCED, even in-era.** Measured in-era: mountain **0.95**, entropy **0.1557**.
  `0.95` is the engine's saturation ceiling (0.95 + 5 × 0.01 = 1.00) and `0.1557` is exactly the
  entropy of that saturated shape — note it is **identical across all three arms**, because it is a
  property of the shape, not of which type wins. The recorded `0.990/0.031` corresponds to no shape
  this engine produces in any arm. Treat those digits as unreliable transcription; treat the
  qualitative claim as witnessed.

**So hypothesis 3 ("wrong when recorded") is refuted on substance** and survives only as
"the digits were mis-transcribed," which changes nothing about the ruling.

## Why the engine changed — deliberate, ruled, and two days later

`coupling_invariant_rope` is **not** a new signature; it already existed in the era engine (11
occurrences in `signature_detection.pl`). What changed is that **`natural_law_signature` stopped
firing for maxwell**:

```
HEAD all_signatures=[coupling_invariant_rope]
HEAD natural_law_signature=does_not_fire
```

The cause is **OQ-70**, ruled and landed `72ec2cdd`, **2026-06-05** — *"no signature may read a
single authored perspective as a story-level claim."* The old `claimed_natural/2` source 2 read ANY
single authored mountain perspective as a naturality claim; both bait clauses were removed.
Maxwell's file carries exactly that shape — an authored mountain perspective
(`kernel_v1/maxwell_demon_impossibility.pl:155`) and no explicit story-level naturality claim.

**The gate-2 premise expired two days after it was ruled, by a deliberate operator ruling, and the
registry entry it had licensed was never re-checked.** That is the whole story. It is not drift, not
a regression, and not an error in the original read.

This is also precisely the regime-boundness CLAUDE.md already records for OQ-70: pre-reset
signature readings are an authoring-convention artifact, never a detection result. The gate-2
citation is one such reading.

## Where that leaves gate 2

Gate 2 asks: *does the host independently deserve the certification the entry releases?*

- **Read mechanically at HEAD: FAILS.** Maxwell no longer certifies `natural_law`.
- **Read on substance: HOLDS — but NOT on the surfaces first proposed.** An earlier draft of this
  section offered ε = 0.08 / suppression = 0.02 as the substantive basis. **That is wrong and is
  superseded by the ruling below:** the gate-2 FAIL case carries the *identical* metrics, so they
  cannot discriminate. The surviving basis is narrative — specifically what the authored omegas are
  *about* — with the host's own beneficiary denial admitted as testimony only. Gate 1
  (proposition-kind) was never in question and is untouched.

The divergence between those two readings is not about maxwell. It is that **after OQ-70, the
engine appears to have no path by which a paradigm natural law certifies `natural_law` absent an
explicit story-level claim.** That is a detector-coverage question, strictly larger than this
registry entry, and it is what actually deserves tracking.

## Blast radius of any disposition

Zero observable, either way: maxwell classifies `rope` at HEAD whether or not the entry exists, and
all five live legs carry zero registered beneficiary values. Nothing in the OQ-66 cutover depends
on it. **This is a correctness-of-the-record decision, not a behaviour decision.**

---

# The ruling (operator, 2026-07-25)

**KEEP the entry — as a NEW, DATED gate-2 ruling, not a re-citation of June's.**

The distinction is load-bearing. "Re-cite" would imply the 2026-06-03 pass stands and is merely
better-sourced now. It does not stand: its stated basis is void. **The entry was NOT continuously
certified between 2026-06-03 and 2026-07-25** — the basis expired on 2026-06-05 and nobody
re-checked. Dating it as a fresh ruling is what stops the next reader inheriting a pass that was
never re-taken.

The test that decides re-cite vs. new ruling: *was the shadow read load-bearing in June, or
decorative?* **It was decorative** — and that is now established, not inferred:

```
maxwell (gate-2 PASS)            base_extractiveness 0.08   suppression_requirement 0.02
tech_inevitability (gate-2 HELD) base_extractiveness 0.08   suppression_requirement 0.02
```

Identical metrics, and the held case's shadow was recorded as maxwell-identical. **A surface shared
with the failing case cannot discriminate.** So gate 2's stated procedure ("host metrics AND
narrative/omegas converge on genuine-law") overstated its own rigor: only the narrative/omega half
ever did work. Either branch of the test lands on the same place — if decorative, the June procedure
was weaker than advertised; if load-bearing, swapping the basis is a new ruling by definition.

## The new basis, and what each part can bear

**DISCRIMINATING — the omegas' subject matter.** Both stories author 11 `omega_variable` facts, so
the June note's "omegas authored empty" is **factually wrong** (a second defect, independent of the
void shadow digits). What separates them is *what the omegas are about*:

| | maxwell (PASS) | tech_inevitability (HELD) |
|---|---|---|
| omega subject | is the second law fundamental or an emergent statistical property; does the information-theoretic route independently confirm it | *"does deployment require intentional beneficiary strategy?"*; is this a law of physics or a contingent historical outcome |
| bears on agency? | **no** — physics grounding | **yes, directly** — whether a beneficiary exists |

Gate 2 turns on whether an agent benefits. The held case's open omegas attack exactly that; maxwell's
do not. **This is the only surface verified against a known gate-2 FAIL.**

**CORROBORATING ONLY — the metrics.** ε=0.08 / suppression=0.02 are provably non-discriminating per
the table above. They must not be cited as if they separate.

**HOST TESTIMONY, NOT INDEPENDENT EVIDENCE — the file's own beneficiary denial.** *"Analysis finds no
institutional beneficiary… The hypothesis benefits, but hypotheses are not agents with exit options"*
is authored by the story that gains from the release — the same authored surface the HELD case
failed on. Admissible, labeled, not establishing.

## Why not the alternatives

**Retire (rejected).** Gate 2 fails only *mechanically*, because OQ-70 narrowed the detector — not
because the host changed. Retiring on that launders an OQ-70 side effect into a curation reversal
and re-admits a proposition as extraction evidence, which 63-A prohibits. Gate 1 is untouched.

**Freeze pending OQ-251 (rejected).** The entry stays live under a freeze, so it would operate as
certified while the record said UNRESOLVED. **A frozen-but-active entry is strictly worse than either
a dated ruling or a retirement — it is the state where substrate and record disagree on purpose.**

## Kill condition

If OQ-251 finds that a path to `natural_law` certification **does** exist post-OQ-70 and maxwell
still fails it, the narrative basis is called into question and gate 2 returns to the operator's seat.

## Standing method rule (added to the two-gate block)

> A gate-2 read must state which surface **DISCRIMINATES** and check it against a known gate-2
> **FAIL**. A surface shared with the failing case is corroboration at best.

## Forward mechanism — OQ-252

This session saw a ruling's premise expire without its dependents being re-checked **twice**: the
reset at seven weeks, OQ-70 at two days. Common gap: rulings record what they decide, not what
depends on them. `Licenses:` is the proposed back-reference. **Operator ruling: forward-facing only —
added when a ruling is created or revisited from 2026-07-25 onward, never backfilled.** Corollary:
absence on an older ruling means "predates the convention," never "licenses nothing." This ruling
carries the first `Licenses:` line.
