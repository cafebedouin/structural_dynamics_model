# `threshold_boundary/5` label adjudication — evidence recorded, **NO COMMIT**

**Branch: G1b** (operator ruling 2026-08-23, scored on the *decisive* reading). Under
**[G1-D]** — the plan's canonical, single-definition statement of G1's effect on Step D — Step D
lands **no commit**; this documentation action still runs; the label question is left **open**, and
this plan does not schedule its return. Any later relabel is a fresh decision requiring its own
evidence.

**This is a false label rendered into a consumed artifact, not a classifier defect.** Args 4/5 of
`threshold_boundary/5` are label-only: `maxent_classifier.pl:611` binds them `_, _`. Nothing
classifies differently because they are wrong. What is wrong is a *rendered claim* in
`outputs/maxent_report.md`, which has two real consumers (`enhanced_report.py:4138`,
`tangled_decomposition.py:42`).

---

## Instance 1 — `maxent_classifier.pl:591`

```prolog
threshold_boundary(snare_epsilon_floor, extractiveness, Thresh, rope, snare) :-
    config:param(snare_epsilon_floor, Thresh).
```

**Claim:** this gate separates `rope` from `snare`.

**Sweep witness** (18 live legs, 17,104 stories, 122,031 located transitions):

| | |
|---|---|
| transitions with FT pair exactly `{rope, snare}` | **0** |
| `snare_epsilon_floor`-DECISIVE transitions | **1** — `tangled_rope → snare` |
| modal FT pair among all 4717 `snare_epsilon_floor`-MOVED transitions | `tangled_rope → naturalized` (4373) |

The `rope` half of the label is falsified on every reading and at every scale. The `snare` half is
reachable exactly once in the entire live corpus — from `tangled_rope`, not from `rope`, on
`testsets_haiku3` only, and not replicating across its own model's redraw triple.

**Rendered counter-witness** (`outputs/maxent_report.md:101-115`), which already falsifies its own
header on the live corpus:

```
### snare_epsilon_floor (threshold=0.4600, rope <-> snare)

| Constraint                                                     | Distance | Det Type     |
| constitutional_text_authority__positivist_reading              | 0.0100   | tangled_rope |
| constitutional_text_authority__living_constitutionalist_reading| 0.0200   | tangled_rope |
| prerequisite_debt_reading                                      | 0.0200   | unknown      |
| qualified_immunity_doctrine__protective_scaffold_reading       | 0.0200   | tangled_rope |
| authority_vacuum_incommensurability                            | 0.0200   | piton        |
| behavioral_mechanism_reading                                   | 0.0200   | unknown      |
| impression_management_reading                                  | 0.0200   | unknown      |
| algorithmic_attribution_flat_control                           | 0.0400   | piton        |
| authorial_legitimacy_kernel_flat_control                       | 0.0400   | tangled_rope |
| blindness_decomposition_kernel_flat_control                    | 0.0400   | unknown      |

*49 constraints within 0.10 of boundary*
```

Ten of ten are `tangled_rope`/`unknown`/`piton`. Not one is `rope` or `snare`, and none of the 49
within 0.10 is either.

**Candidate replacement if ever ruled:** `tangled_rope, snare` — supported by the sole decisive
case and by the plan's arithmetic. **Not committed.**

## Instance 2 — `maxent_classifier.pl:593`

```prolog
threshold_boundary(rope_epsilon_ceiling, extractiveness, Thresh, rope, tangled_rope) :-
    config:param(rope_epsilon_ceiling, Thresh).
```

**Claim:** this gate separates `rope` from `tangled_rope`.

**Sweep witness:**

| population | modal FT pair | n |
|---|---|---|
| all `rope_epsilon_ceiling`-attributed (6571) | `piton → tangled_rope` | 2792 |
| **DECISIVE (1936)** | **`piton → rope`** | **1188** |
| DECISIVE, 2nd | `rope → naturalized` | 617 |
| DECISIVE, MT pairs | `rope → rope` | 1163 |
| labelled pair `rope → tangled_rope`, DECISIVE | — | **0** |

**Rendered counter-witness** (`outputs/maxent_report.md`, `### rope_epsilon_ceiling
(threshold=0.4500, rope <-> tangled_rope)`): the ten nearest constraints are 4× `tangled_rope`,
4× `unknown`, 2× `piton` — no `rope` at all, on a boundary whose label names `rope` as one side.

**The existing label is falsified — and so is the replacement the plan predicted.** The plan's Step
D G0 branch prescribed relabelling `:593` to `rope, naturalized`. The witness does not support it:
`piton → rope` outruns `rope → naturalized` roughly two to one among decisive transitions, and the
modal **MT** pair is `rope → rope`, meaning MT does not move at all and the change lives in the
signature layer. Under G1b nothing commits either way — but the divergence is a finding in its own
right and is carried at full volume into `WRITEUP.md` → *Process findings*.

## Why the strict/strict partition makes the original `:593` label unreachable by ε

`rope` requires χ ≤ 0.35 (`rope_chi_ceiling`); `tangled_rope` requires χ > 0.35 **strict**
(`drl_core.pl:441`, deliberate per OQ-37 Move 1 so the seam is single-valued). Raising ε past 0.45
at a rope-qualifying χ therefore cannot reach `tangled_rope` — it reaches `naturalized`
(`drl_core.pl:467`: ε > rope ceiling ∧ χ < tangled_rope floor). That is why the plan predicted
`rope, naturalized`. The prediction is right about what the *cascade permits* and wrong about what
the *corpus exhibits*: the χ-side movement that actually accompanies the ε crossing puts most
decisive transitions on the `piton` boundary instead. **Reasoning from the cascade to a label is
exactly the move that produced the original error; it produced a second one here.**

## Status

- `:591` — **no commit**, question open. Evidence above.
- `:593` — **no commit**, question open. Evidence above, including that the plan's own predicted
  replacement is unsupported.
- `git diff --stat -- outputs/ python/ prolog/` remains **empty**; no engine or output file was
  touched by this audit.

---

# v2 ADDENDUM (2026-08-23) — the predicted FIX failed against witness, twice, on two independent scorings

**Still no commit.** Branch is **G1b** under v2's repaired gate as it was under v1's; `[G1-D]`
governs; both labels stay open. What follows is the finding with legs, which the v1 body understated
by framing it as "the observed modal pair differs from the plan's prediction."

## The finding is about the CORRECTION, not the original label

Three propositions, each independently witnessed:

1. **`:591`'s original label `rope <-> snare` is wrong.** FT pair `{rope, snare}` observed **0**
   times in 122,031 live transitions, under both scorings.
2. **`:593`'s original label `rope <-> tangled_rope` is wrong.** `rope → tangled_rope` observed
   **0** times among decisive transitions, under both scorings.
3. **The replacement the plan prescribed for `:593` — `rope, naturalized` — is ALSO wrong**, and
   this is the one that matters.

| scoring | n | modal decisive FT pair | `rope → naturalized` |
|---|---|---|---|
| v1 (moved-scored qualifying set) | 1936 | `piton → rope` **1188** | 617 |
| v2 (decisive-scored, repaired gate) | 1936 | `piton → rope` **1188** | 615 |

`piton → rope` leads by ~2:1 on both. **The prediction is unsupported twice, on two independent
scorings of the same data, at two different resolutions of the gate.** It is not a marginal call
that a tighter filter might have rescued — tightening the filter left it exactly where it was.

## Why this is the durable result

The plan's derivation for `rope, naturalized` was clean and cascade-correct: `rope` needs χ ≤ 0.35,
`tangled_rope` needs χ > 0.35 *strict* (`drl_core.pl:441`, OQ-37 Move 1), so raising ε past 0.45 at
a rope-qualifying χ cannot reach `tangled_rope` — it reaches `naturalized` (`drl_core.pl:467`).
**Correct about what the cascade permits. Wrong about what the corpus exhibits.** The χ-side
movement that actually accompanies the ε crossing puts most decisive transitions on the `piton`
boundary, and no amount of reading the cascade would have said so.

So the arc contains **two label errors of the same kind, one layer apart**:

- **2026-06-13** — `:591`'s `rope <-> snare` is authored by cascade reasoning. It is wrong, and it
  is what put the phrase "rope/snare boundary" into OQ-120's premise, generating this entire
  investigation.
- **2026-08-21** — the plan, written to *correct* that class of error and explicitly gating the
  relabel behind a witness, prescribes `rope, naturalized` for `:593` **by the same cascade
  reasoning** — and it is wrong too.

**The correction reproduced the original's error class under review conditions built to catch it.**
That is stronger evidence for gating output-changing relabels behind a witness than any argument in
the plan's prose, because it is the plan's own arithmetic failing under its own discipline. The
discipline worked: the gate held, no wrong label shipped, and the failure is visible only because
the witness was required before the commit.

**Operational rule this supports:** a `threshold_boundary/5` label — or any label asserting which
types a gate separates — is a claim about the *corpus*, not about the *cascade*. Derive candidates
from the cascade if useful; never commit one without the observed pair distribution behind it.
