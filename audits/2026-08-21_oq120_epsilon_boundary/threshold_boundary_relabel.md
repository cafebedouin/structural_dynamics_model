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
