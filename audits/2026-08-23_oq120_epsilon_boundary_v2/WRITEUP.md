# OQ-120 Phase 0 v2 — the rope/snare ε boundary re-scored under a repaired gate: G1b, invariant

**Executed:** 2026-08-23
**OQ:** OQ-120 (stays **open**, re-specified); mints OQ-351; sibling mint OQ-350 from v1
**Verdict:** **G1b — UNCORROBORATED.** Floor met in 12 of 23 strata across 6 models; **1**
`snare_epsilon_floor`-DECISIVE transition in 122,031, from a single model stratum. The rope/snare ε
split is not a free decision boundary: `rope_epsilon_ceiling` is decisive **1936×** while
`snare_epsilon_floor` is decisive **once**, and the FT pair `{rope, snare}` the boundary is named
for is observed **0 times**.
**Substrate:** identical to v1 — 18 live legs (17,104 stories) + `kernel_v1` (1,106), HEAD unmoved
at `f88c8c3c`; `outputs/pipeline_output.json` manifest `2026-08-21T16:36:35Z`, `n_constraints 285`,
`n_stories 258`, `code_commit_short 885151b`.
**Fired:** live
**Evidence map:**

| artifact | what it holds |
|---|---|
| `PREREGISTRATION.md` | v2's spec, **banner-stamped as NOT frozen-before-results**, with the seven changes and what forced each |
| `audit_log.md` | v2 OPEN/CLOSE stamps, both prereg md5s, the ordering-witness statement |
| `gate_readout.md` | branch, the per-stratum floor table, both scorings, controls, the backfill counterfactual |
| `gate_numbers.txt` / `gate_readout.json` | the numbers and every classified transition row |
| `analyze.py` | v2 gate: decisive scoring, per-stratum floor, exhaustive G1a/b/c |
| `build_strata.py` | four-part stratum key `(model, regime, prompt_hash, schema_hash)` |
| `eps_transition_map.py`, `run_all_legs.sh` | the sweep fork and driver, unchanged from v1's final state |
| `threshold_boundary_relabel.md` | `:591`/`:593` — **no commit**; v2 addendum on the failed predicted fix |
| `raw/` | 19 per-corpus sweep JSONs, per-leg stdout/stderr, `sweep_log.txt`, `strata.json` |
| `../2026-08-21_oq120_epsilon_boundary/` | **v1: the ordering witness for the data.** Its `PREREGISTRATION.md` (md5 `b181e1a2a9cd42b86d190be09f61d400`) genuinely precedes the data; its `WRITEUP.md` is unedited; `V2_ADDENDUM.md` there is the pointer |

---

## What v2 is, stated precisely

**Same rows, re-scored, branch invariant.**

v2 swept the identical substrate and **reproduced v1's transition data exactly** — same count, same
`(id, seat, ε bracket, MT/FT pair, gate set)` on every row, **0 of 19 legs differing**. So v2 is not
a second measurement of the world; it is the same dataset re-scored under the MOVED-vs-DECISIVE
definition the operator ruled at v1's checkpoint, having seen both scoring tables already computed.

That invariance is what licenses the arrangement:

- **The ordering witness attaches to the DATA.** v1's prereg genuinely precedes the data; the data
  is bit-identical between runs; therefore v2's numbers inherit v1's ordering witness.
- **The scoring change is separately witnessed** — the operator's ruling is timestamped in the
  session transcript, made with both tables in hand.
- **v2 is the weaker instrument for FREEZING; it is not the weaker instrument for SCORING.** Both
  halves are true. v2's prereg carries a banner saying the first half on its own face.
- **A post-hoc re-specification that survives its own tightening without changing the verdict is
  about as clean as post-hoc specification gets.** The tightening was not cosmetic: decisive
  scoring cut pooled `N_rail` ~5× (9191 → 1852), and the floor went from cleared-900×-over to
  **failed by 11 of 23 strata**. The branch did not move.

It also doubles as a determinism check on a fork that was patched mid-flight during v1 (the
double-emission fix), confirming the published numbers come from the code that shipped.

## The result

```
BRANCH G1b — floor met; 1 snare_epsilon_floor-DECISIVE transition, from 1 model stratum
             (claude-haiku-4-5-20251001) — fails the >=2-distinct-MODELS criterion

pooled memo (satisfies nothing on its own):
  N_eps 1919   N_reach 1917   N_rail 1852     qualifying 1919 (+0 unknown-endpoint)
  all located live transitions 122031          MT-invariant / FT-only 1163 = 60.6%

snare_epsilon_floor    MOVED 4717    DECISIVE 1
rope_epsilon_ceiling                 DECISIVE 1936
FT pair exactly {rope, snare} among snare-DECISIVE: 0
```

**The single decisive case:** `testsets_haiku3`,
`equal_protection_kernel__antisubordination_reading`, analytical seat, ε 0.4599→0.4600,
`tangled_rope → snare`, χ steady at ≈0.680 (χ is not what moved), sole changed gate
`snare_epsilon_floor`. It appears in **1 of 3** same-model redraws of the same seed, on the noisiest
floor in OQ-347's table (haiku, 65% churn with ε pinned). `kernel_v1`: 0 decisive in 10,215. Read
substantively as a draw artifact by both operator and executor — **and still not zero**, which is
why G0 did not fire and why the gate was frozen in the first place.

**What actually happens at 0.46:** `resolve_coalition_power/3` upgrades `powerless → organized`, d
drops, and **χ falls as ε rises**. The modal MOVED outcome is `tangled_rope → naturalized` (4373 of
4717). The split is dominated by χ and by a step that moves d, not by the ε gate.

## The floor discriminates — and surfaces a finding of its own

12 of 23 strata pass `N_rail ≥ 10 ∧ rate ≥ 0.5%`; 6 distinct models; **11 fail**. Full table in
`gate_readout.md`. The sharpest cell:

| stratum | stories | N_rail | rate |
|---|---|---|---|
| `gemini-2.5-flash\|off` (3 backfill cells) | 2873 | **5** | **0.04%** |
| `gemini-2.5-flash\|on` | 1980 | **101** | **1.28%** |

**~30× on the ε-decisive transition rate between thinking-off and thinking-on, in the one model
where both arms are large.** This is a statement about *authoring regime*, not about the rope/snare
boundary, and it is **filed as OQ-351 rather than closed inside this entry** — burying it in a
closed entry about an ε gate is how a real finding becomes unfindable. The old pooled `≥10` could
never have surfaced it: flash's 5 would have sat inside a total of 9,191.

## The backfill counterfactual — recorded verbatim as a reusable pattern

Several legs are **backfilled**: the original pass left gaps, the schema was fixed, and the models
re-did the failed stories to improve cross-leg id matching (operator, 2026-08-23; recorded in-file
in `story_provenance/8`). Measured:

| leg | older cell | backfilled cell |
|---|---|---|
| `testsets_haiku` | **505** @ prompt `22843cdf` / schema `2e9dff2f` / 2026-06-13 | **455** @ `e03e2210` / `685ed7cf` / 2026-08-22 |
| `testsets_flash` | 754 @ same June pair | 206 @ same August pair |
| `testsets_nemotron` | 664 + 188 @ `685ed7cf` | 144 @ `e03e2210` `+rescue1` |
| `testsets_stealth` | 968 @ `685ed7cf` | 36 @ `e03e2210` `+rescue1` |

`testsets_haiku` is **47% re-authored 70 days later under a different prompt AND a different
schema.** OQ-78 ruling 5 forbids pooling ε-keyed denominators across generation regimes *within one
model*, so the stratum key was widened to `(model, regime, prompt_hash, schema_hash)`.

**Then the counterfactual was run, and it says the finer key rescued nothing here:**

- 4-part key: **6** models pass the floor. 2-part key `(model, regime)`: **the same 6**. No
  model-level flip.
- **Branch identical either way.**
- Exactly **two** cell verdicts depend on the split, both tiny mixed cells:
  `claude-sonnet-5|unknown|8080348c|becd0f87` (55 stories) and `…|8080348c|f1436bd4` (18) each fail
  on their own and would pass when merged.

**So: keep the finer key because ruling 5 requires it and because it makes `gemini-2.5-flash|off`'s
three-way split legible — but do not cite it as having rescued a conclusion. It did not.**

*The pattern, stated for reuse:* when you tighten a denominator on principle, run the loose version
too and report whether the tightening changed any verdict. A refinement that is right on principle
and inert in effect should be kept **and** described as inert. The alternative — adopting it and
implying it mattered — is how a methodological improvement quietly becomes an unearned claim.

## `:591` / `:593` — no commit, and the finding is about the correction

Under `[G1-D]` neither label commits. `threshold_boundary_relabel.md` carries the evidence; its v2
addendum carries the finding with legs:

- `:591`'s `rope <-> snare`: FT pair observed **0×** under both scorings.
- `:593`'s `rope <-> tangled_rope`: observed **0×** among decisive under both scorings.
- **The plan's prescribed replacement for `:593` — `rope, naturalized` — is ALSO unsupported, twice.**
  Modal decisive pair is `piton → rope` (1188) vs `rope → naturalized` (615), ~2:1, on v1's scoring
  and on v2's. Tightening the filter left it exactly where it was.

The arc therefore contains **two label errors of the same kind, one layer apart**: `:591`'s original
label authored by cascade reasoning in 2026-06-13 (which is what put "rope/snare boundary" into
OQ-120's premise and generated this investigation), and the plan's 2026-08-21 correction reaching
for the same cascade reasoning and landing wrong again — **under review conditions built to catch
exactly that**. The discipline worked: the gate held, no wrong label shipped, and the second error is
visible only because a witness was required before the commit. **A `threshold_boundary/5` label is a
claim about the corpus, not about the cascade.**

## Controls

C1 PASS ×19 (natural carrier, synthetic fallback never needed), C2 PASS ×19 (C1's transition
vanishes at floor 0.90, restore verified), C3 PASS ×19, C4 PASS ×10 / SKIPPED-precondition ×8 /
SKIPPED-carrier-absent ×1, **0 FAIL** — using the three-way bookkeeping the plan now supplies rather
than inventing it at runtime. C1 fired and C2 declined on every corpus, so the branches are
interpretable. The DECISIVE predicate asserts its own two-sided control in code and additionally has
a naturally-arising positive (the one live case), lifting it off authored-decoy grade.

## Process findings

Carried from v1 (`../2026-08-21_oq120_epsilon_boundary/WRITEUP.md`, unedited) and extended:

1. **The plan's own correction failed against witness, twice.** Above and in the relabel addendum.
2. **Eight instances of one drift shape in planning, six introduced by fixes** — five by cancelling
   an action meant to survive, one by adding an unruled commitment. Removed by `[G1-D]`'s
   single-definition form rather than by patching instances. **At execution, twice, `[G1-D]` held.**
3. **Two defects in v1's own instruments, caught before publishing a number**: `emit_transition`'s
   bare `( A ; B )` disjunction inside `forall/2` double-emitted *differentially* (testsets
   3228 → 1863, not → 1614); and C4 reported an absent precondition as `FAIL`. Both fixed, all
   corpora re-swept, the defective output preserved at `raw_PREFIX_double_emission/` as a free
   negative control.
4. **Two gate criteria were unsatisfiable or vacuous by construction**, and both survived six blind
   review rounds: G0's undefined "attributes to" (unsatisfiable wherever any transition exists at
   0.46) and the pooled `N_rail ≥ 10` (cleared 900× over). Both were only visible once the gate ran
   against real numbers. **A criterion that cannot come out false is not a criterion**, and a
   review process that reads for correctness rather than for falsifiability will pass both.
5. **S8 was false as written** — pristine `f88c8c3c` is GREEN and this audit's own directory reds
   two rows until its `WRITEUP.md` with a `**Fired:**` line lands. Established with a two-sided
   control rather than assumed, which is what showed the reds were the audit's and not the
   concurrent leg generation's.

## Non-authoring-facing

OQ-78 ruling 3 binds this phase's **output**. `gate_readout.json` and `raw/` hold exact ε values at
which types flip. They must never feed a prompt, a seed file, or `epsilon_bin`, which stays a
dangling wire on purpose: re-wiring it would manufacture the concordance this OQ measures. **The
same constraint rides OQ-351** — its rate is reportable, the exact ε values behind it are not.
