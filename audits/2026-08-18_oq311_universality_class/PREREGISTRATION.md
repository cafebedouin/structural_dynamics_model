# PREREGISTRATION — OQ-311 Item 2

**Authored:** 2026-08-18
**Status:** AUTHORED AND FROZEN, **UNFUNDED**. No run is licensed by this document.
Executing it requires an explicit operator spend-go. Item 2 stays OPEN until then.
**Frozen:** this file is never retro-edited. If it is amended after a first run, the
new md5 is logged at its position in `audit_log.md`; that history is the honest record.

**AMENDMENT 1 — 2026-08-18, BEFORE ANY RUN (no results exist to amend against).**
§4 and §5.5 amended to close a live free parameter: the original text anchored the minimum-cell-N
floor to the feasibility table without naming the modal-type tie rule those figures were computed
under, and left the rule to the receiver. Since the cell counts move with the rule, a receiver
picking a different one would move the UNANSWERABLE boundary — the exact degree of freedom a
prereg exists to close. **The floor is now evaluated under all three rules with the minimum
governing**, and the feasibility figures are quoted per rule. Both md5s are logged in
`audit_log.md`. Caught in review by the operator; recorded rather than silently applied.

---

## 0. Receiver's licence to refuse — read before anything else

You are executing a design you did not write. **If any instruction below is correct in
prose and wrong when executed, say so and stop rather than comply.** "The prereg said to"
is not a witness; a compliant run's output looks exactly like work while carrying this
document's authority into the substrate. Report the refusal at the volume of a completion
and route it back, rather than silently repairing the instruction here — a silent repair
leaves the defect in the design, where it fires on the next receiver.

Scope of the licence is narrow: *executed as written, this produces what the design
forbids.* It is **not** "this is hard", "this is unclear", or "I'd do it differently" —
those you flag in one sentence and proceed.

Two things this design specifically wants you to refuse:
- Any instruction to judge success by whether the rebuilt sweep reproduces +0.21/+0.014
  (see §8). Those numbers are withdrawn; judging against them encodes the tested claim
  into the instrument.
- Any instruction to run against the live `prolog/testsets/` leg (see §6).

---

## 1. The conflation that must be resolved FIRST — do not silently pick

`observers_not_humans_v6.md` §2.3 states the geometric condition in two ways that are
**not the same set**:

- **(C-snare)** "Hub 1 spans the snare gate" — the per-constraint χ span crosses
  `snare_chi_floor`.
- **(C-zero)** "institutional beneficiaries below `d_zero` and powerless victims above" —
  the per-constraint χ span crosses **zero** (the sign-flip point; d_zero ≈ 0.1642).

OQ-311 glosses both as "the gate-spanning condition". They are different populations and
can give different answers.

**Required before any run:** either pre-declare ONE of them as the operative condition and
say why, or run **both as separate, separately-reported arms**. Silently picking one is a
Pattern-2 fork on the definition and is exactly what this section exists to prevent.

**Disclosure:** the feasibility probe in `evidence/feasibility_crosstab.py` used **C-zero**.
That was a convenience choice for a design-feasibility question and is **not** a
pre-registration of C-zero as the operative condition. Do not read it as one.

---

## 2. Arms

Same corpus, same variant set, same code state throughout.

- **Arm A — condition cut.** Stratify the sign-flip Jaccard advantage on the geometric
  condition (C-snare and/or C-zero per §1), **ignoring type**.
- **Arm B — label cut.** Stratify the same advantage on the `tangled_rope` label,
  **ignoring the condition**.

"Sign-flip Jaccard advantage" means, as in the original design, the difference between the
sign-flip-present variants (Arm A1–A3 in `range_sweep.py`'s own naming) and the
sign-flip-absent controls (B1–B3), computed within each stratum.

**Naming collision, flagged:** `range_sweep.py` already uses "Arm A / Arm B" for
*sign-flip-present vs sign-flip-absent transformation variants*. This document uses "Arm"
for *stratification scheme*. When reporting, write **"stratum-arm A (condition)"** and
**"variant-arm A (sign-flip present)"** — never bare "Arm A".

---

## 3. The 2×2 cross-tab, with meanings pre-committed

Cross-tabulate condition × label and compute the advantage **within each of the four cells**:

|                        | `tangled_rope`        | ¬`tangled_rope`               |
|------------------------|-----------------------|-------------------------------|
| **condition holds**    | cell TT (confounded)  | **cell FT — DISCRIMINATING**  |
| **condition fails**    | **cell TF — DISCRIMINATING** | cell FF (baseline)     |

Pre-committed readings:

- Advantage present in **FT** and absent in **TF** → the **condition** carries it.
- Advantage present in **TF** and absent in **FT** → the **label** carries it.
- Advantage present in **both** off-diagonals → neither cut is the operative one; both are
  proxies for something else. Report as such; do not pick the more convenient.
- Advantage present in **neither** off-diagonal, only in TT → the effect exists only where
  the two coincide and this design **cannot** separate them. Declare **UNANSWERABLE**.

**A design that cannot populate both FT and TF is a declared UNANSWERABLE, not a result to
ship.** Check occupancy BEFORE computing any Jaccard, and report the occupancy table even
if the run stops there.

---

## 4. Minimum cell N — declared before the run

**Floor: 30 constraints per cell**, applied to all four cells; any cell below it is reported
as `insufficient` and its advantage is **not** computed (no point estimate, not even a
caveated one).

**THE FLOOR IS EVALUATED UNDER ALL THREE TIE RULES, AND THE MINIMUM GOVERNS.** A cell clears
the floor only if it holds ≥ 30 under **`exclude` AND `first` AND `any_tr`** (defined in §5.5).
This is the operative rule for the occupancy determination and therefore for the UNANSWERABLE
call in §3 — it is **not** left to the receiver to pick.

*Why it is fixed here rather than declared by the receiver.* §5.5 asks the receiver to declare
a tie rule for *reporting*, which is fine for reporting and **not** fine for a gate: the cell
counts move with the rule (kernel_v1 TF = 150 / 155 / 160 under exclude / first / any_tr), so a
receiver free to pick the rule is free to move the UNANSWERABLE boundary — the exact undeclared
degree of freedom this document exists to close. Taking the minimum makes the gate conservative
and rule-independent: it can only ever declare UNANSWERABLE more readily than a single-rule
reading would, never less. If the three rules disagree about whether a cell clears the floor,
**report that disagreement** — it is itself a finding about how label-dependent the stratum is,
and it is exactly the kind of thing a single-rule run would silently absorb.

Anchor for that floor is the feasibility table in `evidence/feasibility_crosstab.tsv`, which
is a **proxy on other corpus legs at a different code state** — it is a floor for *design*,
never a predicted result. What it establishes is only that both off-diagonals are populated
on at least some legs. **Stated with the rule named, because the counts are rule-dependent:**
under `exclude`, haiku TF = 40 and kernel_v1 TF = 150; under `first`, 48 and 155; under
`any_tr`, 48 and 160. `testsets` and `testsets_flash` fail on **every** rule (flash
degenerately, TF = 0 under all three). The occupancy **verdict** — which legs populate both
off-diagonals — is stable across all 3 rules × 2 type columns on all four legs; only the
counts move. That stability is why a min-across-rules gate is workable rather than paralysing.

**The floor is on the `original_v6` occupancy, which is unmeasured.** If `original_v6`
turns out flash-like — TF empty — the answer is UNANSWERABLE on this corpus (§3), and that
is a legitimate terminal result, not a failure to be worked around by lowering the floor.
**Do not lower the floor after seeing the occupancy, and do not switch tie rules after seeing
it either** — both are the same move wearing different clothes.

---

## 5. The tool that must be built — this is the spend

**No script in this repo computes a stratified Jaccard.** `range_sweep.py` computes global,
arm-level Jaccard only, and no version of it ever did otherwise (witness:
`evidence/range_sweep_output_keys.txt`). Building the stratified version IS Item 2's cost;
it is not a re-run of something that exists.

The extension is small and is specified exactly so it is not redesigned on the fly:

1. `range_sweep.load_presheaf_set(path)` already returns a **set of per-constraint ids**
   (those with `h0 == 0`; note it correctly excludes OQ-51 `null` h0 as UNDETERMINED —
   preserve that exclusion, and count the excluded ids per stratum in the report).
2. Stratification is a **partition of that id set** by a per-constraint `(type, condition)`
   map, applied to both the baseline set and the variant set *before* `jaccard_stats`.
3. Sources for the map, both of which take a corpus path and overlay via `asserta`:
   - **type** — `run_pipeline.classify_corpus(corpus_path, output_name, expected_model)`
     (`python/run_pipeline.py:147`), the gate-free `json_report` path with built-in refusals
     (zero-glob, load-completeness, single-model fingerprint, raw freshness). **Serialize
     calls** — they share the raw artifact.
   - **χ span / condition** — `python/audits/oq22_starvation_census.py`, which already takes
     a corpus path, overlays `corpus_path` via `asserta`, runs ONE swipl process per corpus,
     and **hard-stops on a load-count shortfall**. Its census TSV carries `chi_min`, `chi_max`,
     `chi_span` per constraint.
4. **Join key.** Both sources key by constraint id. Assert the join is **total** in both
   directions before use: every id in the presheaf set has a `(type, condition)` entry, and
   report any id that does not rather than dropping it silently. An unjoined id is a
   Pattern-6 absorption waiting to happen.
5. **Modal type needs a tie rule, and the three are named here.** `mtype_vec` is per-position;
   reducing it to one label requires a rule for ties. The three swept in the feasibility probe,
   and the only three this prereg recognises:
   - **`exclude`** — a tied constraint is dropped from the cross-tab entirely.
   - **`first`** — first occurrence among the tied maxima, in positional order.
   - **`any_tr`** — counts as `tangled_rope` if `tangled_rope` is among the tied maxima.

   **Report all three.** The cell **counts** move with the rule (kernel_v1 TF: 150 / 155 / 160)
   even where the occupancy **verdict** does not. **The §4 floor gate is evaluated under all
   three and the minimum governs** — that is not the receiver's choice to make; §4 is
   authoritative and this item does not override it. Declaring a single rule for *narrative*
   convenience is permitted; declaring one for the *gate* is not.

   Also declare `mtype_vec` vs `ftype_vec` (metric vs post-signature type) and say why; §2.3's
   "constraint family" is ambiguous between them, and the feasibility sweep showed the choice
   matters more for kernel_v1 (TF 150 vs 118 under `exclude`) than for haiku (40 vs 41).

---

## 6. Corpus and code state — both named, every time

- **Corpus: `prolog/archives/datasets/original_v6`** (3,380 constraints — measured, not
  recalled: `corpus_constraint/1` = 3380, `evidence/load_check.txt`). This is the corpus every
  §2.3 number came from; it is the `prolog_v5` directory renamed at **R100**, byte-identical
  (`evidence/rename_r100.txt`). Chimera-era caveats apply: **ID reuse across runs (OQ-25 /
  v7 §5.11)**.
- **Overlay with `asserta`, never plain `assertz`.** `config.pl` defines the default
  `param(corpus_path, testsets)` as its first clause and the loader takes the first solution,
  so an appended `assertz` is **silently ignored** and you load the live leg while the count
  looks plausible (witnessed hazard).
- **NEVER the live `prolog/testsets/` leg.** It is a deliberately sparse singleton, it moves
  mid-session, and it carries no published count. An engine-regime question witnessed only
  there is under-witnessed. **Refuse any instruction to use it** (§0).
- **Cross-check on the twin legs** (`testsets_haiku`, `testsets_flash`), which is where OQ-01
  falsified the A3 collapse in the first place. Cross-corpus counts are per-generation, never
  expected prevalence.
- **Name the code state.** The χ path has moved since May 2026 — the legacy χ = ε × π path is
  **fully drained** (OQ-67), so the engine that would run this is not the engine that produced
  §2.3. "HEAD yields N" is ambiguous between engine-regime and corpus and has already caused a
  misread. Every reported count names **both** corpus and `code_commit`.
- **Freeze the corpus around the run.** md5-fingerprint each leg around **both** halves of any
  before/after pair and serialize behind any running `c-orchestrator` — operator topic runs
  land stories mid-session (witnessed 2×).

---

## 7. Pre-declared outcomes — including the third

- **Condition carries it (FT yes, TF no).** The framework has a genuine class with a *named
  defining condition*: a population picked out by a structural property in which robustness to
  functional form is a real invariant, with `tangled_rope` merely its most common label in this
  corpus. This licenses §5.5 to go looking for **the condition** — not the label — in RL,
  animal, and embodied populations.
- **Label carries it (TF yes, FT no).** "Universality class" is **unearned**: the robustness is
  a property of this corpus's type mix. §3's main quantitative support for the structural claim
  is withdrawn, and §2.3's framing must be rewritten, not annotated.
- **Neither cleanly** (both off-diagonals show it, or neither does). Routes to the
  engine-regime question OQ-22 sharpened, and is discharged **as an existence claim only** —
  "there exist constraints where the advantage appears without either cut" — with no
  prevalence, no rate, and no corpus-wide gloss.

Nothing below the §4 floor produces any of these three; it produces `insufficient`.

---

## 8. Positive control — a run that cannot decline licenses nothing

A control demonstrates **discrimination, not detection**. Before the spend, **name a
naturally-arising negative**: a population where the sign-flip advantage is known or expected
**absent**, drawn from the corpus rather than authored as a decoy.

The strongest available candidate, and the one to use unless a better one is found:
**the no-sign-flip variant arm (B1–B3) itself.** It is the original design's own control, it
arises naturally, and the stratified pipeline must show the advantage **collapsing** there in
every cell. A stratified sweep that reports an advantage inside the B arm is measuring its own
plumbing, and the run halts.

Second candidate, **conditional and to be checked, not assumed**: `testsets_flash`, where the
TF cell was empty under all six tie-rule × type-column settings tested. If that survives on
`original_v6`-adjacent conditions it is a naturally-arising structural negative.

**Ranking, stated so the report claims at the right altitude:** a decline in the instrument's
own history > a naturally-arising negative from the population > an authored decoy (which shows
only that authored decoys get rejected — a floor, and must be reported at that altitude).

**If no decline is available anywhere in the population, the question is UNANSWERABLE from this
corpus** — that is a verdict to declare and route to a typed Ω, not a caveat to ship under.

---

## 9. Explicit NON-TARGET — the relocating confound, aimed at our own prior

**Reproducing +0.21 / +0.014 / 14.6× is NOT a success criterion, and their absence is NOT a
failure.** Those numbers are **withdrawn as unwitnessed** (`WRITEUP.md`, §2.3 correction block
of 2026-08-18): the script named as their witness cannot produce per-type numbers, and the
subset sizes are arithmetically impossible (2,245 + 1,169 = 3,414 > 3,380).

A rebuilt sweep judged by whether it lands near them would be a **reconstruction of a
withdrawn prior**, encoding the tested claim into the instrument and making the theory
unfalsifiable through its own gate. This is the specific reason Item 1 was executed
**prereg-first, without any corpus run**.

Report whatever the stratified sweep gives. If it happens to land near +0.21, that is a new
result with a new witness — it does not retroactively re-validate the withdrawn citation, and
the correction block stays as written.

---

## 10. What "done" means

Item 2 is done when: the §1 conflation is resolved in writing; the occupancy table is reported
(even if it terminates the run); the tool from §5 exists and its join totality is asserted; the
§8 control has fired **and declined**; and one of the three §7 outcomes — or `UNANSWERABLE`, or
`insufficient` — is stated with corpus and code state named.

Every count pastes its witness in the same turn it is claimed.
