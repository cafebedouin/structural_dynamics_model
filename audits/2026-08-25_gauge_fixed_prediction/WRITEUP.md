# The `gauge_fixed` prediction, run at last — and refuted inside its own stratum

**Executed:** 2026-08-25
**OQ:** none — this is the pre-registered prediction in the source comment at
`prolog/constraint_indexing.pl:798-799`, never executed since it was written. Phase A of the
Benter / structural-blindness arc.
**Fired:** live

**Verdict (scoped):** On the live leg at manifest `2026-08-25T02:36:02Z` (258 stories / 285
members, 4 canonical contexts, 1140 rows), the pre-registered set equality
`{(C,Ctx) : classify_from_restricted ≠ dr_type/3} = {(C,Ctx) : gauge_fixed = true}` is **FALSE
globally and FALSE within the mountain/snare stratum where it is evaluable at all** — 255 of 261
both-real stratum rows sit in the dissent cell `agree ∧ fixed=true`, and only 6/261 (2.3%) land
on the confirming diagonal. **The confound runs the direction that flatters the prediction and
the direction that kills it, and both are measured, not argued:** `restricted_classify/7` is a
simplified cascade whose type vocabulary is a strict subset of `dr_type`'s (it cannot emit
`scaffold`, `tangled_rope` or `naturalized`, forcing 343/1140 rows to disagree or abstain for
vocabulary reasons alone — *manufactured disagreement*), while its mountain and snare gates use
literals numerically **identical** to the config params the real cascade reads, which forces
agreement at exactly the two types `gauge_fixed` fires on — *manufactured agreement*. The
prediction was pitting an instrument against a near-copy of itself in the only region where it
could be scored.

**Manifest cite:** `outputs/pipeline_output.json` — `pipeline_run_at 2026-08-25T02:36:02Z`,
`n_stories 258`, `n_constraints 285`, `schema_version 3`. Loader census at probe time agreed
exactly: `258 stories, 27 non-story, 0 other`.

---

## 1. The load-bearing check first: `gauge_fixed/3`'s firing domain

The plan named three outcomes and only one was a stop. **Outcome 1 was observed: the firing
domain is EXACTLY `dr_type ∈ {mountain, snare}`** — so the global equality is false by
construction and the readable claim is its mountain/snare restriction. Stratification applies.

This was established two ways, and the second is the witness:

- **Code read** — `dirac_classification.pl:210-219`, clause 1 of `gauge_fixed/3` contains
  `member(LocalType, [mountain, snare])`. Clause 2 (`:220`) and the catch-all (`:222`) bind
  `false`. The predicate therefore **never fails**; the `failed` token in the probe's agreement
  column is unreachable for this observable, and `stratum_failed` is 0/1140 as expected.
- **Computed** — the probe asserts it at runtime rather than trusting the read:
  `[probe] local dr_type distribution over true rows: [mountain-8,snare-249]`,
  `[probe] FIRING DOMAIN: every true row is mountain|snare -> EXACTLY {mountain,snare}`,
  over all 257 `fixed=true` rows.

Row-count reconciliation, derived from the artifact and not the loop:
`[probe] rows_written=1140 expected=1140` (285 members × 4 contexts). No mismatch to sign.

The probe's context set is obtained by calling `constraint_indexing:site_contexts/1` — *the same
predicate* `gauge_fixed/3` consults at `dirac_classification.pl:215`, not a second list built to
match. `config:param(site_mode, canonical)` (`config.pl:614`) → the 4 canonical contexts.

## 2. The table

`n` stated plainly, no floors — the reader judges sufficiency from `n`.

**Global (all 1140 rows).** Reported for completeness only; false by construction, as §1 predicts.

|                        | fixed=true | fixed=false |
|---|---:|---:|
| disagree               | **2**  | 165 |
| agree                  | 255 | 109 |
| stratum_indeterminate  | 0   | 372 |
| stratum_unknown        | 0   | 237 |
| stratum_failed         | 0   | 0   |

**Stratum — local `dr_type ∈ {mountain, snare}`. n = 261 rows, over 110 distinct constraints;
all 261 are both-real (no unknown, no indeterminate, no failure).**

|                        | fixed=true | fixed=false |
|---|---:|---:|
| disagree               | **2** (a) *confirming* | **0** (c) *dissent* |
| agree                  | **255** (d) *dissent*  | **4** *confirming* |

**Prediction holds on 6/261 = 2.3% of the stratum.** The dissent cell (d) is not a residue; it is
where 97.7% of the evaluable mass sits.

**Complement stratum (n = 879).** `disagree ∧ ¬fixed` by construction — 165 disagree, 105 agree,
372 indeterminate, 237 unknown, and **0 rows with `fixed=true`**, which is the §1 firing domain
restated from the data. Structural residue, never falsification.

## 3. The confound, named in both directions

The plan required the verdict line to name the confound "whichever way the result falls." It
falls both ways at once, and each direction is measured.

**Manufactured disagreement — vocabulary.** `restricted_classify/7`
(`constraint_indexing.pl:962-975`) emits exactly 5 tokens; the observed restricted vocabulary is
`{indeterminate, mountain, piton, rope, snare}` and the observed `dr_type` vocabulary is
`{mountain, naturalized, piton, rope, scaffold, snare, tangled_rope, unknown}`. **`scaffold`,
`tangled_rope` and `naturalized` are unreachable from the restricted side**, so for the 343/1140
rows carrying those types agreement is *impossible by construction* — 133 of them scored
`disagree`, 210 abstained as `indeterminate`. The single largest disagreement class in the whole
run is `dr=scaffold → restricted=rope`, 105 rows: not an epistemic-restriction effect, a missing
branch.

**Manufactured agreement — gate identity.** The restricted cascade's thresholds are hardcoded
literals. Measured against the config params the real cascade reads (`fixture_run.pl`, REASON 2):

| param | config | restricted literal | |
|---|---|---|---|
| `mountain_suppression_ceiling` | 0.05 | 0.05 | identical |
| `mountain_extractiveness_max`  | 0.25 | 0.25 | identical |
| `snare_chi_floor`              | 0.66 | 0.66 | identical |
| `snare_epsilon_floor`          | 0.46 | 0.46 | identical |
| `snare_suppression_floor`      | 0.60 | 0.60 | identical |
| `rope_chi_ceiling`             | 0.35 | 0.35 | identical |
| `rope_epsilon_ceiling`         | 0.45 | 0.45 | identical |

7/7 identical. Consequence, visible in the data: **all 253 snare rows in the stratum agree —
253/253, no exceptions.** At the powerless seat the restricted view substitutes χ for both ε and
suppression, and χ ≥ 0.66 (the snare floor) implies χ ≥ 0.46 and χ ≥ 0.60, so the restricted snare
gate cannot fail where the real one passed. At every other seat the restricted ε and suppression
*are the true values* (see §4), so the two gates are the same arithmetic on the same numbers. The
only stratum disagreements available are therefore mountain-shaped — and there are 8 mountain
rows in the corpus.

**The restriction is much weaker than the accessibility table implies.** Two mechanisms, both in
`constraint_indexing.pl`:

- **`partial` is implemented identically to `full`.** `restrict_by_access(full, …)` and
  `restrict_by_access(partial, …)` (`:899-902`) have byte-identical bodies — both return
  `get_true_metric/3`. Every `partial` cell in `feature_access/3` is therefore a label with no
  effect. Only `none` (→ `unknown`, then resolved to the χ fallback) and `felt_only` (→ χ proxy)
  degrade anything, which is why real restriction exists **only at the `powerless` seat**.
- **The `beneficiaries` column reaches nothing.** `classify_from_restricted/3` (`:947-953`) binds
  the restricted view's beneficiary slot to `_KnownBen` and never uses it. `restrict_beneficiaries/3` (`:921-931`)
  and its three access clauses compute a value that no classification path reads. (Pattern-1
  shape; flagged, not fixed — see §7.)

## 4. Disagreement is NOT monotone in the accessibility gradient

Among both-real rows, by seat:

| seat | disagree / both-real | rate |
|---|---:|---:|
| powerless     | 59 / 120 | 49.2% |
| moderate      | 32 / 157 | 20.4% |
| **institutional** | **61 / 113** | **54.0%** |
| analytical    | 15 / 141 | 10.6% |

**The institutional seat disagrees with the full classification more often than the powerless
seat does**, despite holding `full` access on five of six features. This is not an epistemic
result — it is mostly §3's vocabulary confound landing differently per seat — but it is a direct,
measured caution for anything that wants to read `feature_access/3` as an ordered power scale.
Stated at its altitude: **this measures `classify_from_restricted` disagreement per seat, not
observer capacity.**

## 5. Cell (c): attempted, declined 16/16, and the emptiness is structural

Cell (c) — `mountain/snare ∧ disagree ∧ fixed=false` — is **empty in the live corpus (0/261)**.
Four adversarial fixtures were built in a scratch overlay corpus
(`fixtures/`, absolute `corpus_path` via `asserta`, never `prolog/testsets/`) and run through the
**same `emit_row/5` code path** as the live leg. The overlay was witnessed as having taken effect
before loading (`[fixture] corpus_path in effect: …/fixtures`, `loaded members: 4`), so a silent
fall-back to the default leg is excluded.

`[fixture] CELL (c) hits: 0` — declined by every fixture. The refusal comes with the observed
`gauge_fixed` semantics attached, as the plan required, and it is a **substrate structure, not a
plan defect and not corpus sparsity**. Two independent closures, each measured:

- **The mountain route is closed by the immutability table.** Real `mountain` requires
  `effective_immutability_for_context(Ctx, mountain)`. Measured over the canonical site
  (`fixture_run.pl`, REASON 1): ctx1 powerless (biographical,trapped) **yes**; ctx2 moderate
  (biographical,mobile) **no**; ctx3 institutional (generational,arbitrage) **no**; ctx4
  analytical (civilizational,analytical) **yes**. So no constraint can be `mountain` at ctx2 or
  ctx3, some other context always differs, and **`dr_type = mountain ⟹ gauge_fixed = true` is a
  theorem of the canonical site**, not a corpus fact. Confirmed in the data: all 8 mountain rows
  are `fixed=true`, and all 8 sit at powerless or analytical only (4 + 4).
- **The snare route is closed by gate identity** (§3): real snare ⟹ restricted snare, so
  `disagree` is unreachable. 253/253 live rows plus 3/3 fixture rows.

**The fixtures also delivered a two-sided discrimination pair on the one real mechanism, for
free.** They were built as cell-(c) attempts and landed as an (a) fire/decline pair differing in
one authored number:

| fixture | ε | supp | powerless `dr_type` | powerless restricted | agreement |
|---|---:|---:|---|---|---|
| `fx_c_mountain_edge`    | 0.20 | 0.04 | mountain | **rope**     | **disagree** ← fires |
| `fx_c_mountain_uniform` | 0.03 | 0.01 | mountain | **mountain** | **agree** ← declines |

Same code path, same file shape, one input moved. This is the mechanism of cell (a) isolated: at
the powerless seat, suppression is `felt_only` and is replaced by the χ proxy, so a constraint
whose *true* suppression clears the 0.05 mountain ceiling can have a *proxy* suppression that does
not — and the restricted view drops to `rope`. That is the one place in this instrument where the
restricted view genuinely models an observer's epistemic position, and the corpus contains exactly
2 rows of it.

**No fixtures were built for cells (a) and (d): they occur NATURALLY, with natural declines**
(cell (a): `alpha_m_supercriticality_kernel_flat_control` and `protein_anabolic_resistance`, both
at powerless; the decline: the other 6 mountain rows, which agree). A natural instance outranks an
authored decoy (`build_discipline.md` → *A positive control demonstrates DISCRIMINATION, not
detection*), so building them would have been strictly worse evidence. The plan's worry — that a
CONFIRMED verdict would rest on an instrument only ever shown able to agree — is inverted here:
the verdict is REFUTED, and the instrument is witnessed landing in **both** confirming cells (2
and 4 rows) as well as the dissent cell (255), so the refutation is not an artifact of an
instrument that can only disagree.

## 6. This is not a dead surface — the gap has three live consumers

`classify_from_restricted/3` is read by `abductive_triggers.pl:885`
(`trigger_epistemic_trap/3`), `diagnostic_summary.pl:313` (`probe_context_gap/4`), and
`quantum_verification_report.pl:345`. (`observer_accessible/3` has **no** consumer outside its own
module.) So §3's confounds are consumer-visible, not academic.

Two consequences worth carrying forward:

- **`indeterminate` is a non-answer, and the substrate already says so.** `trigger_epistemic_trap/3`
  requires `RestrictedType \= indeterminate`; `probe_context_gap/4` maps `indeterminate → agrees`.
  The probe therefore scores it as its own stratum (`stratum_indeterminate`, 372 rows) rather than
  as `disagree`. **This is a deliberate deviation from the plan's four agreement atoms**, forced by
  the substrate and anchored in how both live consumers read the token; scoring it `disagree`
  would have let a non-answer satisfy the prediction. The stratum table in §2 is unaffected (all
  261 stratum rows are both-real).
- **`trigger_epistemic_trap/3`'s metric gate is satisfied on 107/285 constraints — and 48 of those
  have `dr_type = unknown`.** In those 48 the *full-data* classification did not happen at all; the
  restricted classifier answered and the trigger reads the difference as "a powerless observer
  classifies this differently than the full-data classification." An absence presenting as a
  presence. Scoped: 107 is the **metric-gate** set, not the emitted-hypothesis set — the trigger
  also applies `compute_confidence/3` against `abductive_confidence_floor`, which this run did not
  evaluate.

## 7. Incidental finding: OQ-205's fix landed on one of three ingest sites

`get_true_metric/3` (`constraint_indexing.pl:906-918`) carries the comment *"OQ-205 (spec §3):
absence of an authored ε reads `unknown`, never a fabricated 0.0 (a mountain-shaped ε that passes
every floor)"* — directly above two sibling clauses that still fabricate `0.0`:

```prolog
get_true_metric(C, extractiveness,   Val) :- (… -> true ; Val = unknown).   % OQ-205 U1: fixed
get_true_metric(C, suppression_raw,  Val) :- (… -> true ; Val = 0.0).       % still fabricates
get_true_metric(C, theater,          Val) :- (… -> true ; Val = 0.0).       % still fabricates
```

The real path's counterpart, `drl_core:get_raw_suppression/2`, returns `unknown` on absence, and
`classify_from_metrics/6`'s first clause fails closed on a non-number `Supp`. So on a story with
no authored `suppression_requirement` the real classifier refuses and the restricted one proceeds
on a fabricated `0.0` that clears the mountain suppression ceiling. Measured side by side on the
27 members that lack the fact: `drl_core=unknown` vs `constraint_indexing=0.0`, 27/27.

**Prior art: OQ-205 (RESOLVED 2026-07-03), unit U1** — *"both §3 fabrication fallbacks fixed in
own output-changing commits with all-four-corpora byte-identical witnesses (U1 `get_true_metric`
0.0 …)"*. This is therefore not a re-discovery of the pattern but a **partial-fix finding against
a closed OQ**: the value-class ruling was applied at the clause it was written at, not enumerated
across the predicate's ingest sites — precisely the failure CLAUDE.md warns about under *A guard
sweep's find-criterion must model REACHABILITY*. `build_discipline.md` carries no entry for
`get_true_metric`, `classify_from_restricted`, `restricted_classify` or `gauge_fixed`; ISSUES.md
carries none of the four either.

**Exposure, measured on two legs: ZERO stories.** Live leg — 27/285 members lack authored
suppression, all 27 are `*_contradictions.pl` meta-files, 0 stories. `testsets_kimi` (1005
stories) — 0 members lack suppression, theater, or base extractiveness. So the defect is **latent,
not live**: its trigger is a story missing `suppression_requirement`, which the generation pipeline
does not produce. Reachable by hand-authoring or by a repair path that strips the field.

**Not fixed here.** It is an engine behavior change, which Phase A's no-engine-change rail
forbids; it is routed, not patched.

## 8. Evidence map

| artifact | what it is |
|---|---|
| `gauge_fixed_prediction_probe.pl` | the probe. Emits one row per (constraint, context) pair unconditionally; every observable called output-unbound inside `once/1` (Pattern 7); failure binds `failed`, kept distinct from `unknown`; runtime firing-domain assertion; row count reconciled against the written file. |
| `rows.tsv` | 1140 rows, live leg. Every number in §§2-4, 6 is derived from this file. |
| `analysis.txt` | the §2 tables as produced. |
| `analysis_detail.txt` | §4 per-seat rates, §5 mountain-row enumeration, §3 vocabulary counts, §6 trigger set. |
| `fixture_run.pl` | the cell-(c) attempt runner. Overlay witnessed before load; REASON 1 (immutability per canonical context) and REASON 2 (gate-constant comparison) computed, not read off the source. |
| `fixtures/*.pl` | 4 scratch fixtures. Never in `prolog/testsets/`. |
| `fixture_rows.tsv` | 16 rows, same columns, same code path. |
| `audit_log.md` | HEAD stamp pair (identical), run order, corpus stamp, and why there is no `PREREGISTRATION.md`. |

## 9. What this does and does not license

- **Licensed:** the comment's prediction, as written, is false on this corpus at this code state;
  the mountain/snare restriction of it is also false, and by a wide margin; the emptiness of cell
  (c) is structural at the canonical site rather than a sparsity artifact; the disagreement signal
  is dominated by a type-vocabulary mismatch rather than by epistemic restriction.
- **Not licensed:** any claim that the restricted-view *idea* is wrong. The one place the
  restriction genuinely bites — the powerless seat's χ-for-suppression proxy — produces exactly
  the effect the comment describes, in 2 corpus rows and in a constructed fire/decline pair. The
  finding is that the instrument is currently too coarse to test the idea, not that the idea failed.
- **Not licensed:** reading §4 as a statement about observer capacity, or the 107 in §6 as
  emitted hypotheses. Both are scoped in place.
- **Unmeasured:** everything outside the live leg and the single `testsets_kimi` exposure check.
  This is a one-leg result at one manifest; `site_mode` other than `canonical` was not exercised.
