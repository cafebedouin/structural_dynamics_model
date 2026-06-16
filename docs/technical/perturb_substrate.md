# perturb.py — Substrate Gotchas and Classification Probing Notes

Generated 2026-05-29 during Step 2/3 implementation. Documents three failure modes that
each caused a session-critical wrong result. Scope: things that look plausible from the
code but are wrong in practice.

> **Companion:** the same overlay method can drive the **commentary census** as the
> measurement surface instead of the chi export — see `census_perturbation_sweep.md`
> (`python/sweeps/census_sweep.py`), which also documents the rate-vs-domain denominator
> trap that the census's coverage/prevalence/domain split exists to catch.

---

## 1. Corpus loading and CWD dependency — probes return wrong results without it

**The mistake:** calling Prolog predicates in a subprocess or direct query without
explicitly loading the testset corpus. Produces plausible-looking but wrong results.

**Concrete failure:** probing `constraint_signature(welfare_reading, Sig)` returned
`constructed_high_extraction`. The constraint's actual canonical signature with corpus
loaded is `false_natural_law`. The wrong result was used to reason that chi-floor
perturbation should produce final-type flips on welfare_reading — it does not.

**Why it happens:** `[stack]` does NOT load testsets. `corpus_loader` uses
`expand_file_name('testsets/*.pl', Files)` which resolves relative to the Prolog
process's CWD. Running from project root (not `prolog/`) → CWD wrong → glob finds 0
files → 0 testsets → `corpus_loaded` never asserted. Prolog predicates that require
testset facts (`false_natural_law/2`, `narrative_ontology:constraint_metric/3`, etc.)
then fail or fall through to weaker fallback clauses.

`false_natural_law` detection depends on Boltzmann compliance analysis, which requires
the corpus facts. Without corpus: `false_natural_law(C, _)` fails → cascade falls through
to `constructed_high_extraction` (computed from simpler metric thresholds that don't
need corpus facts). The signature LOOKS correct; it is not.

**Check:** after `[stack]`, always verify:
```prolog
findall(C, narrative_ontology:constraint_metric(C, extractiveness, _), Cs),
length(Cs, N), format('corpus metrics: ~w~n', [N]).
```
If N = 0, the corpus is not loaded. Target N ≈ 191 (current product site count) or 223
(total testset count, depending on predicate).

**Correct probe pattern (from Python):**
```python
overlay = """
:- use_module(config).
:- [stack].
:- corpus_loader:load_all_testsets.    % REQUIRED — stack alone does not load corpus
:- signature_detection:constraint_signature(welfare_reading, Sig),
   format('sig: ~w~n', [Sig]).
:- halt.
"""
# Run via subprocess with cwd=PROLOG_DIR, not project root
```

Or use `product_site_export:run_product_export_to/1` which calls
`corpus_loader:load_all_testsets` internally — but only when run as the overlay entry
point, not when `[stack]` is consulted first and `run_product_export_to` is called as
a predicate in the same session without the prior load.

**In perturb.py:** the A1 overlay template (`_OVERLAY_TMPL`) loads `[stack]` and then
calls `run_product_export_to`. This works because `run_product_export_to` calls
`corpus_loader:load_all_testsets` at its start. The `chi_data` loading
(`_load_chi_data`) reads from `pipeline_output.json` (Python file read, no Prolog) — no
corpus loading needed there. `_load_kernel_map` reads `.pl` files via Python regex — also
no Prolog. The only corpus-dependent path is the swipl subprocess.

---

## 2. `constraint_signature/2` bound-argument bypass — wrong counts

**The mistake:** calling `constraint_signature(C, false_ci_rope)` with the second
argument bound to a specific atom.

**Concrete failure:** a query for "all kernel-linked constraints with canonical signature
false_ci_rope" returned 97 results. The correct count is 17.

**Why it happens:** `constraint_signature/2` is defined with cuts:
```prolog
constraint_signature(C, false_natural_law) :-
    false_natural_law(C, _), !.          % CUT fires if this clause succeeds
constraint_signature(C, false_ci_rope) :-
    false_ci_rope(C, _), !.
...
```

When called as `constraint_signature(welfare_reading, false_ci_rope)` (second arg
BOUND), the first clause head `constraint_signature(C, false_natural_law)` fails head
unification (`false_natural_law ≠ false_ci_rope`) — so the cut in that clause NEVER
FIRES. Execution moves directly to the second clause, which succeeds if
`false_ci_rope(welfare_reading, _)` holds. A constraint can satisfy BOTH
`false_natural_law` and `false_ci_rope` predicates; calling with bound second arg finds
ALL that satisfy `false_ci_rope`, regardless of which signature is canonical.

**Correct call** — let Prolog determine the first-matching (canonical) signature, then
filter:
```prolog
% WRONG — bound arg bypasses cut, finds all that satisfy false_ci_rope predicate
findall(C, (
    narrative_ontology:cs_kernel_id(C, _),
    signature_detection:constraint_signature(C, false_ci_rope)
), Cs)

% CORRECT — unbound call, then filter on canonical result
findall(C, (
    narrative_ontology:cs_kernel_id(C, _),
    signature_detection:constraint_signature(C, Sig),
    Sig = false_ci_rope
), Cs)
```

The difference is load-bearing: `welfare_reading` and `birth_threshold_reading` appear
in the wrong version (they satisfy `false_ci_rope` predicate but their canonical
signature is `false_natural_law`). Treating them as canonical `false_ci_rope` leads to
wrong conclusions about what params will produce final-type flips.

---

## 3. Chi-floor params and signature override — "touched-but-stable" is not stability evidence

**The pattern:** a `perturb()` run on a chi-floor param (e.g. `tangled_rope_chi_floor`)
returns `fold_survival=1.0` with `coverage>0` ("touched-but-stable") for kernel-linked
constraints. This LOOKS like genuine stability — the param is on the decision path and
the type doesn't change. It is not. It is signature-locking.

**The mechanism:**

1. `tangled_rope_chi_floor` raised above moderate chi (0.4038) → `classify_from_metrics`
   returns `naturalized` for moderate contexts
2. `drl_core:dr_type` calls `integrate_signature_with_modal(C, naturalized, FinalType)`
3. `constraint_signature(C, Sig)` with corpus loaded → returns `false_natural_law` for
   76/97 kernel-linked readings
4. `resolve_modal_signature_conflict(_, false_natural_law, tangled_rope) :- !.` —
   unconditional tangled_rope regardless of metric result
5. Final type: tangled_rope. No flip. fold_survival=1.0.

`coverage>0` IS correctly non-zero (the chi-floor param genuinely enters the metric
decision path for moderate contexts, as confirmed by zone inference). Coverage measures
"param reached metric decision boundary" not "param reached final-type boundary." These
are different when signatures override metrics.

**Implication for perturb use:** chi-floor and chi-ceiling params CANNOT produce
final-type flips on false_natural_law-canonical readings (76/97 kernel-linked). They
can on false_ci_rope-canonical readings (17/97) IF raising the floor creates metric
perspectival variance that engages the `has_metric_perspectival_variance` gate. And on
`constructed_high_extraction` readings (1 confirmed: `vulnerability_protection_reading`),
which preserve the metric type for non-unknown results.

**The params that DO produce final-type flips** (confirmed by witness runs): epsilon
params that interact with the naturalized/snare classification boundary. `snare_epsilon_floor`
on `end_of_life_decision_authority` (vulnerability_protection_reading has
`constructed_high_extraction` canonical signature) produces fold_survival=0.917, 39
flips at +8.7% displacement.

**Canonical signature distribution (current corpus, kernel-linked readings):**
- `false_natural_law`: 76 — unconditional tangled_rope lock. Chi params: touched-but-stable.
- `false_ci_rope`: 17 — conditional. Chi params: may produce flips if variance gate opens.
- `coupling_invariant_rope`: 3 — unconditional rope lock.
- `constructed_high_extraction`: 1 (`vulnerability_protection_reading`) — preserves metric type.
- `constructed_low_extraction`: 1 — preserves metric type for non-unknown.

**Check the signature before interpreting a perturb result:**
```python
# Before using a perturb result as stability evidence, confirm:
# (a) coverage > 0  — param on metric decision path
# (b) fold_survival < 1.0  — final type actually changed
# Only (a)+(b) together constitute a witnessed governing-param pair.
# (a) alone (touched-but-stable) is signature-locking, not stability.
```

The `_WITNESSED_PARAMS` dict in `python/enhanced_report.py` enforces this: only
(coverage>0 AND fold_survival<1.0) pairs are included. See OQ-30 for the witness backlog.

---

## 4. Product site coverage: 191 of 223 constraints

`run_product_export` loads all 223 testsets but exports only 191 to
`outputs/product_site_orbits.json`. The 32 excluded constraints fail some predicate
in the product site chain (probably missing `constraint_metric/3` or other required
facts). As of 2026-05-29, all 32 non-exported constraints are non-kernel-linked — they
don't appear in `cs_kernel_id/2` facts. Perturb and the demotion pass are unaffected.

If the count drops further (export < 191), inspect the corpus for testsets missing
`narrative_ontology:constraint_metric(C, extractiveness, E)` — `base_extractiveness`
fails without it, and `metric_based_type_indexed` fails, and `dr_type` returns `unknown`,
which may cause the product site to silently omit the entry.

---

## 5. PRH/EM overlay pattern: abolish+reassert, not retract/asserta

**The mistake:** extending the config-param overlay pattern (retract/asserta on
`config:param/2`) to `power_role_heuristic/4` and `exit_modulation/2`.

**Why it fails:** these predicates have `_` wildcards in clause heads:
```prolog
power_role_heuristic(powerless, _, true,  0.85).
power_role_heuristic(powerless, _, false, 0.90).
```
`retract(constraint_indexing:power_role_heuristic(powerless, _, true, _))` succeeds but
then `assertz` with a specific value leaves the old `_, false` clause intact. In SWI-Prolog,
`retract/1` with wildcards in the head retract the FIRST matching clause, not all; the
remaining clauses stay. The predicate ends up with a mixture of old and new values.

**Correct pattern:** abolish the entire predicate and reassert all clauses, with one value
changed:
```prolog
:- use_module(constraint_indexing).
:- abolish(constraint_indexing:power_role_heuristic/4).
:- assertz(constraint_indexing:power_role_heuristic(powerless, _, true,  0.77)).  % perturbed
:- assertz(constraint_indexing:power_role_heuristic(powerless, _, false, 0.90)).  % unchanged
:- assertz(constraint_indexing:power_role_heuristic(moderate,  _, true,  0.70)).  % unchanged
...
```
The `_build_prh_overlay` and `_build_em_overlay` functions in `python/sweeps/perturb.py`
implement this correctly. Check those before writing a new overlay.

**Prerequisite:** `constraint_indexing.pl` must declare the predicate dynamic *before* the
clause definitions:
```prolog
:- dynamic power_role_heuristic/4.
:- dynamic exit_modulation/2.
```
Without this, `abolish` on a static predicate in SWI-Prolog warns or fails. Both
declarations were added at lines 67–68 (2026-05-29).

**`positional_displacement/2` is different:** it has distinct heads (one per power level,
no wildcards), so the simple `retract(constraint_indexing:positional_displacement(powerless, _))`
pattern works. It was already declared `:- dynamic` at line 66.

---

## 6. `x or default` in Python — 0 is falsy, so `0 or 99` = 99

**The mistake:** using `r.get("priority") or 99` to supply a fallback when priority is
absent.

**Concrete failure:** epsilon params have `priority=0` in `demotion_pass.py` (lower =
higher priority, so 0 = highest priority). The sort key
`r.get("priority") or 99` evaluated `0 or 99` = 99 because 0 is falsy in Python.
Epsilon params sorted LAST in the witness backlog instead of first. The ±10% float batch
swept all chi and other params before reaching epsilon params; by then the sort order
appeared normal and the bug wasn't visible until the 4 epsilon params appeared at positions
165–179.

**Fix:**
```python
# WRONG:
key = r.get("priority") or 99

# CORRECT:
key = 99 if r.get("priority") is None else r.get("priority")
```

**General rule:** never use `x or default` when `x = 0` is a valid, meaningful value.
Python's `or` short-circuits on any falsy value (0, 0.0, "", [], {}, False). Use explicit
None-check instead.

**In this repo:** the fix is in `demotion_pass.py`'s `run_demotion_pass` sort key (2026-05-30).
If you add a new priority scheme that starts at 0, use the explicit check.

---

## 7. `fcr_override_enabled=0` produces `unknown` type — load-bearing, not secondary

**The observation:** disabling FCR override (`fcr_override_enabled=0`) causes `dr_type/3`
to return `unknown` for some contexts in multiple kernels (latin_correctness, sovereign_legitimacy,
vaccine_mandate_balance, nuclear_impossibility_kernel). These contexts had type
`tangled_rope` at baseline.

**What this means:** `fcr_override_enabled` is not a secondary reporting feature. It is
on the `dr_type/3` classification path. When disabled, some classification chains fail
entirely (no clause succeeds → `dr_type` falls through to `unknown`). The FCR override
changes which signature predicates fire, which affects what `integrate_signature_with_modal`
returns. Disabling it doesn't just suppress a label — it breaks the chain for affected
constraints.

**Implication for code edits:** if you modify `fcr_override_enabled` logic, `cs_pattern_detection.pl`,
or `drl_core.pl`'s signature integration, run a perturb check at `fcr_override_enabled=0`
and watch for `unknown` counts. A regression shows as an increase in `unknown` types in the
product site export. Current witness: `latin_correctness` kernel, cov=0.333, 156 flips at
val=0 (`outputs/witness_backlog_integer_results.json`).

**This is NOT the same as the product-site coverage issue (§4).** Section 4 documents
constraints absent from the export due to missing `constraint_metric`. This is constraints
present in the export with `unknown` type due to classification chain failure. They are
different bugs with different checks.

---

## 8. Three perturbable surfaces — boltzmann_floor_override is not on the static-type path

**The mistake:** assuming any schema numeric field that looks like a classification parameter
will produce `fold_survival < 1.0` in a perturb() run.

**Concrete case:** `boltzmann_floor_override` appears in testset files as
`narrative_ontology:boltzmann_floor_override(constraint_id, 0.12)` and is a numeric field
in the schema. It looks like it belongs in `_WITNESSED_PARAMS`. It does not.

**Why:** `boltzmann_floor_override` feeds `boltzmann_floor_for/2` →
`excess_extraction/2` → `boltzmann_compliance.pl`. None of these predicates are called
from `dr_type/3` or `classify_from_metrics/6`. They are a separate observable: the
Price of Anarchy "extractive overhead" (ExcessEps = ε − floor). Grep confirms:
```
grep -n "boltzmann_compliance\|boltzmann_floor" prolog/drl_core.pl prolog/product_site_export.pl
→ EXIT 1 (no matches)
```

**The three surfaces:**
1. **Static type** (exported by `product_site_export`): fed by `dr_type/3` ←
   `classify_from_metrics/6`. The 191 engine params + 6 authored data fields are on this path.
   Perturb() fold_survival measures this surface.
2. **Excess extraction / PoA** (`boltzmann_compliance.pl:excess_extraction/2`): fed by
   `boltzmann_floor_override` and `boltzmann_floor_*` config params. Not called from
   `dr_type/3`. Perturb() will always return fold_survival=1.0 (inert) for these.
3. **Temporal / drift** (`classify_at_time/4` in `drl_composition.pl`): fed by
   `narrative_ontology:measurement/5` (time-series ε values) and `interval/3`. Not called
   by `product_site_export:type_at/3`. Perturb() will always return fold_survival=1.0
   for `Measurement.value` / `interval.*` because the export doesn't use the time-indexed
   path.

**Check before adding a param to `_WITNESSED_PARAMS`:** grep that param's predicate name
against `prolog/drl_core.pl` and `prolog/product_site_export.pl`. If both greps return
empty, the param is on a different surface and will produce fold_survival=1.0 in every perturb run.

---

## 9. Integer params in config.pl — config_schema.pl is the type authority

**The mistake:** running the ±10% float sweep against params declared as integer-typed in
`config_schema.pl`. Produces config_schema rejection before classification runs.

**Concrete failure:** `perturb("abductive_enabled", [0.9, 1.0, 1.1])` →
```
ERROR: CONFIG ERROR: param(abductive_enabled, 0.9) has wrong type (expected integer)
```
The overlay writes `param(abductive_enabled, 0.9)` as a Prolog float; `config_schema.pl`
validates after corpus load and halts with error. No export produced. Batch records
`{"error": "export_failed"}` — this is ERRORED_UNTESTED, not inert.

**How to identify integer params:**
```bash
grep "^\s*param(" prolog/config.pl | grep -vE ",\s*-?[0-9]+\." | grep -E ",\s*-?[0-9]+"
```
19 params as of 2026-05-30: enable flags (0/1) and count thresholds (2–20). The authoritative
type and range declarations are in `prolog/config_schema.pl` → `param_spec/4`.

**Secondary schema constraint:** enable flags have `oneof([0,1])` in config_schema.pl.
Integer step val+1=2 also rejects:
```
ERROR: CONFIG ERROR: param(abductive_enabled, 2) violates constraint oneof([0,1])
```
The valid perturbation domain for enable flags is `[0, 1]`; for count thresholds `[val-1, val, val+1]`.

**Correct sweep:** use `python3 python/sweeps/witness_backlog.py --integer-only`. Results
in `outputs/witness_backlog_integer_results.json`. Three survivors confirmed 2026-05-30:
`boltzmann_min_classifications`, `critical_mass_threshold`, `fcr_override_enabled`.
Remaining 16 witnessed inert (coverage=0 at all valid integer values).
