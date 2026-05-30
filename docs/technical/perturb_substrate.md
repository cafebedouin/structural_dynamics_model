# perturb.py — Substrate Gotchas and Classification Probing Notes

Generated 2026-05-29 during Step 2/3 implementation. Documents three failure modes that
each caused a session-critical wrong result. Scope: things that look plausible from the
code but are wrong in practice.

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
