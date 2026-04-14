# Blocking Gate Audit — 2026-04-14

**Scope**: All predicates that issue classification certificates or type overrides in the DR Prolog engine.
**Principle under audit**: Tests are evidence, not verdicts. Classification must not be blocked by test failure.
**Reference**: The known violation (`coupling_invariant_rope/2`) was fixed — `excess_extraction` demoted from gate to diagnostic payload; see signature_detection.pl:910-913 comment.
**Note**: `docs/metrics_as_routing.md` referenced in the audit prompt does not exist. The principle is stated in the prompt itself and in the CI_Rope fix comment.

---

## PASS 1 — EVIDENCE COLLECTION (NO ANALYSIS)

### 1.1 Predicates that issue classification certificates or type overrides

These are predicates whose output reaches `constraint_signature/2` → `integrate_signature_with_modal/3` → `resolve_modal_signature_conflict/3` → `dr_type/3`.

**Call chain** (drl_core.pl:398-412 → signature_detection.pl:643-673):
```prolog
% drl_core.pl:398-412
dr_type(C, Context, Type) :-
    constraint_indexing:valid_context(Context),
    metric_based_type_indexed(C, Context, MetricType),
    signature_detection:integrate_signature_with_modal(C, MetricType, FinalType),
    !,
    Type = FinalType.

% signature_detection.pl:643-645
integrate_signature_with_modal(C, ModalType, AdjustedType) :-
    constraint_signature(C, Signature),
    resolve_with_perspectival_check(C, ModalType, Signature, AdjustedType).
```

**Certificate predicates** (feed `constraint_signature/2` → override pipeline):

#### A. `coupling_invariant_rope/2` — signature_detection.pl:897-917
```prolog
coupling_invariant_rope(C, ci_rope_evidence(Compliance, ScopeResult,
                                             ExcessEps, true)) :-
    % Must be Boltzmann-compliant
    boltzmann_compliant(C, Compliance),
    Compliance = compliant(_),

    % Must be scope-invariant
    scope_invariance_test(C, ScopeResult),
    ScopeResult = invariant,

    % Must have a coordination function
    narrative_ontology:has_coordination_function(C),

    % Collect excess extraction as diagnostic evidence (not a gate).
    % The floor override in boltzmann_floor_for/2 is editorial data,
    % not a classification input — gating here allowed overrides to
    % suppress CI_Rope certification on genuinely coordinating constraints.
    (   excess_extraction(C, ExcessEps)
    ->  true
    ;   ExcessEps = 0.0
    ).
```
Feeds: `constraint_signature/2:105-106` → `resolve_modal_signature_conflict/3:725` → unconditional `rope`.

**Flagged conditions**: none. All gates are structural (`boltzmann_compliant`, `scope_invariance_test`, `has_coordination_function`). `excess_extraction` is soft collection only — the `-> true` branch accepts any value without testing it.

---

#### B. `false_natural_law/2` — signature_detection.pl:829-847
```prolog
false_natural_law(C, fnl_evidence(Claim, BoltzmannResult, CouplingScore,
                                   CoupledPairs, ExcessExtraction)) :-
    % Must claim to be natural/mountain
    claimed_natural(C, Claim),

    % Must fail Boltzmann compliance
    boltzmann_compliant(C, BoltzmannResult),
    BoltzmannResult = non_compliant(_, _),

    % Gather diagnostic evidence
    cross_index_coupling(C, CouplingScore),
    (   detect_nonsensical_coupling(C, CoupledPairs, _)
    ->  true
    ;   CoupledPairs = []
    ),
    (   excess_extraction(C, ExcessExtraction)
    ->  true
    ;   ExcessExtraction = unknown
    ).
```
Feeds: `constraint_signature/2` → `resolve_modal_signature_conflict/3:721` → unconditional `tangled_rope`.

**Flagged conditions**: none. Gates are structural — `claimed_natural/2` checks claim data; `boltzmann_compliant` gates on `CouplingScore =< Threshold` (see §1.3), not on excess. `excess_extraction` at line 844 is soft collection only.

---

#### C. `false_ci_rope/2` — signature_detection.pl:1046-1074
```prolog
false_ci_rope(C, fcr_evidence(AppearanceType, FailedTests, CouplingScore,
                               ExcessExtraction, ScopeResult)) :-
    % Must appear to be a rope from metrics
    appears_as_rope(C, AppearanceType),

    % Must fail at least one Boltzmann structural test
    collect_fcr_failures(C, FailedTests),
    FailedTests \= [],

    % Gather diagnostic data
    (   cross_index_coupling(C, CouplingScore)
    ->  true
    ;   CouplingScore = unknown
    ),
    (   excess_extraction(C, ExcessExtraction)         % LINE 1060: soft collection
    ->  true
    ;   ExcessExtraction = unknown
    ),
    (   scope_invariance_test(C, ScopeResult)
    ->  true
    ;   ScopeResult = unknown
    ),

    % Zero-excess exemption: if a constraint has no extractive overhead,
    % coupling alone is insufficient evidence of coordination washing.
    % Scope-sensitive classification with zero excess is the indexical
    % system working correctly, not a sign of hidden extraction.
    % Requires at least one non-coupling failure to flag as FCR.
    \+ zero_excess_coupling_only(ExcessExtraction, FailedTests).   % LINE 1074: gate
```
Feeds: `constraint_signature/2` → `resolve_with_perspectival_check/4` → `tangled_rope` (when `fcr_override_enabled=1` and no perspectival variance).

**Flagged conditions**:
- **Line 1053**: `FailedTests \= []` — gate on the output of `collect_fcr_failures/2` (a `findall` aggregator)
- **Line 1074**: `\+ zero_excess_coupling_only(ExcessExtraction, FailedTests)` — gate inside the certificate predicate. `ExcessExtraction` was collected softly at line 1060 from `excess_extraction/2`. The `zero_excess_coupling_only/2` predicate contains a threshold comparison against this value (see §1.4).
- `ExcessExtraction` flows from `excess_extraction(C, ExcessExtraction)` at line 1060 → `boltzmann_floor_for/2` at boltzmann_compliance.pl:442 → accepts `boltzmann_floor_override/2` at boltzmann_compliance.pl:449.

---

#### D. `false_summit_mountain/2` — signature_detection.pl:1187-1212
```prolog
false_summit_mountain(C, fsm_evidence(BeneficiaryCount, CouplingScore)) :-
    % Metric profile must be consistent with mountain classification.
    drl_core:base_extractiveness(C, BaseEps),
    config:param(mountain_extractiveness_max, MaxX),
    BaseEps =< MaxX,                                   % LINE 1193: threshold gate
    drl_core:get_raw_suppression(C, Supp),
    config:param(mountain_suppression_ceiling, SuppCeil),
    Supp =< SuppCeil,                                  % LINE 1196: threshold gate
    domain_priors:emerges_naturally(C),

    % Primary gate: must have at least one identifiable beneficiary.
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Beneficiaries),
    Beneficiaries \= [],                               % LINE 1203: structural gate
    length(Beneficiaries, BeneficiaryCount),

    % Coupling as diagnostic evidence only — not a hard gate.
    (   catch(cross_index_coupling(C, CS), _, CS = 0.0)
    ->  CouplingScore = CS
    ;   CouplingScore = 0.0
    ).
```
Feeds: `constraint_signature/2` → `resolve_modal_signature_conflict/3:744-749` → `tangled_rope`.

**Flagged conditions**:
- **Line 1193**: `BaseEps =< MaxX` — threshold comparison against config param `mountain_extractiveness_max`. `BaseEps` is raw metric from `drl_core:base_extractiveness/2`, not an `excess_extraction` result.
- **Line 1196**: `Supp =< SuppCeil` — threshold comparison against config param `mountain_suppression_ceiling`. Raw metric.
- **Line 1203**: `Beneficiaries \= []` — structural data fact, not a test result.
- Coupling is explicitly diagnostic (comment: "not a hard gate").

---

#### E. `resolve_modal_signature_conflict/3` — signature_detection.pl:717-766 (verbatim key clauses)
```prolog
resolve_modal_signature_conflict(_, natural_law, Result) :- !, Result = mountain.
resolve_modal_signature_conflict(_, false_natural_law, Result) :- !, Result = tangled_rope.
resolve_modal_signature_conflict(_, coupling_invariant_rope, Result) :- !, Result = rope.
resolve_modal_signature_conflict(ModalType, false_ci_rope, Result) :-
    !,
    (   config:param(fcr_override_enabled, 1)
    ->  Result = tangled_rope
    ;   Result = ModalType
    ).
resolve_modal_signature_conflict(mountain, false_summit_mountain, Result) :-
    !,
    (   config:param(false_summit_override_target, Target)
    ->  Result = Target
    ;   Result = tangled_rope
    ).
% ... (constructed_*, unknown/*, identity fallback)
resolve_modal_signature_conflict(ModalType, _, ModalType).
```
Pure dispatch table. No threshold comparisons, no diagnostic calls. **No flagged conditions.**

---

### 1.2 Flagged conditions summary (verbatim)

| # | Predicate | File:Line | Flag Type | Verbatim |
|---|-----------|-----------|-----------|---------|
| F1 | `false_ci_rope/2` | signature_detection.pl:1074 | Gate calling predicate with internal threshold | `\+ zero_excess_coupling_only(ExcessExtraction, FailedTests)` |
| F2 | `zero_excess_coupling_only/2` | signature_detection.pl:1083 | Threshold against test result | `Excess =< 0.05` |
| F3 | `fcr_test_failure/2` | signature_detection.pl:1143 | Threshold against test result | `Excess > 0.05` (preceded by `excess_extraction(C, Excess)`) |
| F4 | `boltzmann_invariant_mountain/2` | boltzmann_compliance.pl:509 | Threshold against test result | `Excess =< 0.01` (preceded by `excess_extraction(C, Excess)`) |
| F5 | `purity_test_excess/2` | signature_detection.pl:991 | Threshold against test result | `Excess =< 0.05` (preceded by `excess_extraction(C, Excess)`) |
| F6 | `false_summit_mountain/2` | signature_detection.pl:1193 | Threshold against raw metric | `BaseEps =< MaxX` |
| F7 | `false_summit_mountain/2` | signature_detection.pl:1196 | Threshold against raw metric | `Supp =< SuppCeil` |

---

### 1.3 `boltzmann_floor_for/2` call sites

**Definition** (boltzmann_compliance.pl:445-455):
```prolog
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:boltzmann_floor_override(C, Floor), !.    % LINE 449: override priority
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_floor_param(Type, ParamName),
    config:param(ParamName, Floor), !.
boltzmann_floor_for(_, Floor) :-
    config:param(boltzmann_floor_default, Floor).
```

**`excess_extraction/2` definition** (boltzmann_compliance.pl:439-443):
```prolog
excess_extraction(C, ExcessEps) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, Eps),
    boltzmann_floor_for(C, Floor),
    ExcessEps is max(0.0, Eps - Floor).
```
`boltzmann_floor_for` is the ONLY source of the floor. A `boltzmann_floor_override/2` declaration in a testset takes first priority (line 449 cut), overriding coordination-type defaults and the global default.

**`boltzmann_compliant/2` definition** (boltzmann_compliance.pl:88-97):
```prolog
boltzmann_compliant(C, Result) :-
    (   epistemic_access_check(C, true)
    ->  cross_index_coupling(C, CouplingScore),
        complexity_adjusted_threshold(C, Threshold),
        (   CouplingScore =< Threshold
        ->  Result = compliant(CouplingScore)
        ;   Result = non_compliant(CouplingScore, Threshold)
        )
    ;   Result = inconclusive(insufficient_classifications)
    ).
```
`boltzmann_compliant/2` does NOT read `excess_extraction` or `boltzmann_floor_for`. Its gate is `CouplingScore =< Threshold` — coupling score vs complexity-adjusted threshold. **Floor override does not affect `boltzmann_compliant/2`.**

**All `excess_extraction` call sites** (from grep):

| File | Line | Predicate | Gate or Evidence? |
|------|------|-----------|------------------|
| boltzmann_compliance.pl | 129 | `boltzmann_shadow_audit/2` | Evidence only (soft collection: `-> true ; Excess = unknown`) |
| boltzmann_compliance.pl | 508 | `boltzmann_invariant_mountain/2` | **Gate**: `Excess =< 0.01` at line 509 |
| signature_detection.pl | 844 | `false_natural_law/2` | Evidence only (soft collection: `-> true ; ExcessExtraction = unknown`) |
| signature_detection.pl | 914 | `coupling_invariant_rope/2` | Evidence only (soft collection: `-> true ; ExcessEps = 0.0`) — known fix |
| signature_detection.pl | 990 | `purity_test_excess/2` | **Gate**: `Excess =< 0.05` at line 991 |
| signature_detection.pl | 1060 | `false_ci_rope/2` | Soft collection at line 1060 — **but result feeds gate at line 1074** |
| signature_detection.pl | 1142 | `fcr_test_failure/2` | **Gate**: `Excess > 0.05` at line 1143 |

---

### 1.4 Diagnostic + threshold in same clause (grep output, relevant subset)

Full grep command:
```bash
grep -n "excess_\|_compliance\|_invariance\|_score\|_check\|coupling_" \
  prolog/boltzmann_compliance.pl prolog/drl_core.pl \
  prolog/signature_detection.pl prolog/drl_composition.pl | head -80
```

Relevant matches (threshold-adjacent):

```
boltzmann_compliance.pl:508:    (   excess_extraction(C, Excess)
boltzmann_compliance.pl:510:        ->  T3 = pass(no_excess_extraction)
boltzmann_compliance.pl:511:        ;   T3 = fail(excess_extraction, Excess)

signature_detection.pl:990:    (   excess_extraction(C, Excess)
signature_detection.pl:992:        ->  Result = pass(no_excess_extraction)
signature_detection.pl:993:        ;   Result = fail(excess_extraction, Excess)

signature_detection.pl:1060:    (   excess_extraction(C, ExcessExtraction)

signature_detection.pl:1142:    excess_extraction(C, Excess),
```

The `0.05` and `0.01` thresholds appear in surrounding context:
- `boltzmann_compliance.pl:509`: `Excess =< 0.01` (inside `boltzmann_invariant_mountain`)
- `signature_detection.pl:991`: `Excess =< 0.05` (inside `purity_test_excess`)
- `signature_detection.pl:1083`: `Excess =< 0.05` (inside `zero_excess_coupling_only`)
- `signature_detection.pl:1143`: `Excess > 0.05` (inside `fcr_test_failure`)

All four thresholds are hardcoded literals, not config params. None are in `config.pl`.

---

### 1.5 `resolve_modal_signature_conflict/3` — inputs, outputs, feeders

**Inputs**: `(ModalType, Signature, Result)`  
**Output**: binds `Result` to the resolved classification type  
**Entry path**: `dr_type/3` → `integrate_signature_with_modal/3` → `resolve_with_perspectival_check/4` → `resolve_modal_signature_conflict/3`

**FCR-specific path** (signature_detection.pl:660-671):
```prolog
resolve_with_perspectival_check(C, piton, false_ci_rope, piton) :-
    drl_core:coordination_dead(C), !.
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :-
    !,
    (   config:param(fcr_override_enabled, 1)
    ->  (   ModalType \= unknown,
            has_metric_perspectival_variance(C)
        ->  AdjustedType = ModalType    % Preserve: indexical differentiation detected
        ;   AdjustedType = tangled_rope % Override: uniform or unknown classification
        )
    ;   AdjustedType = ModalType        % Ablation: preserve metric-based type
    ).
```
FCR override is gated by `has_metric_perspectival_variance/1` and `fcr_override_enabled` config param.

**Predicates that feed `resolve_modal_signature_conflict/3`** (via `constraint_signature/2`):
- `coupling_invariant_rope/2` → emits atom `coupling_invariant_rope`
- `false_natural_law/2` → emits atom `false_natural_law`
- `false_ci_rope/2` → emits atom `false_ci_rope`
- `false_summit_mountain/2` → emits atom `false_summit_mountain`
- Constructed sub-signatures (`constructed_low_extraction`, `constructed_high_extraction`, `constructed_constraint`) → emits respective atoms
- Natural law, coordination scaffold → emits respective atoms

---

## PASS 2 — ANALYSIS

### 2.1 Classification of flagged conditions

**F1: `\+ zero_excess_coupling_only(ExcessExtraction, FailedTests)` — false_ci_rope/2:1074**

**Classification: VIOLATION**

`ExcessExtraction` is collected via `excess_extraction/2` at line 1060 using the soft-collection pattern (`-> true ; ExcessExtraction = unknown`). This creates the appearance of a diagnostic collection. However, the value is immediately fed to `zero_excess_coupling_only/2` at line 1074, where it is tested against a hardcoded `0.05` threshold (F2). The soft collection does not prevent the value from being used as a gate — it only provides a fallback if `excess_extraction/2` fails entirely.

Override path to this gate: `boltzmann_floor_override(C, Floor)` → `boltzmann_floor_for/2:449` → `excess_extraction/2:442` → `ExcessExtraction` → `zero_excess_coupling_only/2:1083` → gate at `false_ci_rope/2:1074`.

This gate is inside the certificate predicate (`false_ci_rope/2` itself issues the FCR certificate). The override path reaches a hard gate inside the certificate. Cite: §1.1C, §1.3.

---

**F2: `Excess =< 0.05` — zero_excess_coupling_only/2:1083**

**Classification: VIOLATION**

Full clause (signature_detection.pl:1081-1089):
```prolog
zero_excess_coupling_only(Excess, FailedTests) :-
    number(Excess),
    Excess =< 0.05,
    FailedTests \= [],
    forall(
        member(F, FailedTests),
        coupling_based_failure(F)
    ).
```

Exists solely to suppress `false_ci_rope/2` when excess is at/below 0.05 and all failures are coupling-based. Threshold 0.05 is hardcoded, not config-driven. The `Excess` argument comes from `excess_extraction/2` (via `false_ci_rope/2:1060`), which reads `boltzmann_floor_for/2`, which accepts `boltzmann_floor_override`. This is the mechanism by which editorial floor data suppresses an FCR certificate. Cite: §1.1C, §1.3.

---

**F3: `excess_extraction(C, Excess), Excess > 0.05` — fcr_test_failure/2:1142-1143**

**Classification: VIOLATION (one level from certificate)**

Full clause (signature_detection.pl:1140-1143):
```prolog
% Test 3: Excess extraction above Boltzmann floor
fcr_test_failure(C, excess_above_floor(Excess)) :-
    excess_extraction(C, Excess),
    Excess > 0.05.  % Above noise floor
```

This gates whether `excess_above_floor(Excess)` enters the `FailedTests` list via `collect_fcr_failures/2` (a `findall`). It does not gate `false_ci_rope/2` directly. If the floor is overridden high, this test fails silently, removing one FCR signal.

However, this is one level removed from the certificate: `false_ci_rope/2` requires only `FailedTests \= []` — if other tests (boltzmann_non_compliant, scope_variant, nonsensical_coupling) still produce failures, FCR fires regardless. The severity is therefore Medium: the certificate can be suppressed via this path only if this is the LAST remaining failure AND it is suppressed. Cite: §1.1C, §1.3.

The threshold 0.05 is hardcoded, not config-driven. Same override path: `boltzmann_floor_override` → `boltzmann_floor_for` → `excess_extraction` → gate fails.

---

**F4: `Excess =< 0.01` — boltzmann_invariant_mountain/2:509**

**Classification: NOT A VIOLATION**

`boltzmann_invariant_mountain/2` is explicitly marked "SHADOW MODE: Results logged, not enforced." (boltzmann_compliance.pl:477-478). The predicate is called from `boltzmann_shadow_audit/2` (line 133), which is used for diagnostic logging only. It does not feed `constraint_signature/2` or any certificate predicate. Cite: §1.1, §1.3.

---

**F5: `Excess =< 0.05` — purity_test_excess/2:991**

**Classification: NOT A VIOLATION**

`purity_test_excess/2` feeds `structural_purity/2`, which feeds `signature_confidence/3` (for the CI_Rope signature, affecting confidence level, not blocking). `structural_purity/2` does NOT feed `constraint_signature/2`, which is the entry point into the certificate pipeline. The threshold at line 991 determines a purity classification label (`pass`/`fail`) used in reports, not in any gate. Cite: §1.3, grep output showing `structural_purity` reads in §1.4.

---

**F6: `BaseEps =< MaxX` — false_summit_mountain/2:1193**
**F7: `Supp =< SuppCeil` — false_summit_mountain/2:1196**

**Classification: LEGITIMATE GATES**

Both compare raw metric values (`base_extractiveness`, suppression) against config params (`mountain_extractiveness_max`, `mountain_suppression_ceiling`). Neither flows through `excess_extraction` or `boltzmann_floor_for`. These replicate the mountain metric profile from `classify_from_metrics/6` to scope the FSM certificate to genuinely mountain-profiled constraints. Cite: §1.1D.

---

### 2.2 `boltzmann_floor_for/2` to gate condition trace

**Path A** (override reaches gate inside certificate):
```
boltzmann_floor_override(C, Floor)        [testset: editorial data]
  → boltzmann_floor_for/2:449             [boltzmann_compliance.pl]
  → excess_extraction/2:442              [boltzmann_compliance.pl]
  → false_ci_rope/2:1060-1062            [soft collection, ExcessExtraction bound]
  → zero_excess_coupling_only/2:1083     [Excess =< 0.05 gate]
  → false_ci_rope/2:1074                 [\+ zero_excess_coupling_only(...)]
  OUTCOME: false_ci_rope certificate suppressed
```
Effect: if `boltzmann_floor_override(C, 0.99)` is declared and all FCR failures are coupling-based, `zero_excess_coupling_only` succeeds (Excess ≈ 0, coupling-only failures), and `false_ci_rope/2` fails entirely. No FCR certificate is issued.

**Path B** (override reaches failure aggregator):
```
boltzmann_floor_override(C, Floor)        [testset: editorial data]
  → boltzmann_floor_for/2:449
  → excess_extraction/2:442
  → fcr_test_failure/2:1142-1143         [Excess > 0.05 gate; gate fails]
  → collect_fcr_failures/2              [findall: excess_above_floor not collected]
  → false_ci_rope/2:1053                 [FailedTests may still be non-empty if other tests fire]
  OUTCOME: one FCR signal suppressed; certificate may still fire
```
Effect: `excess_above_floor` failure disappears from the failure list. If boltzmann_non_compliant, scope_variant, or nonsensical_coupling also fired, `FailedTests \= []` still holds and FCR fires — but only if `zero_excess_coupling_only` doesn't suppress it (Path A above).

**`boltzmann_compliant/2` is clean**: it gates on coupling score vs complexity threshold, with no dependency on `boltzmann_floor_for/2`. Floor overrides do not affect Boltzmann compliance results. Cite: §1.3.

---

### 2.3 Violation table

| Predicate | File:Line | Gate Condition | Violation Type | Severity |
|-----------|-----------|----------------|----------------|----------|
| `false_ci_rope/2` | signature_detection.pl:1074 | `\+ zero_excess_coupling_only(ExcessExtraction, FailedTests)` — inner test: `Excess =< 0.05` (hardcoded) | Floor-override-contaminated excess used as exemption gate *inside* the certificate predicate | **High** |
| `fcr_test_failure/2` | signature_detection.pl:1142–1143 | `excess_extraction(C, Excess), Excess > 0.05` (hardcoded) | Floor-override-contaminated excess gates failure collection one level below certificate; certificate may still fire via other failures | **Medium** |

**Already fixed (reference):**

| Predicate | File:Line | Gate Condition | Fix Applied |
|-----------|-----------|----------------|-------------|
| `coupling_invariant_rope/2` | signature_detection.pl:914-917 | was `excess_extraction(C, ExcessEps), ExcessEps =< Threshold` | Demoted to diagnostic soft collection; comment added explaining why |

**Clean (not violations):**

| Predicate | File:Line | Reason |
|-----------|-----------|--------|
| `boltzmann_invariant_mountain/2` | boltzmann_compliance.pl:508-512 | Shadow mode — not enforced, does not feed certificate |
| `purity_test_excess/2` | signature_detection.pl:988-996 | Feeds `structural_purity/2` → `signature_confidence` only, not certificate pipeline |
| `false_summit_mountain/2` | signature_detection.pl:1193, 1196 | Raw metric vs config param — no override path |
| `classify_from_metrics/6` | drl_core.pl:300-385 | All gates on raw metrics (BaseEps, Chi, Supp) against config params — no test intermediary |
| `false_natural_law/2` | signature_detection.pl:835-836 | Gates on `boltzmann_compliant` which uses coupling score, not excess |

---

### 2.4 Recommended fixes

#### High violation — `false_ci_rope/2:1074` + `zero_excess_coupling_only/2`

**Problem**: The zero-excess exemption was introduced to handle the legitimate case where scope-sensitive classification reflects the indexical system working correctly, not coordination washing. The intent is correct. The implementation is wrong: it uses a floor-override-contaminated excess value as a hard gate inside the certificate predicate — the same architectural error as the original CI_Rope violation.

**Fix (gate demotion — same pattern as CI_Rope fix):**

In `false_ci_rope/2`, change the exemption from a blocking gate to a diagnostic flag:

```prolog
% BEFORE:
\+ zero_excess_coupling_only(ExcessExtraction, FailedTests).

% AFTER: collect the flag as evidence, do not block
(   zero_excess_coupling_only(ExcessExtraction, FailedTests)
->  ZeroExcessFlag = true
;   ZeroExcessFlag = false
),
```

Include `ZeroExcessFlag` in the evidence record:
```prolog
false_ci_rope(C, fcr_evidence(AppearanceType, FailedTests, CouplingScore,
                               ExcessExtraction, ScopeResult, ZeroExcessFlag))
```

The MaxEnt classifier already receives `ExcessExtraction` and failure details; it can apply the exemption logic probabilistically rather than as a binary gate. This mirrors the CI_Rope fix: the override floor is editorial data that should inform classification confidence, not suppress certification.

**Corollary**: `zero_excess_coupling_only/2` becomes a diagnostic helper, not a certificate gate. Its 0.05 threshold should be promoted to a config param (`fcr_zero_excess_ceiling`) for auditability.

---

#### Medium violation — `fcr_test_failure/2:1143`

**Problem**: `Excess > 0.05` is a hardcoded threshold on a floor-override-contaminated value. A testset declaring `boltzmann_floor_override(C, 0.99)` silently removes the `excess_above_floor` failure from the FCR evidence.

**Fix A (config param only — minimal change)**:
Replace hardcoded `0.05` with a config param:
```prolog
% BEFORE:
fcr_test_failure(C, excess_above_floor(Excess)) :-
    excess_extraction(C, Excess),
    Excess > 0.05.

% AFTER:
fcr_test_failure(C, excess_above_floor(Excess)) :-
    excess_extraction(C, Excess),
    config:param(fcr_excess_floor, NoiseFloor),
    Excess > NoiseFloor.
```
Add `param(fcr_excess_floor, 0.05).` to config.pl. This makes the threshold auditable and sweepable without changing behavior.

**Fix B (gate demotion — addresses override path)**:
Remove the threshold entirely from `fcr_test_failure` and always record excess as evidence:
```prolog
fcr_test_failure(C, excess_above_floor(Excess)) :-
    excess_extraction(C, Excess),
    Excess > 0.0.  % Any positive excess is recorded
```
This records even small excess values in the failure list. Combined with the High fix (zero_excess_coupling_only demoted to diagnostic flag), the 0.05 threshold moves entirely to the MaxEnt layer where it can be weighted rather than used as a binary gate.

**Recommended order**: Fix the High violation first. The Medium violation becomes more important once the zero_excess_coupling_only exemption gate is removed — at that point, `fcr_test_failure/2:1143` is the only remaining hardcoded excess threshold in the FCR pipeline.

---

## Summary

Two violations found. Both in the FCR detection pipeline. Both share the same override path (`boltzmann_floor_override` → `boltzmann_floor_for` → `excess_extraction`), which was already identified as the violation mechanism in the CI_Rope fix.

The known CI_Rope fix documented the principle correctly: "gating here allowed overrides to suppress CI_Rope certification on genuinely coordinating constraints." The same error recurs in FCR: the zero-excess exemption uses the same override-contaminated excess value to suppress FCR certification on constraints that may have genuine structural issues masked by an artificially high floor.

The architectural fix in both cases is the same: collect `excess_extraction` as evidence, never as a gate, whenever the calling predicate is a certificate issuer.
