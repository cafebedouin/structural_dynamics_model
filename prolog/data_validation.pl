:- module(data_validation, [
    validate_all/0,
    validate_constraint_completeness/0,
    validate_metric_ranges/0,
    validate_classification_consistency/0,
    validate_edge_cases/0,
    validate_domain_coverage/0,
    validation_summary/0,
    % OQ-205 ε declaration checkers (single source: consumed here AND by the
    % fail-fast gate suite tests/test_epsilon_declaration.pl — two copies
    % would fork the check, Pattern 2)
    epsilon_provenance_drift/2,
    missing_epsilon_provenance/1,
    % OQ-153 keying (b) — MEASURE-ONLY, deliberately not in validate_all (see clause)
    inconsistent_update_authority/2
]).

:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(drl_core).
:- use_module(config).
:- use_module(utils).  % For safe metric retrieval

/* ============================================================================
   DATA VALIDATION SUITE — Stage 3 of Validation Pipeline
   ============================================================================

   Pipeline architecture (run in order):
     Stage 1: data_repair.pl     — Imputation. Fills missing measurements
                                    using domain priors. Run BEFORE tests.
     Stage 2: data_verification.pl — Gate. Verifies structural completeness
                                    of measurement/5 facts (32-point coercion
                                    vectors, value ranges, intent logic,
                                    paired temporal measurements). Blocks on
                                    failure during test_harness execution.
     Stage 3: data_validation.pl — Audit (THIS MODULE). Checks constraint_metric/3
                                    quality: completeness, ranges, classification
                                    consistency, edge cases, domain coverage.
                                    Runs AFTER all tests, reports quality issues.

   NOTE: Stage 2 and Stage 3 check different data structures:
     - data_verification checks measurement/5 (temporal coercion vectors)
     - data_validation checks constraint_metric/3 (static constraint properties)
   This is intentional — they validate different layers of the ontology.

   See also: test_harness.pl (orchestrates Stages 1-2 per test case),
             validation_suite.pl (runs 730+ test scenarios, calls Stage 3 at end).
   ============================================================================ */

:- dynamic validation_error/3.    % validation_error(Type, Constraint, Details)
:- dynamic validation_warning/3.   % validation_warning(Type, Constraint, Details)
:- dynamic validation_info/2.      % validation_info(Type, Message)

/* ============================================================================
   1. MAIN VALIDATION ENTRY POINT
   ============================================================================ */

%% validate_all/0
%  Runs all validation checks and reports results.
validate_all :-
    retractall(validation_error(_, _, _)),
    retractall(validation_warning(_, _, _)),
    retractall(validation_info(_, _)),

    format('~n====================================================~n'),
    format('   DATA VALIDATION SUITE                           ~n'),
    format('====================================================~n~n'),

    write('Running validation checks...'), nl, nl,

    % Run all validation checks
    validate_constraint_completeness,
    validate_metric_ranges,
    validate_update_authority,
    validate_classification_consistency,
    validate_edge_cases,
    validate_domain_coverage,

    % Report results
    validation_summary.

/* ============================================================================
   2. CONSTRAINT COMPLETENESS VALIDATION
   ============================================================================ */

%% validate_constraint_completeness/0
%  Checks that all constraints have required data.
validate_constraint_completeness :-
    format('[CHECK 1: Constraint Completeness]~n'),
    findall(C, narrative_ontology:constraint_claim(C, _), Constraints),
    length(Constraints, Total),
    format('  Checking ~w constraints...~n', [Total]),

    findall(C, (member(C, Constraints), is_complete_constraint(C)), Complete),
    length(Complete, CompleteCount),
    Incomplete is Total - CompleteCount,

    (Incomplete > 0
    -> (format('  ✗ ~w incomplete constraint(s) found~n~n', [Incomplete]),
        forall((member(C, Constraints), \+ is_complete_constraint(C)),
               report_incomplete_constraint(C)))
    ;  format('  ✓ All constraints complete~n~n', [])).

%% is_complete_constraint(+Constraint)
%  True if constraint has all required data.
%  Mandatory fields: base_extractiveness, constraint_metric(extractiveness),
%  constraint_metric(suppression_requirement), domain category, classification.
is_complete_constraint(C) :-
    % MANDATORY: Must have base_extractiveness/2 fact (used by dual-threshold classifier)
    (drl_core:base_extractiveness(C, BaseE)
    -> (number(BaseE), BaseE >= 0.0, BaseE =< 1.0)
    ;  (assertz(validation_error(missing_base_epsilon, C,
            'No base_extractiveness/2 fact. Required for dual-threshold classification.')), fail)),

    % MANDATORY: Must have extractiveness constraint_metric (used by reports/audit)
    (narrative_ontology:constraint_metric(C, extractiveness, E)
    -> (E >= 0.0, E =< 1.0)
    ;  (assertz(validation_error(missing_metric, C, extractiveness)), fail)),

    % CONSISTENCY: base_extractiveness and constraint_metric should agree
    (drl_core:base_extractiveness(C, BE),
     narrative_ontology:constraint_metric(C, extractiveness, ME),
     abs(BE - ME) > 0.01
    -> assertz(validation_warning(epsilon_mismatch, C,
            'base_extractiveness differs from constraint_metric(extractiveness)'))
    ;  true),

    % THREE-SITE ε equality, site 2 (OQ-205 spec §3, R2 amendment): the in-file
    % domain_priors:base_extractiveness/2 duplicate must match the live
    % constraint_metric read (the check above compares the DELEGATING
    % drl_core read, which equals constraint_metric for corpus stories —
    % near-vacuous on the dual-authoring fork this site closes).
    (domain_priors:base_extractiveness(C, DPE),
     narrative_ontology:constraint_metric(C, extractiveness, ME2),
     abs(DPE - ME2) > 0.01
    -> assertz(validation_warning(epsilon_mismatch_domain_priors, C,
            'domain_priors:base_extractiveness differs from constraint_metric(extractiveness)'))
    ;  true),

    % THREE-SITE ε equality, site 3 (OQ-205 spec §3): where epsilon_provenance/5
    % exists, ValueAsWritten joins the equality — drift is a HARD
    % validation_error (zero on the pre-build corpus by construction: no
    % pre-build story carries the fact; enforcement lives in the dedicated
    % pipeline gate, since this suite is WARN-only at the orchestrator).
    (epsilon_provenance_drift(C, DriftDetail)
    -> (assertz(validation_error(epsilon_provenance_drift, C, DriftDetail)), fail)
    ;  true),

    % LOUD-NULL census (OQ-205, operator ruling 2026-07-03): a constraint with
    % an authored ε but no epsilon_provenance/5 fact is the DECLARED loud-null
    % stratum — warning-grade BY RULING (expected on the pre-build corpus, not
    % a defect). Counted so the denominator stays honest; never a to-fix list.
    (missing_epsilon_provenance(C)
    -> assertz(validation_warning(missing_epsilon_provenance, C,
            'no epsilon_provenance/5 fact (loud-null stratum, pre-build story)'))
    ;  true),

    % MANDATORY: Must have suppression_requirement metric
    (narrative_ontology:constraint_metric(C, suppression_requirement, S)
    -> (S >= 0.0, S =< 1.0)
    ;  (assertz(validation_error(missing_metric, C, suppression_requirement)), fail)),

    % Must have domain category
    (domain_priors:category_of(C, Cat)
    -> (Cat \= unknown, Cat \= unknown_novel)
    ;  (assertz(validation_warning(missing_category, C, 'No domain category assigned')), fail)),

    % Should have an AGENT SURFACE (OQ-109 B3 presence-gate migration, 2026-06-12;
    % dispatch-consistent with the unanimity guard): authored classifications
    % (legacy four-tuple arm — dies at Phase C) OR the stakeholder surface
    % (>=1 compiled seat, or the schema-sanctioned authored-empty case:
    % six-questions authored with disappearance_verdict = world_unchanged).
    % Fail-closed when NEITHER surface exists (Pattern 5 — absence must not
    % satisfy the gate).
    (agent_surface_present(C)
    -> true
    ;  (assertz(validation_warning(missing_agent_surface, C, 'No authored classifications and no stakeholder surface')), fail)).

%% agent_surface_present(+C)
%  The story's agent surface, either format. Arm order mirrors the unanimity
%  dispatch: legacy decides where present; stakeholder surface is the
%  Phase-C-surviving arm.
agent_surface_present(C) :-
    constraint_indexing:constraint_classification(C, _, _), !.
agent_surface_present(C) :-
    narrative_ontology:constraint_stakeholder(C, _, _, _, _, _, _), !.

/* ----------------------------------------------------------------------------
   OQ-205 ε declaration checkers (spec §3; R2 + amendment ratified 2026-07-03)
   Single source of truth for the three-site equality and the loud-null
   census — consumed by is_complete_constraint/1 above AND by the fail-fast
   pipeline gate (tests/test_epsilon_declaration.pl). Do not duplicate the
   logic there (Pattern 2).
   ---------------------------------------------------------------------------- */

%% epsilon_provenance_drift(?C, -Detail)
%  Holds iff C carries an epsilon_provenance/5 fact whose ValueAsWritten
%  differs (>0.01, the pre-existing epsilon_mismatch tolerance) from either
%  of the other two in-file ε sites. Only post-build stories can drift —
%  the pre-build corpus authors no epsilon_provenance fact.
epsilon_provenance_drift(C, Detail) :-
    narrative_ontology:epsilon_provenance(C, VW, _, _, _),
    (   narrative_ontology:constraint_metric(C, extractiveness, ME),
        abs(VW - ME) > 0.01
    ->  format(atom(Detail),
               'epsilon_provenance ValueAsWritten ~w != constraint_metric(extractiveness) ~w',
               [VW, ME])
    ;   domain_priors:base_extractiveness(C, DP),
        abs(VW - DP) > 0.01
    ->  format(atom(Detail),
               'epsilon_provenance ValueAsWritten ~w != domain_priors:base_extractiveness ~w',
               [VW, DP])
    ;   fail
    ).

%% missing_epsilon_provenance(?C)
%  The loud-null stratum: C has a resolvable authored ε but no
%  epsilon_provenance/5 fact. Second tier over module-level ε literals
%  (drl_core:base_extractiveness/2 direct multifile facts) so the
%  constraint_instances.pl hand-authored class is in-domain and counted.
%  Enumeration with unbound C can yield duplicates — census callers
%  findall+sort; per-constraint callers bind C. The domain_priors site is in
%  the disjunction so a story authoring ε ONLY via its in-file
%  domain_priors:base_extractiveness/2 duplicate cannot silently escape the
%  census (every site of the three-site fork is a way INTO the stratum).
missing_epsilon_provenance(C) :-
    (   narrative_ontology:constraint_metric(C, extractiveness, _)
    ;   drl_core:base_extractiveness(C, _)
    ;   domain_priors:base_extractiveness(C, _)
    ),
    \+ narrative_ontology:epsilon_provenance(C, _, _, _, _).
agent_surface_present(C) :-
    % authored-empty stakeholders[] is schema-legal ONLY with world_unchanged;
    % the compiled verdict fact is the witness that six_questions was authored.
    narrative_ontology:disappearance_verdict(C, world_unchanged).

%% report_incomplete_constraint(+Constraint)
%  Reports what data is missing for a constraint.
report_incomplete_constraint(C) :-
    format('  Constraint: ~w~n', [C]),

    % Check base_extractiveness/2 (mandatory for dual-threshold classifier)
    (\+ drl_core:base_extractiveness(C, _)
    -> format('    ✗ Missing: base_extractiveness/2 (REQUIRED for classification)~n')
    ;  true),

    % Check extractiveness metric
    (\+ narrative_ontology:constraint_metric(C, extractiveness, _)
    -> format('    ✗ Missing: extractiveness metric~n')
    ;  true),

    % Check epsilon consistency
    (drl_core:base_extractiveness(C, BE),
     narrative_ontology:constraint_metric(C, extractiveness, ME),
     abs(BE - ME) > 0.01
    -> format('    ⚠ Epsilon mismatch: base_extractiveness=~2f vs metric=~2f~n', [BE, ME])
    ;  true),

    % Check suppression
    (\+ narrative_ontology:constraint_metric(C, suppression_requirement, _)
    -> format('    ✗ Missing: suppression_requirement metric~n')
    ;  true),

    % Check category
    (\+ domain_priors:category_of(C, _)
    -> format('    ✗ Missing: domain category~n')
    ; domain_priors:category_of(C, Cat),
      (Cat = unknown ; Cat = unknown_novel)
    -> format('    ⚠ Category is ~w (should be specific)~n', [Cat])
    ;  true),

    % Check agent surface (either format; OQ-109 B3)
    (\+ agent_surface_present(C)
    -> format('    ⚠ Missing: agent surface (no classifications, no stakeholders)~n')
    ;  true),

    nl.

/* ============================================================================
   3. METRIC RANGE VALIDATION
   ============================================================================ */

%% validate_metric_ranges/0
%  Checks that all metrics are within valid ranges.
validate_metric_ranges :-
    format('[CHECK 2: Metric Range Validation]~n'),

    % Check all constraint metrics
    findall(C-M-V, narrative_ontology:constraint_metric(C, M, V), Metrics),
    length(Metrics, TotalMetrics),
    format('  Checking ~w metrics...~n', [TotalMetrics]),

    findall(C-M-V,
            (narrative_ontology:constraint_metric(C, M, V),
             \+ valid_metric_range(M, V)),
            InvalidMetrics),

    length(InvalidMetrics, InvalidCount),
    (InvalidCount > 0
    -> (format('  ✗ ~w invalid metric(s) found~n~n', [InvalidCount]),
        forall(member(C-M-V, InvalidMetrics),
               (format('    ✗ ~w.~w = ~w (out of range)~n', [C, M, V]),
                assertz(validation_error(invalid_metric_range, C, M-V)))))
    ;  format('  ✓ All metrics in valid range~n', [])),
    nl.

%% valid_metric_range(+Metric, +Value)
%  Checks if a metric value is in valid range.
valid_metric_range(_, V) :-
    number(V),
    V >= 0.0,
    V =< 1.0.

%% validate_update_authority/0
%  OQ-153 (step 2): institutional revision-authority enum. Membership is checked
%  ONLY on AUTHORED facts — the findall ranges over existing update_authority/2
%  clauses, so an unauthored constraint has NO fact, is NEVER flagged, and is NEVER
%  defaulted. This is deliberate and load-bearing (operator, 2026-07-24): a default
%  would let a downstream blind authoring pass read a value instead of deciding one,
%  and `absent_diffuse` ("nobody owns the kernel", a substantive finding) must stay
%  distinct from unauthored ("not looked at") — the value travels with the fact or
%  not at all. No consumer yet; this guards the authoring surface only.
validate_update_authority :-
    format('[CHECK: Update-Authority Enum (OQ-153)]~n'),
    % (1) MEMBERSHIP — authored facts only (findall over existing clauses); no default.
    findall(C-V,
            ( narrative_ontology:update_authority(C, V),
              \+ valid_update_authority_value(V) ),
            Invalid),
    forall(member(C-V, Invalid),
           ( format('    ✗ ~w: update_authority = ~w (not in {licensed_revisable, frozen, absent_diffuse})~n', [C, V]),
             assertz(validation_error(invalid_update_authority, C, V)) )),
    % (2) UNIQUENESS — at most one fact per constraint. The step-3 blind authoring
    % pass emits these; a per-context row or duplicate would pass membership yet let
    % the five-condition husk signature read a constraint authored two ways.
    findall(C, narrative_ontology:update_authority(C, _), AllC),
    sort(AllC, UniqC),
    findall(C-N,
            ( member(C, UniqC),
              aggregate_all(count, narrative_ontology:update_authority(C, _), N),
              N > 1 ),
            Dups),
    forall(member(C-N, Dups),
           ( format('    ✗ ~w: ~w update_authority facts (must be at most one)~n', [C, N]),
             assertz(validation_error(duplicate_update_authority, C, N)) )),
    % (3) ORPHAN CID — a fact keyed on a typo'd/unknown constraint passes membership
    % and never joins (Kill-A would read it as authored variance that never reaches
    % the annotation). Authority: corpus_loader:corpus_constraint/1. Gated on the
    % corpus being loaded so a corpus-free load degrades to a skip, not a false-orphan.
    % Gate on a NON-EMPTY corpus, not merely a DEFINED predicate: corpus_constraint/1
    % is dynamic, so it is `defined` while empty (e.g. after [stack] with no
    % load_all_testsets). Firing then would flag EVERY authored fact orphan against an
    % empty corpus — a false-fire. Require ≥1 loaded constraint; else skip (loud note).
    (   predicate_property(corpus_loader:corpus_constraint(_), defined),
        corpus_loader:corpus_constraint(_)
    ->  findall(C,
                ( narrative_ontology:update_authority(C, _),
                  \+ corpus_loader:corpus_constraint(C) ),
                Orphans),
        forall(member(C, Orphans),
               ( format('    ✗ ~w: update_authority on a non-corpus constraint (orphan)~n', [C]),
                 assertz(validation_error(orphan_update_authority, C, not_a_corpus_constraint)) ))
    ;   Orphans = [],
        format('    (orphan check skipped — no corpus loaded)~n')
    ),
    length(Invalid, NI), length(Dups, ND), length(Orphans, NO),
    (   NI =:= 0, ND =:= 0, NO =:= 0
    ->  format('  ✓ update_authority: enum ok, unique, no orphans (or none authored)~n', [])
    ;   format('  ✗ update_authority: ~w out-of-enum, ~w duplicate, ~w orphan~n', [NI, ND, NO])
    ),
    nl.

%% valid_update_authority_value(+V)  — the closed enum; membership only, NO default.
valid_update_authority_value(licensed_revisable).
valid_update_authority_value(frozen).
valid_update_authority_value(absent_diffuse).

%% inconsistent_update_authority(-KernelId, -Values)   [MEASURE-ONLY — NOT a gate]
%  OQ-153 keying (b): update_authority is CID-keyed, but revision authority is a
%  KERNEL property — sibling readings of one kernel share one amending institution.
%  This reports kernels whose CIDs carry DIFFERING authored values. It is deliberately
%  NOT wired into validate_all (operator 2026-07-24): during the step-3 measurement
%  phase, cross-kernel disagreement IS the reliability signal (test-retest); gating on
%  it would convert that signal into an error to reconcile away. Measure, record, THEN
%  enforce. Scoped to AUTHORED facts only; ONE-authored / one-unauthored is PARTIAL
%  COVERAGE, not inconsistency (only >=2 distinct AUTHORED values on one kernel count).
inconsistent_update_authority(K, Values) :-
    setof(K0, C0^V0^( narrative_ontology:update_authority(C0, V0),
                      narrative_ontology:cs_kernel_id(C0, K0) ), Ks),
    member(K, Ks),
    findall(V, ( narrative_ontology:cs_kernel_id(C, K),
                 narrative_ontology:update_authority(C, V) ), Vs),
    sort(Vs, Values),          % distinct authored values on this kernel
    Values = [_, _|_].         % >=2 distinct => inconsistent (1 value or 1 authored => not reported)

/* ============================================================================
   4. CLASSIFICATION CONSISTENCY VALIDATION
   ============================================================================ */

%% validate_classification_consistency/0
%  Checks that claimed types match calculated types.
validate_classification_consistency :-
    format('[CHECK 3: Classification Consistency]~n'),
    findall(C, narrative_ontology:constraint_claim(C, _), Constraints),
    length(Constraints, Total),
    format('  Checking ~w constraints for consistency...~n', [Total]),

    findall(C-Claimed-Expected,
            (narrative_ontology:constraint_claim(C, Claimed),
             infer_expected_type(C, Expected),
             Claimed \= Expected),
            Inconsistencies),

    length(Inconsistencies, IncCount),
    (IncCount > 0
    -> (format('  ⚠ ~w potential inconsistenc(ies) found~n~n', [IncCount]),
        forall(member(C-Claimed-Expected, Inconsistencies),
               report_classification_inconsistency(C, Claimed, Expected)))
    ;  format('  ✓ All classifications consistent with metrics~n', [])),
    nl.

%% infer_expected_type(+Constraint, -Type)
%  Infers what type a constraint should be based on metrics.
%  Uses extractiveness_for_agent (v6.0 directionality chain) for Chi
%  and delegates to drl_core:classify_from_metrics/6 (Single Source of Truth).
infer_expected_type(C, Type) :-
    (   drl_core:base_extractiveness(C, Extr),
        narrative_ontology:constraint_metric(C, suppression_requirement, Supp)
    ->  constraint_indexing:default_context(Context),
        constraint_indexing:extractiveness_for_agent(C, Context, Chi),
        drl_core:classify_from_metrics(C, Extr, Chi, Supp, Context, Type)
    ;   Type = unknown).

%% report_classification_inconsistency(+C, +Claimed, +Expected)
report_classification_inconsistency(C, Claimed, Expected) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, Supp),
    drl_core:base_extractiveness(C, Extr),
    config:param(mountain_suppression_ceiling, Ceil),

    format('  Constraint: ~w~n', [C]),
    format('    Claimed: ~w~n', [Claimed]),
    format('    Expected (from metrics): ~w~n', [Expected]),
    format('    Metrics: suppression=~2f, extractiveness=~2f, ceiling=~2f~n', [Supp, Extr, Ceil]),

    (Expected = snare, Claimed = mountain
    -> (format('    ⚠ CRITICAL: False Mountain (extractive but claimed unchangeable)~n'),
        assertz(validation_error(false_mountain, C, Claimed-Expected)))
    ; Expected = mountain, Claimed = snare
    -> (format('    ⚠ False Snare (fair but claimed extractive)~n'),
        assertz(validation_warning(false_snare, C, Claimed-Expected)))
    ;  assertz(validation_warning(classification_mismatch, C, Claimed-Expected))
    ),
    nl.

/* ============================================================================
   5. EDGE CASE DETECTION
   ============================================================================ */

%% validate_edge_cases/0
%  Detects unusual patterns and edge cases.
validate_edge_cases :-
    format('[CHECK 4: Edge Case Detection]~n'),

    % Check for mandatrophies (unchangeable + extractive)
    findall(C,
            (narrative_ontology:constraint_claim(C, mountain),
             domain_priors:base_extractiveness(C, E),
             E > 0.7),
            Mandatrophies),

    length(Mandatrophies, MandCount),
    (MandCount > 0
    -> (format('  ⚠ ~w potential mandatroph(ies) detected~n', [MandCount]),
        forall(member(C, Mandatrophies),
               (domain_priors:base_extractiveness(C, E),
                format('    - ~w (mountain with E=~2f > 0.7)~n', [C, E]),
                assertz(validation_warning(mandatrophy, C, E)))))
    ;  format('  ✓ No mandatrophies detected~n', [])),

    % Check for extreme values
    findall(C-M-V,
            (narrative_ontology:constraint_metric(C, M, V),
             (V > 0.95 ; V < 0.05),
             % OQ-37: resistance_to_change dropped — never compiler-emitted, 0 facts on every
             % corpus (testsets/haiku/flash/kernel_v1), so it could never contribute an extreme
             % value. Behavior-preserving (the findall is identical). The live grid metric is
             % `resistance` (distinct referent, OQ-64) — adding it here is output-changing, deferred.
             member(M, [extractiveness, suppression_requirement])),
            ExtremeValues),

    length(ExtremeValues, ExtCount),
    (ExtCount > 0
    -> (format('  ⚠ ~w extreme value(s) detected (>0.95 or <0.05)~n', [ExtCount]),
        forall(member(C-M-V, ExtremeValues),
               (format('    - ~w.~w = ~2f~n', [C, M, V]),
                assertz(validation_info(extreme_value, C-M-V)))))
    ;  format('  ✓ No extreme values~n', [])),

    nl.

/* ============================================================================
   6. DOMAIN COVERAGE VALIDATION
   ============================================================================ */

%% validate_domain_coverage/0
%  Checks distribution across domain categories.
validate_domain_coverage :-
    format('[CHECK 5: Domain Coverage]~n'),

    findall(Cat, domain_priors:category_of(_, Cat), AllCats),
    sort(AllCats, UniqueCats),
    length(UniqueCats, NumCats),

    format('  Found ~w domain categories~n', [NumCats]),

    forall(member(Cat, UniqueCats),
           (findall(C, domain_priors:category_of(C, Cat), Cs),
            length(Cs, Count),
            format('    - ~w: ~w constraint(s)~n', [Cat, Count]))),

    % Check for underrepresented categories
    findall(Cat,
            (member(Cat, UniqueCats),
             findall(C, domain_priors:category_of(C, Cat), Cs),
             length(Cs, Count),
             Count < 3),
            UnderRep),

    (UnderRep \= []
    -> (nl, format('  ⚠ Underrepresented categories (<3 constraints):~n'),
        forall(member(Cat, UnderRep),
               (format('    - ~w~n', [Cat]),
                assertz(validation_info(underrepresented_category, Cat)))))
    ;  true),

    nl.

/* ============================================================================
   7. VALIDATION SUMMARY
   ============================================================================ */

%% validation_summary/0
%  Displays summary of validation results.
validation_summary :-
    format('====================================================~n'),
    format('   VALIDATION SUMMARY                              ~n'),
    format('====================================================~n~n'),

    % Count errors
    findall(E, validation_error(_, _, E), Errors),
    length(Errors, ErrorCount),

    % Count warnings
    findall(W, validation_warning(_, _, W), Warnings),
    length(Warnings, WarningCount),

    % Count info
    findall(I, validation_info(_, I), Infos),
    length(Infos, InfoCount),

    format('Results:~n'),
    format('  Errors:   ~w~n', [ErrorCount]),
    format('  Warnings: ~w~n', [WarningCount]),
    format('  Info:     ~w~n~n', [InfoCount]),

    % Show errors
    (ErrorCount > 0
    -> (format('ERRORS:~n'),
        forall(validation_error(Type, C, Details),
               format('  [ERROR] ~w: ~w - ~w~n', [Type, C, Details])),
        nl)
    ;  true),

    % Show critical warnings
    (WarningCount > 0
    -> (format('WARNINGS:~n'),
        forall(validation_warning(Type, C, Details),
               format('  [WARN] ~w: ~w - ~w~n', [Type, C, Details])),
        nl)
    ;  true),

    % Overall assessment
    (ErrorCount = 0, WarningCount = 0
    -> format('✓ DATA QUALITY: EXCELLENT - No issues detected~n')
    ; ErrorCount = 0, WarningCount < 10
    -> format('✓ DATA QUALITY: GOOD - Minor warnings only~n')
    ; ErrorCount > 0, ErrorCount < 5
    -> format('⚠ DATA QUALITY: FAIR - Some errors need attention~n')
    ;  format('✗ DATA QUALITY: POOR - Multiple errors require fixes~n')
    ),

    format('~n====================================================~n').
