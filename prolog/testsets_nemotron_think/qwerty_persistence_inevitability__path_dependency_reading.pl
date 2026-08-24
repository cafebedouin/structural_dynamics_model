% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence as Path Dependency
 *   domain: technology_history/political_economy
 *
 * SUMMARY:
 *   The QWERTY keyboard layout originated from mechanical constraints of
 *   1870s typewriters (preventing key jamming). Once established, network
 *   effects — typist training, manufacturing tooling, software defaults —
 *   created massive switching costs. This reading holds that no manufacturer
 *   cartel or strategic action engineered the lock-in; manufacturers merely
 *   responded to market demand for compatibility. The efficiency loss from
 *   QWERTY versus alternatives (e.g., Dvorak) is a diffuse externality borne
 *   by all typists, not extracted by a beneficiary group. The constraint is
 *   claimed as a mountain: given the initial accident, the outcome was
 *   inevitable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.1).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence as Path Dependency").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'd22e93fe-98fa-435e-8696-c45a70a2797f').
narrative_ontology:cs_kernel_codification('d22e93fe-98fa-435e-8696-c45a70a2797f', distributed).
narrative_ontology:cs_authority_grounding('d22e93fe-98fa-435e-8696-c45a70a2797f', diffuse_epistemic).
narrative_ontology:cs_reading_relation('d22e93fe-98fa-435e-8696-c45a70a2797f', qwerty_persistence_inevitability__strategic_lock_in_reading, forecloses).
narrative_ontology:cs_axiom('d22e93fe-98fa-435e-8696-c45a70a2797f', foundational, qwerty_persistence_is_accidental).
narrative_ontology:cs_axiom_status(qwerty_persistence_is_accidental, holdable).
narrative_ontology:cs_axiom_grounding('d22e93fe-98fa-435e-8696-c45a70a2797f', qwerty_persistence_is_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('d22e93fe-98fa-435e-8696-c45a70a2797f', historical_accident_origin).
narrative_ontology:cs_drift_state('d22e93fe-98fa-435e-8696-c45a70a2797f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d22e93fe-98fa-435e-8696-c45a70a2797f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependency_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, historical_accident_lock_in).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is near-zero (0.05) because no party collects rents from the layout's persistence; suppression is low (0.1) because no active enforcement prevents alternative layouts — they are commercially available but rarely adopted. Theater ratio is negligible (0.05) because there is no performative maintenance of a degraded function. Accessibility collapse is high (0.9) because once the network effects are understood, alternative adoption becomes individually irrational. Resistance is minimal (0.1) because the constraint is not actively defended; it simply persists.
 *
 * DIRECTIONALITY LOGIC:
 *   No agent is a structural beneficiary or victim. Manufacturers, typists, and educators all face the same switching costs. The engine's directionality derivation will assign symmetric d ≈ 0.5 to all, yielding effective extraction near zero for every seat — consistent with a mountain.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is QWERTY persistence a natural inevitability (path dependency) or a constructed lock-in (strategic action)?',
    'Historical analysis of manufacturer behavior in late 19th/early 20th century: evidence of cartel standardization vs. passive response to demand.',
    'If strategic lock-in is evidenced, the constraint reclassifies from mountain to snare or tangled_rope with manufacturers as beneficiaries and users as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Disambiguates the kernel''s two readings.').

omega_variable(
    efficiency_loss_diffuseness,
    'Is the efficiency loss from QWERTY truly diffuse, or does it concentrate on specific groups (e.g., professional typists, non-English users)?',
    'Empirical studies of typing speed/injury rates across layouts and user populations; economic analysis of switching cost distribution.',
    'If loss concentrates on identifiable groups, a victim set emerges even without strategic extraction, potentially shifting classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_loss_diffuseness, empirical, 'Whether the externality is uniformly diffuse or has identifiable bearers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 150, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
