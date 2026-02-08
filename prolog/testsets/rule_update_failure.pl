% ============================================================================
% CONSTRAINT STORY: rule_update_failure
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_rule_update_failure, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rule_update_failure
 * human_readable: Obsolete Protocol Enforcement
 * domain: technological/social
 * * SUMMARY:
 * This constraint occurs when a system continues to enforce a behavioral rule
 * that no longer serves its original purpose due to a change in environment.
 * The update mechanism has failed, causing a former 'Rope' to degrade over time,
 * eventually becoming a high-friction 'Snare' for users and a 'Piton' for auditors.
 * * KEY AGENTS:
 * - End User: Subject (Powerless)
 * - System Maintainer: Beneficiary (Institutional)
 * - Protocol Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.72) because the obsolete rule costs time/energy for no gain.
domain_priors:base_extractiveness(rule_update_failure, 0.72).
domain_priors:suppression_score(rule_update_failure, 0.58).
domain_priors:theater_ratio(rule_update_failure, 0.75). % High theater/inertia triggers Piton.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rule_update_failure, extractiveness, 0.72).
narrative_ontology:constraint_metric(rule_update_failure, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rule_update_failure, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(rule_update_failure, piton).

% Binary flags
narrative_ontology:has_sunset_clause(rule_update_failure).
domain_priors:requires_active_enforcement(rule_update_failure).

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(rule_update_failure, system_maintainers).
narrative_ontology:constraint_victim(rule_update_failure, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Users are trapped by a rule that serves no purpose, effectively a predatory snare.
constraint_indexing:constraint_classification(rule_update_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the rule as a coordination 'Rope' that maintains stability.
constraint_indexing:constraint_classification(rule_update_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% Because a sunset clause exists, the original architect views it as a temporary Scaffold,
% even though its high extraction (0.72) violates the typical Scaffold threshold (<=0.30).
constraint_indexing:constraint_classification(rule_update_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(rule_update_failure).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.75) > 0.70 triggers Piton: a non-functional inertial spike.
constraint_indexing:constraint_classification(rule_update_failure, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(rule_update_failure, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rule_update_failure_tests).

test(perspectival_gap) :-
    % Verify Snare (Powerless) vs Rope (Institutional).
    constraint_indexing:constraint_classification(rule_update_failure, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rule_update_failure, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rule_update_failure, piton,
        context(agent_power(analytical), _, _, _)).

test(piton_threshold) :-
    % Ensure high theater ratio results in Piton classification.
    domain_priors:theater_ratio(rule_update_failure, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(rule_update_failure, piton,
        context(agent_power(analytical), _, _, _)).

:- end_tests(rule_update_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models institutional decay. It began as a low-extraction coordination
 * mechanism (a Rope), but as the environment changed, its function atrophied while
 * enforcement continued. This caused extraction (friction) and theater to rise.
 * The final extraction score (0.72) represents the high cost of compliance with an
 * obsolete rule. The high theater_ratio (0.75) indicates that enforcement is now
 * purely performative, justifying the Piton classification from an analytical view.
 *
 * PERSPECTIVAL GAP:
 * The End User feels a Snare because they pay the cost of friction for no benefit.
 * The Institution feels a Rope because the rule, however obsolete, maintains a
 * legible and stable system state, preventing the need for costly updates. The
 * Architect still sees their original intent, a temporary Scaffold. The Auditor
 * sees the reality: a functionless, inertial Piton.
 *
 * [RESOLVED MANDATROPHY]:
 * Resolved by the Piton classification. While the constraint has the structural
 * properties of a Tangled Rope (coordination, asymmetric extraction, enforcement),
 * the audit identifies it as a Piton because the high theater ratio confirms
 * that the coordination function is performative, not functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required because extraction (0.72) > 0.46.
omega_variable(
    omega_update_lag,
    'Is the failure to update the rule due to technical debt (Mountain) or active gatekeeping (Snare)?',
    'A/B testing the removal of the rule in an isolated regional node.',
    'If system stability holds: Snare. If system fails: Mountain (hidden dependency).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rule_update_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the decay of the rule from a useful coordination
% mechanism into a high-friction, high-theater Piton.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ruf_tr_t0, rule_update_failure, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ruf_tr_t5, rule_update_failure, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ruf_tr_t10, rule_update_failure, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ruf_ex_t0, rule_update_failure, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ruf_ex_t5, rule_update_failure, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ruf_ex_t10, rule_update_failure, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(rule_update_failure, enforcement_mechanism).

% Network relationships (structural influence edges)
% The obsolete rule's persistence is coupled with a dependency on a legacy system.
narrative_ontology:affects_constraint(rule_update_failure, legacy_system_dependency).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */