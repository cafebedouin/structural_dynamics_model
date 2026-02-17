% ============================================================================
% CONSTRAINT STORY: thai_senate_veto_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_thai_senate_veto_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: thai_senate_veto_2026
 * human_readable: The Senate Constitutional Veto (Post-Transitory Residual)
 * domain: political
 * * SUMMARY:
 * Following the expiration of the five-year transitory provision that allowed the
 * junta-appointed Senate to vote for the Prime Minister, the Senate
 * retains a one-third veto power over constitutional amendments under Article 256.
 * In December 2025, Parliament voted to preserve this power despite attempts to
 * move to a simple majority. It now functions as a high-extraction Snare whose
 * original "coordination" utility (managing a political transition) has atrophied,
 * leaving behind a structural veto maintained by "theatrical" claims of ensuring stability.
 * * KEY AGENTS:
 * - Thai Voters: Subject (Powerless) - Unable to see their elected representatives enact constitutional reform.
 * - The People's Party: Subject (Organized) - Blocked from structural reform by the veto.
 * - The Senate: Beneficiary (Institutional) - Maintaining a non-representative check on the House.
 * - The Election Commission: Auditor (Analytical) - Overseeing election results which this veto constrains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction: The Senate can block the will of the 500 elected MPs.
domain_priors:base_extractiveness(thai_senate_veto_2026, 0.72).
% High suppression: Alternatives like simple majority voting were explicitly rejected.
domain_priors:suppression_score(thai_senate_veto_2026, 0.80).
% High Theater: The Senate's "oversight" role is a performance of stability, as its original transition-management function is gone.
domain_priors:theater_ratio(thai_senate_veto_2026, 0.85).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(thai_senate_veto_2026, extractiveness, 0.72).
narrative_ontology:constraint_metric(thai_senate_veto_2026, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(thai_senate_veto_2026, theater_ratio, 0.85).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a mechanism for enforcing stability.
narrative_ontology:constraint_claim(thai_senate_veto_2026, snare).
narrative_ontology:human_readable(thai_senate_veto_2026, "The Senate Constitutional Veto (Post-Transitory Residual)").

% Binary flags
domain_priors:requires_active_enforcement(thai_senate_veto_2026).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, conservative_establishment).
narrative_ontology:constraint_victim(thai_senate_veto_2026, reformist_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE THAI VOTER (SNARE)
% The veto is an inescapable trap preventing the will of the electorate from being realized.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE REFORMIST MP (SNARE)
% An organized but still constrained agent sees the veto as a trap preventing the legal "exit" from the 2017 framework.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CONSERVATIVE ESTABLISHMENT (ROPE)
% Viewed as a necessary coordination mechanism (a "rope") to safeguard state structure and stability.
constraint_indexing:constraint_classification(thai_senate_veto_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% Analytically, the high extraction (0.72), high suppression (0.80), and active enforcement
% classify this as a Snare. The high theater ratio (0.85) is a symptom of its atrophied
% justification, not a reclassification to a low-extraction Piton.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_senate_veto_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    domain_priors:base_extractiveness(thai_senate_veto_2026, E),
    E >= 0.46. % Triggers Omega and Temporal Data requirements.

test(theater_ratio_check) :-
    domain_priors:theater_ratio(thai_senate_veto_2026, TR),
    TR > 0.70. % Indicates atrophied justification.

:- end_tests(thai_senate_veto_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a classic Snare with Piton-like symptoms. Its original function—
 * managing the post-coup political transition—has expired. However, its extractive
 * power (vetoing constitutional change) remains fully intact (E=0.72), actively enforced,
 * and suppresses all alternatives (S=0.80). The extremely high theater ratio (0.85)
 * is not indicative of a low-extraction Piton; rather, it's a symptom of a Snare
 * that has lost its original justification and must now "perform" legitimacy through
 * claims of ensuring stability. The analytical observer classifies it as a Snare
 * because its material effect is pure, asymmetric extraction, regardless of the historical narrative.
 *
 * [RESOLVED MANDATROPHY]
 * Extraction > 0.7 requires resolution: The system resolves this as Mandatrophy
 * because the "coordination" value claimed by proponents (preventing instability)
 * is vastly outweighed by the suppression of democratic alternatives and the direct
 * blocking of the electorate's will.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% High-extraction resolution hook.
omega_variable(
    omega_senate_composition_2026,
    'Will the new Senate composition formula effectively bypass or reinforce the veto power?',
    'Analysis of the CDA selection process and voting blocs within the new Senate post-2026.',
    'Reform (Snare is weakened or removed) vs Entrenchment (Snare hardens)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(thai_senate_veto_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's degradation from a component of a
% transitory framework into a pure Snare. As its justification atrophied after
% 2024, its theater ratio spiked while its extractive power was solidified.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(thai_senate_veto_2026_tr_t0, thai_senate_veto_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(thai_senate_veto_2026_tr_t5, thai_senate_veto_2026, theater_ratio, 5, 0.75).
narrative_ontology:measurement(thai_senate_veto_2026_tr_t10, thai_senate_veto_2026, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(thai_senate_veto_2026_ex_t0, thai_senate_veto_2026, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(thai_senate_veto_2026_ex_t5, thai_senate_veto_2026, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(thai_senate_veto_2026_ex_t10, thai_senate_veto_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's *claimed* function is enforcement of stability.
narrative_ontology:coordination_type(thai_senate_veto_2026, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */