% ============================================================================
% CONSTRAINT STORY: suanne_coup_of_peace
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_suanne_coup_of_peace, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: suanne_coup_of_peace
 * human_readable: The Hostile Social Environment at the Lead Basketball Game
 * domain: social/cultural
 * * SUMMARY:
 * This constraint models the hostile, racist social environment faced by the Pine Ridge basketball team
 * during a game in Lead, South Dakota, as described in Ian Frazier's "On the Rez." The environment
 * functions as a tool of social dominance, extracting dignity and focus from the visiting team through
 * coordinated mockery. The narrative's resolution—SuAnne Big Crow's "Coup of Peace"—is an action that
 * breaks this constraint, but the constraint itself is the initial predatory atmosphere.
 * * KEY AGENTS:
 * - Pine Ridge Team: Subject (Powerless), victims of the harassment.
 * - Lead Fans Collective: Beneficiary (Institutional), perpetrators of the harassment.
 * - Systems Analyst: Auditor (Analytical), observing the structure of the conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(suanne_coup_of_peace, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(suanne_coup_of_peace, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(suanne_coup_of_peace, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(suanne_coup_of_peace, extractiveness, 0.80).
narrative_ontology:constraint_metric(suanne_coup_of_peace, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(suanne_coup_of_peace, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% The heckling is framed as a legitimate part of "home court advantage."
narrative_ontology:constraint_claim(suanne_coup_of_peace, tangled_rope).
narrative_ontology:human_readable(suanne_coup_of_peace, "The Hostile Social Environment at the Lead Basketball Game").
narrative_ontology:topic_domain(suanne_coup_of_peace, "social/cultural").

% Binary flags
domain_priors:requires_active_enforcement(suanne_coup_of_peace). % The heckling is an active, coordinated performance.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(suanne_coup_of_peace, lead_fans_collective).
narrative_ontology:constraint_victim(suanne_coup_of_peace, pine_ridge_team).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the visiting team, the coordinated harassment is a predatory trap,
% extracting their dignity and ability to focus.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the home crowd, the heckling is a coordination mechanism (a Rope) to
% establish home-court advantage and enforce local social dominance.
constraint_indexing:constraint_classification(suanne_coup_of_peace, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (Rope) and the severe,
% asymmetric extraction (Snare). This hybrid nature, requiring active
% enforcement, defines it as a Tangled Rope.
constraint_indexing:constraint_classification(suanne_coup_of_peace, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_coup_of_peace_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(suanne_coup_of_peace, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the base extraction is high, justifying Snare/Tangled Rope classifications.
    domain_priors:base_extractiveness(suanne_coup_of_peace, E),
    E >= 0.46.

:- end_tests(suanne_coup_of_peace_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a highly predatory social environment. The extraction (0.80) represents the
 * theft of dignity, psychological safety, and focus from the visiting team. The suppression (0.70)
 * represents the overwhelming social pressure that prevents any alternative narrative from emerging
 * without a significant intervention. The theater ratio is very low (0.05) because the hostility is
 * genuine, not merely performative.
 *
 * The Perspectival Gap is stark:
 * - The Pine Ridge team experiences this as a Snare: a trap with high coercion and no escape.
 * - The Lead fans see it as a Rope: a tool for coordinating group identity and asserting dominance.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this from an analytical perspective as a Tangled Rope is crucial. A simple Snare
 * classification would miss the coordination function that gives the constraint its power and social
 * legitimacy among the perpetrators. By acknowledging both the coordination (Rope) and the extraction
 * (Snare) components, the system avoids mischaracterizing the mechanism and correctly identifies why
 * it is so stable and effective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_suanne_coup_of_peace,
    "Is the 'Coup of Peace' a repeatable protocol for de-escalation, or was its success dependent on the unique charisma of SuAnne Big Crow?",
    "Audit of similar 'coup' attempts by different agents in diverse hostile environments, analyzing the role of individual charisma versus replicable cultural protocols.",
    "If dependent on charisma, the resolution is an outlier event. If a repeatable protocol, it is a functional 'Rope' for social change.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(suanne_coup_of_peace, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The hostile environment was a stable,
% pre-existing condition, so the metrics are flat leading up to the event.
% The "Coup of Peace" is the event that breaks this stable constraint.
%
% Theater ratio over time (consistently low, as hostility was genuine):
narrative_ontology:measurement(scop_tr_t0, suanne_coup_of_peace, theater_ratio, 0, 0.05).
narrative_ontology:measurement(scop_tr_t5, suanne_coup_of_peace, theater_ratio, 5, 0.05).
narrative_ontology:measurement(scop_tr_t10, suanne_coup_of_peace, theater_ratio, 10, 0.05).

% Extraction over time (consistently high):
narrative_ontology:measurement(scop_ex_t0, suanne_coup_of_peace, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(scop_ex_t5, suanne_coup_of_peace, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(scop_ex_t10, suanne_coup_of_peace, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The heckling serves as a mechanism to enforce a social hierarchy.
narrative_ontology:coordination_type(suanne_coup_of_peace, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */