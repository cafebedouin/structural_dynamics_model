% ============================================================================
% CONSTRAINT STORY: rogers_commission_institutional_analysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_institutional_analysis, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rogers_commission_institutional_analysis
 *   human_readable: The Silent Safety Silo (NASA Decision-Making Pre-Challenger)
 *   domain: political/technological
 *
 * SUMMARY:
 *   The Rogers Commission identified a "silent safety" constraint where
 *   critical technical data regarding O-ring erosion was filtered out before
 *   reaching top-level NASA management. This created a flawed decision-making
 *   process that ultimately contributed to the Challenger disaster. The
 *   constraint involved the suppression of dissenting opinions, the
 *   prioritization of launch schedules over safety, and a degraded
 *   communication system that prevented informed decision-making.
 *
 * KEY AGENTS:
 *   - Challenger Astronauts: Primary victims (powerless/trapped) - bore the ultimate cost of the flawed decision-making process.
 *   - NASA Engineers: Secondary victims (moderate/constrained) - raised concerns but were constrained by the organizational structure.
 *   - NASA Top Management: Beneficiary/Victim (institutional/constrained) - benefited from the continued launch schedule but operated within a degraded communication system.
 *   - Morton Thiokol Management: Primary beneficiary (institutional/arbitrage) - benefited from maintaining their contract with NASA and avoided potential penalties.
 *   - Analytical Observer: Observers (analytical/analytical) - able to see complete structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_institutional_analysis, 0.75).
domain_priors:suppression_score(rogers_commission_institutional_analysis, 0.8).
domain_priors:theater_ratio(rogers_commission_institutional_analysis, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, extractiveness, 0.75).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_institutional_analysis, snare).
narrative_ontology:human_readable(rogers_commission_institutional_analysis, "The Silent Safety Silo (NASA Decision-Making Pre-Challenger)").
narrative_ontology:topic_domain(rogers_commission_institutional_analysis, "political/technological").

domain_priors:requires_active_enforcement(rogers_commission_institutional_analysis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, nasa_top_management).
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, morton_thiokol_management).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, challenger_astronauts).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, nasa_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Astronauts were unaware of the O-ring risks and had no way to exit the situation. They bore the ultimate cost.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Engineers who raised concerns were constrained by the organizational structure and pressure to launch. Their concerns were suppressed, and they were victims of the flawed decision-making process.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% NASA management, while benefiting from the continued launch schedule and perceived success, ultimately operated within a degraded communication system that prevented them from making informed decisions. The system was in place but failing.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Morton Thiokol management benefited from maintaining their contract with NASA and avoided potential penalties for delaying the launch. They actively suppressed dissenting engineering opinions.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An objective analysis reveals a system where critical safety information was actively filtered out, creating a dangerous environment. While there was a coordination function (launching the shuttle), asymmetric extraction (suppression of safety concerns for launch schedule) means this operated as a tangled rope.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_institutional_analysis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rogers_commission_institutional_analysis, TR),
    TR >= 0.70.

:- end_tests(rogers_commission_institutional_analysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The suppression of safety concerns for the sake of maintaining the launch schedule represents a significant extraction from safety protocols and ethical considerations. Suppression (0.80): High. The active filtering of information and the discouragement of dissenting opinions indicate a high degree of suppression. Theater ratio (0.60): Moderate. There were formal safety procedures in place, but they were not effectively implemented or followed, leading to a theatrical performance of safety rather than genuine safety measures.
 *
 * PERSPECTIVAL GAP:
 *   The Challenger astronauts were unaware and trapped, seeing a snare. NASA engineers saw a snare because they had safety concerns that were ignored, despite their moderate power. NASA management was constrained by the information they received (or did not receive) and the need to launch on schedule. Morton Thiokol management saw an opportunity for arbitrage, prioritizing profit over safety, actively suppressing dissenting engineering opinions to achieve this. From an analytical perspective, the entire system of NASA decision making acted as a tangled rope that suppressed safety for the sake of continued operation and launch schedules.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agents structural role within the constraint. NASA management and Morton Thiokol, who benefitted from the launch going ahead, have low d values. Those whose opinions were suppressed, or who were otherwise disadvantaged by the constraint, have a high d. The analytical observer sees the entire picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by highlighting the trade-offs between safety and operational goals. While NASA had the coordination function of launching the shuttle, the suppression of safety concerns means this operated as a tangled rope rather than a pure rope, and for those who paid the ultimate price, a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_culture_influence,
    'To what extent did NASA''s organizational culture prioritize schedule and budget over safety?',
    'Historical analysis of NASA documents, interviews with personnel, and comparative studies with other organizations.',
    'If culture was a dominant factor, the constraint is more deeply entrenched. If it was a temporary deviation, reforms may be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_culture_influence, empirical, 'Assessment of organizational culture influence').

omega_variable(
    communication_channel_integrity,
    'How effective were the formal and informal communication channels for conveying critical safety information?',
    'Analysis of communication protocols, interviews with engineers and managers, and simulations of information flow.',
    'If channels were fundamentally flawed, the constraint is more severe. If they were simply misused, targeted improvements may suffice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communication_channel_integrity, empirical, 'Integrity of communication channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_institutional_analysis, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_institutional_analysis, theater_ratio, 0, 0.3).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_institutional_analysis, theater_ratio, 3, 0.5).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_institutional_analysis, theater_ratio, 5, 0.6).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_institutional_analysis, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(roge_be_t3, rogers_commission_institutional_analysis, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(roge_be_t5, rogers_commission_institutional_analysis, base_extractiveness, 5, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_institutional_analysis, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
