% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colorado_sbe_decentralization_friction, []).

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
 *   constraint_id: colorado_sbe_decentralization_friction
 *   human_readable: Colorado SBE Institutional Preservation (Educational Decentralization Friction)
 *   domain: political/regulatory
 *
 * SUMMARY:
 *   The Colorado State Board of Education (SBE) acts as an institutional
 *   gatekeeper for educational legitimacy. While intended to ensure quality
 *   and consistency across the state, its role creates friction with local
 *   school districts and innovative schools seeking autonomy and flexibility.
 *   This tension results in a complex interplay of coordination and
 *   extraction.
 *
 * KEY AGENTS:
 *   - Colorado SBE: Primary beneficiary (institutional/arbitrage) - Benefits from maintaining control over educational standards.
 *   - Local School Districts: Primary target (powerless/trapped) - Face compliance costs and reduced autonomy.
 *   - Innovative Schools: Secondary target (moderate/constrained) - Constrained by the need for SBE approval, but also benefit from legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.55).
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.65).
domain_priors:theater_ratio(colorado_sbe_decentralization_friction, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, 0.55).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, tangled_rope).
narrative_ontology:human_readable(colorado_sbe_decentralization_friction, "Colorado SBE Institutional Preservation (Educational Decentralization Friction)").
narrative_ontology:topic_domain(colorado_sbe_decentralization_friction, "political/regulatory").

domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, colorado_sbe).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, local_school_districts).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, innovative_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Local school districts often feel trapped by SBE regulations, lacking the resources or political capital to effectively challenge them. They bear the brunt of extraction due to compliance costs and limited autonomy.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Innovative schools are constrained by the need for SBE approval to implement novel educational programs. While they benefit from the legitimacy the SBE provides, they also face extraction through bureaucratic hurdles and potential rejection of their proposals.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% The SBE benefits from maintaining its authority over educational standards and practices, which enhances its institutional power and legitimacy. It experiences this as a coordination function: ensuring consistent quality across the state.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the SBE as a tangled rope, balancing coordination with extraction. It provides a necessary function in setting standards, but also creates friction for local control and innovative approaches.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorado_sbe_decentralization_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(colorado_sbe_decentralization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The SBE extracts resources from local districts and innovative schools through compliance costs and approval processes. Suppression (0.65): Moderate-High. The SBE actively suppresses alternative educational approaches that do not align with its standards. Theater Ratio (0.40): Moderate. Some of the SBE's activities are performative, such as standardized testing, but there is also genuine effort to improve educational quality.
 *
 * PERSPECTIVAL GAP:
 *   Local school districts experience the SBE as a snare, limiting their autonomy and imposing bureaucratic burdens. Innovative schools see a tangled rope, balancing the need for legitimacy with the challenges of gaining approval for new programs. The SBE views its role as a rope, coordinating educational standards across the state. The analytical observer sees the full complexity of the situation, recognizing both the benefits and drawbacks of the SBE's institutional preservation.
 *
 * DIRECTIONALITY LOGIC:
 *   The SBE benefits from the preservation of its institutional power, experiencing the constraint as a coordination mechanism. Local school districts, lacking exit options, bear the costs of compliance and standardization. Innovative schools are constrained but can sometimes arbitrage through SBE approval, giving them moderate power. The directionality values reflect these structural relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_decentralization_level,
    'What is the optimal level of decentralization in education to maximize innovation and student outcomes while maintaining quality standards?',
    'Comparative studies of states with varying levels of educational decentralization, analyzing student performance, innovation adoption rates, and equity metrics.',
    'Determines whether the SBE''s role is primarily beneficial (rope) or excessively restrictive (snare) for local districts and schools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_decentralization_level, empirical, 'Determining the optimal level of decentralization in education.').

omega_variable(
    sbe_responsiveness_to_innovation,
    'How responsive is the SBE to new educational approaches and evidence-based practices?',
    'Analysis of SBE approval rates for innovative school proposals, stakeholder surveys, and policy documentation review.',
    'High responsiveness indicates a more flexible and adaptive SBE, reducing extraction and increasing coordination. Low responsiveness suggests institutional inertia and increased friction for innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sbe_responsiveness_to_innovation, empirical, 'Assessing the SBE''s responsiveness to innovative educational approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colorado_sbe_decentralization_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colo_tr_t0, colorado_sbe_decentralization_friction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(colo_tr_t5, colorado_sbe_decentralization_friction, theater_ratio, 5, 0.4).
narrative_ontology:measurement(colo_tr_t10, colorado_sbe_decentralization_friction, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(colo_be_t0, colorado_sbe_decentralization_friction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(colo_be_t5, colorado_sbe_decentralization_friction, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(colo_be_t10, colorado_sbe_decentralization_friction, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colorado_sbe_decentralization_friction, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
