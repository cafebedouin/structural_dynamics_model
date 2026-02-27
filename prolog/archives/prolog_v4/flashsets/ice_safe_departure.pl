% ============================================================================
% CONSTRAINT STORY: ice_safe_departure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_safe_departure, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ice_safe_departure
 *   human_readable: ICE Safe Departure Program
 *   domain: political
 *
 * SUMMARY:
 *   The ICE Safe Departure Program was an initiative designed to allow
 *   certain immigrants with final deportation orders to leave the United
 *   States in a managed manner. This program presents a complex constraint
 *   with both coordination and extraction aspects. ICE and federal courts
 *   benefit from the program through streamlined processes, while departing
 *   immigrants and their families face potential disruption and hardship. The
 *   program's effectiveness and human rights implications are key
 *   uncertainties.
 *
 * KEY AGENTS:
 *   - Departing Immigrants: Primary targets (powerless/trapped) - experience significant disruption and limited options.
 *   - Immigrant Families: Secondary targets (moderate/constrained) - face potential separation and hardship.
 *   - ICE: Primary beneficiary (institutional/arbitrage) - benefits from streamlined departures and reduced burden.
 *   - Federal Courts: Secondary beneficiary (institutional/arbitrage) - benefits from reduced caseload.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_safe_departure, 0.55).
domain_priors:suppression_score(ice_safe_departure, 0.65).
domain_priors:theater_ratio(ice_safe_departure, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_safe_departure, extractiveness, 0.55).
narrative_ontology:constraint_metric(ice_safe_departure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ice_safe_departure, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_safe_departure, tangled_rope).
narrative_ontology:human_readable(ice_safe_departure, "ICE Safe Departure Program").
narrative_ontology:topic_domain(ice_safe_departure, "political").

domain_priors:requires_active_enforcement(ice_safe_departure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_safe_departure, ice).
narrative_ontology:constraint_beneficiary(ice_safe_departure, federal_courts).
narrative_ontology:constraint_victim(ice_safe_departure, departing_immigrants).
narrative_ontology:constraint_victim(ice_safe_departure, immigrant_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Immigrants with deportation orders have limited options and face significant disruption.
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Families may be separated and face emotional and financial hardship. Exit options are constrained but not non-existent.
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% ICE benefits from streamlined departures, reduced administrative burden, and possibly improved public image. Arbitrage in the sense they can choose whom to target.
constraint_indexing:constraint_classification(ice_safe_departure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Federal Courts see reduced case loads, freeing up judicial resources.
constraint_indexing:constraint_classification(ice_safe_departure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a broad perspective, the program presents a mixed picture of managed departures and potential human rights concerns.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_safe_departure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_safe_departure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_safe_departure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ice_safe_departure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ice_safe_departure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.55) reflects the program's impact on departing immigrants, who face significant disruption. The suppression score (0.65) reflects the limited options available to immigrants with deportation orders. The theater ratio (0.40) reflects the program's mixed emphasis on procedural efficiency and genuine support for departing immigrants. The program qualifies as a Tangled Rope because it includes both a coordination function (managed departures) and asymmetric extraction (disruption to immigrants).
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a significant gap between the institutional benefits (ICE and Federal Courts) and the individual costs (Departing Immigrants and Families). ICE sees it as Rope; analytical sees as Tangled Rope; targets see as Snare. This conflict is at the heart of the ethical debate surrounding immigration enforcement policies.
 *
 * DIRECTIONALITY LOGIC:
 *   ICE and Federal Courts are beneficiaries with high exit options and receive low values. Departing immigrants and families are the victims with lower exit options and receive higher directionality values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    program_effectiveness,
    'How effectively does the program achieve its stated goals?',
    'Statistical analysis of program outcomes compared to traditional deportation methods.',
    'If effective, may shift classification towards a more favorable assessment. If ineffective, extraction may be viewed as pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_effectiveness, empirical, 'Assessment of program''s operational efficiency.').

omega_variable(
    human_rights_impact,
    'What are the long-term human rights implications of the program?',
    'Longitudinal studies of departing immigrants and their families.',
    'Understanding of the true impact on the immigrants and families. Determines whether to maintain tangled rope classification or pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_impact, empirical, 'Evaluation of the social impacts on human rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_safe_departure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice__tr_t0, ice_safe_departure, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ice__tr_t5, ice_safe_departure, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ice__tr_t10, ice_safe_departure, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ice__be_t0, ice_safe_departure, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ice__be_t5, ice_safe_departure, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ice__be_t10, ice_safe_departure, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ice_safe_departure, immigration_enforcement_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
