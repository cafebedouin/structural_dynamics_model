% ============================================================================
% CONSTRAINT STORY: indo_german_defense_pact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_german_defense_pact, []).

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
 *   constraint_id: indo_german_defense_pact
 *   human_readable: India-Germany Defense Industrial Partnership
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The strategic partnership between India and Germany aims to deepen
 *   defense industry collaboration through co-development, co-production, and
 *   technology transfer. This collaboration is expected to modernize the
 *   Indian military while providing market access for German defense firms.
 *   Potential tensions exist between the goals of promoting domestic Indian
 *   defense production and relying on foreign technology.
 *
 * KEY AGENTS:
 *   - German Defense Firms: Primary beneficiary (institutional/arbitrage)
 *   - Indian Military: Mixed beneficiary/victim (institutional/constrained)
 *   - Domestic Indian Defense Industry: Primary victim (powerless/trapped)
 *   - German Taxpayers: Secondary Victim (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_german_defense_pact, 0.55).
domain_priors:suppression_score(indo_german_defense_pact, 0.4).
domain_priors:theater_ratio(indo_german_defense_pact, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_german_defense_pact, extractiveness, 0.55).
narrative_ontology:constraint_metric(indo_german_defense_pact, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(indo_german_defense_pact, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_german_defense_pact, tangled_rope).
narrative_ontology:human_readable(indo_german_defense_pact, "India-Germany Defense Industrial Partnership").
narrative_ontology:topic_domain(indo_german_defense_pact, "geopolitical/economic").

domain_priors:requires_active_enforcement(indo_german_defense_pact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, german_defense_firms).
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, indian_military).
narrative_ontology:constraint_victim(indo_german_defense_pact, domestic_indian_defense_industry).
narrative_ontology:constraint_victim(indo_german_defense_pact, german_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of domestic Indian defense firms: faces suppression due to increased competition and technology dependency; trapped due to lack of scale and political influence
constraint_indexing:constraint_classification(indo_german_defense_pact, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of German defense firms: benefits from increased market access and co-production opportunities; can arbitrage partnerships with other nations; long term gains
constraint_indexing:constraint_classification(indo_german_defense_pact, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of the Indian Military: benefits from access to advanced German technology but also constrained by dependency and geopolitical alignment requirements; a mixed role
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer perspective, showing extraction and coordination elements
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_german_defense_pact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_german_defense_pact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indo_german_defense_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Reflects the extraction from domestic Indian firms and potential overpayment by Indian and German governments. Suppression (0.40): Reflects the suppression of domestic competition. Theater Ratio (0.30): Some performative elements but mostly functional.
 *
 * PERSPECTIVAL GAP:
 *   German defense firms see a rope (market access), domestic Indian firms see a snare (competition), and the Indian military sees a tangled rope (advanced tech but dependency).
 *
 * DIRECTIONALITY LOGIC:
 *   German firms benefit with arbitrage; Indian military benefits, but is constrained. Indian domestic firms are victims with no exit. German taxpayers are a secondary victim.
 *
 * MANDATROPHY ANALYSIS:
 *   Prevents mislabeling coordination as pure extraction by recognizing mutual gains (technology transfer) but also potential asymmetric benefits and costs (domestic competition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_effectiveness,
    'How effective is the actual transfer of technology and know-how from Germany to India?',
    'Audits of Indian defense production facilities; surveys of Indian engineers; comparison of independently produced systems.',
    'If low: primarily a commercial arrangement (rope for German firms, snare for Indian industry). If high: genuine capability enhancement (scaffold with sunset for dependency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_effectiveness, empirical, 'Effectiveness of technology transfer determines whether the partnership increases long-term Indian capabilities.').

omega_variable(
    geopolitical_alignment_costs,
    'To what extent does the partnership constrain India''s foreign policy autonomy?',
    'Analysis of India''s voting patterns in international forums; interviews with Indian diplomats; comparison to India''s relations with other arms suppliers.',
    'If significant: the partnership becomes a tangled rope (Indian autonomy is extracted). If minimal: primarily a commercial and technological arrangement (rope with limited geopolitical implications).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_alignment_costs, conceptual, 'Geopolitical alignment determines constraints on India''s foreign policy autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_german_defense_pact, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indo_tr_t0, indo_german_defense_pact, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indo_tr_t5, indo_german_defense_pact, theater_ratio, 5, 0.3).
narrative_ontology:measurement(indo_tr_t10, indo_german_defense_pact, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(indo_be_t0, indo_german_defense_pact, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(indo_be_t5, indo_german_defense_pact, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(indo_be_t10, indo_german_defense_pact, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_german_defense_pact, resource_allocation).
narrative_ontology:affects_constraint(indo_german_defense_pact, make_in_india_initiative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
