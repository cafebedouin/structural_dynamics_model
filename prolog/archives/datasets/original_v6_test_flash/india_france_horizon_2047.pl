% ============================================================================
% CONSTRAINT STORY: india_france_horizon_2047
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_france_horizon_2047, []).

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
 *   constraint_id: india_france_horizon_2047
 *   human_readable: India-France "Horizon 2047" Strategic Partnership
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The "Horizon 2047" agreement between India and France is a long-term
 *   strategic partnership spanning defense, space, nuclear energy, and AI. It
 *   represents a complex interplay of cooperation and competition, with
 *   potential benefits for both nations but also risks of disadvantaging
 *   smaller nations and altering regional power dynamics. The agreement aims
 *   to foster technological advancement, enhance security cooperation, and
 *   promote shared geopolitical interests. However, the extent to which it
 *   creates exclusive arrangements and impacts regional stability remains
 *   uncertain.
 *
 * KEY AGENTS:
 *   - Indian Defense Sector: Beneficiary (institutional/arbitrage) - gains access to advanced technology and enhanced capabilities.
 *   - French Defense Sector: Beneficiary (institutional/arbitrage) - gains access to the Indian market and enhanced geopolitical influence.
 *   - Indian Space Program: Beneficiary (institutional/arbitrage) - gains access to advanced technology and collaboration opportunities.
 *   - French Space Program: Beneficiary (institutional/arbitrage) - gains access to collaboration opportunities and enhanced geopolitical influence.
 *   - Other National Defense Sectors: Victim (powerless/trapped) - may face increased competition and limited access to resources.
 *   - Other National Space Programs: Victim (powerless/trapped) - may face increased competition and limited access to resources.
 *   - European Union: Regional actor (moderate/constrained) - experiences both benefits and challenges due to the partnership.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_france_horizon_2047, 0.35).
domain_priors:suppression_score(india_france_horizon_2047, 0.2).
domain_priors:theater_ratio(india_france_horizon_2047, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_france_horizon_2047, extractiveness, 0.35).
narrative_ontology:constraint_metric(india_france_horizon_2047, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(india_france_horizon_2047, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_france_horizon_2047, tangled_rope).
narrative_ontology:human_readable(india_france_horizon_2047, "India-France \"Horizon 2047\" Strategic Partnership").
narrative_ontology:topic_domain(india_france_horizon_2047, "geopolitical").

domain_priors:requires_active_enforcement(india_france_horizon_2047).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, indian_defense_sector).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_defense_sector).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, indian_space_program).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_space_program).
narrative_ontology:constraint_victim(india_france_horizon_2047, other_national_defense_sectors).
narrative_ontology:constraint_victim(india_france_horizon_2047, other_national_space_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% France benefits from enhanced geopolitical influence, access to the Indian market, and strategic alignment. Arbitrage exit option due to diverse partnership options.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% India benefits from access to advanced technology, enhanced defense capabilities, and strategic partnerships. Arbitrage exit option due to diverse partnership options.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Smaller nations may be disadvantaged by the increased competitive power of India and France in these sectors, with limited ability to exit or compete. Trapped exit due to economic and technological constraints.
constraint_indexing:constraint_classification(india_france_horizon_2047, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% EU experiences a mixed bag - benefits from increased global stability through cooperation, but faces competition in defense and space sectors. Constrained exit option - cannot fully exit due to existing treaties and obligations, but can seek alternative collaborations.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From a global perspective, the partnership represents a complex interplay of cooperation and competition, with potential benefits for global stability but also risks of exacerbating inequalities.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_france_horizon_2047_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(india_france_horizon_2047_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The partnership extracts resources and influence from smaller nations by increasing the competitive power of India and France. Suppression (0.20): Low. Other nations are not entirely prevented from pursuing their own defense and space programs, but they may face greater challenges. Theater ratio (0.40): Moderate. The partnership involves a degree of performative diplomacy and symbolic gestures, but it also has substantial practical implications for technological development and security cooperation.
 *
 * PERSPECTIVAL GAP:
 *   The French and Indian governments view the partnership as a rope, facilitating cooperation and mutual benefit. Smaller nations view the partnership as a snare, limiting their access to resources and increasing their competitive disadvantage. The EU views the partnership as a tangled rope, offering both opportunities and challenges. An analytical observer sees a complex interplay of cooperation and competition with uncertain long-term consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (India and France) experience low extraction due to their access to resources and influence. Victims (smaller nations) experience high extraction due to their limited access to resources and increased competitive disadvantage. The EU experiences a mixed level of extraction due to its constrained exit option and mixed benefits/challenges.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership could be mislabeled as pure extraction (snare) if the focus is solely on the potential disadvantages for smaller nations. However, the coordination and cooperation aspects for India and France, as well as the potential benefits for global stability, suggest that it is more accurately classified as a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_exclusivity,
    'To what extent does this partnership create exclusive arrangements, limiting cooperation with other nations?',
    'Analysis of specific agreements and their terms, tracking joint projects and their openness to participation from other countries.',
    'If highly exclusive: partnership becomes a snare for other nations. If open and collaborative: partnership remains a rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_exclusivity, empirical, 'Extent to which the partnership creates exclusive arrangements').

omega_variable(
    impact_on_regional_stability,
    'Does the increased military cooperation between India and France enhance or undermine regional stability in the Indo-Pacific?',
    'Monitoring of regional security dynamics, analysis of military deployments and exercises, assessment of diplomatic responses from neighboring countries.',
    'If enhances stability: Partnership is a rope or scaffold. If undermines stability: Partnership is a snare or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_regional_stability, empirical, 'Whether the partnership enhances or undermines regional stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_france_horizon_2047, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, india_france_horizon_2047, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indi_tr_t15, india_france_horizon_2047, theater_ratio, 15, 0.3).
narrative_ontology:measurement(indi_tr_t30, india_france_horizon_2047, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, india_france_horizon_2047, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(indi_be_t15, india_france_horizon_2047, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(indi_be_t30, india_france_horizon_2047, base_extractiveness, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_france_horizon_2047, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
