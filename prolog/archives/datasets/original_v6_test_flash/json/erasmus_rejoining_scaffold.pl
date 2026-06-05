% ============================================================================
% CONSTRAINT STORY: erasmus_rejoining_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_erasmus_rejoining_scaffold, []).

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
 *   constraint_id: erasmus_rejoining_scaffold
 *   human_readable: UK's potential re-entry into the EU Erasmus+ student exchange program
 *   domain: political
 *
 * SUMMARY:
 *   Following its exit from the EU and the Erasmus+ program, the UK is
 *   considering rejoining. This scenario presents a complex interplay of
 *   political, economic, and social factors, leading to varied perspectives
 *   on the potential benefits and drawbacks. Rejoining serves as a temporary
 *   measure to enhance international collaboration and student mobility, a
 *   coordinated scaffold.
 *
 * KEY AGENTS:
 *   - UK Government: Primary driver of the decision (powerful/mobile)
 *   - UK Students: Beneficiaries of exchange opportunities (moderate/constrained)
 *   - UK Universities: Institutions managing exchanges (institutional/constrained)
 *   - EU Universities: Partners in the exchange program (institutional/arbitrage)
 *   - UK Taxpayers: Potentially bearing the financial burden (powerless/trapped)
 *   - Short Term Domestic Alternatives: A domestic alternative (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(erasmus_rejoining_scaffold, 0.35).
domain_priors:suppression_score(erasmus_rejoining_scaffold, 0.2).
domain_priors:theater_ratio(erasmus_rejoining_scaffold, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(erasmus_rejoining_scaffold, scaffold).
narrative_ontology:human_readable(erasmus_rejoining_scaffold, "UK's potential re-entry into the EU Erasmus+ student exchange program").
narrative_ontology:topic_domain(erasmus_rejoining_scaffold, "political").

domain_priors:requires_active_enforcement(erasmus_rejoining_scaffold).
narrative_ontology:has_sunset_clause(erasmus_rejoining_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_students).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_universities).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, eu_universities).
narrative_ontology:constraint_victim(erasmus_rejoining_scaffold, taxpayers_uk).
narrative_ontology:constraint_victim(erasmus_rejoining_scaffold, short_term_domestic_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% UK taxpayers may see this as a snare if they feel they are disproportionately funding a program with limited direct benefits for them and they have no say in the matter.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% EU universities benefit from the influx of UK students and the associated funding/prestige. They can also participate in exchanges with other countries, but the UK's participation strengthens the network.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% The UK government (assuming a pro-rejoin stance) views this as a scaffold - a temporary measure to enhance international collaboration and student mobility while developing long-term domestic alternatives. The government can adjust its position and funding over time.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% UK students benefit from the exchange opportunities, but are also constrained by application processes, living costs in other countries, and potential disruption to their studies.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the program's effectiveness may degrade over time, becoming more about political signaling than actual student enrichment if the UK develops successful, independent programs (or if the EU shifts its own priorities). Rejoining maintains relationships, but if the UK builds its own rival it becomes a theatrical exercise.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% UK universities benefit from the program by attracting more students. However, they may be constrained by the administrative burden of managing exchanges, ensuring quality standards, and balancing international and domestic opportunities.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erasmus_rejoining_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(erasmus_rejoining_scaffold, TR),
    TR >= 0.70.

:- end_tests(erasmus_rejoining_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate extraction reflects the financial costs to UK taxpayers and the potential administrative burden on universities. Suppression (0.20): Some suppression of domestic alternatives as rejoining favors EU cooperation. Theater ratio (0.75): Moderate as the focus is on academic experience and benefits.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ based on the actors' power, exit options, and benefits from the exchange. Taxpayers see it as a snare due to financial burden and minimal control. EU universities view it as rope and positive with benefits, while the government perceives it as a scaffold to coordinate efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like UK students and EU Universities will have low directionality, while victims such as the taxpayers have a high directionality rating. The UK government’s d value is determined by its exit options and perceived advantages.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis highlights the scaffold nature of rejoining. The program may become a piton, if independent programs gain strength, and a rope, if cooperation increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_alternative_viability,
    'Will the UK be able to develop a viable domestic alternative to Erasmus+?',
    'Analysis of funding allocations, participation rates, and student satisfaction with domestic exchange programs.',
    'If a viable alternative emerges, the rejoining becomes a short term scaffold. If not, then rejoining can shift to a longer term rope or entangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_alternative_viability, empirical, 'Viability of UK domestic alternative to Erasmus+').

omega_variable(
    political_stability_eu_uk,
    'Will political relations between the UK and the EU remain stable enough to support long-term participation?',
    'Monitoring of trade agreements, diplomatic engagements, and public opinion polls in both regions.',
    'If political relations deteriorate, then rejoining could become a political piton (performative activity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_stability_eu_uk, conceptual, 'Stability of UK-EU political relations').

omega_variable(
    funding_sustainability,
    'Can sustainable funding be secured from the UK government for the rejoin into the Erasmus+ program, and what would that look like?',
    'Monitoring the UK government spending and public opinions.',
    'If the funding for the program dwindles, then rejoining becomes a political piton (performative activity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability, empirical, 'Sustainability of funding the rejoin into the Erasmus+ program').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(erasmus_rejoining_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eras_tr_t0, erasmus_rejoining_scaffold, theater_ratio, 0, 0.6).
narrative_ontology:measurement(eras_tr_t5, erasmus_rejoining_scaffold, theater_ratio, 5, 0.7).
narrative_ontology:measurement(eras_tr_t10, erasmus_rejoining_scaffold, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(eras_be_t0, erasmus_rejoining_scaffold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eras_be_t5, erasmus_rejoining_scaffold, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(eras_be_t10, erasmus_rejoining_scaffold, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(erasmus_rejoining_scaffold, resource_allocation).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, uk_eu_relations).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, uk_higher_education_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
