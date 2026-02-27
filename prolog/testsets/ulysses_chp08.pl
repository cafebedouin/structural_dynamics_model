% ============================================================================
% CONSTRAINT STORY: ulysses_chp08
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp08, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp08
 *   human_readable: The Lestrygonian Food Chain (Lunchtime Dublin)
 *   domain: social/economic/biological
 *
 * SUMMARY:
 *   Leopold Bloom's lunchtime wandering through Dublin in the Lestrygonian
 *   episode (Chapter 8 of Ulysses) presents the city's food economy as a
 *   predatory hierarchy. Bloom observes restaurant windows, passing food
 *   vendors, eating crowds, and experiences the tension between biological
 *   hunger and economic constraint. The constraint operates at multiple
 *   scales: individual (Bloom's own middle-class sufficiency vs working-class
 *   deprivation), social (merchant class extracting from those with limited
 *   purchasing power), and conceptual (the narrator's slippage between
 *   'Lestrygonian giants' devouring weaker creatures and Dublin's
 *   class-stratified food access). The food chain metaphor naturalizes what
 *   is fundamentally a social/economic extraction mechanism. Extractiveness
 *   (0.58) reflects that the merchant class captures substantial surplus from
 *   those compelled to consume at subsistence levels. Suppression (0.48)
 *   reflects partial suppression through wage structure and market position —
 *   complete suppression would require active enforcement; instead, the
 *   system relies on structural position and biological necessity. Theater
 *   ratio (0.35) is relatively low, indicating that the extraction mechanism
 *   is primarily material rather than performative, though social ritual does
 *   play a role in maintaining class differentiation.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Analytical observer (moderate/mobile) — middle-class consciousness that sees predation without experiencing it acutely; capable of sympathetic imagination but structurally protected from the trap
 *   - Merchant Class & Restaurant Owners: Primary beneficiaries (institutional/arbitrage) — capture surplus from wage-constrained consumers; enjoy high exit options and price-setting power
 *   - Working Poor & Unemployed: Primary victims (powerless/trapped) — compelled to consume at subsistence or less; no exit options; bear full extraction cost
 *   - Clerks & Shop Assistants: Secondary victims (moderate/constrained) — employed but with limited wage; experience mixed coordination (employment) and extraction (insufficient consumption)
 *   - Food Distribution System: Institutional mechanism (institutional/arbitrage) — enables extraction by maintaining supply scarcity and price hierarchy
 *   - Dublin Social Order: Generational constraint (institutional/arbitrage) — maintains class differentiation through performative ritual and deference patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp08, 0.58).
domain_priors:suppression_score(ulysses_chp08, 0.48).
domain_priors:theater_ratio(ulysses_chp08, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp08, extractiveness, 0.58).
narrative_ontology:constraint_metric(ulysses_chp08, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ulysses_chp08, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp08, snare).
narrative_ontology:human_readable(ulysses_chp08, "The Lestrygonian Food Chain (Lunchtime Dublin)").
narrative_ontology:topic_domain(ulysses_chp08, "social/economic/biological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp08, merchant_class).
narrative_ontology:constraint_beneficiary(ulysses_chp08, restaurant_owners).
narrative_ontology:constraint_beneficiary(ulysses_chp08, food_vendors).
narrative_ontology:constraint_victim(ulysses_chp08, working_poor).
narrative_ontology:constraint_victim(ulysses_chp08, unemployed_laborers).
narrative_ontology:constraint_victim(ulysses_chp08, marginalized_hunger).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUNGRY UNEMPLOYED (SNARE) — Completely trapped. Must eat to survive but lack resources to purchase food. Dublin's lunchtime food system extracts dignity and leisure from those who cannot afford participation. Observation of abundance creates psychological extraction as well as material deprivation.
constraint_indexing:constraint_classification(ulysses_chp08, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WORKING POOR (SNARE) — Trapped by biological necessity and wage structure. Must consume at survival minimum while watching merchant class consume lavishly. Extraction occurs through wage suppression that forces consumption rationing. No exit — cannot stop eating, cannot leave Dublin, cannot escape class position within the observed interval.
constraint_indexing:constraint_classification(ulysses_chp08, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MERCHANT CLASS & VENDORS (ROPE) — Benefits from food distribution infrastructure. Restaurants, butchers, grocers experience the constraint as coordination: organizing supply chains, managing inventory, capturing consumer attention. Net beneficiaries. High exit options through price discrimination and market position.
constraint_indexing:constraint_classification(ulysses_chp08, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: CLERK OR SHOP ASSISTANT (TANGLED ROPE) — Constrained but not powerless. Employed but with limited wage. Experiences both the coordination function (retail distribution provides employment) and extraction (wage insufficient for leisurely dining). Can exit through career mobility or relocation but at biographical cost. Mixed experience: extraction tempered by employment stability and incremental advancement possibility.
constraint_indexing:constraint_classification(ulysses_chp08, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: DUBLIN SOCIAL ORDER (PITON) — Institutional degradation through performative maintenance. Class stratification in food access persists through social ritual rather than active enforcement: the 'right' restaurants for the 'right' classes; deference to merchant-class consumption. Theater ratio high (0.35 suggests lower theater than verification example, but still performative — social codes matter more than economic logic). The constraint is inertial: it persists because alternatives haven't been built, not because active suppression is necessary.
constraint_indexing:constraint_classification(ulysses_chp08, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, predation in the food chain is a biological law: organisms must consume resources; stronger organisms secure advantage. This perspective risks naturalizing social hierarchy as inherited from animal predation. The constraint appears immutable: 'nature' determines hierarchy. However, the structural data contradicts the mountain classification — actual enforcement mechanisms are social and economic, not biological. The 'lestrygonian' framing naturalizes contingent institutional arrangements.
constraint_indexing:constraint_classification(ulysses_chp08, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp08_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp08, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp08, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp08, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp08, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp08_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The merchant class and food vendors extract substantial surplus from those whose biological needs force consumption despite limited purchasing power. Bloom's observation of feast windows and eating crowds emphasizes the gap between appetite and ability. The extractiveness rises over the interval (0.42 → 0.58) as industrial wage suppression intensifies — workers' purchasing power declines while merchant class consolidates food distribution. This is not maximally extractive because some material goods are accessible (bread, basic subsistence) — the trap is not starvation but deprivation relative to visible abundance. Suppression (0.48): Moderate. Suppression operates through wage structure (employers set low wages) and market position (merchant class controls pricing). But suppression is not backed by explicit coercion — workers exit the trap would require coordination to withhold labor or organize collective purchasing. The system appears 'natural' (market forces) rather than enforced (police, law). This natural appearance is part of the trap: structural suppression feels like individual failure. Theater ratio (0.35): Low-moderate. The extraction is primarily material rather than performative. Bloom's observation focuses on actual consumption differences, not social ritual around consumption. However, some theater exists: the 'right' restaurants for 'right' classes, deference to merchant-class diners, social shame around hunger. The theater rises slightly over the interval as industrial society develops more elaborate class performance.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival gap between beneficiaries and victims. The merchant class sees coordination and profit opportunity (Rope). Clerks and shop assistants see mixed experience: employment stability tempered by wage constraint (Tangled Rope). The working poor and unemployed see pure extraction with no escape (Snare). Bloom's analytical position risks the mountain misclassification: reading predation as 'natural law' inherited from animal hierarchy naturalizes what is fundamentally a social/economic arrangement. The piton perspective (Dublin social order persisting through ritual and inertia) identifies the constraint as institutionally maintained but degraded — no one believes the system is just, yet it persists. This gap between description (everyone sees predation) and naturalization (some perspectives treat it as inevitable) is diagnostic of the mandatrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from declared beneficiaries/victims and exit options. Merchant class (institutional/arbitrage) experience low or negative d — they benefit from the constraint, have high exit options, and structure their environment favorably. Working poor (powerless/trapped) experience high d approaching 1.0 — they are victims, have zero exit options, and bear extraction fully. Clerks (moderate/constrained) experience moderate d around 0.65 — mixed position with some employment benefit (coordination) but constrained mobility and insufficient wages (extraction). Bloom's analytical position (analytical/analytical) experiences d ≈ 0.72 — he observes the full structure but is not caught in the trap. The sigmoid f(d) translates these d values into experienced extractiveness modifiers: beneficiaries see rope (low chi), victims see snare (high chi), moderates see tangled rope (mixed chi), analysts see potential mountain (but the structural data refutes the natural law framing).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED through perspectival decomposition. The apparent contradiction is whether the Lestrygonian food chain is (a) a natural biological law (Mountain from civilizational view) or (b) a social extraction mechanism (Snare from victim view). The resolution: both are accurate descriptions of different perspectives. The mountain classification is a FALSE SUMMIT — it naturalizes contingent institutional arrangements. The snare classification is structurally correct — extractiveness, suppression, and victim status are all present. The scaffold and piton classifications reveal the constraint's vulnerability: open-science parallels would be collective purchasing systems, cooperative food distribution, wage negotiation. The constraint persists through institutional inertia (piton) and performative naturalization (false mountain), not because it is immutable. This mandatrophy resolution shows that 'natural law' framing of social hierarchies is the characteristic error mode of powerless observation: the observer from the civilizational analytical position risks misclassifying institutional choice as biological necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_social_predation,
    'Is the Lestrygonian food chain a natural biological constraint or a socially constructed economic one?',
    'Comparative analysis of food access systems across cultures and time periods; identification of whether scarcity or distribution mechanisms determine hunger; historical counterfactuals',
    'If biological: constraint is Mountain (extractiveness intrinsic to human survival). If social: constraint is Snare (extractiveness is institutional choice). This omega determines whether the system is reformable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_vs_social_predation, conceptual, 'Whether predation is biological law or social construction').

omega_variable(
    collective_action_threshold,
    'At what population density or organization level can the powerless coordinate to alter the food chain structure?',
    'Historical analysis of food riots, collective wage negotiation, collective purchasing; identification of critical mass for exit from trapped status',
    'If threshold low: powerless can shift to organized power atom (snare becomes scaffold or tangled rope). If threshold high: trap persists as structural feature. Determines whether ''trapped'' exit option is permanent or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Coordination threshold for powerless escape from food chain trap').

omega_variable(
    consumption_visibility_necessity,
    'Does the psychological extraction (humiliation of watching merchant abundance) require visible difference, or is extraction purely from material deprivation?',
    'Historical comparison: hidden hunger vs visible class difference; impact analysis of rationing systems vs inequality systems on reported well-being',
    'If visibility necessary: extraction includes performative theater. If material only: extractiveness is lower. Affects theater_ratio and suppression values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumption_visibility_necessity, empirical, 'Whether psychological extraction requires visible consumption differences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp08, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lest_tr_t0, ulysses_chp08, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lest_tr_t5, ulysses_chp08, theater_ratio, 5, 0.32).
narrative_ontology:measurement(lest_tr_t10, ulysses_chp08, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(lest_be_t0, ulysses_chp08, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lest_be_t5, ulysses_chp08, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(lest_be_t10, ulysses_chp08, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp08, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp08, dublin_wage_suppression).
narrative_ontology:affects_constraint(ulysses_chp08, victorian_class_stratification).
narrative_ontology:affects_constraint(ulysses_chp08, consumption_visibility_display).

% DUAL FORMULATION NOTE:
% The Lestrygonian food chain decomposes into three linked constraints: (1) wage suppression mechanisms (economic), (2) class stratification enforcement (social/ritual), (3) visibility of consumption differences (psychological). Each has different ε. This story addresses the integrated constraint — how the three mechanisms couple to create the predatory food chain structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp08, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
