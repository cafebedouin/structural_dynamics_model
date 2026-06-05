% ============================================================================
% CONSTRAINT STORY: ulysses_chp08
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   On lunchtime in Dublin (June 16, 1904), Leopold Bloom moves through the
 *   city observing food—consumption, hunger, the predatory nature of
 *   existence. The constraint is the system of food distribution that
 *   coordinates supply across the city while simultaneously extracting
 *   maximum surplus from those with the least, naturalizing this arrangement
 *   as inevitable (predatory nature, food chains, survival of the fittest).
 *   Bloom experiences the constraint acutely: aware of his own consumption
 *   privileges, unsettled by the hunger of beggars, morally conscious of the
 *   violence embedded in appetite. The Lestrygonian episode (Chapter 8 of
 *   Ulysses) presents this constraint from multiple structural positions: the
 *   beggar sees only exclusion (snare); Bloom sees mixed coordination and
 *   discomfort (tangled rope); restaurateurs see profitable supply (rope);
 *   charitable institutions see a humanitarian problem requiring discipline
 *   (tangled rope); labor organizers see a temporary system awaiting
 *   overthrow (scaffold); the cultural narrative of predatory nature sees
 *   inevitable law (piton, with theater masking contingency); the analytical
 *   observer risks naturalizing social inequality as immutable thermodynamic
 *   fact (false mountain). The constraint's extractiveness (0.38) and
 *   suppression (0.48) mark it as a genuine tangled rope: it does solve a
 *   real coordination problem (food must be distributed somehow) but does so
 *   by concentrating access and enforcing inequality. The rising theater
 *   ratio (0.35 → 0.55 over the interval) reflects increasing naturalization
 *   — as Darwin's ideas diffuse through Victorian culture, the social
 *   mechanisms of food stratification become cloaked in biological
 *   inevitability.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Middle-class consumer (moderate/constrained) — benefits from food access but morally troubled by inequality; structurally between beneficiaries and victims
 *   - Hungry Beggar / Laboring Poor: Excluded from food despite physical proximity (powerless/trapped) — primary victims bearing full cost of artificial scarcity
 *   - Restaurateurs and Food Merchants: Proprietors (institutional/arbitrage) — primary beneficiaries; coordinate food distribution AND extract surplus through scarcity
 *   - Poor Law Administrators and Charity Organizations: Organized institutional response (organized/constrained) — coordinate survival while enforcing class discipline and dignity loss
 *   - Labor Movement and Socialist Critique: Organized political force (organized/mobile) — see food distribution as temporary and replaceable
 *   - Victorian Naturalistic Culture: Intellectual discourse (institutional/arbitrage) — naturalizes predation as inevitable law, providing legitimation theater
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing food distribution constraints as natural laws rather than social contingencies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp08, 0.38).
domain_priors:suppression_score(ulysses_chp08, 0.48).
domain_priors:theater_ratio(ulysses_chp08, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp08, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp08, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ulysses_chp08, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp08, tangled_rope).
narrative_ontology:human_readable(ulysses_chp08, "The Lestrygonian Food Chain (Lunchtime Dublin)").
narrative_ontology:topic_domain(ulysses_chp08, "social/economic/biological").

domain_priors:requires_active_enforcement(ulysses_chp08).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp08, proprietors_restaurateurs).
narrative_ontology:constraint_beneficiary(ulysses_chp08, food_merchants).
narrative_ontology:constraint_victim(ulysses_chp08, laboring_poor).
narrative_ontology:constraint_victim(ulysses_chp08, homeless_destitute).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABORING POOR / DESTITUTE (SNARE) — Physically present in the city but excluded from food. No purchasing power, no escape from hunger, no alternative. d≈0.93, f(d)≈1.40, σ=0.8 → χ≈0.52. Pure extraction: constraint extracts the dignity of presence without access.
constraint_indexing:constraint_classification(ulysses_chp08, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BLOOM / MIDDLE-CLASS CONSUMER (TANGLED ROPE) — Benefits from food availability and social coordination (restaurants, markets, eating customs). But constrained by money, social class, digestive concerns, moral unease about predation. The constraint coordinates food distribution AND extracts surplus value through artificial scarcity. d≈0.65, f(d)≈0.95, σ=0.8 → χ≈0.29. Mixed experience: coordination mechanism for some, extraction for consciousness of inequality.
constraint_indexing:constraint_classification(ulysses_chp08, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: RESTAURATEURS / FOOD MERCHANTS (ROPE) — Primary beneficiary. The constraint (food scarcity, class-based access, hunger as enforcement mechanism) coordinates food distribution across Dublin AND provides profit margins. Sees it as pure coordination: customers get fed, businesses thrive. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.04. Negative effective extraction = net beneficiary perspective.
constraint_indexing:constraint_classification(ulysses_chp08, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: POOR LAW / CHARITY SYSTEM (TANGLED ROPE) — Organized institutional response to hunger that both coordinates survival (soup kitchens, workhouses, charity) AND enforces class discipline. The system has coordination function (prevents mass starvation) but also extraction (forces labor, denies dignity, maintains inequality). Requires active enforcement of eligibility criteria. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.26. Mixed: coordination mechanism with asymmetric extraction embedded.
constraint_indexing:constraint_classification(ulysses_chp08, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR / SOCIALIST MOVEMENT (SCAFFOLD) — Sees food distribution system as temporary and replaceable by rational allocation. The constraint's extraction mechanism (artificial scarcity, class-based access) has a sunset: socialist redistribution or labor-organized supply chains. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.20. Low effective extraction because this agent sees an organized exit path and a future without the constraint.
constraint_indexing:constraint_classification(ulysses_chp08, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: VICTORIAN NATURALISM / EVOLUTIONARY METAPHOR (PITON) — The constraint is cloaked in biological inevitability: 'nature is predatory, food chains are natural law, survival of the fittest.' This theatrical naturalization (theater_ratio=0.55) maintains the system through intellectual inertia — naturalizing social inequality as immutable law. The metaphor is degraded — its actual descriptive power is weak (social distribution is not governed by Darwinian selection) — but persists for legitimation. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(ulysses_chp08, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN?) — From a civilizational perspective, food scarcity, energy limitations, and metabolic requirements are natural constraints. Some form of food distribution allocation is inevitable. But the specific form — class-based access, artificial scarcity, moral theater around predation — is contingent. The engine will flag this as a false summit: ε=0.38, suppression=0.48, theater=0.55 do not match the mountain signature (would require ε≤0.25, suppression≤0.05, accessibility_collapse≥0.85). The natural law reading misattributes social contingency to physical necessity.
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
    constraint_indexing:constraint_classification(ulysses_chp08, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp08, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp08_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The food distribution system in 1904 Dublin coordinates genuine supply needs but does so by creating artificial scarcity and class-based access. Not maximal extraction (suppression is high but not total — some mobility exists for working-class people, some charity operates, labor organizing is possible). The value reflects that the system solves a real problem (food must reach consumers) while simultaneously extracting through inequality. Suppression (0.48): Moderate-high. Significant barriers: hunger itself is a suppression mechanism (it constrains behavior, drives desperation), class barriers to restaurants and markets, lack of purchasing power, Poor Law eligibility restrictions, social shame around destitution. But suppression is not totalizing — charity exists, underground food economies operate, labor organizing suggests alternatives. Theater ratio (0.55): Moderate. The predatory-nature metaphor is substantially theatrical (it performs legitimation work without descriptive accuracy — actual food distribution is not governed by Darwinian selection). But it's not pure theater (people genuinely believe the metaphor, it shapes policy). The ratio reflects that theater has increased over time as naturalistic rhetoric diffuses.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The beggar sees pure extraction (snare): excluded by class, hunger enforced, no alternatives. Bloom sees coordination with moral cost (tangled rope): the system works to feed the city, but at the price of inequality and dignity. Restaurateurs see pure coordination (rope): market mechanism, supply and demand, profit incentive aligns with service. The charity system sees mixed coordination and paternalistic extraction (tangled rope): prevents starvation but enforces workhouse discipline. The labor movement sees temporary extraction awaiting replacement (scaffold): the system is contingent, not inevitable. The naturalistic culture sees immutable law (piton, theatrical): evolution, nature, predation — biologizes social arrangements. The analytical observer risks seeing thermodynamic necessity (false mountain): energy constraints are real, but the specific distribution mechanism is contingent. The engine's false summit detector catches this: ε=0.38 (too high for mountain), theater=0.55 (below piton gate), no natural law signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Laboring poor / destitute: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — cannot escape hunger, no purchasing power, no mobility. Bloom: Both beneficiary (can eat) and victim (morally conscious of cost) + constrained exit → d≈0.65, f(d)≈0.95. Mixed position; experiences extraction through consciousness. Restaurateurs: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary — constraint rewards them. Poor Law system: Both beneficiary (extracts labor discipline) and victim (constrained to humanitarian mission) + constrained → d≈0.52, f(d)≈0.68. Mixed institutional position. Labor movement: Victim (constrained by system) + mobile (can organize alternative) → d≈0.45, f(d)≈0.45. Low effective extraction because exit path exists. Victorian naturalism: Beneficiary (legitimates inequality) + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification driven by theater, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of false naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification resolves the apparent mandatrophy by decomposing the system into coordination and extraction. The food distribution system in Dublin 1904 is NOT pure coordination (rope) because it extracts through artificial scarcity and class-based access. It is NOT pure extraction (snare) because it genuinely coordinates food supply across a complex city and solves a real allocation problem. It is tangled rope because BOTH functions are present: coordination (restaurants, markets, supply chains) AND asymmetric extraction (hunger enforcement, artificial scarcity, class barriers). The rising theater ratio over the interval (0.35 → 0.55) indicates increasing naturalization through Darwinian rhetoric — the coordination function is genuine, but it is being cloaked in pseudo-biological inevitability, which strengthens the extraction by delegitimizing alternatives. Bloom's moral consciousness is the key diagnostic: he can see both functions simultaneously, which is precisely the tangled rope perspective. A pure rope observer would see only coordination benefit; a pure snare observer would see only extraction. Bloom's discomfort reveals the hybrid. The mandatrophy is resolved: this is legitimately a tangled rope, not a false natural law (mountain) naturalizing social contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predation_metaphor_causality,
    'Does Victorian naturalistic predation metaphor (food chains, survival of the fittest) legitimize the extraction, or does the extraction exist independently and the metaphor merely describe it?',
    'Historical comparison: food distribution systems in societies without Darwinian rhetoric vs those with it. If rhetoric is causally inert, systems should be structurally similar; if rhetoric sustains extraction, societies without it should show lower suppression and higher mobility.',
    'If metaphor is causal: the constraint is partly discursive (can be delegitimized by changing narrative). If metaphor is post-hoc: the constraint is purely structural (requires material intervention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predation_metaphor_causality, conceptual, 'Whether predation metaphor legitimizes or merely describes the extraction').

omega_variable(
    scarcity_authenticity,
    'Is Dublin''s food scarcity (1904) genuine (insufficient food production) or artificial (artificial hoarding, profit maximization)?',
    'Agricultural production data; price markups above cost; comparison to food supply in less stratified societies of equal production capacity.',
    'If genuine scarcity: extraction is coordination failure, constraint closer to Rope. If artificial: extraction is predatory, constraint closer to Snare. This determines mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_authenticity, empirical, 'Whether food scarcity in Dublin 1904 is genuine or artificially maintained').

omega_variable(
    bloom_moral_consciousness_function,
    'Does Bloom''s moral discomfort with predation serve as a pressure valve (releasing tension without changing system) or as incipient resistance (seed of future change)?',
    'Textual analysis of whether Bloom''s consciousness produces any material action; historical follow-up on whether middle-class moral unease correlates with later food system reforms.',
    'If pressure valve: consciousness is theaterized (piton element strengthened). If resistance seed: consciousness enables scaffold perspective (organized change becomes possible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bloom_moral_consciousness_function, preference, 'Whether middle-class moral consciousness enables or merely decorates the extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp08, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lestry_tr_t0, ulysses_chp08, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lestry_tr_t5, ulysses_chp08, theater_ratio, 5, 0.48).
narrative_ontology:measurement(lestry_tr_t10, ulysses_chp08, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(lestry_be_t0, ulysses_chp08, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lestry_be_t5, ulysses_chp08, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(lestry_be_t10, ulysses_chp08, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp08, resource_allocation).
narrative_ontology:boltzmann_floor_override(ulysses_chp08, 0.25).
narrative_ontology:affects_constraint(ulysses_chp08, dublin_labor_market_1904).
narrative_ontology:affects_constraint(ulysses_chp08, poor_law_workhouse_system).

% DUAL FORMULATION NOTE:
% The Lestrygonian food chain is downstream of broader economic inequality and upstream of labor mobilization. The constraint family includes the Dublin labor market (showing why workers are hungry despite producing food) and the Poor Law system (showing how institutional response both mitigates and reinforces extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
