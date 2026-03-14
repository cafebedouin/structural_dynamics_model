% ============================================================================
% CONSTRAINT STORY: russian_soft_power_in_africa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_soft_power_in_africa, []).

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
 *   constraint_id: russian_soft_power_in_africa
 *   human_readable: Russian Soft Power Extraction in Africa
 *   domain: geopolitical/economic/cultural
 *
 * SUMMARY:
 *   Russian soft power in Africa represents a structural extraction mechanism
 *   embedded within a coordination narrative. Since the early 2010s, Russia
 *   has developed a systematic approach to influence in African states
 *   through military training, arms sales, resource partnerships, media
 *   platforms, and financial partnerships. The constraint operates as a
 *   tangled hybrid: genuine coordination functions exist (African states seek
 *   alternative great power partners to balance Western pressure; Russia
 *   provides military training and equipment that addresses real security
 *   needs) but are systematically exploited for asymmetric extraction
 *   (political alignment on UN votes, regulatory capture favoring Russian
 *   extractive interests, undermining of democratic institutions,
 *   disinformation targeting domestic opponents). The theater component
 *   (0.68) reflects that much of the soft power narrative invokes
 *   decolonization, anti-imperialism, and Cold War solidarity — performative
 *   rhetoric that structures perception of the engagement while actual
 *   benefit flows accrue to Russian state interests and extractive capital.
 *   The extractiveness trajectory shows steady accumulation: from 0.35
 *   (initial opportunistic engagement) through 0.48 (institutionalization of
 *   political coordination) to 0.58 (systematic institutional capture in
 *   resource-dependent states). Theater ratio follows a similar arc: initial
 *   framing as genuine partnership (0.52) progressing to elaborate
 *   performative infrastructure (0.68) as the actual coordination function
 *   decays relative to the extraction mechanism.
 *
 * KEY AGENTS:
 *   - Russian State Apparatus: Primary beneficiary (institutional/arbitrage) — captures geopolitical positioning, UN voting alignment, resource access, and strategic influence with exit options to redeploy resources elsewhere
 *   - Russian Extractive Capital: Primary beneficiary (powerful/arbitrage) — mining, oil, and mineral extraction companies gain preferential access to African resources and regulatory capture
 *   - Russian Military Contractors: Secondary beneficiary (institutional/constrained) — Wagner, Gazprom-linked entities, and paramilitaries expand operational footprint and market share
 *   - Resource-Dependent African States: Primary victim (powerless/trapped) — caught between Western institutional pressure and Russian debt dependency; lose policy autonomy through financing conditions and military dependence
 *   - African Civil Society and Independent Media: Secondary victim/constrained beneficiary (moderate/constrained) — some benefit from alternative funding sources but face suppression through disinformation, editorial capture, and intimidation
 *   - African Populations: Tertiary victim (powerless/trapped) — bear costs of institutional degradation, kleptocracy enabled by Russian support, regional destabilization from mercenary activity, and cultural erosion through disinformation
 *   - Western Institutional Complex: Competitor-victim (institutional/constrained) — experiences constraint as undermining of development conditioning, democracy promotion, and institutional leverage; constrained by need to compete for African partnerships
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_soft_power_in_africa, 0.58).
domain_priors:suppression_score(russian_soft_power_in_africa, 0.62).
domain_priors:theater_ratio(russian_soft_power_in_africa, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_soft_power_in_africa, extractiveness, 0.58).
narrative_ontology:constraint_metric(russian_soft_power_in_africa, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(russian_soft_power_in_africa, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_soft_power_in_africa, tangled_rope).
narrative_ontology:human_readable(russian_soft_power_in_africa, "Russian Soft Power Extraction in Africa").
narrative_ontology:topic_domain(russian_soft_power_in_africa, "geopolitical/economic/cultural").

domain_priors:requires_active_enforcement(russian_soft_power_in_africa).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_soft_power_in_africa, russian_state_interests).
narrative_ontology:constraint_beneficiary(russian_soft_power_in_africa, extractive_resource_companies).
narrative_ontology:constraint_beneficiary(russian_soft_power_in_africa, russian_military_contractors).
narrative_ontology:constraint_victim(russian_soft_power_in_africa, african_state_sovereignty).
narrative_ontology:constraint_victim(russian_soft_power_in_africa, african_democratic_institutions).
narrative_ontology:constraint_victim(russian_soft_power_in_africa, local_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-DEPENDENT AFRICAN STATE (SNARE) — Trapped by debt dependence on Russian financing, military support, and market access for primary commodities. Exit options are severely constrained: alternative financing sources impose transparency and democracy requirements incompatible with kleptocratic governance; Western partners condition aid on rights improvements. No exit; maximum extraction through political alignment requirements, regulatory capture by Russian interests, and loss of policy autonomy.
constraint_indexing:constraint_classification(russian_soft_power_in_africa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AFRICAN CIVIL SOCIETY AND MEDIA (TANGLED ROPE) — Constrained but not trapped. Benefits from Russian funding of alternative media outlets, cultural organizations, and educational exchanges that provide platforms independent of Western dominance. Simultaneously faces extraction: disinformation campaigns target domestic opponents, Russian funding creates dependency and editorial capture, and coordination undermines regional institutional capacity. High suppression (media intimidation, funding dependence) with genuine but asymmetric coordination function.
constraint_indexing:constraint_classification(russian_soft_power_in_africa, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RUSSIAN STATE ACTOR (ROPE) — Benefits from coordination of political influence, market access, and strategic positioning without significant cost. Experiences the constraint as pure coordination: supplying arms, training, media platforms, and financial incentives to achieve alignment on UN votes, resource access, and geopolitical positioning. Net beneficiary with arbitrage exit options (can reallocate resources to other regions if African returns decline).
constraint_indexing:constraint_classification(russian_soft_power_in_africa, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN INSTITUTIONAL COMPLEX (TANGLED ROPE) — Experiences Russian soft power as both coordination problem and extraction mechanism. Coordination function: Western institutions (IMF, World Bank, UN bodies) have built a system for development assistance, democratization support, and market integration that requires coordination among donors and alignment with recipient state policies. Extraction mechanism: Russian engagement extracts from this system by offering alternative paths (debt without reform conditions, alignment without governance requirements) that undermine Western institutional leverage while maintaining asymmetric resource flows favoring Russia.
constraint_indexing:constraint_classification(russian_soft_power_in_africa, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL LEGACY (PITON) — Performative residue of Soviet-era anti-Western positioning and non-aligned movement rhetoric. The constraint's theater includes invocation of decolonization narratives, sovereignty claims, and anti-imperialism framing that structure the soft power narrative. Much of the effectiveness is theatrical — reenacting Cold War rivalry and anti-Western positioning — rather than functional integration. Theater ratio high because the geopolitical positioning is largely for domestic and international audience consumption.
constraint_indexing:constraint_classification(russian_soft_power_in_africa, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, power asymmetries in the international system create immutable conditions where weak states must extract value from great power competition. Africa's geographic position, resource endowments, and structural powerlessness relative to global capital flows make it inherently subject to external power projection. The constraint appears natural — a necessary feature of global power distribution. However, the structural data reveals this as naturalization of contingent geopolitical arrangements: Russia's soft power is recent and policy-dependent, not a law of nature.
constraint_indexing:constraint_classification(russian_soft_power_in_africa, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_soft_power_in_africa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_soft_power_in_africa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_soft_power_in_africa, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_soft_power_in_africa, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_soft_power_in_africa, TR),
    TR >= 0.70.

:- end_tests(russian_soft_power_in_africa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine coordination value (Africa gains military alternatives to Western suppliers, financing without transparency conditions) alongside substantial asymmetric extraction (political alignment, institutional capture, resource access). The metric is not maximal (0.72) because some African states retain genuine agency — they are not universally trapped, and the relationship involves actual service delivery (training, arms, financing) rather than pure predation. Suppression (0.62): Moderate-high. Material barriers include debt dependency and military reliance. Institutional barriers include international sanctions on alternative partners, making exit costly. Cognitive barriers include disinformation and historical anti-Western framing that makes Russian partnership narratively appealing despite extraction. Theater ratio (0.68): High, reflecting that much of the soft power operates through narrative infrastructure (decolonization framing, anti-imperialism, Cold War solidarity) rather than functional institutional change. The performative component has increased over the measurement interval — as extraction has grown and actual coordination benefits have become more dubious, the theater investment has increased to maintain legitimacy narratives.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Russian institutional perspective (Rope: pure coordination) and the powerless African state perspective (Snare: pure extraction) is maximal. The same structural arrangement — Russia supplies arms, financing, and political support; African states align politically and grant resource access — is experienced as beneficial coordination from the beneficiary position and catastrophic extraction from the victim position. This gap is the diagnostic signal: when a constraint simultaneously classifies as Rope and Snare from different perspectives, the underlying mechanism is asymmetric extraction disguised as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and structural position relative to the extraction flow. Russian state actors: low d (0.15) via institutional + arbitrage (beneficiaries with exit options experience negative effective extraction). Resource-dependent African states: high d (0.92) via powerless + trapped (victims with no exit experience maximum extraction, f(d) ≈ 1.38). African civil society: moderate-high d (0.68) via moderate + constrained (mixed beneficiary/victim position with constrained but not eliminated exit options). Western institutional complex: moderate d (0.55) via institutional + constrained (competitor position with significant but limited exit options). Theater ratio accumulation is independently tracked and reflects Goodhart drift: as the actual coordination function weakens, the performative component strengthens to maintain legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   GEOPOLITICAL EXTRACTION: The constraint resolves mandatrophy by clarifying that Russian soft power is neither pure coordination (Rope) nor pure coercion (Snare), but a tangled hybrid where coordination functions exist (genuine military training, real financing alternatives) but are systematically exploited for asymmetric extraction (political alignment, regulatory capture, institutional degradation). The tangled_rope classification prevents both the Western institutional rationalization ('this is just coordination, Africa benefits from alternatives') and the simplistic victimology ('Africa is helpless'). The actual structure is: coordination is real, but extraction flows through it. Exit options exist (mobile/arbitrage for beneficiaries, constrained for moderate victims), but are expensive. Theater is high (0.68) but not dominant, indicating that the performative component matters but is not the primary mechanism. The suppression metric (0.62) reflects material barriers (debt, military dependence) plus institutional constraints (sanctions on alternatives) plus cognitive capture (disinformation). The perspectival gap between Rope and Snare is the key diagnostic: Africa is neither helpless nor in voluntary coordination, but in constrained extraction that offers some real benefits alongside asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_coercion_boundary,
    'Do Russian offers of financing and military support constitute genuine coordination (solving collective action problems) or pure coercion disguised as voluntary exchange?',
    'Counterfactual analysis: What would African states choose absent Russian options? Comparison of policy outcomes under Russian vs Western financing to assess whether states pursue own-interest policies or alignment policies.',
    'If genuine coordination: tangled_rope classification appropriate, suppression metric is overstated. If pure coercion: snare classification applies more broadly, extraction is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coercion_boundary, empirical, 'Whether Russian soft power offers coordination or coercion').

omega_variable(
    alternative_path_availability,
    'Are the alternative financing and alliance pathways Russia offers genuinely available to African states, or does the framing of alternatives exceed the actual capacity to exit Western institutional frameworks?',
    'Historical analysis of African states that have attempted to balance Russian and Western relationships; measurement of actual policy autonomy gained vs autonomy promised; tracking of states that attempted to shift away and faced economic consequences.',
    'If alternatives are real and accessible: exit_options for constrained agents should upgrade to mobile; suppression metric decreases; classification shifts away from snare. If alternatives are illusory: exit_options remain trapped; suppression confirmed; snare classification dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_path_availability, empirical, 'Whether alternative Russian pathways provide genuine exit options').

omega_variable(
    disinformation_effectiveness_mechanism,
    'Does Russian disinformation and media influence actually change political outcomes in African states, or does it primarily reinforce existing elite preferences and tribal divisions?',
    'Causal analysis of disinformation campaigns and policy changes; counterfactual assessment of whether elites would have pursued same policies absent Russian messaging; measurement of disinformation reach vs actual behavioral change.',
    'If effective (changes outcomes): extraction mechanism is real, suppression metric justified. If reinforcement only: suppression is overstated; theater component increases; piton classification becomes more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disinformation_effectiveness_mechanism, empirical, 'Whether disinformation campaigns cause policy changes or reinforce existing preferences').

omega_variable(
    russian_dependency_sustainability,
    'Can Russian soft power extraction persist indefinitely given Russia''s economic constraints and sanctions pressure, or is it a temporally bounded strategy dependent on specific geopolitical windows?',
    'Projection of Russian financial and military capacity over 10-20 year horizon; analysis of sanctions impact on ability to fund African engagements; assessment of strategic value ROI for Russian investment.',
    'If unsustainable: scaffold classification becomes appropriate (temporary support with sunset); extracted value represents temporary redistribution rather than permanent institutional change. If sustainable: snare classification appropriate; permanent structural lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_dependency_sustainability, empirical, 'Whether Russian soft power engagement is sustainably resourced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_soft_power_in_africa, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rspa_tr_t0, russian_soft_power_in_africa, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rspa_tr_t5, russian_soft_power_in_africa, theater_ratio, 5, 0.62).
narrative_ontology:measurement(rspa_tr_t10, russian_soft_power_in_africa, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rspa_be_t0, russian_soft_power_in_africa, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rspa_be_t5, russian_soft_power_in_africa, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rspa_be_t10, russian_soft_power_in_africa, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_soft_power_in_africa, resource_allocation).
narrative_ontology:affects_constraint(russian_soft_power_in_africa, western_development_conditionality).
narrative_ontology:affects_constraint(russian_soft_power_in_africa, chinese_debt_dependency_africa).
narrative_ontology:affects_constraint(russian_soft_power_in_africa, african_state_capacity_degradation).

% DUAL FORMULATION NOTE:
% Russian soft power in Africa is part of a broader constraint family involving great power competition for African resources and political alignment. Upstream constraints include Western development conditionality frameworks and Chinese Belt and Road infrastructure financing. Downstream effects include institutional capacity degradation in African states and regional destabilization. The extractiveness values differ: Western conditionality focuses on institutional reform requirements (ε ≈ 0.35); Russian soft power emphasizes political alignment and resource access (ε ≈ 0.58); Chinese financing emphasizes debt accumulation (ε ≈ 0.52). All three are tangled hybrids involving coordination plus extraction, creating competing extraction mechanisms that fragment African institutional coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_soft_power_in_africa, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
