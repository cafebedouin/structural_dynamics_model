% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Climate Adaptation Priority: Immediate Resilience Investment with Unequal Protection
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response commits to immediate
 *   large-scale investment in resilience infrastructure ($540B annually for
 *   universal protection) while accepting that atmospheric warming is now
 *   inevitable given locked-in emissions. This reading prioritizes protecting
 *   vulnerable populations from climate impacts over reducing future warming
 *   through mitigation or transforming growth-dependent economic systems. The
 *   constraint exhibits tangled_rope structure: genuine coordination function
 *   (mobilizing urgent protective infrastructure for at-risk populations)
 *   coexists with asymmetric extraction (North-South financing gap of $350B
 *   annually, dependence of developing nations on wealthy-nation capital and
 *   terms, perpetuation of protection disparities based on fiscal capacity).
 *   The reading naturalizes the inevitability premise through a false-summit
 *   framing — treating 'warming is unavoidable, so adapt' as a law of physics
 *   rather than a policy choice that benefits institutional actors
 *   controlling adaptation capital. The constraint operates between three
 *   structural positions: (1) wealthy nations and finance institutions
 *   (beneficiaries, arbitrage exit), (2) developing nations and vulnerable
 *   populations (victims, trapped/constrained exit), and (3) organized
 *   adaptation sector (both benefits from mobilization and constrained by
 *   changing climate justice narratives). The temporal trajectory shows
 *   increasing extraction and suppression as the financing gap widens and
 *   more populations exhaust local adaptive capacity.
 *
 * KEY AGENTS:
 *   - Wealthy nations (North America, Western Europe, Japan, Australia): Institutional beneficiaries with arbitrage exit. Control capital allocation, set financing terms and conditionality, benefit from stability of market access and geopolitical predictability. Experience constraint as coordination mechanism.
 *   - Developing nations (Global South, least-developed countries): Primary victims with constrained exit. Bear fiscal burden of adaptation investment relative to GDP; dependent on external capital; limited negotiating power over terms. Experience mixed coordination-extraction.
 *   - Climate-vulnerable populations (small island states, sub-Saharan Africa, South Asia deltas, drylands): Powerless victims with trapped exit. Cannot migrate without losing livelihood; depend on government allocation of adaptation resources; geographic exposure is immutable. Experience pure extraction or incomplete coordination.
 *   - Adaptive capacity service providers (engineering firms, green infrastructure vendors, climate tech specialists, development banks): Organized beneficiaries with constrained exit. Benefit from mobilization demand; constrained by regulatory frameworks and climate justice scrutiny. Experience mixed coordination-extraction.
 *   - Analytical observers and policy framers (climate scientists, development economists, resilience experts): Identity-locked at analytical position. Career and epistemic authority fused with adaptation-priority framing; structurally mobile but identity-trapped; capable of recognizing extraction while unable to exit the frame.
 *   - Future generations: Powerless victims with trapped exit (temporal). Inherit adapted infrastructure locked into current climate baselines and higher ultimate warming; their adaptation costs rise as the warming target shifts upward.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.58).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Climate Adaptation Priority: Immediate Resilience Investment with Unequal Protection").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1').
narrative_ontology:cs_kernel_codification('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', distributed).
narrative_ontology:cs_authority_grounding('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', extraction).
narrative_ontology:cs_interpretation_layer_present('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1').
narrative_ontology:cs_reading_relation('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', foundational, locked_in_warming_unavoidability).
narrative_ontology:cs_axiom_status(locked_in_warming_unavoidability, holdable).
narrative_ontology:cs_axiom_grounding('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', locked_in_warming_unavoidability, empirically_contingent).
narrative_ontology:cs_axiom('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', foundational, capital_intensive_protection_imperative).
narrative_ontology:cs_axiom_status(capital_intensive_protection_imperative, holdable).
narrative_ontology:cs_axiom_grounding('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', capital_intensive_protection_imperative, instrumental).
narrative_ontology:cs_reference_frame('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', capital_mobilization_for_vulnerable_protection).
narrative_ontology:cs_drift_state('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', post_empirical_warming_acceleration_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a17446e-d6a3-430a-a9f8-77e2d4c5f1e1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, adaptive_capacity_service_providers).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_investors).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPROTECTED VULNERABLE POPULATION (SNARE) — Trapped by geographic exposure, fiscal inability to fund local adaptation, and dependence on global capital flows controlled by wealthy nations. Cannot exit adaptation through migration or relocation without losing livelihood and community. Experiences pure extraction: bears climate costs while locked out of protective infrastructure financing. Maximum extraction without coordination benefit.
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION GOVERNMENT (TANGLED ROPE) — Faces constraint requiring immediate capital investment ($540B annually for universal protection globally; nations' proportional share typically 5-20% of GDP) while fiscal capacity is limited by debt servicing and existing development needs. Constrained exit: can build some local adaptive capacity but cannot fully protect without external financing; depends on terms set by wealthy nations and multilateral institutions. Mixed: coordination benefit (receives technical support, climate finance, knowledge transfer) alongside asymmetric extraction (financing gap, conditionality, structural dependence on external terms).
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY NATION & CLIMATE FINANCE INSTITUTION (ROPE) — Institutional actor with exit option (arbitrage: can allocate capital elsewhere, can negotiate favorable terms, can exit climate finance commitments politically). Experiences the constraint as coordination: mobilizing adaptation finance enables continued stable market access, prevents climate-driven migration, reduces geopolitical instability. Net beneficiary through control of capital flows, conditionality-setting power, and reputational positioning as climate leader. Extraction runs away from this agent.
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED ADAPTATION SECTOR (TANGLED ROPE) — Coalition of engineering firms, green infrastructure providers, climate finance specialists, and adaptive technology vendors. Benefits from the immediate investment requirement (creates large market for dikes, irrigation systems, early warning infrastructure, resilience bonds) — this is a genuine coordination function, mobilizing capital for urgent protective measures. But also experiences constrained exit: regulatory requirements, carbon accountability frameworks, and climate justice narratives increasingly question the sufficiency of adaptation-only approaches; sector's dependency on wealthy-nation demand means constrained autonomy. Mixed: coordination (mobilizes necessary protective infrastructure) with asymmetric advantage (concentrated in wealthy-nation service providers, excludes local-capacity engineering in developing nations).
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / ADAPTATION-COMMITTED FRAMERS (TANGLED ROPE with identity_locked exit) — Analysts and policy framers who have invested their professional identities in the adaptation-priority framing: climate scientists warning of unavoidable warming, economists modeling adaptation cost curves, development policy experts advocating for resilience investment. Structurally mobile (could shift to mitigation or degrowth framings; empirical evidence supports multi-pathway approaches) but identity-locked: decades of career trajectory, publication record, institutional positioning, and epistemic authority are fused with the adaptation frame. Cannot exit without becoming a different professional. Experiences this constraint as a genuine coordination mechanism (mobilizing protective infrastructure) while theoretically capable of recognizing it as extractive (perpetuating inequality, deferring emissions reductions). The perspectival gap reveals the identity lock: analytical capacity to see the constraint's asymmetry is suppressed by identity fusion with the adaptation frame.
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: FALSE SUMMIT — INEVITABLE WARMING AS NATURAL LAW (MOUNTAIN) — This perspective naturalizes the reading's core premise: 'accepting temperature rise as inevitable' is reframed as a law of physics rather than a policy choice. The perspective holds that given locked-in emissions and inertia in climate systems, warming is immutable physical fact and we can only adapt. However, the structural data contradicts mountain classification: wealthy nations benefit from accepting inevitability (reduces pressure to invest in expensive mitigation); the reading's architecture requires active enforcement (financing terms, project oversight, conditionality); vulnerability is not uniformly distributed by natural law but by capital access. The engine's false summit detector will identify this as naturalization of a contingent institutional choice.
constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_action__adaptation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The adaptation-priority reading extracts asymmetric benefit from developing nations and vulnerable populations through (1) capital dependence: creating $350B annual financing gap that transfers wealth from South to North via conditionality and return on adaptation investment; (2) temporal extraction: deferring emissions reductions that would lower ultimate climate impact, thereby shifting adaptation costs to future generations; (3) structural dependence: locking vulnerable nations into perpetual reliance on wealthy-nation technology and capital for continued protection. The reading is not pure extraction because genuine coordination exists: resilience infrastructure provides real protective value, technical knowledge transfer occurs, and the constraint does address urgent protective needs. But the coordination benefit is distributed unequally — wealthy nations and adaptation service providers capture larger gains than vulnerable populations. Suppression (0.62): Moderate-high and increasing. Suppression mechanisms include (1) fiscal barriers: high upfront capital requirements exclude developing nations from self-determined adaptation pathways; (2) epistemic barriers: the 'warming is inevitable' framing suppresses questions about whether earlier mitigation could have prevented the need for such large-scale adaptation; (3) political barriers: climate finance is conditioned on governance compliance, privatization, and market-based mechanisms, restricting autonomy; (4) temporal barriers: the immediate investment requirement forecloses deliberation about degrowth or transformation alternatives. The measurement trajectory (0.50 → 0.71) reflects that as the financing gap widens and climate impacts accelerate, suppression intensifies — vulnerable populations face harder choices between accepting external conditionality and remaining unprotected. Theater ratio (0.48, moderate and stable): The adaptation-priority reading involves significant performative elements — climate adaptation commits are often pledged but underfunded, resilience projects undergo planning without implementation, and adaptation narratives emphasize readiness while infrastructure remains incomplete. But the theater is not dominant: unlike piton constraints, there is real functional adaptation activity underway. The stable theater trajectory reflects that performative and functional components coexist without resolution.
 *
 * PERSPECTIVAL GAP:
 *   The adaptation-priority reading exhibits maximum perspectival divergence across structural positions. Wealthy nations and finance institutions classify the constraint as Rope — genuine coordination for solving the urgent adaptation problem with themselves as beneficial organizers. Developing nations classify as Tangled Rope — they experience real protective infrastructure benefit alongside forced dependence and extraction through financing terms. Vulnerable populations classify as Snare — they bear climate impacts and are locked into waiting for external capital allocation with no autonomous protective capacity. The organized adaptation sector sees Tangled Rope — mobilization creates real markets and coordination benefit, but regulatory constraints and climate justice scrutiny limit their exit freedom. Analytical observers with professional identity fused to the adaptation frame see Tangled Rope while theoretically capable of seeing Snare — they recognize the mixed coordination-extraction but cannot exit the frame due to identity lock. The false summit perspective (analytical/universal) naturalizes the constraint as Mountain (warming is physically inevitable) despite structural data showing institutional benefit to wealthy nations and conditionality-based suppression of vulnerable populations' autonomy — revealing the false summit signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint varies sharply across beneficiaries and victims, producing the high effective extractiveness (χ) experienced by vulnerable populations. Wealthy nations: institutional power + arbitrage exit → d ≈ 0.05 (full beneficiary), f(d) ≈ -0.12 (low/negative effective extraction, they escape the constraint's burden). Developing nations: moderate power + constrained exit → d ≈ 0.55 (slight victim bias), f(d) ≈ 0.75 (moderate effective extraction). Vulnerable populations: powerless + trapped exit → d ≈ 0.95 (full target), f(d) ≈ 1.42 (maximum effective extraction). The constraint's spatial scope (global) applies σ(S)=1.2, amplifying χ for all agents — the global financing architecture makes extraction mechanisms harder to escape through local exit. Analytical observers at identity_locked exit: structural derivation produces d ≈ 0.72 (observer position), but identity lock suppresses recognition of the constraint's extractive asymmetry despite analytical capacity. The directionality gap (wealthy nations experience d ≈ 0.05 while vulnerable populations experience d ≈ 0.95) is the core structural feature that drives the tangled_rope classification: same base extractiveness (0.58) produces dramatically different effective extraction (χ) depending on position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading is rooted in the committer-frame ambiguity: the adaptation-priority reading IS one reading of a contested kernel (climate_response_action), coexisting with mitigation_priority and degrowth_transformation readings. The reading resolves its internal mandatrophy (how can coordination coexist with extraction?) by distributing coordination and extraction across different structural positions: wealthy nations and the adaptation sector genuinely coordinate the mobilization of protective infrastructure; vulnerable populations and developing nations experience primarily extraction through fiscal dependence and suppression of alternative pathways. The reading escapes the paradox by being explicitly asymmetric — it does not claim that all agents experience the same constraint type. However, the reading creates an external mandatrophy at the kernel level: the three readings (adaptation, mitigation, degrowth) cannot all be simultaneously operationalized to their full commitments. Adaptation-priority requires accepting warming as inevitable (mandatrophy_1: if warming is inevitable, why invest in expensive adaptation rather than cheaper mitigation?). Mitigation-priority requires rapid emissions reduction (mandatrophy_2: if mitigation is primary, why invest in current adaptation for warming that should not occur?). Degrowth-transformation requires economic restructuring (mandatrophy_3: if transformation is needed, why not use that restructuring to achieve both mitigation and adaptation simultaneously?). The adaptation-priority reading resolves this by asserting temporal priority (act now on adaptation because warming is locked in; other readings are too slow) and by forecloses degrowth_transformation while coexisting with mitigation_priority. However, omega_id mitigation_vs_adaptation_budget_tradeoff reveals that under fiscal constraints, the temporal priority claim may force an actual choice between adaptation and mitigation capital allocation. This is the constraint's unresolved mandatrophy: it claims feasibility of both adaptation and some mitigation, but the base_extractiveness trajectory (0.42 → 0.68) and suppression_requirement rise (0.50 → 0.71) suggest that adaptation investment is crowding out mitigation in actual institutional practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warming_inevitability_threshold,
    'At what climate scenario (temperature rise, emissions pathway) does adaptation-only response become insufficient and transformation become structurally necessary?',
    'Empirical climate modeling: comparison of adaptation cost curves across IPCC warming scenarios (1.5°C, 2°C, 3°C+); loss-and-damage assessment of unprotectable populations and systems',
    'If threshold ≤ 1.5°C (Paris target): adaptation priority is already overshooting; mitigation remains primary. If threshold > 2.5°C: adaptation priority gains structural legitimacy as long as emissions pathway stays below threshold. If threshold is undefined (some systems already in unrecoverable loss): reading forecloses mitigation_priority and degrowth_transformation by accepting losses that other readings reject.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_inevitability_threshold, empirical, 'Temperature threshold where adaptation-only response becomes insufficient').

omega_variable(
    financing_gap_closure_feasibility,
    'Can the $350B annual North-South climate finance gap be closed through institutional reform (Green Climate Fund scaling, debt-for-climate swaps, reparations frameworks) or does it require structural economic transformation (degrowth, wealth redistribution)?',
    'Historical analysis of climate finance commitments vs disbursements; modeling of capital reallocation under different governance frameworks; comparative study of successful large-scale wealth redistribution mechanisms (Marshall Plan, debt relief, tax reform)',
    'If closure is feasible through institutional reform: adaptation-priority reading remains viable, tangled_rope classification holds. If closure requires transformation: reading forecloses degrowth_transformation and coexists with mitigation_priority only through continued inequality. If unfeasible: victims'' experienced constraint reclassifies from tangled_rope/snare to pure snare with no coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(financing_gap_closure_feasibility, empirical, 'Feasibility of closing climate finance gap through institutional reform vs structural transformation').

omega_variable(
    local_adaptive_capacity_sufficiency,
    'Can vulnerable populations and developing nations build sufficient local adaptive capacity (water systems, crop diversity, livelihood diversification, early warning) to protect themselves without perpetual dependence on wealthy-nation capital and technology transfers?',
    'Long-term tracking of local adaptation outcomes in 50+ developing nations; analysis of autonomous adaptive capacity (indigenous knowledge, traditional water management, crop breeding) relative to capital-dependent adaptation',
    'If local capacity is sufficient: extraction mechanism weakens; constraint reclassifies toward rope (genuine coordination without dependence). If local capacity is insufficient: victims remain permanently locked into capital dependence; constraint intensifies as snare. If heterogeneous: different populations experience different constraint types, requiring decomposition into regional stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_adaptive_capacity_sufficiency, empirical, 'Sufficiency of local autonomous adaptive capacity vs capital dependence').

omega_variable(
    mitigation_vs_adaptation_budget_tradeoff,
    'Does prioritizing immediate adaptation investment ($540B annually) reduce available capital for emissions mitigation (e.g., renewable energy infrastructure, grid modernization) in a zero-sum funding environment?',
    'Global capital flow analysis; modeling of financing scenarios with different mitigation/adaptation splits; historical study of climate finance crowding-out effects on other development priorities',
    'If tradeoff is real and significant: adaptation priority forecloses mitigation_priority at the global financing level; becomes a false choice framed as necessity. If tradeoff is avoidable (capital exists for both): reading coexists with mitigation_priority without logical contradiction. Theta-sensitive: the answer depends on political will to mobilize sufficient capital rather than physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_vs_adaptation_budget_tradeoff, empirical, 'Whether adaptation investment crowds out mitigation funding in zero-sum budget environment').

omega_variable(
    adaptation_as_lock_in_mechanism,
    'Does building large-scale adaptation infrastructure (dikes, irrigation, resilient agriculture) create sunk-cost path dependencies that make future emissions reduction politically and economically infeasible because adapted systems become optimized for the new climate state?',
    'Historical case study of infrastructure lock-in: analysis of flood defense systems that became expensive to upgrade; irrigation infrastructure tied to particular rainfall patterns; urban planning locked into assumed climate baselines',
    'If lock-in is real: adaptation priority structurally forecloses mitigation_priority — early adaptation investment commits civilizations to accepting warming that later reduction efforts could have prevented. Transforms the constraint from tangled_rope (temporary mixed coordination-extraction) to snare (long-term extraction through entrenchment). If lock-in is manageable (infrastructure remains flexible): reading coexists with mitigation as non-exclusive strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_as_lock_in_mechanism, empirical, 'Whether adaptation infrastructure creates sunk-cost lock-in preventing future mitigation').

omega_variable(
    reading_kernel_contest_ambiguity,
    'Is ''climate response action'' a single contested kernel with three legitimate readings, or do the three readings (adaptation_priority, mitigation_priority, degrowth_transformation) describe incompatible commitments that cannot coexist within one institutional framework?',
    'Institutional analysis: examine whether any national government, multilateral institution, or organized coalition explicitly holds multiple readings simultaneously as live options, or whether readings are distributed across irreconcilable factions',
    'If single kernel with coexisting readings: the three constraints are three perspectives on a unified problem; policy should address tension through multi-pathway investment. If incompatible commitments: one reading forecloses the others; political contestation resolves which reading governs. This omega documents the committer-frame ambiguity: is this a presheaf (multiple readings observed from different positions) or a genuine kernel (one commitment system holding contested readings)?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest_ambiguity, conceptual, 'Whether climate response action is a single kernel with coexisting readings or incompatible competing commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_adapt_theater_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clim_adapt_theater_t10, climate_response_action__adaptation_priority, theater_ratio, 10, 0.45).
narrative_ontology:measurement(clim_adapt_theater_t20, climate_response_action__adaptation_priority, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_adapt_extract_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_adapt_extract_t10, climate_response_action__adaptation_priority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_adapt_extract_t20, climate_response_action__adaptation_priority, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_adapt_suppress_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clim_adapt_suppress_t10, climate_response_action__adaptation_priority, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_adapt_suppress_t20, climate_response_action__adaptation_priority, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, north_south_climate_finance_asymmetry).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, intergenerational_climate_cost_shifting).

% DUAL FORMULATION NOTE:
% The climate_response_action kernel decomposes into three structurally distinct constraints with incompatible institutional orderings: (1) adaptation_priority (this story): ε=0.58, Tangled Rope, prioritizes immediate protection via capital mobilization; (2) mitigation_priority (sibling): ε=0.35, Rope/Scaffold, prioritizes emissions reduction via technology and markets; (3) degrowth_transformation (sibling): ε=0.72, Snare/Tangled Rope, prioritizes structural economic transformation. Each reading has a different extractiveness profile, different beneficiary/victim allocation, and different temporal horizon. The stories are linked via network.affects_constraints because the institutional commitment to one reading crowds out capital and political will for the others, but they remain live as distinct constraint stories because each instantiates a genuine institutional position held by different coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__adaptation_priority, institutional, 0.02).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
