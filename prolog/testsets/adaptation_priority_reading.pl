% ============================================================================
% CONSTRAINT STORY: adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adaptation_priority_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adaptation_priority_reading
 *   human_readable: Adaptation Priority in Climate Response (Reading: Resilience and Damage Reduction)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response frames the constraint
 *   as one where present-day climate-exposed populations in developing
 *   nations require immediate resilience-building investment and damage
 *   reduction, with global mitigation treated as a longer-term (aspirational)
 *   goal. This reading contends that capital constraints make simultaneous
 *   global mitigation and regional adaptation infeasible, and that justice
 *   demands prioritizing the immediate suffering of vulnerable populations.
 *   The constraint exhibits tangled-rope structure: adaptation projects
 *   provide genuine coordination benefit (disaster risk reduction, livelihood
 *   diversification, infrastructure hardening) while simultaneously enabling
 *   extraction (deferral of emissions reductions that would prevent future
 *   exposure, preservation of incumbent carbon interests, and redistribution
 *   of costs to present-day poor nations and future generations). The rising
 *   extractiveness over the 2005-2025 interval reflects that adaptation has
 *   become increasingly recognized as insufficient without mitigation — the
 *   gap between adaptation promises and adaptation outcomes has widened as
 *   climate impacts accelerate. The theater ratio indicates moderate
 *   performative content: adaptation frameworks, climate finance mechanisms,
 *   and resilience pledges constitute both genuine action and legitimacy
 *   theater that substitutes for binding emissions commitments.
 *
 * KEY AGENTS:
 *   - Present-day Exposed Populations (powerless/trapped): Bears immediate climate hazards and adaptation capital requirements; locked in by geography and economic dependency; experiences maximum extraction as adaptation defers mitigation
 *   - High-Capacity Nations (institutional/arbitrage): Primary beneficiaries; can invest in both adaptation and independent mitigation; extract technology licensing revenue and maintain fossil fuel operations under adaptation-deferral logic
 *   - Adaptation Technology Providers (institutional/arbitrage): Beneficiary class; sell adaptation solutions globally; profit from climate exposure without bearing mitigation costs
 *   - Incumbent Carbon Industries (powerful/arbitrage): Beneficiary class; adaptation-priority framing permits continued hydrocarbon production; deferral of mitigation investment preserves profit streams
 *   - Middle-Income Nations (moderate/constrained): Mixed position; benefit from adaptation investment but bear costs of mitigation deferral through future exposure; constrained by capital requirements they cannot meet
 *   - Climate Justice and Transition Coalitions (organized/mobile): Organized actors seeing adaptation as temporary framework; coalition-building and technological change enable exit from constraint space
 *   - International Climate Governance (institutional/constrained): Maintains adaptation frameworks; constrained by state sovereignty and capital limitations; theater maintains legitimacy while underlying mitigation commitment degrades
 *   - Future Generations (powerless/analytical): Not present in negotiation; bear cumulative carbon load from mitigation deferral; ultimate victim set of intergenerational extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adaptation_priority_reading, 0.58).
domain_priors:suppression_score(adaptation_priority_reading, 0.68).
domain_priors:theater_ratio(adaptation_priority_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adaptation_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(adaptation_priority_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(adaptation_priority_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(adaptation_priority_reading, "Adaptation Priority in Climate Response (Reading: Resilience and Damage Reduction)").
narrative_ontology:topic_domain(adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(adaptation_priority_reading, 'd6f023f2-d04f-4051-a786-b345a2ce594c').
narrative_ontology:cs_created_at('d6f023f2-d04f-4051-a786-b345a2ce594c', '').
narrative_ontology:cs_kernel_codification('d6f023f2-d04f-4051-a786-b345a2ce594c', formalized).
narrative_ontology:cs_authority_grounding('d6f023f2-d04f-4051-a786-b345a2ce594c', lineage).
narrative_ontology:cs_interpretation_layer_present('d6f023f2-d04f-4051-a786-b345a2ce594c').
narrative_ontology:cs_kernel_id(adaptation_priority_reading, climate_response_imperative).
narrative_ontology:cs_reading_relation('d6f023f2-d04f-4051-a786-b345a2ce594c', mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6f023f2-d04f-4051-a786-b345a2ce594c', degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('d6f023f2-d04f-4051-a786-b345a2ce594c', foundational, immediate_suffering_priority).
narrative_ontology:cs_axiom_status(immediate_suffering_priority, holdable).
narrative_ontology:cs_axiom_grounding('d6f023f2-d04f-4051-a786-b345a2ce594c', immediate_suffering_priority, deontological).
narrative_ontology:cs_axiom('d6f023f2-d04f-4051-a786-b345a2ce594c', foundational, capital_constraint_binding).
narrative_ontology:cs_axiom_status(capital_constraint_binding, holdable).
narrative_ontology:cs_axiom_grounding('d6f023f2-d04f-4051-a786-b345a2ce594c', capital_constraint_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('d6f023f2-d04f-4051-a786-b345a2ce594c', equitable_climate_response_with_capital_prioritization).
narrative_ontology:cs_drift_state('d6f023f2-d04f-4051-a786-b345a2ce594c', contemporary_post_paris_implementation, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adaptation_priority_reading, high_capacity_nations).
narrative_ontology:constraint_beneficiary(adaptation_priority_reading, adaptation_technology_providers).
narrative_ontology:constraint_beneficiary(adaptation_priority_reading, incumbent_carbon_interests).
narrative_ontology:constraint_victim(adaptation_priority_reading, present_day_exposed_populations).
narrative_ontology:constraint_victim(adaptation_priority_reading, climate_mitigation_imperative).
narrative_ontology:constraint_victim(adaptation_priority_reading, future_generations_carbon_accumulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATIONS IN VULNERABLE REGIONS (SNARE) — Face immediate climate hazards (flooding, drought, heat stress) with no capacity to exit or migrate. Trapped by geography and economic dependency. Adaptation framing offers only damage reduction at the margin while extraction mechanism (deferral of mitigation investment) continues to accumulate the carbon load that drives future exposure. Maximum experienced extraction: present costs of adaptation, future costs of unmitigated climate change.
constraint_indexing:constraint_classification(adaptation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE-INCOME NATIONS AND REGIONAL ACTORS (TANGLED ROPE) — Benefit from adaptation investment (infrastructure, technology transfer, capacity building) while simultaneously bearing costs of mitigation deferral through future carbon exposure. Constrained by capital requirements they cannot fully meet. Both coordination (adaptation projects require collaboration) and extraction (resource asymmetry means wealthy nations extract technology licensing revenue while middle-income actors bear risk).
constraint_indexing:constraint_classification(adaptation_priority_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-CAPACITY NATIONS AND TECHNOLOGY PROVIDERS (ROPE) — Experience the constraint as pure coordination: adaptation projects are legitimate development assistance, technology transfer is profitable, climate finance creates new markets. Net beneficiary position. Arbitrage options: can invest in both adaptation AND pursue independent mitigation; can sell adaptation solutions globally; can defer own exposure through wealth-based adaptation. See this reading as rational resource allocation.
constraint_indexing:constraint_classification(adaptation_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT CARBON INTERESTS (TANGLED ROPE) — Benefit directly from adaptation-priority framing because it defers mitigation investment that would constrain their operations. Adaptation-first logic permits continued hydrocarbon production under the premise that societies will 'adapt' rather than transition energy systems. Mixed: genuine coordination function exists (adaptation projects do reduce some risks) but extraction mechanism is primary (deferral of structural transition preserves profit streams).
constraint_indexing:constraint_classification(adaptation_priority_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — UNFCCC, climate finance mechanisms, and adaptation frameworks perform legitimacy maintenance while underlying mitigation commitments degrade. Theater ratio reflects: adaptation conferences, adaptation funds, adaptation pledges constitute performative climate action that substitutes for emissions reductions. The apparatus has theater_ratio = 0.51 because some adaptation work is genuine while much is ritual signaling. Governance persists through institutional inertia — adaptation commitment is easier to perform than mitigation enforcement.
constraint_indexing:constraint_classification(adaptation_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE JUSTICE AND TRANSITION COALITIONS (SCAFFOLD) — Organized actors (frontline communities, environmental justice movements, climate science institutions, renewable energy sectors) see the adaptation-priority reading as a temporary framework that generates its own obsolescence. As climate exposure accelerates beyond adaptation capacity, the reading's own logic forces recognition that mitigation (not adaptation) is the binding constraint. Sunset mechanism: adaptation becomes demonstrably insufficient, forcing political reorientation toward mitigation and system transition. Constrained by current policy and capital structures but mobile within the constraint space — coalition building, technology deployment, narrative shifts can reorient priorities.
constraint_indexing:constraint_classification(adaptation_priority_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a civilizational/universal perspective, atmospheric carbon accumulation is a physical constraint: once emitted, CO2 persists for centuries, and adaptation cannot reduce cumulative radiative forcing. Climate response to present emissions is locked in regardless of future mitigation. This perspective risks classifying as mountain — presenting adaptation necessity as natural law rather than policy choice. However, the structural data reveals this as a false summit: the adaptation-priority framing naturalizes a contingent political choice (deferring mitigation investment) as inevitable physics.
constraint_indexing:constraint_classification(adaptation_priority_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adaptation_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adaptation_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adaptation_priority_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adaptation_priority_reading, TR),
    TR >= 0.70.

:- end_tests(adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The adaptation-priority reading enables extraction through three mechanisms: (1) immediate capital extraction from wealthy nations through adaptation finance that flows to high-capacity nations and technology providers; (2) deferral of mitigation investment that preserves incumbent carbon interests' profit streams; (3) redistribution of temporal burden from present-high-emission nations to present-day exposed nations and future generations. The 0.58 value reflects that adaptation work is genuine (ε is not 0.80+ as pure extraction would be) but the coordination function is substantially compromised by the deferral mechanism. Suppression (0.68): High. Barriers to recognizing the constraint structure include: political economy concealment (adaptation-priority framing appears equity-focused), scientific uncertainty (climate projections enable adaptation-sufficiency optimism), capital constraints (genuine resource scarcity makes simultaneous mitigation+adaptation appear infeasible), and intergenerational distance (future harm is discounted). Theater ratio (0.51): Moderate. Adaptation frameworks perform significant legitimacy maintenance (UNFCCC adaptation programs, climate finance mechanisms, resilience pledges) while underlying mitigation commitments degrade. However, adaptation work has meaningful functional content — disaster risk reduction, livelihood support, infrastructure improvements are real — so theater is not dominant. The 0.51 value reflects the constraint approaching the boundary between tangled_rope and piton as the gap between adaptation promises and adaptation outcomes widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence. The exposed population sees snare extraction (immediate costs, trapped exit, no mitigation pathway). High-capacity nations see rope coordination (adaptation is development aid, technology markets, risk reduction). Carbon interests see tangled rope that benefits them (genuine adaptation coordination value + extraction via deferral). The climate justice coalition sees scaffold with sunset (adaptation temporarily reduces harm but becomes demonstrably insufficient, forcing political reorientation). The governance apparatus sees piton (adaptation ritual that masks mitigation degradation). The analytical observer risks mountain classification (presenting adaptation necessity as natural law of climate physics) but the structural data reveals false summit: adaptation-priority is a political choice that naturalizes deferral of mitigation investment. The gap is irreducible because actors genuinely occupy different structural positions relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is determined by position within the adaptation-deferral mechanism. High-capacity nations benefit from both adaptation investment flows AND from deferral of mitigation that would constrain their energy systems — d ≈ 0.20, f(d) ≈ 0.02, negative effective extractiveness (subsidy). Exposed populations bear immediate adaptation costs and future mitigation burden — d ≈ 0.92, f(d) ≈ 1.38, maximum experienced extraction. Middle-income nations are structurally split: they benefit from adaptation investment (reduced d) but bear costs of deferral (increased d), resulting in intermediate directionality ≈ 0.55-0.65. The constraint's scope (global) amplifies χ via σ(S) = 1.2. The organized coalition has exit options through technological change and political reorientation, constraining their experienced d despite victim-adjacent structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-LEVEL MANDATROPHY: The adaptation-priority reading resolves mandatrophy by acknowledging that it is ONE legitimate reading of a contested kernel, not THE reading of climate response. Its core claim (resilience-building and damage reduction in exposed regions as primary focus) is coherent within frameworks prioritizing immediate suffering reduction. However, the structural data reveals extraction mechanisms that the reading's own equity framing should reject: present-day poor nations bearing costs of high-emitting nations' deferral is precisely the inequity the reading claims to remedy. The reading contains an internal contradiction that becomes acute once adaptation-sufficiency thresholds are exceeded. Mandatrophy is resolved by classifying the constraint as tangled rope (genuine coordination + extraction), acknowledging the reading's temporal instability through omega variables (adaptation-sufficiency threshold, intergenerational cost-shifting), and routing the contested frame to cs_structure (this is one reading of the kernel, not objective truth about climate response).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_sufficiency_threshold,
    'At what cumulative warming level does adaptation become physically insufficient regardless of investment? Does the adaptation-priority reading''s own logic collapse when exposure exceeds adaptive capacity?',
    'Climate impact modeling: comparison of adaptation investment requirements vs. expected damages at 1.5°C, 2.0°C, 2.5°C warming. Empirical tracking of adaptation failure rates as observed hazard intensity increases.',
    'If threshold < 2.0°C: adaptation-priority reading becomes incoherent within its own framework (cannot deliver on its promise of resilience-building). If threshold > 3.0°C: reading remains coherent but only for very high-capital nations; cascading adaptation failures in lower-capacity regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_sufficiency_threshold, empirical, 'Physical warming threshold where adaptation becomes insufficient').

omega_variable(
    capital_requirements_vicious_cycle,
    'Do adaptation capital requirements for exposed regions exceed debt-service capacity, creating permanent dependency dynamics rather than resilience? Does adaptation-priority framing trap developing nations in perpetual external finance relationships?',
    'Financial analysis: present-value cost of adaptation in exposed regions vs. GDP and fiscal capacity; debt-sustainability analysis of adaptation finance; comparison with alternative scenarios (early mitigation investment reducing future adaptation needs).',
    'If vicious cycle confirmed: adaptation-priority reading redistributes intergenerational burden from high-emission nations to present-day low-emission nations. If adaptation finance becomes self-sustaining: reading is structurally viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_requirements_vicious_cycle, empirical, 'Whether adaptation capital requirements trap exposed regions in permanent dependency').

omega_variable(
    mitigation_deferral_carbon_accumulation,
    'Does the adaptation-priority framing''s implicit deferral of mitigation investment lock in carbon emissions that make future adaptation exponentially more expensive? Is there a critical window where deferring mitigation makes adaptation-priority logic impossible?',
    'Carbon budget analysis: compare cumulative emissions under adaptation-priority policy pathway vs. mitigation-priority pathway at comparable investment scales. Model future adaptation costs as function of cumulative atmospheric carbon.',
    'If deferral locks in unmanageable emissions: adaptation-priority reading contains a hidden time bomb — it is viable only within a narrow window and becomes incoherent beyond it. If carbon-neutral adaptation is possible: reading''s deferral logic is less constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_deferral_carbon_accumulation, empirical, 'Whether mitigation deferral makes future adaptation exponentially more expensive').

omega_variable(
    adaptation_vs_mitigation_framework_contest,
    'Is the adaptation-priority reading a genuine alternative framework for climate response, or is it a reading imposed on a contested kernel where mitigation-priority actors have stronger legitimacy claims?',
    'Historical analysis: trace how adaptation-priority framing emerged (post-Kyoto climate negotiations, UNFCCC adaptation framework). Identify which actors advanced this reading and what structural interests it served. Compare legitimacy grounding (expertise, procedural authority, equity claims) across readings.',
    'If adaptation-priority is genuine alternative: both it and mitigation-priority readings coexist with equal validity. If it emerged as political compromise from mitigation-priority logic: it is a secondary derivative with inherent instability. If it represents equity intuition (not primary source): classification framework may need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_mitigation_framework_contest, conceptual, 'Whether adaptation-priority is a genuine alternative framework or derivative of mitigation-priority logic').

omega_variable(
    intergenerational_cost_shifting,
    'Does this reading''s core claim — that present-day focus should be on resilience-building in exposed regions rather than global mitigation — constitute intergenerational cost-shifting from high-emitting nations to future generations globally, including in high-emitting nations?',
    'Intergenerational equity analysis: present-value cost to current exposed populations (adaptation investment) vs. future cost to all populations (unmitigated warming). Distributional analysis: who bears present costs vs. who will bear future costs?',
    'If cost-shifting confirmed: adaptation-priority reading violates intergenerational equity principles it may claim to uphold. If present adaptation investment sufficient: equity concern is reduced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_cost_shifting, preference, 'Whether adaptation-priority reading constitutes intergenerational cost-shifting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adaptation_priority_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adap_tr_t0, adaptation_priority_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(adap_tr_t10, adaptation_priority_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(adap_tr_t20, adaptation_priority_reading, theater_ratio, 20, 0.51).

% Extraction over time
narrative_ontology:measurement(adap_be_t0, adaptation_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(adap_be_t10, adaptation_priority_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(adap_be_t20, adaptation_priority_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(adaptation_priority_reading, degrowth_reading).
narrative_ontology:affects_constraint(adaptation_priority_reading, climate_finance_architecture).

% DUAL FORMULATION NOTE:
% The adaptation-priority reading is one decomposition of the contested kernel 'climate_response_imperative'. It is linked to mitigation-priority and degrowth readings through shared kernel identity. Each reading has its own ε-invariant structure: adaptation-priority (ε=0.58, tangled rope), mitigation-priority (ε=0.38, rope or scaffold), degrowth (ε=0.65, snare or tangled rope). The network captures how advancing one reading creates structural pressure on siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(adaptation_priority_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
