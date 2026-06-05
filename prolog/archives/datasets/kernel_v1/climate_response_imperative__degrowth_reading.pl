% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative: Degrowth Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The degrowth reading of the climate response imperative frames climate
 *   stabilization and adaptation as requiring structural economic
 *   transformation in the Global North: mandatory reduction in absolute
 *   material consumption, redistribution of wealth and resource access toward
 *   Global South and future generations, post-growth institutional
 *   reorganization (work-time reduction, basic income, public goods
 *   expansion), and elimination of reliance on unproven carbon dioxide
 *   removal (CDR) technologies as escape hatches. This reading instantiates
 *   one pole of a contested kernel about what climate response fundamentally
 *   means. The constraint exhibits the hallmark properties of Tangled Rope:
 *   genuine coordination function (enabling adaptation and mitigation
 *   simultaneously, distributing limited resource base equitably across
 *   regions and generations) paired with high extraction from present-day
 *   Global North populations (reduced consumption, working-time
 *   reorganization, wealth transfer). Suppression is high (0.72) because
 *   capital-intensive sectors and Global North incumbents have material
 *   interest in maintaining growth pathways and resisting institutional
 *   transformation. Theater ratio is moderate (0.55) and rising, reflecting
 *   the gap between official climate commitments (net-zero pledges, corporate
 *   sustainability) and actual consumption reduction (which remains minimal
 *   or negative in most Global North jurisdictions). The constraint is
 *   contested not because its physics is uncertain but because its political
 *   implications are irreversible: adopting the degrowth reading commits
 *   present-day Global North majorities to material sacrifice with uncertain
 *   compensation. This reading coexists with two alternative readings of the
 *   same kernel: mitigation-priority (emissions reduction via technology and
 *   markets, deferring systemic change) and adaptation-priority
 *   (resilience-building in exposed regions, treating mitigation as
 *   aspirational). Each reading produces different beneficiary/victim
 *   distributions, different institutional mechanisms, and different
 *   classifications from the same base properties.
 *
 * KEY AGENTS:
 *   - Future generations: Primary beneficiary (no exit capacity) — stabilized climate within livable range, redistributed resource base, preserved ecosystem function
 *   - Global South populations and low-income regions: Primary beneficiary (constrained exit) — direct mitigation benefits, adaptation resource access, climate justice redistribution
 *   - Present-day Global North working-class and middle-income populations: Primary victim (trapped exit) — mandatory consumption reduction, work-time reorganization, wealth redistribution, identity disruption
 *   - Carbon-intensive capital and fossil fuel incumbents: Secondary victim (arbitrage exit, but delegitimized) — profit model collapse, capital controls, decarbonization imperatives
 *   - Post-growth coalition (labor movements, ecological movements, some Global South governments): Enforcer/beneficiary hybrid (constrained exit) — coordinates institutional transformation, executes redistribution, enforces consumption reduction
 *   - Global North states and green growth apparatus: Piton actor (constrained exit) — maintains legitimacy while deferring structural transformation, sustains performative compliance
 *   - Analytical observer / degrowth reading position: Sees genuine Tangled Rope with irreversible distributional consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.58).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative: Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'b61ebcb5-f62c-4940-ac1f-405f40c60535').
narrative_ontology:cs_kernel_codification('b61ebcb5-f62c-4940-ac1f-405f40c60535', distributed).
narrative_ontology:cs_authority_grounding('b61ebcb5-f62c-4940-ac1f-405f40c60535', distributed).
narrative_ontology:cs_reading_relation('b61ebcb5-f62c-4940-ac1f-405f40c60535', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('b61ebcb5-f62c-4940-ac1f-405f40c60535', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('b61ebcb5-f62c-4940-ac1f-405f40c60535', foundational, consumption_reduction_necessary).
narrative_ontology:cs_axiom_status(consumption_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b61ebcb5-f62c-4940-ac1f-405f40c60535', consumption_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b61ebcb5-f62c-4940-ac1f-405f40c60535', foundational, intergenerational_redistribution_required).
narrative_ontology:cs_axiom_status(intergenerational_redistribution_required, holdable).
narrative_ontology:cs_axiom_grounding('b61ebcb5-f62c-4940-ac1f-405f40c60535', intergenerational_redistribution_required, deontological).
narrative_ontology:cs_axiom('b61ebcb5-f62c-4940-ac1f-405f40c60535', secondary, capital_controls_mandatory).
narrative_ontology:cs_axiom_status(capital_controls_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('b61ebcb5-f62c-4940-ac1f-405f40c60535', capital_controls_mandatory, instrumental).
narrative_ontology:cs_reference_frame('b61ebcb5-f62c-4940-ac1f-405f40c60535', growth_imperative_as_survival_necessity).
narrative_ontology:cs_drift_state('b61ebcb5-f62c-4940-ac1f-405f40c60535', contemporary_climate_emergency, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b61ebcb5-f62c-4940-ac1f-405f40c60535', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, ecosystem_stability).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, high_consumption_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL NORTH WORKING-CLASS (SNARE) — Face immediate reduced consumption, working-time reorganization, and wealth redistribution without effective exit. Material barriers are total: cannot arbitrage to lower-growth jurisdictions without losing employment and social protection. Perceives the constraint as coercive redistribution with no coordination benefit. Maximum experienced extraction — direct material loss, identity disruption (consumption-indexed identity), social status degradation. Exit to status quo ante is blocked by climate physics and by the constraint's institutional enforcement.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL SOUTH ADAPTATION-PRIORITY (TANGLED ROPE) — Benefit from redistribution and mitigation (genuine coordination: securing stable climate for agriculture and freshwater). But also experience constraint through enforced participation in decarbonization pathways that may bypass cheaper fossil development. Constrained exit: dependency on Global North capital and technology, but real agency through regional coalition-building. Mixed experience: coordination gains from climate stabilization, extraction costs from economic subordination pathways.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FUTURE GENERATIONS / ECOSYSTEMS (ROPE) — Clear beneficiaries with no capacity for exit negotiation (temporal boundary). From their perspective, the constraint is pure coordination: stabilizing climate within livable range requires present-day extraction from Global North. No perceivable extraction from future generation's standpoint — they cannot pay costs. This perspective instantiates the ethical core of the degrowth reading: beneficiaries without exit capacity enforce constraints on present actors.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-GROWTH COALITION (TANGLED ROPE) — Organized agents (ecological movements, labor unions, degrowth theorists, some Global South governments) experience this as coordination with enforcement: reducing consumption requires institutional architecture and union power to restructure work, distribute income, and resist capital exit. Constrained exit: coalition members can defect to growth-coalition, but do so at cost of ideological rupture and material consequence. Genuine coordination function (distributing reduced resource base equitably) alongside extraction mechanism (enforcement of consumption reduction, work reorganization).
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL & CARBON-INTENSIVE CAPITAL (SNARE) — Experience the constraint as existential threat with material arbitrage (capital flight, jurisdictional arbitrage, regulatory capture). Perceive extraction as catastrophic: their profit model collapses under degrowth + decarbonization + capital controls. Suppression mechanism is defensive: political influence to delay constraint adoption, fund denial, promote technological substitutes (CDR, carbon capture) that preserve growth model. High effective extraction from their perspective — the constraint eliminates their value proposition. But their power creates symmetrical extraction pressure: they suppress the constraint's legitimacy through media, policy influence, and ideological work.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL NORTH STATES / GREEN GROWTH (PITON) — Official climate policy apparatus maintains commitment to emissions reduction while avoiding structural transformation. Theater ratio high (0.55 baseline, rising): climate summits, net-zero pledges, carbon markets, and corporate sustainability reporting create performative compliance without reducing absolute consumption or redirecting capital from fossil fuels. States experience constraint as maintaining legitimacy (acting on climate) while deferring costs (capital-friendly pathways). Piton classification: degraded from what rope-level coordination would require (mandatory consumption reduction) to what theater sustains (voluntary efficiency improvements, technological optimism, carbon pricing without demand destruction). Enforcement is weak because enforcement would collapse the political coalition.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: BIOPHYSICAL LIMITS (MOUNTAIN) — From the analytical/civilizational view, climate physics imposes an immutable constraint: carbon budget limits, ecological carrying capacity, and biophysical thresholds are not negotiable. The degrowth reading claims this is a genuine mountain — that reducing Global North consumption to stabilize atmospheric CO2 and enable adaptation is a law of physics, not a political choice. However, the structural data undermines this: the constraint exhibits high suppression (0.72), active enforcement requirements, victim declarations, and beneficiary coordination — all incompatible with natural law. The engine will flag this as a false summit: naturalizing what is actually a political commitment as if it were physics. The debate is whether climate response is fundamentally constrained by physics (mountain) or whether it is a political-institutional arrangement (tangled rope / snare).
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DEGROWTH READING (TANGLED ROPE) — The analytical position endorsing the degrowth reading sees the constraint as a genuine tangled rope: real coordination function (stabilizing climate, redistributing resource base, enabling adaptation) paired with real extraction mechanism (reducing present-day Global North consumption, work reorganization, capital controls, elimination of unproven CDR as escape hatch). This perspective acknowledges suppression is high because capital has genuine interests in preventing the constraint's enforcement. The constraint is not natural law — it is a contested political commitment grounded in intergenerational justice and biophysical limits. Efficacy depends on institutional enforcement, coalition power, and cultural shift.
constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_imperative__degrowth_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, TR),
    TR >= 0.70.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The degrowth reading imposes mandatory material sacrifice on present-day Global North populations (direct extraction) while enabling adaptation and climate stabilization for beneficiaries. The extraction is not maximal (0.72+) because genuine coordination benefits exist: stable climate, equitable resource distribution, and ecosystem stability are real public goods, not pure zero-sum rent extraction. The constraint is Tangled Rope rather than Snare precisely because coordination function is real, not theatrical. The measurement trajectory (0.32 → 0.58) reflects that as decarbonization becomes empirically necessary and climate impacts accelerate, the extraction mechanism hardens — deferral becomes impossible, mandatory reduction replaces voluntary efficiency improvements. Suppression (0.72): High. Suppression is driven by structural interests: capital-intensive sectors and Global North incumbents suppress constraint adoption through political influence, funding denial (climate research obstruction, regulatory capture), ideological work (growth optimism, technological solutionism), and institutional lock-in (carbon-based infrastructure, financial systems betting on growth). The measurement trajectory (0.48 → 0.72) reflects that suppression intensifies as the constraint becomes more binding — the costs of resistance rise as alternatives become exhausted. Theater ratio (0.55): Moderate. Rising over time. The official climate apparatus generates performative activity (net-zero pledges, carbon markets, ESG reporting, climate summits) that creates perceived action while deferring structural change. The theater is not total (hence piton, not pure inertia) because some genuine emissions reduction occurs through efficiency and renewable deployment. But the theater is real: consumption continues rising in Global North despite decades of climate commitments, indicating that official policy space is occupied by low-cost symbolic measures rather than redistributive transformation. The trajectory (0.28 → 0.55) reflects that as genuine decarbonization becomes impossible without consumption reduction, the gap between performative climate policy and required structural change widens, raising theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The degrowth reading produces maximum perspectival divergence. Working-class Global North populations see Snare (coercive extraction with no exit). Fossil fuel capital sees Snare (existential threat). Post-growth coalition sees Tangled Rope (coordination with enforcement). Global South sees Tangled Rope (coordination with subordination dynamics). Future generations see Rope (pure coordination from their temporal standpoint). The piton perspective (official state apparatus) sees theatrical compliance space. The mountain perspective risks naturalizing what is political choice. These gaps reflect genuine structural differences in how agents experience the constraint: those bearing costs see extraction; those enabling stability see coordination; those deferring change see theater. The presheaf over this constraint is irreducible — all classifications are correct from their respective observation positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's classification derives from directionality values (d) computed from beneficiary/victim status and exit options. Working-class Global North (powerless/trapped/victim) gets high d (0.90+) → high f(d) → high χ → Snare. Post-growth coalition (organized/constrained/beneficiary+enforcer) gets moderate d (0.45) → moderate f(d) → moderate χ → Tangled Rope. Fossil capital (powerful/arbitrage/victim by definition) gets moderate-high d (0.65-0.75) → high f(d) → high χ → Snare (despite power, because their profit model is the target of extraction). Future generations have no exit option and receive pure benefit → d near 0 → negative χ → Rope from their framing. The piton perspective derives from theater gate (0.55 > 0.5) despite moderate ε, indicating degraded enforcement. The mountain perspective's d is analytical (0.73) but the structural data (beneficiaries, victims, enforced redistribution) contradicts natural law signature, triggering false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading resolves mandatrophy by explicitly rejecting the false choice between coordination and extraction: the constraint IS both. Coordinating adaptation and mitigation simultaneously for all populations (including future generations and Global South) requires extracting consumption and capital from Global North beneficiaries of growth. This is not extraction in the pejorative sense (wasteful rent-seeking) but in the structural sense (transfer of resources and labor against the wishes of those bearing costs). The mandatrophy resolution hinges on accepting that intergenerational justice and climate stabilization cannot be decoupled from distributive conflict. The constraint is Tangled Rope precisely because both functions are real and necessary: the coordination function (enabling adaptation, stabilizing climate, redistributing survival resources) is genuine, and the extraction mechanism (mandatory reduction, work reorganization, capital controls) is genuine. Denying the extraction function by calling it 'necessary sacrifice' does not make it disappear — it only obscures who bears costs and who captures benefits. The tangled rope classification makes this transparency explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_temporal_distribution,
    'How should remaining carbon budget be temporally and spatially allocated between present-day Global North consumption reduction and future-generation adaptation flexibility?',
    'Climate modelling integrating carbon cycle feedback, adaptation cost functions, and intergenerational welfare trade-offs. Empirical mapping of consumption reduction pathways and their emission profiles.',
    'If present-day reduction > 5% annual: constraint classification shifts to Snare from many perspectives (severity increases). If present-day reduction < 2% annual: constraint degrades to Piton (insufficient enforcement to prevent further temperature rise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_temporal_distribution, empirical, 'How remaining carbon budget is allocated temporally and spatially').

omega_variable(
    technological_carbon_removal_feasibility,
    'Can negative-emission technologies (direct air capture, enhanced weathering, biochar) scale to obviate the need for absolute consumption reduction in Global North?',
    'Engineering cost analysis, physical feasibility modeling (land requirements, energy input, permanence timescales), deployment pathway assessment. Comparison of CDR costs vs. consumption-reduction costs across sectors.',
    'If CDR is viable at scale and cost < consumption-reduction burden: degrowth reading becomes optional rather than mandatory (constraint classification shifts to Rope from beneficiary perspectives, Mountain from technological substitution perspective). If CDR fails feasibility gates: degrowth constraint becomes binding (classification hardens to Snare from Global North perspectives, strengthens mandatrophy resolution for tangled rope classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_carbon_removal_feasibility, empirical, 'Whether negative-emission technologies can scale to obviate consumption reduction').

omega_variable(
    this_reading_vs_siblings_foreclosure_status,
    'Does the degrowth reading logically foreclose the mitigation-priority or adaptation-priority readings, or do all three remain coexistent as live political framings?',
    'Logical analysis of foundational premises. If degrowth axioms (absolute consumption reduction necessary, capital controls required) are deontological commitments, siblings coexist. If degrowth axioms are empirically contingent (CDR fails, carrying capacity is hard limit), then mitigation-priority reading is foreclosed by evidence. If adaptation must be prioritized ahead of global equity (adaptation-priority reading), then degrowth''s equity focus is overridden.',
    'If foreclosure occurs: a single reading becomes analytically dominant (constraint type converges to single classification across perspectives, mandatrophy resolves). If coexistence holds: the kernel remains contested (perspectives maintain divergent classifications, the presheaf over the kernel is irreducible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(this_reading_vs_siblings_foreclosure_status, conceptual, 'Whether degrowth reading forecloses sibling readings or coexists with them').

omega_variable(
    indigenous_and_global_south_degrowth_coercion,
    'Does Global South degrowth (if enforced) constitute non-consensual participation in a Global North-designed institutional framework, or is redistributive degrowth a genuine coordination mechanism enabling Global South sovereignty?',
    'Qualitative research on Global South policy agency and institutional voice in designing decarbonization pathways. Empirical assessment of whether degrowth pathways preserve or eliminate Global South autonomy over development choices.',
    'If coercive: degrowth constraint exhibits colonialism-pattern extraction (Global South becomes victim in addition to present-day Global North). Classification shifts toward Snare for Global South populations. If consensual: constraint is genuine Tangled Rope (coordination + redistribution). This is critical to whether degrowth reading is a liberation framing or a Northern-imposed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_and_global_south_degrowth_coercion, empirical, 'Whether Global South degrowth is coercive or consensual participation').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the contested kernel ''climate_response_imperative''. What are the structural differences between this reading (degrowth_reading) and its siblings (mitigation_priority_reading, adaptation_priority_reading)?',
    'Explicit comparison of core axioms, beneficiary/victim sets, and institutional mechanisms across the three readings. Document what each reading claims about necessary conditions, temporal horizons, and actor relationships.',
    'This omega documents the reading identity itself. Different readings produce different constraint classifications from the same baseline facts. The degrowth reading''s classification as Tangled Rope depends on axioms about consumption-reduction necessity and capital-control requirement. Alternative readings with different axioms will produce different classifications. The engine''s reading_relations structure in cs_structure captures this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this reading''s structural position within the contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_deg_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_deg_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(clim_deg_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(clim_deg_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_deg_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(clim_deg_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_deg_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_deg_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_deg_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, carbon_removal_technological_solutionism).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, growth_imperative_institutional_lock).

% DUAL FORMULATION NOTE:
% The climate response imperative is one kernel with three structurally distinct readings. Each reading produces a different constraint story with different ε values, beneficiary/victim distributions, and classifications. The degrowth_reading (this file) is downstream of the broader kernel contest. It influences both sibling readings (constrains their institutional space) and influences downstream constraints about carbon removal and growth dependency. The three readings should be understood as a presheaf: different observation positions produce different classifications from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
