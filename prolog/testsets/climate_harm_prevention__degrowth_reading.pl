% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Reading: Planned Economic Contraction as Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth reading of the contested kernel
 *   climate_harm_prevention. The degrowth reading claims that planned
 *   economic contraction in the Global North is a necessary precondition for
 *   adequate climate harm prevention because: (1) remaining carbon budget for
 *   1.5°C warming is exhausted by Global North consumption patterns under any
 *   growth scenario; (2) decoupling of emissions from growth is empirically
 *   too slow relative to the required pace of carbon reduction; (3)
 *   therefore, contraction is not optional but physically mandated by climate
 *   physics and carbon budget limits. This reading's structure identifies
 *   Global South climate-vulnerable populations and future generations as
 *   primary beneficiaries (those whose survival depends on strict emissions
 *   limits) and Global North present consumption and incumbent high-carbon
 *   capital as victims (those bearing the costs of contraction). The
 *   constraint exhibits classic snare characteristics from the perspective of
 *   powerless agents (Global South, future generations): they are trapped by
 *   physical constraints and lack exit options. From the perspective of
 *   Global North state apparatus and green capital, it appears as tangled
 *   rope or rope — mixed coordination (managing transition) with extraction
 *   (capital protection). The piton perspective (climate policy bureaucracy)
 *   reflects the gap between mandated decarbonization and actual
 *   institutional capacity: policy theater (net-zero targets, carbon
 *   accounting) substitutes for material emissions reduction while growth
 *   framework remains unchallenged. The analytical/physics perspective risks
 *   naturalizing a contingent institutional reading (contraction as THE
 *   response) as an immutable law of nature. The primary measurement dynamics
 *   show rising extractiveness (0.42→0.68) over a 20-year interval as
 *   contraction becomes more urgent and the distribution burden falls more
 *   heavily on workers and Global South. Suppression requirement also rises
 *   (0.55→0.72) as political resistance intensifies — incumbent capital,
 *   growth-dependent states, and consumer constituencies actively suppress
 *   the degrowth claim. Theater ratio declines slightly (0.68→0.58) as
 *   performative policy gives way to material constraints, but remains
 *   substantial because state policy still substitutes symbolic commitments
 *   (Paris targets, net-zero pledges) for implemented contraction.
 *
 * KEY AGENTS:
 *   - Global South Climate-Vulnerable Populations: Primary victim (powerless/trapped) — face existential climate harms (sea-level rise, water scarcity, crop failures) from carbon already committed; no exit option; bear full extraction cost
 *   - Future Generations: Primary victim (powerless/trapped) — structurally trapped by carbon budget allocation set in current period; cannot participate in decision-making; inherit climate state and reduced adaptive capacity
 *   - Global North Present Consumption: Declared victim (moderate/constrained) — bear contraction costs through reduced material consumption, employment disruption, wealth reallocation; can organize resistance but face suppression
 *   - Incumbent Fossil & High-Carbon Capital: Declared victim (institutional/trapped) — assets become liabilities under degrowth reading; cannot arbitrage into green sectors without accepting fundamental contraction; experience maximum extraction relative to this reading
 *   - Global North State Apparatus: Secondary actor (powerful/constrained) — faces coordination problem of managing transition; also benefits from delayed transition (extraction from Global South and future); trapped between climate physics and growth-dependent fiscal legitimacy
 *   - Green Capital / Clean Tech Sector: Beneficiary (institutional/arbitrage) — benefits from transition narrative; perceives degrowth as coordination mechanism mobilizing investment; experiences low extraction because benefits align with stated constraint direction
 *   - Climate Policy Bureaucracy: Institutional degradation (institutional/constrained) — mandated to address climate but structurally constrained to do so within growth framework; maintains performative compliance (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, snare).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Reading: Planned Economic Contraction as Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'a9a9185d-df49-4386-a518-c12f5f2fda4f').
narrative_ontology:cs_kernel_codification('a9a9185d-df49-4386-a518-c12f5f2fda4f', distributed).
narrative_ontology:cs_authority_grounding('a9a9185d-df49-4386-a518-c12f5f2fda4f', distributed).
narrative_ontology:cs_reading_relation('a9a9185d-df49-4386-a518-c12f5f2fda4f', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('a9a9185d-df49-4386-a518-c12f5f2fda4f', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('a9a9185d-df49-4386-a518-c12f5f2fda4f', foundational, carbon_budget_incompatible_with_growth).
narrative_ontology:cs_axiom_status(carbon_budget_incompatible_with_growth, holdable).
narrative_ontology:cs_axiom_grounding('a9a9185d-df49-4386-a518-c12f5f2fda4f', carbon_budget_incompatible_with_growth, empirically_contingent).
narrative_ontology:cs_axiom('a9a9185d-df49-4386-a518-c12f5f2fda4f', foundational, contraction_necessary_for_adequacy).
narrative_ontology:cs_axiom_status(contraction_necessary_for_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('a9a9185d-df49-4386-a518-c12f5f2fda4f', contraction_necessary_for_adequacy, deontological).
narrative_ontology:cs_created_at('a9a9185d-df49-4386-a518-c12f5f2fda4f', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_consumption).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, incumbent_growth_dependent_capital).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH POPULATIONS (SNARE) — Trapped by atmospheric carbon already committed; face existential climate harms (sea-level rise, water scarcity, monsoon collapse) regardless of consent to Global North's growth trajectory. No exit from constraint. High experienced extraction as their adaptive capacity is exhausted while Global North accumulates carbon debt. Suppression is total — material survival constraints preclude refusal.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Structurally trapped. Inherit a climate state determined by current carbon budget allocation. No option to exit, negotiate, or refuse the constraint. Maximum extraction: bear full climate harms without having benefited from the growth that caused them. Cannot be organized or represented in current-period political processes.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL NORTH WORKING-CLASS (SNARE) — Face contraction via job loss, reduced consumption, material reallocation without compensation for capital holders. Constrained exit: can organize collectively but face suppression via labor market dependence, credit debt, and political marginalization. Experience significant extraction as they bear contraction costs while capital is prioritized in transition planning. Perceive constraint as distributionally unjust (contraction without corresponding capital reallocation).
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL NORTH STATE APPARATUS (TANGLED ROPE) — Faces genuine coordination problem: managing stable transition during contraction while maintaining social legitimacy. Also benefits from delayed transition (extraction from future generations, Global South via delayed climate reckoning). Constrained exit: states cannot simply refuse the climate physics, but can defer costs. Experiences mixed coordination (transition planning) and extraction (preferential capital protection). Suppression high because political pressure from capital holders prevents equitable transition design.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GREEN CAPITAL / CLEAN TECH SECTOR (ROPE) — Benefits from transition narrative framing contraction as managed via green investment, not redistribution. Arbitrage: can exit growth dependence by capturing transition investment flows. Perceives the degrowth reading as coordination mechanism (mobilizing capital and state resources for transition infrastructure). Low perceived extraction because benefits align with stated constraint's direction — though underlying distribution is asymmetric.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT FOSSIL / HIGH-CARBON CAPITAL (SNARE) — Trapped by the degrowth reading's core claim: growth framework cannot solve climate crisis, therefore high-carbon sectors must contract rather than transition. No arbitrage option (cannot green sufficiently to maintain growth). Maximum extraction relative to this reading — assets become liabilities, profit streams evaporate, political legitimacy collapses. Experiences suppression through regulatory closure of carbon-intensive production.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE POLICY BUREAUCRACY (PITON) — Degraded institutional capacity. Mandated to address climate crisis but structurally constrained to do so within growth framework ('decoupling,' net-zero via carbon markets, green growth). Theater ratio high: policy performance (climate negotiations, carbon accounting, net-zero targets) substitutes for material emissions reduction. Perceives the degrowth reading as contradicting institutional mandate, hence maintains performative compliance rather than structural change.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, carbon budget limits are immutable physical constraints: global warming is determined by cumulative emissions, and the carbon budget remaining for 1.5°C or 2°C limits is a fixed quantity. No negotiation with physics. However, this perspective risks naturalizing a contingent institutional reading (degrowth as THE response to immutable limits) as itself immutable.
constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_harm_prevention__degrowth_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, TR),
    TR >= 0.70.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The degrowth reading extracts substantial material resources, wealth, and consumption opportunities from Global North present populations and incumbent capital. The extraction is structural — not incidental but central to the claim that contraction is necessary for climate limits to hold. The reading explicitly identifies who bears costs (Global North, incumbent capital) and who benefits (Global South, future generations). Base extractiveness rises from 0.42 to 0.68 over the interval as the reading becomes more empirically pressing (carbon budget depletes faster than anticipated) and suppression intensifies (political mobilization against contraction). Suppression (0.72): High. Multiple sources: (1) incumbent capital's active suppression of contraction narrative; (2) growth-dependent fiscal structures of Global North states (debt service, welfare programs legitimized through growth); (3) consumer constituencies' resistance to consumption reduction; (4) ideological suppression (framing growth as natural/necessary); (5) institutional inertia of state apparatus dependent on growth for legitimacy. Suppression requirement rises from 0.55 to 0.72 as the reading becomes more politically acute — more enforcement machinery is needed to actually implement contraction against intensifying resistance. Theater ratio (0.58): Moderate-High. Policy theater is substantial — net-zero targets, carbon markets, green investment pledges constitute performative response to the degrowth reading while avoiding implemented contraction. However, unlike pure piton (which is entirely theater), the degrowth reading drives some material policy (renewable deployment, efficiency mandates), hence theater_ratio is moderate rather than high. Theater ratio declines from 0.68 to 0.58 over the interval as material constraints become undeniable — policy theater has less room to substitute for actual emissions reduction as carbon budget tightens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence. Global South and future generations classify it as snare (trapped, no exit, high extraction). Global North working class classifies it as snare (constrained, high costs, organized but suppressed). Incumbent capital classifies it as snare (trapped by asset devaluation). State apparatus classifies it as tangled_rope (genuine transition coordination problem mixed with extraction benefit from delay). Green capital classifies it as rope (coordination mechanism benefiting them). Climate policy bureaucracy classifies it as piton (performative compliance with degraded function). The analytical observer risks classifying it as mountain (immutable carbon budget physics) — but the engine's false summit detector should identify this as naturalization of a contingent institutional reading. The perspectival gap reveals that this constraint's true character is political-economic (who bears contraction costs, how are losses distributed) layered over physical constraints (carbon budget is real). The physics gate is real; the extraction mechanism is institutional.
 *
 * DIRECTIONALITY LOGIC:
 *   The degrowth reading's directionality structure differs sharply across agent positions. Global South climate-vulnerable populations face d→1.0 (trapped + victim = maximum experienced extraction): they have zero exit options and bear full climate costs. Future generations face d→1.0 (trapped + victim + powerless = transcendental victimhood). Global North working-class populations face d→0.85-0.90 (constrained + victim + moderate power = high but partially contestable extraction). Incumbent high-carbon capital faces d→0.95 (trapped + victim + institutional power = maximum extraction because assets become worthless, but some capacity to resist through political mobilization). The state apparatus faces d→0.50 (powerful + constrained + mixed coordination/extraction = symmetric). Green capital faces d→0.10-0.20 (arbitrage + beneficiary = low effective extraction, even slight negative). The derivation chain prioritizes: structural data (trapped/constrained/mobile) determines base d; beneficiary/victim declarations refine it; exit options modulate it. The piton perspective's d is derived from theater_ratio gate rather than directionality — degraded institutional capacity produces piton classification independent of extraction direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading avoids mandatrophy collapse by clearly distinguishing its beneficiary set (Global South, future generations) from its victim set (Global North present consumption, incumbent capital). The constraint exhibits high extractiveness (0.68) because this asymmetry is precisely what the reading claims — contraction necessarily extracts from some agents to prevent harm to others. The reading does NOT falsely label this as coordination; it explicitly frames contraction as redistributive extraction with a justice aim (preventing climate harms to most-vulnerable populations). The mandatrophy is resolved through: (1) explicit beneficiary/victim declaration; (2) clear temporal framing (present bears costs to protect future); (3) spatial framing (Global North bears costs to protect Global South); (4) omega variables documenting the empirical and normative uncertainties (is contraction physically necessary? is the distribution ethically justified?). The reading's strength is that it owns the extraction rather than naturalizing it as coordination. Its vulnerability is that sibling readings (mitigation, adaptation) deny the extraction is necessary — they claim harm prevention can be achieved without Global North contraction, hence deny the beneficiary/victim asymmetry itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_substitutability_boundary,
    'Is high-carbon growth truly non-substitutable by green growth/decoupling within climate boundaries, or is the degrowth reading prematurely foreclosing a physically feasible alternative?',
    'Empirical testing of decoupling rates: comparison of global CO2 intensity improvement trajectories vs emissions reduction required for 1.5°C target; assessment of renewable energy scalability, electrification feasibility, and circular economy potential under growth scenarios',
    'If decoupling is feasible at required scale/speed: degrowth reading forecloses a viable alternative and misclassifies contraction as necessary rather than choice (reading transitions from prescriptive to foreclosed). If decoupling is physically infeasible: degrowth reading''s core claim holds and growth framework genuinely cannot solve climate within physics boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_substitutability_boundary, empirical, 'Whether high-carbon growth can be substituted by green growth without exceeding carbon budget').

omega_variable(
    contraction_distribution_separability,
    'Is planned economic contraction in Global North separable from redistribution to Global South, or does the reading conflate two distinct political-economic problems (climate physics vs. equity)?',
    'Decomposition analysis: model contraction scenarios with varying distribution mechanisms; compare climate outcomes (carbon reduction) vs. equity outcomes (welfare distribution) to test whether they are coupled or independent policy choices',
    'If separable: contraction reading is about carbon budget only; equity is a distinct constraint (justice reading). If inseparable: degrowth reading correctly identifies the political-economic coupling (contraction without redistribution is extraction). This is critical for understanding whether sibling readings (mitigation_priority, adaptation_priority) genuinely coexist or whether they obscure this coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_distribution_separability, conceptual, 'Whether contraction is separable from redistribution or inseparably coupled').

omega_variable(
    emission_budget_attribution,
    'How should historical cumulative emissions vs. current emissions vs. consumption-based accounting allocate responsibility for carbon budget remaining? Does this methodological choice determine whether degrowth reading''s beneficiary set (Global South, future generations) is structurally justified?',
    'Comparison of three attribution methodologies (production-based, consumption-based, cumulative historical responsibility) across different agent groups; assessment of which produces carbon budget allocations that align with degrowth reading''s victim/beneficiary claims',
    'If consumption-based accounting reveals high-income Global North populations as responsible for far larger share of remaining budget: degrowth reading''s structural claims are strongly empirically supported. If production-based accounting distributes responsibility differently: victim/beneficiary sets shift (manufacturing-dependent Global South nations may appear as larger carbon debtors). This omega is fundamentally about measurement-dependent classification — signals constraint decomposition may be needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emission_budget_attribution, empirical, 'Attribution methodology for historical and current emissions determines victim/beneficiary claims').

omega_variable(
    political_economy_coupling_to_growth,
    'Is the Global North state apparatus structurally dependent on growth for legitimacy and fiscal stability, or can contraction be managed through redistributive state capacity without regime collapse?',
    'Historical analysis of state stability under contraction (wartime economies, post-socialist transitions, structural adjustment); assessment of debt-to-GDP sustainability under negative growth; modeling of social legitimacy under redistribution vs. growth dependence',
    'If state legitimacy is growth-dependent: suppression (0.72) is correct because state apparatus cannot credibly enforce contraction without risking regime breakdown; degrowth reading becomes politically impossible (reads as foreclose on mitigation/adaptation readings). If states can manage redistribution and legitimacy under contraction: suppression should be lower (0.45-0.55), degrowth reading becomes an alternative rather than a necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_coupling_to_growth, empirical, 'Whether state apparatus can sustain legitimacy under planned contraction with redistribution').

omega_variable(
    reading_normative_vs_descriptive_ambiguity,
    'Is this reading a descriptive claim about physical/political necessity (''contraction is required by climate physics and political economy''), a normative claim about justice (''contraction should be imposed on Global North as reparation''), or both?',
    'Textual analysis of degrowth discourse; separation of empirical claims (growth incompatible with climate limits) from prescriptive claims (Global North should bear costs); assessment of whether the reading''s force depends on conflating these',
    'If primarily descriptive (physics): reading forecloses growth/mitigation readings; classification is determined by physical constraints. If primarily normative (justice): reading coexists with mitigation/adaptation readings that propose alternative ethical frameworks; classification becomes preference-dependent. If conflated: classification is indeterminate until claims are separated. This omega is epistemically critical — determines whether disagreement with the reading is empirical or axiological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_normative_vs_descriptive_ambiguity, conceptual, 'Whether reading is descriptive necessity, normative claim, or inseparably conflated').

omega_variable(
    technological_lock_in_reversibility,
    'To what degree can high-carbon infrastructure (power plants, transportation systems, buildings, industrial capacity) be rapidly transitioned vs. abandoned? Does the technical lock-in support degrowth reading''s claim that growth framework cannot pivot fast enough?',
    'Engineering assessment of infrastructure lifetimes and retrofit feasibility; cost analysis of accelerated retirement vs. operational life; empirical measurement of transition speed in sectoral decarbonization (electricity, transport, buildings)',
    'If lock-in is severe and transition is slower than carbon budget allows: degrowth reading''s physical constraint is supported (growth cannot transition fast enough). If lock-in can be overcome via accelerated investment: contraction may be choice rather than necessity, weakening degrowth reading''s foreclosing claim on mitigation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_lock_in_reversibility, empirical, 'Whether high-carbon infrastructure lock-in prevents rapid transition within growth').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_degrowth_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(clim_degrowth_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(clim_degrowth_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_degrowth_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_degrowth_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_degrowth_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_degrowth_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_degrowth_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(clim_degrowth_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, global_north_fiscal_legitimacy_dependence).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, incumbent_fossil_capital_stranding).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family under the climate_harm_prevention kernel. The degrowth_reading proposes contraction as necessary; mitigation_priority reading proposes technology/policy within growth; adaptation_priority reading proposes resilience/managed retreat. These are not observational variants of one constraint but genuinely different structural claims about what prevents climate harm. The family structure is linked via network.affects_constraints because each reading's feasibility affects the others' plausibility. The degrowth reading influences the other two by claiming its preconditions are necessary; the other readings influence this one by claiming its preconditions are false. Decomposition is intentional — each reading gets its own epsilon-invariant constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
