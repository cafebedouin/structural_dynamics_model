% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy (Decoupling via Innovation)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading treats climate response as an engineering
 *   problem solvable through technological innovation (renewables, battery
 *   storage, carbon capture, green hydrogen) combined with carbon pricing
 *   mechanisms (cap-and-trade, carbon tax) that make fossil fuels expensive
 *   while preserving growth trajectories in wealthy economies. The reading
 *   asserts that absolute decoupling (growth without emissions rise) is
 *   achievable at necessary scale through substitution and efficiency gains.
 *   This constraint story models the legitimacy claim of that reading: what
 *   it instantiates as a standing arrangement, who it benefits, who it
 *   extracts from, and what happens if it fails. The mitigation-priority
 *   reading competes with adaptation-priority (which accepts some warming and
 *   prioritizes protecting vulnerable populations) and
 *   degrowth-transformation (which argues growth is incompatible with climate
 *   stability). This story instantiates ONLY the mitigation-priority reading
 *   as a clean, ε-invariant constraint; the siblings are other constraint
 *   stories in the same family. The kernel contest sits in omegas and
 *   cs_structure, not embedded in the base narrative.
 *
 * KEY AGENTS:
 *   - Incumbent fossil fuel industries: benefit from gradual transition timelines and offset mechanisms that preserve extractive value.
 *   - High-income wealthy economies: benefit from growth preservation and outsourced mitigation through carbon credits.
 *   - Technology innovation sectors: capture rents through subsidy-driven renewable and CDR deployment.
 *   - Future generations: trapped payers bearing residual warming if decoupling fails or scales too slowly.
 *   - Low-income populations present: constrained payers bearing transition costs without equivalent benefit.
 *   - Carbon-dependent regions: payers bearing economic base destruction without alternative pathways.
 *   - Policy technical elite: agenda-setters who author and gate-keep the decoupling narrative and carbon pricing design.
 *   - Adaptation-priority advocates: excluded voices arguing mitigation-first sacrifices present vulnerability.
 *   - Degrowth advocates: excluded voices arguing growth preservation guarantees target failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy (Decoupling via Innovation)").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '3c7cd764-28f0-4b1c-8208-0179690e9cac').
narrative_ontology:cs_kernel_codification('3c7cd764-28f0-4b1c-8208-0179690e9cac', formalized).
narrative_ontology:cs_authority_grounding('3c7cd764-28f0-4b1c-8208-0179690e9cac', expertise).
narrative_ontology:cs_interpretation_layer_present('3c7cd764-28f0-4b1c-8208-0179690e9cac').
narrative_ontology:cs_reading_relation('3c7cd764-28f0-4b1c-8208-0179690e9cac', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('3c7cd764-28f0-4b1c-8208-0179690e9cac', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('3c7cd764-28f0-4b1c-8208-0179690e9cac', foundational, growth_decoupling_conjecture).
narrative_ontology:cs_axiom_status(growth_decoupling_conjecture, holdable).
narrative_ontology:cs_axiom_grounding('3c7cd764-28f0-4b1c-8208-0179690e9cac', growth_decoupling_conjecture, empirically_contingent).
narrative_ontology:cs_axiom('3c7cd764-28f0-4b1c-8208-0179690e9cac', foundational, technological_optimism_doctrine).
narrative_ontology:cs_axiom_status(technological_optimism_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3c7cd764-28f0-4b1c-8208-0179690e9cac', technological_optimism_doctrine, instrumental).
narrative_ontology:cs_reference_frame('3c7cd764-28f0-4b1c-8208-0179690e9cac', market_efficiency_with_climate_corrective).
narrative_ontology:cs_drift_state('3c7cd764-28f0-4b1c-8208-0179690e9cac', contemporary_carbon_accounting_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c7cd764-28f0-4b1c-8208-0179690e9cac', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_fuel_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, high_consumption_high_income_economies).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, technology_innovation_sectors).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_populations_present).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, carbon_dependent_regions).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, technological_optimism_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, growth_decoupling_conjecture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incumbent energy and materials producers benefit from carbon pricing architecture that sets gradual price floors and allows offset credits; they can amortize stranded assets over extended timescales and invest accumulated capital in renewable transition at profitable rates. They actively shape the policy design (carbon pricing mechanism, offset eligibility, transition timelines) to preserve extractive value during the shift. Growth preservation means their business models remain structurally viable.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_fossil_fuel_industries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_fossil_fuel_industries, agenda_setter).

% Wealthy nations commit to emissions reductions while preserving GDP growth, consumption levels, and capital accumulation. They outsource manufacturing emissions through carbon-offset purchases in the Global South, allowing domestic metrics to show decoupling while global emissions often rise. They possess capital for technology investment, renewable infrastructure, and climate adaptation, concentrating adaptation benefits among their own populations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, high_consumption_high_income_economies, beneficiary,
    institutional, generational, arbitrage, continental).

% Solar, wind, battery, carbon capture, and synthetic fuel industries capture rents through technology deployment subsidies, carbon pricing revenue recycling, and intellectual property on renewable systems. The constraint's focus on innovation-as-solution creates demand for their products and services at scale.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, technology_innovation_sectors, beneficiary,
    institutional, biographical, mobile, global).

% Bear the residual warming from emissions reductions that fall short of Paris targets (if decoupling fails or scales too slowly), experiencing sea-level rise, climate cascade failures, mass migration, and ecosystem collapse. They have no choice set, no negotiating power, no exit. They do not participate in the present benefit stream and cannot retroactively alter the carbon budget their inheritance receives.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% In both Global North and South, bear transition costs: energy price increases from carbon pricing, job losses in fossil fuel regions without adequate retraining, reduced access to cheap electricity during the transition, and exposure to climate impacts without the capital for adaptation. They are excluded from policy design (decoupling framings originate in wealthy governments and technical bodies); their voices are heard only as 'climate vulnerable' or 'just transition' afterthoughts, never as co-designers of legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_populations_present, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, low_income_populations_present, excluded).

% Economies structurally dependent on fossil fuel extraction (coal regions, oil-dependent petrostate economies) experience carbon pricing as an external imposition that destroys local economic bases without offering transition pathways. They bear transition costs while the decoupling benefit accrues elsewhere, and their exit options are constrained by geography and capital availability.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_dependent_regions, payer,
    moderate, biographical, constrained, regional).

% Climate economists, energy modelers, and policy designers embedded in wealthy governments, central banks, and multilateral institutions author the decoupling narrative and carbon pricing frameworks. They exercise gate-keeping power over which problem framings count as legitimate and which solutions count as technically sound. Their models assume continued growth and substitution at scales not yet proven.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, policy_technical_elite, agenda_setter,
    institutional, generational, mobile, global).

% Climate scientists, development economists, and representatives from highly vulnerable nations and populations who argue adaptation infrastructure and resilience-building deserve equal or greater priority than mitigation. They are excluded from setting the legitimacy frame, which treats mitigation-first as the default and adaptation as secondary or context-specific.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_priority_advocates, excluded,
    moderate, biographical, constrained, global).

% Ecological economists, labor movements, and climate justice advocates who argue decoupling is empirically implausible and growth itself is the problem, proposing structural transformation instead. They are largely excluded from mainstream policy design and face suppression through marginalization in academic and policy venues; their framing is treated as economically naive rather than as an alternative legitimate response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_transformation_advocates, excluded,
    powerless, civilizational, constrained, global).

% The IPCC and global climate science community provide the empirical warrant for the existential threat, but they remain deliberately neutral on policy approaches. Their consensus certifies the problem (warming) without endorsing the mitigation-priority solution; some IPCC pathways include degrowth and high adaptation, others rely on speculative CDR at planetary scale.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, observer_scientific_consensus, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, high_consumption_high_income_economies).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common emissions reduction target (Paris alignment, net-zero by 2050) and a global mechanism (carbon pricing, offset credits) to allocate mitigation burden, creating a single framework for tracking progress instead of fragmented national policies with gaming incentives.
% TRANSFER_FUNCTION: Transfers immediate transition costs (stranded asset write-downs, worker displacement, energy price increases) to present-day low-income populations and carbon-dependent regions, while preserving growth-trajectory benefits for wealthy economies and transferring long-term residual warming risk to future generations. Transfers wealth through carbon credits from high-income to low-income nations, contingent on those nations adopting compatible carbon-reduction frameworks.
% ABSENT_VOICES: Adaptation-priority advocates (who would argue mitigation-first sacrifices present vulnerable populations to speculative future technological success) and degrowth advocates (who would argue growth preservation guarantees failure) are excluded from legitimacy design. Low-income populations most exposed to transition costs are heard only as beneficiaries of 'just transition' rhetoric, not as co-designers of the constraint itself.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing and its carbon pricing machinery vanished overnight, climate policy would fragment into uncoordinated national actions; fossil fuel industries would extend extraction timelines; technology investment would collapse where subsidies end; and competing framings (adaptation-first or degrowth-transformation) would contend for legitimacy, altering investment patterns, transition timelines, and distributional consequences fundamentally.
% FOUNDING_PROBLEM: Global emissions are accumulating toward irreversible climate tipping points, requiring a response that stabilizes atmospheric CO2 while preserving the economic capacity of states to fund adaptation and development.
% FOUNDING_PROBLEM_CORROBORATION: Climate science (IPCC reports) confirms the emissions accumulation and tipping-point risks. The mitigation-priority reading's claim that decoupling is achievable at necessary scale is CONTESTED: (a) some climate scientists and energy modelers endorse it; (b) ecological economists and climate data analysts argue decoupling is empirically failing to materialize at required pace; (c) development economists argue the founding problem is ALREADY solved for wealthy nations (they can afford both mitigation and adaptation) and remains open only for poor nations (who bear costs without capacity). Advocates from outside the high-income policy elite dispute whether growth preservation is structurally compatible with the Paris target.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers transition costs asymmetrically to powerless present-day populations and future generations while preserving growth benefits for wealthy economies. The constraint is NOT pure extraction (it does solve a genuine coordination problem: common emissions targets require common frameworks); it is tangled rope — real coordination wrapped in asymmetric cost distribution. Suppression (0.62) is moderately high because the constraint's persistence requires suppressing alternative framings (adaptation-priority, degrowth) through institutional marginalization, model selection bias toward growth-compatible scenarios, and rhetorical control of what counts as 'realistic' or 'economically sound.' Theater (0.48) is elevated because corporate decoupling messaging and 'net-zero commitments' often consist of accounting maneuvers (Scope 3 exclusions, offsetting-via-credit), net-zero target rhetoric unmatched by near-term action, and speculative CDR promises substituting for emissions reductions. The measurement series tracks increasing extractiveness and steady theater as the constraint matures: incumbent industries adapt to carbon pricing by investing in renewable infrastructure (arbitrage optionality), transition costs compound for locked-in populations, and technological feasibility gaps widen (forcing more reliance on speculative future CDR). All metrics share one time grid so the engine can track co-evolution without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the policy-technical-elite and beneficiary seats (high-income governments, technology sectors), the mitigation-priority constraint is legitimate coordination: a framework that enables global emissions reductions while preserving the capacity to invest in solutions. From the powerless and constrained seats (future generations, low-income populations, carbon-dependent regions), the same structure is asymmetric extraction: a framework that protects present wealth by deferring and distributing costs to those with no choice set. The engine will compute different types per seat. The beneficiary seats likely compute rope or tangled_rope with low effective extraction toward themselves; the victim seats compute snare or tangled_rope with high effective extraction targeting them. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil fuel industries and high-income economies benefit from the constraint's growth-preservation framing (low d toward beneficiaries; their effective extraction χ dampens or reverses). They have exit options (arbitrage-class: can invest capital, shift markets, adjust business models). Future generations and low-income populations are structurally trapped (high d toward victims; their effective extraction χ amplifies). They have no exit, no choice set, no ability to renegotiate. The policy technical elite set the constraint but do not uniformly bear its costs; overrides may be warranted to differentiate their directionality from beneficiaries (they are institutional but not benefiting in the same way). Technology sectors sit between: they benefit from deployment incentives but are not as centrally positioned as fossil fuel incumbents. Carbon-dependent regions are powerful enough to negotiate at the regional/national level but trapped within the global framework; their d sits moderate-to-high, reflecting constrained rather than fully trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (emissions accumulation toward tipping points) is empirically live: atmospheric CO2 is rising, climate impacts are observable, and warming continues. However, the founding problem's SOLUTION STATUS is contested. The mitigation-priority reading asserts that technological decoupling can solve it while preserving growth. This assertion is a normative claim grounded in technological optimism and market efficiency, not an established fact. Degrowth advocates argue the founding problem is unsolvable under growth preservation; adaptation advocates argue growth-preservation is a luxury that diverts resources from protecting vulnerable present populations. The constraint's mandatrophy risk is HIGH: if decoupling fails to materialize at the scale and pace required (carbon intensity falling faster than economic growth), the founding problem persists while the constraint's costs compound — future generations inherit both the original problem AND the opportunity cost of foregone transformation pathways. A Tangled Rope classification captures this: genuine coordination (emissions targets, common framework) plus asymmetric extraction (growth preservation for some, cost distribution to others) plus active enforcement (suppressing alternative framings). If the engine computes Snare across multiple seats, it signals the coordination function is being overwhelmed by the extraction function — a mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_empirical,
    'Is absolute decoupling (economic growth without emissions rise) achievable at the scale and pace required to meet Paris targets through technological innovation and carbon pricing alone?',
    'Empirical tracking: compare carbon intensity trajectory against growth rate at national and global scales over next 15–25 years. If emissions fall faster than GDP grows in high-income economies while Scope 3 (outsourced) emissions and cumulative stock rise globally, decoupling is regional accounting, not material achievement. Natural experiments from carbon pricing regimes (EU ETS, Nordic carbon tax) provide data; long-term energy projections from modeling centers (IEA Net Zero scenario, IPCC AR6 scenarios) provide baseline.',
    'If decoupling fails to achieve required pace, the constraint''s founding problem remains unsolved while its costs accumulate — reclassification toward Snare (extraction without coordination function). If decoupling succeeds, Tangled Rope classification holds and the constraint remains structurally defensible from the mitigation-priority frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_feasibility_empirical, empirical, 'Whether technological decoupling materializes at necessary scale and pace.').

omega_variable(
    technological_feasibility_at_scale,
    'Is the speculative technology (carbon capture and removal at gigatonne scale, green hydrogen at industrial scale, synthetic fuels for aviation/shipping) technically and economically viable at the deployment scale required by 2050?',
    'Pilot project performance, learning curve progression for emerging technologies, energy return on investment analyses, and capital requirements modeling. If cost trajectories stall, deployment lags behind required scale, or energy inputs remain unsustainable, the mitigation-priority reading''s technological optimism breaks.',
    'If speculative technologies fail to scale, the mitigation-priority constraint loses its main mechanism for avoiding catastrophic warming while preserving growth — bifurcating into a choice between accepting higher warming (reclassify toward adaptation-priority) or accepting transformation (reclassify toward degrowth). This is an axiom_overriding drift event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_feasibility_at_scale, empirical, 'Whether emerging climate technologies achieve necessary scale and cost.').

omega_variable(
    growth_emissions_causality_contest,
    'Is the constraint''s core assumption true — that technological decoupling can break the structural link between economic growth and emissions — or is growth itself the driver of emissions that must be addressed directly?',
    'Ecological economics analysis, input-output modeling of production networks, and long-term time-series analysis of growth vs. emissions correlations. If structural analyses show that growth generates emissions through material throughput regardless of technological efficiency gains (rebound effects, composition-shift, absolute resource depletion), the causal premise fails.',
    'If growth is causally inseparable from emissions, the mitigation-priority axiom (growth_decoupling_conjecture) is overridden by empirical evidence. The constraint would shift toward acknowledging that growth preservation is incompatible with climate targets — reclassifying toward degrowth-transformation as an alternative reading or foreclosing the mitigation-priority reading entirely within the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_emissions_causality_contest, conceptual, 'Whether economic growth and emissions are structurally decoupable or causally inseparable.').

omega_variable(
    intergenerational_extraction_mechanism,
    'Is the constraint''s transfer of residual warming risk to future generations a structural feature of the mitigation-priority approach, or could it be addressed through different policy design (higher near-term carbon prices, faster technology deployment, adaptation investment)?',
    'Comparative policy modeling: run identical climate models under different mitigation-priority designs (high price/fast transition vs. moderate price/gradual transition) and measure cumulative warming, adaptation costs, and distributional outcomes across present and future. If all mitigation-priority designs produce futures where present generations preserve growth while future generations bear warming residuals, extraction is structural; if some designs shift the distribution, the extraction is policy-dependent.',
    'If extraction is structural to the reading (all growth-preserving mitigation paths transfer risk), the Tangled Rope classification is stable. If extraction is policy-dependent, alternative mitigation-priority designs might reduce it — softening the reclassification risk but leaving the foundational question open (whether growth preservation is compatible with climate targets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_mechanism, empirical, 'Whether intergenerational cost transfer is inherent to mitigation-priority or policy-contingent.').

omega_variable(
    suppression_of_alternative_framings,
    'Is the institutional suppression of adaptation-priority and degrowth-transformation readings a side effect of legitimate scientific consensus, or a structural feature of a constraint that benefits from excluding those voices?',
    'Institutional history: audit funding flows, journal acceptance rates, policy venue access, and rhetorical space for competing climate response framings in major policy bodies (UNFCCC, national governments, multilateral development banks). If adaptation and degrowth framings receive proportional institutional space and funding despite scientific consensus favoring mitigation, suppression is not structural; if they are systematically excluded despite having scientific and ethical warrant, suppression is structural and serves the constraint''s extraction function.',
    'If suppression is structural, it enters the theater ratio and indicates the constraint is performing legitimacy rather than achieving it — elevating mandatrophy risk. If suppression is incidental to scientific consensus, theater drops and the classification remains clearer (coordination + extraction without performative overhead).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_framings, conceptual, 'Whether suppression of alternative climate framings is structural or incidental to consensus.').

omega_variable(
    kernel_reading_relationship,
    'What is the relationship between the three climate-response readings (mitigation-priority, adaptation-priority, degrowth-transformation) in the climate_response_legitimacy kernel? Do they coexist as live alternatives for different parties, or does one reading logically foreclose another?',
    'Conceptual analysis: examine whether the core axioms of each reading are logically contradictory (foreclosure) or represent different legitimate priorities that could coexist in a pluralistic response (coexistence). Empirical check: monitor whether any party or institution actually holds multiple readings simultaneously (practical coexistence) or actively rejects coexistence (foreclosure claim).',
    'If readings foreclose each other, the kernel contest is a zero-sum winner-take-all dispute and one constraint story captures the entire climate-response legitimacy surface. If readings coexist, the three constraints are genuinely independent accounts of different legitimate framings, and a meta-constraint story may be needed to model how different societies/actors choose among them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Logical and practical relationships among climate-response readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t3, climate_response_legitimacy__mitigation_priority, theater_ratio, 3, 0.4).
narrative_ontology:measurement_basis(clim_tr_t3, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__mitigation_priority, theater_ratio, 6, 0.43).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__mitigation_priority, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__mitigation_priority, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(clim_tr_t18, projected).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(clim_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t3, climate_response_legitimacy__mitigation_priority, base_extractiveness, 3, 0.56).
narrative_ontology:measurement_basis(clim_be_t3, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__mitigation_priority, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__mitigation_priority, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__mitigation_priority, base_extractiveness, 18, 0.66).
narrative_ontology:measurement_basis(clim_be_t18, projected).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t3, climate_response_legitimacy__mitigation_priority, suppression_requirement, 3, 0.58).
narrative_ontology:measurement_basis(clim_su_t3, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__mitigation_priority, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__mitigation_priority, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__mitigation_priority, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(clim_su_t18, projected).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(clim_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate_response_legitimacy kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what constitutes legitimate climate response. The mitigation-priority reading (this story) prioritizes emissions reduction via innovation and carbon pricing while preserving growth. The adaptation-priority reading accepts warming trajectory and prioritizes resilience infrastructure. The degrowth-transformation reading requires structural economic transformation and growth dismantling. These are not the same constraint viewed from different angles — they have different ε values (different referents assessed by different readings), different victim/beneficiary structures, and different persistence mechanisms. They are linked via network.affects_constraints to enable cross-story analysis of how each reading shapes the others' legitimacy conditions and resource availability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
