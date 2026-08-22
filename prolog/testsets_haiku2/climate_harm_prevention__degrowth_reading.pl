% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction for Climate Mitigation (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of climate harm prevention frames legitimate
 *   mitigation as requiring deliberate, planned reduction of material
 *   consumption and GDP in the Global North, not technological decoupling
 *   from growth. This reading instantiates a specific constraint: the claim
 *   that emissions reductions compatible with climate targets and
 *   intergenerational justice require absolute contraction in high-income
 *   economies, that this contraction is the governing binding constraint (not
 *   technological transition, not green growth), and that the constraint
 *   distributes costs asymmetrically—Global North present consumption bears
 *   the burden, Global South populations and future generations are primary
 *   beneficiaries. This is one of three readings of the contested kernel
 *   'climate harm prevention'; it coexists with the growth-compatible
 *   mitigation reading and the adaptation-priority reading. The degrowth
 *   reading rejects the framing that growth is a boundary condition and
 *   instead treats it as a variable that must contract to satisfy physical
 *   and ethical constraints.
 *
 * KEY AGENTS:
 *   - global_north_present_consumers: bear the consumption contraction (payer)
 *   - fossil_fuel_dependent_workers: trapped payers with identity-locked exit
 *   - global_south_populations: primary beneficiaries, structurally unequal power to enforce
 *   - future_generations: beneficiaries excluded from voice
 *   - degrowth_aligned_movements: agenda-setters framing the reading
 *   - orthodox_climate_institutions: excluded from degrowth epistemic authority
 *   - global_north_governments: institutional agenda-setters and payers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.78).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction for Climate Mitigation (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '8f5c1f20-4e62-4fc6-991c-f0a3fe641c79').
narrative_ontology:cs_kernel_codification('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', distributed).
narrative_ontology:cs_authority_grounding('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', distributed).
narrative_ontology:cs_reading_relation('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', foundational, contraction_is_binding_constraint).
narrative_ontology:cs_axiom_status(contraction_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', contraction_is_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', foundational, growth_framework_legitimacy_compromised).
narrative_ontology:cs_axiom_status(growth_framework_legitimacy_compromised, holdable).
narrative_ontology:cs_axiom_grounding('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', growth_framework_legitimacy_compromised, deontological).
narrative_ontology:cs_reference_frame('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', emissions_budget_allocation_framework).
narrative_ontology:cs_drift_state('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', contemporary_climate_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f5c1f20-4e62-4fc6-991c-f0a3fe641c79', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, fossil_fuel_dependent_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, growth_dependent_capital).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations in wealthy nations who have benefited from energy-intensive consumption patterns (transportation, heating, meat consumption, manufactured goods). Under the degrowth reading, they must reduce consumption materially—not incrementally—to make room for emissions reductions while Global South develops. Exit options are structural: leaving the nation abandons the constraint but not the global carbon budget; individual consumption reduction is insufficient without systemic contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_present_consumers, payer,
    moderate, biographical, constrained, national).

% Workers in coal mining, oil extraction, gas utilities, and related supply chains in the Global North. The degrowth reading requires rapid workforce transition away from fossil-fuel sectors. They bear the direct cost (job loss, retraining uncertainty, identity loss tied to occupational community) and have the fewest exit options—geographic immobility, skill-specific training, community dependence on fossil-fuel economies. They may benefit from just-transition support under degrowth framing, but this is often promised rather than delivered.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, fossil_fuel_dependent_workers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, fossil_fuel_dependent_workers, beneficiary).

% Populations in lower-income nations bearing the heaviest climate impacts despite minimal historical emissions responsibility. Under the degrowth reading, Global North contraction frees up a larger share of the global carbon budget for their development needs—energy access, agricultural transformation, industrialization. They are positioned as primary beneficiaries, but their power to enforce the constraint on the Global North is mediated through international agreements and climate negotiations where historical power asymmetries persist.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    powerful, generational, mobile, continental).

% Humans born after ~2050 who inherit the climate and ecological state resulting from today's mitigation choices. The degrowth reading treats them as the primary moral beneficiary: lower warming, preserved habitability, reduced climate-forced migration. They have zero exit options and no voice in current policy formation; they are excluded from negotiations by definition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, future_generations).

% Financial and corporate capital structures (asset managers, multinational corporations, growth-oriented financial institutions) whose business models depend on GDP growth. Degrowth directly threatens asset valuations, investment returns, and operational assumptions. They have high exit options (capital flight, regulatory arbitrage, pivot to growth markets elsewhere) but face constraint enforcement through carbon pricing, fossil-fuel divestment, and supply-chain restrictions. They are positioned as payers because the constraint requires reallocation of capital away from growth-oriented accumulation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_dependent_capital, payer,
    institutional, biographical, arbitrage, global).

% Environmental justice movements, labor unions advocating just transition, youth climate movements, and ecosocialist political formations that frame the constraint as necessary and legitimate. They set the reading's agenda by framing mitigation as requiring contraction, by linking climate justice to redistribution, and by advocating enforcement mechanisms (carbon budgets, planned workforce transitions, wealth redistribution). Their power derives from coalition-building and legitimacy claims rather than institutional authority.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_aligned_movements, agenda_setter,
    organized, generational, mobile, national).

% IPCC, central banks, mainstream climate policy bodies, and macroeconomic establishments that officially endorse mitigation-within-growth as the viable path. They are excluded from the degrowth reading's legitimacy set—their institutional authority is treated as compromised by growth-dependence. Their alternative reading (mitigation_priority) coexists as a live institutional consensus, but the degrowth reading denies their framing as valid for its purposes. They would argue that planned contraction is politically impossible; degrowth proponents counter that it is growth that is politically and ecologically impossible.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, orthodox_climate_and_economic_institutions, excluded,
    institutional, biographical, constrained, global).

% State actors in wealthy nations who must implement or resist the degrowth constraint. Under the reading, they are positioned as agenda-setters because enforcement requires deliberate policy (carbon budgets, planned industrial contraction, wealth redistribution). They are simultaneously payers because their electoral constituencies bear the consumption costs. They face domestic political pressure from growth-dependent capital and present-consumption constituencies against the constraint, and international pressure from Global South and climate movements for it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_governments, payer).

% The physical climate system and biosphere whose stability and regenerative capacity depend on cumulative emissions constraints. Under the degrowth reading, they are primary beneficiaries: lower atmospheric CO2, preserved ecosystem services, reduced tipping-point risk. They have no agency in the constraint and no voice except through the scientific representation that measures their state.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_impacts_and_ecological_limits, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, climate_impacts_and_ecological_limits).

% Scientific and analytical seat that observes the constraint's structure without internal stake in its outcome. Measures whether the degrowth reading's claims about physical feasibility and political sustainability hold empirically, documents beneficiary/victim relationships, and assesses whether contraction-based mitigation is structurally distinguishable from growth-based mitigation or merely a different framing of the same constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, degrowth_aligned_movements).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates a global carbon budget across nations and generations by requiring deliberate contraction in high-emissions economies, creating space for development emissions in low-income regions and preserving habitability for future people. Solves the collective-action problem of cumulative emissions by making explicit what growth-framing obscures: that there is a hard physical cap and someone must not exceed their share.
% TRANSFER_FUNCTION: Transfers consumption entitlements (embodied energy, material throughput, carbon budget allocation) from Global North present consumers to Global South populations and future generations. Moves capital and labor from growth-dependent sectors to contraction-enabling sectors (renewable infrastructure, ecosystem restoration, care work). Redirects wealth from capital-dependent accumulation to need-based redistribution.
% ABSENT_VOICES: Orthodox economists and mainstream climate institutions are explicitly excluded from the degrowth reading's epistemic authority—their framing of growth as compatible with mitigation is treated as structural bias rather than legitimate disagreement. Fossil-fuel workers in the Global North appear in policy but are often unheard on the question of whether just transition is materially available under the reading's constraints. Global South workers and informal-economy populations have limited voice in international climate negotiations despite being primary beneficiaries. Future generations are absent by definition.
% DISAPPEARANCE_RATIONALE: If the degrowth constraint were to vanish, Global North consumption would continue rising, cumulative emissions would exceed climate targets, Global South development would be foreclosed by climate impacts rather than by contraction logic, and habitability thresholds would be crossed. The disappearance of the constraint is empirically equivalent to the disappearance of the possibility of staying within 1.5–2°C warming bounds without massive future climate impacts—the world does not rearrange by choice but is rearranged by forced adaptation to a destabilized climate.
% FOUNDING_PROBLEM: Climate-safe emissions pathways require cumulative CO2 budget allocation that gives Global South room for development while preventing catastrophic warming. Growth-based mitigation (decoupling carbon from GDP) has failed to produce the required emissions reductions in 30+ years of policy despite repeated commitments. The technological and political constraints on growth-based mitigation mean the only physically viable path requires deliberate contraction in high-emissions economies.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy modelers (IPCC AR6, Hickel & Kallis, Haberl et al.) document that growth-based decoupling has not materialized at necessary scale and timeline. Global South climate negotiators and environmental justice movements attest that adaptation burdens are unsustainable without mitigation and that growth frameworks perpetuate unequal allocation. Mainstream economic institutions dispute whether contraction is necessary or politically viable—their testimony frames the founding problem as solved by technology and policy, not by contraction. The disagreement itself is structural evidence: those bearing the costs dispute the diagnosis, those bearing the risks affirm it.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 final) because the degrowth reading requires material sacrifice from present Global North consumers with unequal distribution of burden and benefit—the constraint extracts consumption entitlements from one constituency (present high-income populations) and allocates them to others (Global South, future generations) without compensation. Suppression is substantial (0.68) because the constraint's enforcement depends on actively preventing growth-oriented capital flight, on resisting the legitimacy of growth-centered institutions, and on constraining consumption behavior that would otherwise expand. Theater ratio is moderate (0.42): some portion of the constraint's enforcement consists of discourse about green growth and just transition that obscures the degree of actual contraction required; simultaneously, the measurement series shows theater rising early (0.28→0.42 from t=0 to t=40) then stabilizing, suggesting that as the constraint's enforcement hardens, theatrical cover becomes less tenable and the pure extraction becomes more visible. The measurement trajectory shows extractiveness rising substantially over the interval (0.45→0.78), reflecting the cumulative binding of the constraint as climate impacts intensify and Global North resistance hardens, forcing more explicit enforcement mechanisms. Suppression requirement rises in parallel (0.42→0.68), indicating that maintaining the constraint against capital exit and consumer resistance requires progressively more coercive enforcement. This is a tangled rope: it carries a genuine coordination function (allocating a global carbon budget justly) AND asymmetric extraction (those who did not create the carbon stock must contract, those who did not benefit from its accumulation receive development room). Active enforcement is required: carbon budgets must be monitored, capital flows must be redirected, growth-dependent sectors must be wound down, and the legitimacy of growth-centered institutions must be continuously contested.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Global North consumers, fossil-fuel workers, growth-dependent capital) and the beneficiary seats (Global South, future generations, degrowth movements) compute to dramatically different constraint types. From the Global North payer perspective, the degrowth reading appears as a snare—a structure that forecloses alternatives (growth paths are denied), suppresses exit (capital cannot relocate freely, consumption cannot be maintained), and offers no compensation (the burden is framed as ecological necessity rather than purchased acceptance). From the Global South beneficiary and future-generation perspective, the degrowth reading appears as a rope—a genuine coordination solution to the collective-action problem of emissions allocation, with legitimate benefits. From the degrowth movement's perspective, it is a rope defending a necessary boundary condition. The engine computes this divergence from the structural data: payers experience high d (near 1.0, full target), beneficiaries experience low d (near 0.0, beneficiary seat), agenda-setters experience mixed d reflecting their institutional position. No single observed type emerges; instead, the per-seat divergence IS the signal—a constraint whose legitimacy depends entirely on whose seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   The degrowth reading declares global_south_populations and future_generations as beneficiaries (they gain development and habitability room) and global_north_present_consumers and fossil_fuel_dependent_workers as victims (they bear consumption contraction and sectoral displacement). This asymmetry drives directionality: beneficiaries get low d (their exit options matter—Global South is mobile across policy regimes, future generations are analytically defined, and both have leverage through legitimacy claims and international climate negotiations); victims get high d (Global North consumers are identity-locked into consumption norms, fossil-fuel workers are trapped geographically and occupationally, and both lack exit to other carbon budgets). Growth-dependent capital is a victim (d≈0.75) because it bears extraction (capital reallocation, asset devaluation) but has higher exit options (arbitrage, capital flight, sectoral pivot) than worker victims. Degrowth-aligned movements are secondary beneficiaries (they gain agenda-setting authority and moral framing) despite being Global North citizens; their d is lowered because they have mobile exit options (they can leave growth-dependent capital/consumption as a lifestyle choice) and they gain status/authority from the reading. The directionality logic is NOT symmetric across power levels: two moderate-power actors in the same reading can have vastly different d based on their exit options and structural position (e.g., a Global North moderate-power degrowth activist vs. a Global North moderate-power oil-worker). This demands close attention to exit_options in the stakeholder declarations: identity_locked (workers), constrained (general consumers, governments), mobile (degrowth movements, Global South), and arbitrage (capital) all compress or amplify the structural directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading contains an irreducible mandatrophy tension: it claims to solve the founding problem (just allocation of a carbon budget) but that solution is contested to the point of excluding the institutional authority (orthodox climate and economic institutions) that would normally implement it. If the founding problem is 'how to allocate emissions fairly,' then mainstream mitigation-within-growth is a valid response (the mitigation_priority reading); the degrowth reading's answer is 'growth frameworks are illegitimate for this problem.' This is not mandatrophy in the classical sense (the founding problem has died and the constraint persists theatrically) but rather a **reading conflict**: two different answers to the same founding problem cannot both be true, and the degrowth reading's persistence depends on continuously contesting the legitimacy of the competing reading. The constraint is NOT a degraded rope (theater_ratio is moderate, not high; the functional extraction is real). Rather, it is a tangled_rope that carries both genuine coordination (carbon budget allocation) and genuine extraction (contraction burden on Global North). The mandatrophy signal appears in the excluded-institution structure: the degrowth reading cannot succeed without delegitimizing orthodox institutions, which means its enforcement is partially theatrical (it must perform the illegitimacy of the excluded parties rather than simply solve the founding problem). This is not false consciousness but rather honest structural conflict: you cannot enforce a reading that depends on excluding the authority of the institutions meant to implement it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_empirical_possibility,
    'Can energy and material throughput be decoupled from GDP growth at the scale and speed required for climate targets, or is contraction structurally necessary?',
    'Time-series analysis of decoupling trends at national and global scale; examination of whether sectoral decoupling (renewable energy sector growth) represents genuine absolute decoupling or substitution effects masked by measurement scope. Controlled comparison of high-carbon and low-carbon growth pathways under climate constraints.',
    'If decoupling is empirically possible at required scale/speed, the degrowth reading''s foundational claim fails and the mitigation_priority reading''s framing becomes viable. If decoupling proves impossible or too slow, the degrowth reading''s diagnosis is confirmed and the constraint''s legitimacy strengthens, though enforcement suppression may also increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_decoupling_empirical_possibility, empirical, 'Whether growth-based decoupling is physically feasible at climate-required scale.').

omega_variable(
    political_feasibility_of_planned_contraction,
    'Is deliberate, planned economic contraction politically feasible in democratic Global North societies, or does contraction inevitably occur through collapse and crisis?',
    'Observation of whether any Global North government successfully implements carbon-budget-constrained contraction with broad consent; study of pre-transition periods where contraction was politically sustained (post-1945 demobilization, 1970s oil-crisis responses) to identify feasibility conditions. Modeling of contraction pathways that maintain legitimacy through redistribution and community stability.',
    'If planned contraction proves politically infeasible and only crisis-driven contraction occurs, the degrowth reading''s prescription fails even if its diagnosis is correct—the constraint cannot be enforced, and adaptation-priority reading gains ground. If planned contraction is politically viable under certain conditions (strong redistribution, community embedding, legitimacy from Global South), the constraint''s feasibility and suppression metrics require revision downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_planned_contraction, empirical, 'Whether democratic governments can enforce planned contraction without crisis.').

omega_variable(
    just_transition_availability_under_contraction,
    'Can fossil-fuel workers and affected communities be materially supported through contraction-based transitions, or is just transition rhetorically promised but structurally unavailable under contraction budgets?',
    'Examination of actual just-transition spending relative to worker displacement in early contraction regions; modeling of whether renewable/care-sector job creation can absorb displaced fossil-fuel workers at equivalent wage/status; comparison of transition outcomes in regions with strong union power vs. weak. Study of whether workers who experience transition support maintain political consent or perceive it as insufficient compensation.',
    'If just transition is materially available, victim status of fossil-fuel workers is partly reframed as payer-with-support, and their identity-lock may be loosened through community rebuilding. If just transition is unavailable, their victim status hardens, resistance increases, and suppression requirement rises. This directly affects whether workers become coalition partners with degrowth movements or opponents of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_availability_under_contraction, empirical, 'Whether just-transition support is materially sufficient under contraction.').

omega_variable(
    global_south_enforcement_power,
    'Can Global South populations and governments enforce contraction commitments on Global North actors, or does the power asymmetry mean Global North actors can unilaterally exit the constraint?',
    'Examination of enforcement mechanisms in international climate agreements (financial commitments, trade sanctions, climate litigation); assessment of whether Global South has sufficient coalition power to impose costs on non-compliant Global North actors; study of whether alternative development pathways (regional industrialization, South-South trade) reduce dependence on Global North compliance.',
    'If Global South enforcement power is sufficient, the constraint is plausibly enforceable and the beneficiary role is credible. If Global South lacks enforcement power, the constraint becomes a unilateral sacrifice by Global North populations without guarantee of Global South benefit, which increases victim perception and resistance. This directly affects suppression_requirement and resistance metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_enforcement_power, empirical, 'Whether Global South has structural power to enforce contraction on Global North.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the degrowth reading logically foreclose the mitigation_priority reading (growth-compatible mitigation), or do they coexist as incompatible but simultaneously-held institutional positions?',
    'Logical analysis of whether ''contraction is necessary'' and ''growth-compatible decoupling is sufficient'' can be held in the same framework. Empirical observation of whether institutional actors (central banks, IPCC, governments) hold both readings simultaneously or are forced to choose. Historical examination of whether one reading would liquidate the other if given full institutional authority.',
    'If foreclosure holds, one reading must ultimately triumph and the other will be institutionally suppressed—the constraint becomes existential to the policy regime that adopts it. If coexistence holds, both readings persist, creating institutional ambiguity and allowing actors to claim both commitment to climate action and growth compatibility. This affects the long-term stability and theater ratio of the degrowth constraint—high theater if readings coexist, lower theater if foreclosure occurs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether degrowth and growth-compatible readings logically foreclose each other.').

omega_variable(
    indigenous_knowledge_and_alternative_development,
    'Does incorporation of indigenous knowledge systems and non-capitalist development pathways fundamentally reframe the degrowth reading, or does it remain a reading of a Western climate-harm-prevention kernel?',
    'Engagement with indigenous scholars and communities on whether the degrowth framing captures their ontologies or imposes a Western growth-critical lens on different knowledge systems. Study of whether indigenous-led development models (communal land management, subsistence-embedded economies) should be classified as pre-degrowth (they existed before growth''s dominance) or as alternative constraints entirely. Examination of whether the degrowth reading''s Global South beneficiary framing erases Global South indigenous leadership or elevates it.',
    'If indigenous pathways are authentically incorporated, the degrowth reading''s beneficiary class shifts from abstract ''Global South populations'' to specific indigenous communities and their knowledge-keepers, which changes stakeholder power dynamics and demands changes to agenda-setter roles. If degrowth remains a Western reading imposed on Global South, the constraint''s legitimacy among Global South populations may be lower than assumed, and beneficiary perception is complicated by colonial dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_and_alternative_development, conceptual, 'Whether degrowth reading authentically incorporates indigenous development pathways or imposes Western framings.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.68 final) primarily structural (external barriers, capital controls, institutional exclusion) or internalized (citizens believe contraction is right, suppress their own desires), and how does this ratio shift over the interval?',
    'Post-exit observation: if populations exit a contraction region/regime and suppression persists despite barrier removal, suppression is internalized. Survey evidence on whether citizens perceive contraction as imposed (structural) or legitimate (internalized). Study of whether enforcement costs change as internalization increases—a fully internalized constraint should require less active enforcement.',
    'If suppression is structural, the constraint is fragile and depends on continuous coercive expenditure (high cost to enforce, vulnerability to capital exit). If suppression is internalized, the constraint is more stable but raises questions about whether it is legitimate in a liberal-democratic sense—citizens have accepted constraints on freedom of choice. A shift from structural to internalized over time (observed in the measurement series) suggests successful legitimacy-building but also deeper identity-alteration. This directly affects sustainability of the constraint and political feasibility of planned contraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized and how the ratio shifts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(clim_tr_t50, climate_harm_prevention__degrowth_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(clim_be_t50, climate_harm_prevention__degrowth_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(clim_su_t50, climate_harm_prevention__degrowth_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% The degrowth_reading is one of three readings of the contested kernel climate_harm_prevention. Each reading answers the same founding problem (how to prevent climate harm to present and future populations) with a different mechanism and burden distribution. Siblings: mitigation_priority (growth-compatible technology-driven emissions reduction) and adaptation_priority (resilience-building given infeasible mitigation). Network links establish family relationships; each reading has its own ε, stakeholder structure, and type classification. Do not attempt to average or reconcile ε across readings—ε is a property of a reading, not a topic. The degrowth reading's high extractiveness (0.78) reflects the contraction burden on Global North consumers; the mitigation_priority reading's lower extractiveness reflects its framing of technology as compatible with growth; the adaptation_priority reading reflects acceptance of higher warming and thus lower immediate extraction (future harm instead). These are three different constraints reading the same kernel differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, moderate, 0.88).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, powerless, 0.9).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
