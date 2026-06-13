% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth-Based Climate Response: Structural Economic Transformation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   Degrowth transformation is ONE READING of the contested climate response
 *   kernel. This reading argues that climate stability requires systematic
 *   rejection of GDP growth as organizing principle and instead prioritizes
 *   sufficiency (meeting human needs within planetary boundaries), equity
 *   (redistributing from Global North to Global South and from present to
 *   future generations), and reduced resource throughput. It demands deep
 *   structural reorganization: universal basic services, working time
 *   reduction, democratic firm ownership, regenerative land use, and asset
 *   transfer from fossil fuel sectors to climate restoration. The reading
 *   frames climate response not as a technological problem solvable while
 *   maintaining growth, but as a redistributive political economy problem
 *   requiring acceptance of lower material throughput in wealthy regions and
 *   equitable access in developing regions. Sibling
 *   readings—mitigation_priority (emissions reduction via technology and
 *   markets while maintaining growth) and adaptation_priority (accepting
 *   temperature rise and investing in resilience)—coexist in public discourse
 *   but are rejected as sufficient or legitimate by this reading's advocates.
 *
 * KEY AGENTS:
 *   - global_north_wealthy_populations: Payers bearing consumption reduction, wealth redistribution, and working time transition
 *   - global_south_development_communities: Beneficiaries gaining development rights and equitable access to basic services
 *   - future_generations: Beneficiaries whose survival depends on present-era extraction limits
 *   - labor_movement: Dual beneficiary (working time reduction, job security) and payer (transitional cost)
 *   - fossil_fuel_corporations: Payers facing asset stranding and structural elimination
 *   - climate_justice_advocates: Agenda-setters defining the framework through mass mobilization and policy design
 *   - technological_substitution_advocates: Excluded from authoritative coalition; their core premise is rejected
 *   - international_financial_institutions: Excluded; their mandate contradicts degrowth implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth-Based Climate Response: Structural Economic Transformation").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, 'a8d7bf5e-e374-4c30-889f-64b33dca1bce').
narrative_ontology:cs_kernel_codification('a8d7bf5e-e374-4c30-889f-64b33dca1bce', distributed).
narrative_ontology:cs_authority_grounding('a8d7bf5e-e374-4c30-889f-64b33dca1bce', distributed).
narrative_ontology:cs_reading_relation('a8d7bf5e-e374-4c30-889f-64b33dca1bce', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('a8d7bf5e-e374-4c30-889f-64b33dca1bce', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('a8d7bf5e-e374-4c30-889f-64b33dca1bce', foundational, growth_decoupling_empirically_impossible).
narrative_ontology:cs_axiom_status(growth_decoupling_empirically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('a8d7bf5e-e374-4c30-889f-64b33dca1bce', growth_decoupling_empirically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('a8d7bf5e-e374-4c30-889f-64b33dca1bce', foundational, intergenerational_equity_non_negotiable).
narrative_ontology:cs_axiom_status(intergenerational_equity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('a8d7bf5e-e374-4c30-889f-64b33dca1bce', intergenerational_equity_non_negotiable, deontological).
narrative_ontology:cs_axiom('a8d7bf5e-e374-4c30-889f-64b33dca1bce', secondary, consumption_reduction_required_in_wealthy_regions).
narrative_ontology:cs_axiom_status(consumption_reduction_required_in_wealthy_regions, holdable).
narrative_ontology:cs_axiom_grounding('a8d7bf5e-e374-4c30-889f-64b33dca1bce', consumption_reduction_required_in_wealthy_regions, empirically_contingent).
narrative_ontology:cs_reference_frame('a8d7bf5e-e374-4c30-889f-64b33dca1bce', post_growth_sufficiency_baseline).
narrative_ontology:cs_drift_state('a8d7bf5e-e374-4c30-889f-64b33dca1bce', contemporary_carbon_budget_exhaustion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a8d7bf5e-e374-4c30-889f-64b33dca1bce', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_development_communities).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, ecosystem_integrity).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, labor_movement).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_north_wealthy_populations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_fuel_corporations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, high_consumption_sector_workers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, labor_movement).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_sector_workers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, national_governments).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundaries_doctrine).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, intergenerational_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently enjoy consumption levels 5-10x sustainable per-capita throughput. Degrowth transformation requires immediate reduction in material consumption, energy use, and resource throughput via mandatory efficiency standards, carbon pricing with redistribution, working time reduction (transitioning to 30-hour work weeks), and access to universal basic services rather than private consumption. Exit options are constrained by the physical limits of the Earth system—no alternative planet, and opting out individually while others comply merely frees up carbon budget for others' use.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_wealthy_populations, payer,
    powerful, biographical, constrained, global).

% Historically denied development rights due to Global North carbon occupation of atmospheric space. Degrowth framework reallocates emissions budget and development capital, enabling access to universal basic services (electricity, clean water, healthcare, education, dignified housing) without repeating the resource-intensive development path. Gains equitable access to technology transfer and climate finance repurposed as reparations and development rights, not loans.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_communities, beneficiary,
    moderate, generational, mobile, global).

% Inherit a climate system and biosphere whose stability depends on current-era emissions reductions. Degrowth framework prioritizes their survival and flourishing over present consumption levels. They cannot negotiate, exit, or reverse decisions made now; their interests are represented only through formal intergenerational ethics frameworks and present-era advocacy.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Degrowth framework includes working time reduction (30-hour work weeks with maintained income via universal basic services), democratic firm ownership, and job transition support for fossil fuel sectors. Gains security, time sovereignty, and governance power. Simultaneously bears transitional costs of massive skill retraining, industry restructuring, and initial income uncertainty during the reallocation phase.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, labor_movement, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, labor_movement, payer).

% Face mandatory divestment, asset stranding, and prohibition of new extraction. Their business model is structurally incompatible with degrowth transformation. Their employees and communities face transition; the corporations themselves face either conversion to renewable-energy operators (losing profit margins) or liquidation.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_fuel_corporations, payer,
    institutional, biographical, trapped, global).

% Work in advertising, luxury goods, automotive, aviation, and fast fashion sectors whose primary function is stimulating consumption above sufficiency levels. Face transition: either sector shrinkage and retraining into care, restoration, and public infrastructure work, or reorientation toward durable, repairable, locally-sourced goods. Gain job security, reduced precarity, and time sovereignty through universal basic services, but lose current specialization and income status.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, high_consumption_sector_workers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, high_consumption_sector_workers, beneficiary).

% Non-agent entity: the biophysical system's resilience, biodiversity, and carbon sequestration capacity. Benefits from lower throughput, reduced habitat destruction, and regenerative land use. Represented only through scientific frameworks and non-human rights discourse.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, ecosystem_integrity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_action__degrowth_transformation, ecosystem_integrity).

% Climate technologists, carbon capture vendors, geoengineering researchers, and growth-decoupling advocates who argue that emissions reductions can be achieved while maintaining GDP growth through innovation, carbon markets, and technological substitution. Degrowth framework categorically rejects their core premise as empirically implausible given remaining carbon budget and energy return thresholds. They are excluded from the authoritative coalition defining climate response policy; their exclusion is the reading's defining feature.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, technological_substitution_advocates, excluded,
    institutional, biographical, analytical, global).

% Indigenous communities, Global South climate movements, labor unions, and climate-focused NGOs advocating degrowth transformation. Set the agenda via mass mobilization, policy proposals, litigation, and narrative framing. Author the framework's core premises: sufficiency over growth, equity as climate stability requirement, and intergenerational justice as organizing principle.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_justice_advocates, agenda_setter,
    organized, generational, mobile, global).

% Face pressure to implement degrowth policies (carbon taxes, working time reduction, universal basic services, public ownership transition) while managing political resistance from wealthy constituencies and capital holders. Those that implement face capital flight, investor downgrades, and accusations of unilateral sacrifice. Those that refuse face climate litigation, social instability, and loss of climate legitimacy. Exit is geophysically impossible—climate does not recognize borders.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, national_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, national_governments, agenda_setter).

% IMF, World Bank, regional development banks whose standard operating procedure mandates GDP growth, fiscal austerity, and privatization. Degrowth framework directly contradicts their core mandate. They are operationally excluded from designing climate response under a degrowth model; their exclusion is active and structural.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, international_financial_institutions, excluded,
    institutional, biographical, trapped, global).

% Provides biophysical evidence (carbon budgets, ecosystem tipping points, energy return on investment thresholds) that forms the epistemic ground for the reading. Takes no direct stake but provides the empirical warrant for the claim that technological substitution alone is insufficient.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, scientific_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, global_south_development_communities).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing climate system collapse and cascading ecosystem failure by coordinating rapid, equitable reduction in global resource throughput to remain within planetary boundaries. Without coordinated action, the tragedy of the commons produces uncoordinated emissions and habitat destruction that doom all actors regardless of individual compliance. The constraint coordinates behavior toward sufficiency (meeting human needs) rather than growth.
% TRANSFER_FUNCTION: Moves development rights, carbon budget, and consumption space FROM Global North wealthy populations TO Global South communities and future generations. Simultaneously transfers wealth (via carbon taxes, asset seizure from fossil fuel sectors, and reparative climate finance) from high-consumption sectors to labor transitions, ecosystem restoration, and universal basic services. Moves time sovereignty FROM capital accumulation TO workers via working time reduction.
% ABSENT_VOICES: Technological substitution advocates and carbon market proponents are structurally excluded from authority—their core claim (decoupling is possible without degrowth) is rejected as empirically implausible within this reading. International financial institutions and debt-holding countries are excluded because their mandate (GDP growth, austerity, debt service) directly conflicts with degrowth implementation. Fossil fuel corporations are excluded because their survival requires the constraint's failure. The excluded parties would argue: growth is still possible with innovation, carbon pricing solves the externality, transition costs are too high, and Global South development requires the same high-carbon path Global North took.
% DISAPPEARANCE_RATIONALE: If degrowth transformation vanished—if the constraint were removed and business-as-usual growth resumed—carbon budgets would be exhausted within 7-15 years, triggering runaway climate instability, mass ecosystem collapse, and permanent loss of habitability in large regions. The world would not merely rearrange; it would destabilize beyond human adaptive capacity. The constraint's disappearance would remove the only framework that prevents physical catastrophe.
% FOUNDING_PROBLEM: Climate system warming driven by cumulative fossil fuel emissions and high-throughput consumption in Global North. Existing technological and market-based approaches (renewable substitution, carbon pricing, efficiency gains) have failed to produce sufficient emissions reductions while maintaining growth; they rest on false assumptions about energy return thresholds and resource availability. Degrowth transformation was articulated to solve what growth-based mitigation cannot: absolute emissions reduction compatible with equity, regeneration, and planetary boundaries.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Sixth Assessment Report confirms carbon budgets are exhausted on current trajectories; external corroboration from systems ecologists (Georgescu-Roegen, Hickel), energy researchers (Hall, Murphy) on energy return thresholds, and climate justice scholars document that growth decoupling is not occurring at required speed. Global South climate negotiators, indigenous communities, and labor movements attest the problem is not solved. Contradicted by growth-alliance advocates (WEF, major central banks, technology sector) who claim innovation can maintain growth; their corroboration carries conflict-of-interest signals. External validation from UN regional commissions and independent economic analyses supports the founding problem's live status.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 endpoint because the constraint imposes asymmetric costs (consumption reduction) on Global North wealthy populations while benefits accrue to Global South communities and future generations who bear the physical burden of climate catastrophe. The measurement series shows extractiveness rising initially (t0=0.42 to t20=0.67) as the constraint becomes more concretely implemented, then stabilizing (t25-t40=0.68) once steady-state degrowth economics establish. Suppression starts at 0.55 and rises to 0.72 because implementation requires active enforcement against: fossil fuel corporations' resistance to asset seizure, wealthy populations' resistance to consumption reduction, and capital holders' resistance to wealth redistribution. The enforcement machinery includes carbon pricing with steep penalties, mandatory transition of extractive industries, wealth taxes, and border carbon adjustments to prevent capital flight. Theater ratio rises from 0.25 to 0.41 then plateaus because early implementation includes substantial performative elements (corporate sustainability pledges, greenwashing, false carbon offsets) that gradually reduce as actual restructuring proceeds; the plateau at 0.41 reflects persistent performative maintenance of growth narratives even within degrowth systems. Accessibility of alternatives collapses steeply (structural level 0.88→0.92) because the constraint eliminates the option to continue high-consumption growth within planetary boundaries—it is physically impossible, not politically optional. At individual level (0.48→0.65) alternatives appear more accessible because individual exit is possible (emigration, informal economy participation, preference signaling) even if systemic exit is not. Resistance is high and persistent (0.72→0.82 at structural level) because wealthy populations and capital holders mount sustained opposition; class-level resistance remains high (0.82→0.80) because the constraint redistributes within social hierarchies. The coercion grid reflects intensifying suppression at organizational level (primary institutional resistance locus) while resistance also intensifies—this dual escalation is characteristic of tangled ropes where coordination is real but extraction is substantial.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (climate justice advocates, labor movements) computes the constraint as necessary coordination with equitable benefit distribution. The payer seat (global_north_wealthy_populations, fossil_fuel_corporations) computes the same structure as punitive extraction violating their sovereignty and development trajectory. A third perspective—technological advocates excluded from the coalition—computes the constraint as unnecessary and counterproductive, arguing their preferred mitigation pathway is less costly and more feasible. The engine computes each seat's classification from power + time_horizon + exit_options + beneficiary/victim status: global_north_wealthy agents (powerful, biographical horizon, constrained exit, payer role) should compute as high-d targets (directionality 0.75-0.85); global_south development communities (moderate power, generational horizon, mobile exit, beneficiary role) compute as lower-d beneficiaries (directionality 0.15-0.25); labor (organized power, biographical horizon, constrained exit, dual beneficiary/payer role) computes as intermediate (directionality 0.45-0.55). The divergence is structural and unavoidable—the constraint's benefits and costs genuinely distribute asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Global_north_wealthy_populations are high-d targets (d≈0.78): powerful enough to resist collectively, biographical time horizon means they bear the full weight of transition costs, constrained exit (cannot move to another planet), and payer role (consumption reduction is direct extraction). Directionality is not overridden but accurately describes their structural position—the constraint extracts from them by design. Global_south_development_communities are low-d beneficiaries (d≈0.22): moderate power, generational horizon aligns with constraint's stability benefits, mobile exit (can pursue alternative development paths regionally), and beneficiary role (gain development rights and equitable resource access). Labor_movement is intermediate (d≈0.48): organized power, biographical horizon, constrained exit, but genuinely dual position (benefits from working time reduction and job security, pays transition costs). Fossil_fuel_corporations are high-d targets (d≈0.81): institutional power is fragile (confronting physical elimination of business model), biographical time horizon of corporation is short, trapped exit (cannot transform into something else without ceasing to be fossil fuel operators), payer role (asset seizure, revenue loss). Climate_justice_advocates are moderate-d beneficiaries (d≈0.32): organized power, generational horizon, mobile exit (can shift to other movement work), agenda-setter role mixed with beneficiary role (they author the framework but benefit from it). No directionality overrides are required; the structural data produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate destabilization from high-throughput consumption and fossil fuel emissions) is live and acute—carbon budgets are being exhausted in real time. The founding problem status is not obsolete. However, the mandatrophy question is whether degrowth transformation is the mandated response or whether alternative responses (technological mitigation, adaptation investment) could be equally valid. The constraint prevents mandatrophy collapse by maintaining that its founding problem (climate system stability) requires the specific response (degrowth), not that any response solves it. The measurement series shows extractiveness plateauing at 0.68 rather than declining toward zero—this is NOT mandatrophy because the extraction is not from a solved problem, but from the asymmetric distribution of the solution's costs. Mandatrophy would appear if extractiveness rose while the founding problem (climate destabilization) declined—evidence that the constraint had become extractive theater maintained for its own sake. Current projections show suppression increasing in parallel with resistance, indicating the constraint is actively defended because its beneficiaries benefit and its payers resist, not because it has become empty maintenance. The constraint avoids mandatrophy but faces high political feasibility risk—see omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_possibility_ambiguity,
    'Is absolute decoupling of emissions from economic growth physically possible at required speed and scale using available energy and technology, or does remaining carbon budget require absolute reduction in global resource throughput?',
    'Empirical data on energy return on investment thresholds, life-cycle assessments of renewable energy infrastructure, and modeling of full transition timelines under different growth assumptions. Resolved by independent research institutions conducting scenario analysis.',
    'If decoupling is possible at required speed, mitigation_priority reading becomes empirically viable and degrowth transformation''s core mandate collapses. If decoupling is not possible, degrowth transformation''s founding problem remains live and mandatory. This is the empirical foundation of the reading''s differentiation from sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_possibility_ambiguity, empirical, 'Whether growth-compatible emissions reduction is physically achievable within remaining carbon budget.').

omega_variable(
    political_feasibility_of_enforcement,
    'Can degrowth transformation be implemented against the resistance of capital-holding classes and powerful institutions, or will capital flight, non-compliance, and political reversal prevent sustained enforcement?',
    'Natural experiments from jurisdictions attempting degrowth-aligned policies (Costa Rica, Bhutan, some Nordic countries): track whether carbon taxes, working time reduction, and wealth redistribution laws persist under political pressure or get reversed. Examine coalition durability in movement organizations and electoral bases.',
    'If enforcement fails and capital exits, the constraint collapses into theater (high theater_ratio, low actual extractiveness) and becomes a piton—maintained for legitimacy but non-functional. If enforcement persists, the constraint remains a functional tangled rope. Political feasibility failure is not logically foreclosing but empirically devastating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_enforcement, empirical, 'Whether the constraint can be sustained against institutional and capital resistance.').

omega_variable(
    intergenerational_representation_legitimacy,
    'Do present-era actors have legitimate epistemic and normative standing to make binding choices on behalf of future generations, or is the constraint''s claim to represent future interests a form of paternalism that forecloses their own decision-making?',
    'Philosophical and legal scholarship on intergenerational justice; empirical investigation of what future-generation preferences would be if they could be consulted (impossible by definition, but approximated by studying young people''s stated preferences and inheritance expectations). Constitutional frameworks recognizing standing of future generations (some national constitutions now include this).',
    'If present actors have no standing to decide, degrowth transformation''s legitimacy rests on a shaky foundation—it is paternalism applied to the powerless and unborn. If present actors do have standing (most compelling argument: climate catastrophe denies future generations choice entirely, so present action preserves their ability to choose), then the constraint''s authority is robust. This affects whether the constraint is accepted as legitimate coordination or perceived as illegitimate extraction by excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_representation_legitimacy, conceptual, 'Legitimacy of present-era constraint decisions affecting future generations.').

omega_variable(
    global_south_autonomy_paradox,
    'Does degrowth transformation respect Global South autonomy and development aspirations, or does it impose a consumption ceiling that denies Southern populations the development trajectory Northern populations took?',
    'Empirical study of how degrowth policies are received and implemented in Global South contexts; analysis of whether transformation frameworks center Southern voice and priorities or impose Northern climate ethics. Investigation of whether universal basic services provide genuine development access or mere poverty management.',
    'If degrowth respects autonomy and provides genuine access to flourishing at lower throughput, the reading''s equity mandate is fulfilled and beneficiary/victim structure is accurate. If degrowth imposes a ceiling that denies Southern populations development, the reading collapses into a Northern-preservation movement using Southern rights language—victims would include Global South populations, and extractiveness would increase substantially. This affects whether the constraint is a genuine tangled rope (coordination with asymmetric distribution) or a snare (pure extraction with equity cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_autonomy_paradox, conceptual, 'Whether degrowth transformation respects Global South development autonomy or imposes external consumption limits.').

omega_variable(
    democratic_firm_ownership_implementation,
    'Can democratic ownership of productive assets be scaled to complex, interconnected economic systems, or does it collapse under coordination difficulty at the scale of supply chains and infrastructure?',
    'Empirical evidence from cooperative networks, worker-owned enterprises, and municipal utilities operating at scale. Assessment of governance mechanisms in large democratic firms. Modeling of transition from current capital structures to democratic ownership at full economic scale.',
    'If democratic ownership can scale, degrowth transformation''s structural logic is feasible and the constraint maintains coherence. If it cannot scale, alternative ownership structures (public sector, common-pool management, strategic hybrid models) would be necessary, shifting the constraint''s implementation and potentially increasing theater_ratio and suppression_requirement significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_firm_ownership_implementation, empirical, 'Whether democratic firm ownership is implementable at full economic scale.').

omega_variable(
    kernel_reading_differentiation_from_mitigation_priority,
    'Is this reading (degrowth_transformation) logically distinct from the mitigation_priority reading, or are they both describing the same physical requirement with different rhetorical frames?',
    'Examination of whether the readings produce divergent policy prescriptions when applied to concrete cases (e.g., aviation industry, agricultural systems, energy infrastructure). If prescriptions diverge (degrowth: reduce aviation; mitigation: make it carbon-neutral), readings are distinct. If prescriptions converge once decoupling is operationalized, readings may be rhetorical variants of the same constraint.',
    'If readings are truly distinct (coexists_with relation is correct), the corpus maintains two constraints with different implementations and ethical premises. If readings converge (influences or forecloses relation becomes applicable), the constraint family structure requires revision. This affects whether sibling readings genuinely compete or are merely framing differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_differentiation_from_mitigation_priority, conceptual, 'Whether degrowth and mitigation-priority readings are structurally distinct constraints or rhetorical variants.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (enforced by external barriers, incentive structures, legal prohibition) or internalized (wealthy populations accept reduced consumption as legitimate, incorporating it into their self-concept)?',
    'Post-transition tracking: examine whether high-consumption preferences persist when external constraints are removed (e.g., in jurisdictions where degrowth policies are reversed). Study whether internalization occurs through education, cultural change, and normalization of sufficiency. Behavioral research on consumption preferences under different institutional contexts.',
    'If suppression is purely structural, removing enforcement mechanisms would eliminate the constraint. If suppression is internalized, consumption reduction becomes self-maintaining and the constraint transitions to rope-like stability. If suppression is mixed, the constraint''s stability depends on perpetual enforcement—a feature of high-maintenance tangled ropes and vulnerable pitons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of high-consumption preferences is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, projected).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__degrowth_transformation, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(clim_tr_t5, projected).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__degrowth_transformation, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, projected).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__degrowth_transformation, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(clim_tr_t15, projected).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__degrowth_transformation, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__degrowth_transformation, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_action__degrowth_transformation, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_action__degrowth_transformation, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(clim_be_t0, projected).
narrative_ontology:measurement(clim_be_t5, climate_response_action__degrowth_transformation, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(clim_be_t5, projected).
narrative_ontology:measurement(clim_be_t10, climate_response_action__degrowth_transformation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, projected).
narrative_ontology:measurement(clim_be_t15, climate_response_action__degrowth_transformation, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, projected).
narrative_ontology:measurement(clim_be_t20, climate_response_action__degrowth_transformation, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_response_action__degrowth_transformation, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_action__degrowth_transformation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_action__degrowth_transformation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, projected).
narrative_ontology:measurement(clim_su_t5, climate_response_action__degrowth_transformation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(clim_su_t5, projected).
narrative_ontology:measurement(clim_su_t10, climate_response_action__degrowth_transformation, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(clim_su_t10, projected).
narrative_ontology:measurement(clim_su_t15, climate_response_action__degrowth_transformation, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(clim_su_t15, projected).
narrative_ontology:measurement(clim_su_t20, climate_response_action__degrowth_transformation, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_response_action__degrowth_transformation, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_action__degrowth_transformation, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_action__degrowth_transformation, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_response_action__degrowth_transformation, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_02, climate_response_action__degrowth_transformation, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(clim_grid_03, climate_response_action__degrowth_transformation, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_04, climate_response_action__degrowth_transformation, accessibility_collapse(individual), 40, 0.65).
narrative_ontology:measurement(clim_grid_05, climate_response_action__degrowth_transformation, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(clim_grid_06, climate_response_action__degrowth_transformation, accessibility_collapse(organizational), 40, 0.78).
narrative_ontology:measurement(clim_grid_07, climate_response_action__degrowth_transformation, accessibility_collapse(structural), 0, 0.88).
narrative_ontology:measurement(clim_grid_08, climate_response_action__degrowth_transformation, accessibility_collapse(structural), 40, 0.92).
narrative_ontology:measurement(clim_grid_09, climate_response_action__degrowth_transformation, resistance(class), 0, 0.82).
narrative_ontology:measurement(clim_grid_10, climate_response_action__degrowth_transformation, resistance(class), 40, 0.8).
narrative_ontology:measurement(clim_grid_11, climate_response_action__degrowth_transformation, resistance(individual), 0, 0.68).
narrative_ontology:measurement(clim_grid_12, climate_response_action__degrowth_transformation, resistance(individual), 40, 0.72).
narrative_ontology:measurement(clim_grid_13, climate_response_action__degrowth_transformation, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(clim_grid_14, climate_response_action__degrowth_transformation, resistance(organizational), 40, 0.81).
narrative_ontology:measurement(clim_grid_15, climate_response_action__degrowth_transformation, resistance(structural), 0, 0.72).
narrative_ontology:measurement(clim_grid_16, climate_response_action__degrowth_transformation, resistance(structural), 40, 0.82).
narrative_ontology:measurement(clim_grid_17, climate_response_action__degrowth_transformation, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(clim_grid_18, climate_response_action__degrowth_transformation, stakes_inflation(class), 40, 0.78).
narrative_ontology:measurement(clim_grid_19, climate_response_action__degrowth_transformation, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(clim_grid_20, climate_response_action__degrowth_transformation, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(clim_grid_21, climate_response_action__degrowth_transformation, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(clim_grid_22, climate_response_action__degrowth_transformation, stakes_inflation(organizational), 40, 0.85).
narrative_ontology:measurement(clim_grid_23, climate_response_action__degrowth_transformation, stakes_inflation(structural), 0, 0.95).
narrative_ontology:measurement(clim_grid_24, climate_response_action__degrowth_transformation, stakes_inflation(structural), 40, 0.98).
narrative_ontology:measurement(clim_grid_25, climate_response_action__degrowth_transformation, suppression(class), 0, 0.55).
narrative_ontology:measurement(clim_grid_26, climate_response_action__degrowth_transformation, suppression(class), 40, 0.7).
narrative_ontology:measurement(clim_grid_27, climate_response_action__degrowth_transformation, suppression(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_28, climate_response_action__degrowth_transformation, suppression(individual), 40, 0.64).
narrative_ontology:measurement(clim_grid_29, climate_response_action__degrowth_transformation, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(clim_grid_30, climate_response_action__degrowth_transformation, suppression(organizational), 40, 0.73).
narrative_ontology:measurement(clim_grid_31, climate_response_action__degrowth_transformation, suppression(structural), 0, 0.58).
narrative_ontology:measurement(clim_grid_32, climate_response_action__degrowth_transformation, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_action kernel. The sibling readings (mitigation_priority and adaptation_priority) are separate constraint stories with different ε values, beneficiary/victim structures, and enforcement requirements. All three readings address the same founding problem (climate destabilization) but instantiate structurally distinct constraints. The kernel decomposition follows ε-invariance: measuring degrowth transformation one way (enforcement against consumption reduction) yields high ε; measuring it another way (coordination of resource allocation within planetary boundaries) yields lower ε. These are not different measurements of the same constraint—they are different constraints (the reading-specific constraint vs. the underlying coordination problem). Each story must maintain a single, stable ε. Degrowth transformation carries the higher ε because it foregrounds the asymmetric extraction from Global North wealthy populations. A parallel story focused on the coordination function (sufficiency coordination without framing as extraction from wealthy) would carry lower ε and would be a different reading or a different constraint entirely. The network links show how one reading's adoption affects the viability of sibling readings: if degrowth implementation succeeds, mitigation_priority and adaptation_priority become largely superseded; if it fails, adaptation_priority becomes the default fallback.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, institutional, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
