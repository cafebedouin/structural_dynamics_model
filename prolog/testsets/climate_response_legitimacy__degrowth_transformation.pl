% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation as Climate Legitimacy Standard
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the DEGROWTH TRANSFORMATION reading of the
 *   contested kernel 'climate_response_legitimacy'. The reading defines
 *   legitimate climate response as mandatory dismantling of growth
 *   imperatives in wealthy nations through structural economic
 *   transformation: universal basic services, working time reduction (20-30
 *   hour work weeks), and transition to democratic/cooperative firm
 *   ownership. This is one of three competing readings of what counts as
 *   legitimate climate policy (alongside mitigation_priority and
 *   adaptation_priority). The degrowth reading operationalizes
 *   intergenerational justice and ecological limits as the primary legitimacy
 *   criteria, making current-generation wealthy-nation workers and capital
 *   owners the cost-bearing set. Future generations and currently vulnerable
 *   populations are the primary beneficiaries, though they cannot participate
 *   in the arrangement being made on their behalf.
 *
 * KEY AGENTS:
 *   - wealthy_nation_workers: face mandatory income and time reduction, dual cost-bearer and (reluctant) beneficiary through climate stability
 *   - capital_owners_wealthy_nations: face ownership restructuring and dividend reduction; have arbitrage exit options
 *   - future_generations_in_all_regions: benefit from climate stabilization without bearing implementation costs; powerless and trapped
 *   - global_vulnerable_populations: currently suffer disproportionate climate impacts; benefit from wealthy-nation consumption reduction
 *   - wealthy_nation_middle_class: lose consumption access and status positioning; gain time and economic security
 *   - climate_movement_organizations: agenda-setters who institutionalize this reading's legitimacy framing
 *   - mitigation_priority_advocates: structurally excluded by this reading's redefinition of legitimacy away from growth-compatible solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Climate Legitimacy Standard").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '861d2a63-335e-4ef0-9b48-4f59adf079ba').
narrative_ontology:cs_kernel_codification('861d2a63-335e-4ef0-9b48-4f59adf079ba', distributed).
narrative_ontology:cs_authority_grounding('861d2a63-335e-4ef0-9b48-4f59adf079ba', distributed).
narrative_ontology:cs_reading_relation('861d2a63-335e-4ef0-9b48-4f59adf079ba', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('861d2a63-335e-4ef0-9b48-4f59adf079ba', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('861d2a63-335e-4ef0-9b48-4f59adf079ba', foundational, growth_ecological_impossibility_under_planetary_boundaries).
narrative_ontology:cs_axiom_status(growth_ecological_impossibility_under_planetary_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('861d2a63-335e-4ef0-9b48-4f59adf079ba', growth_ecological_impossibility_under_planetary_boundaries, empirically_contingent).
narrative_ontology:cs_axiom('861d2a63-335e-4ef0-9b48-4f59adf079ba', foundational, intergenerational_justice_requires_emissions_reduction_not_adaptation).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_emissions_reduction_not_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('861d2a63-335e-4ef0-9b48-4f59adf079ba', intergenerational_justice_requires_emissions_reduction_not_adaptation, deontological).
narrative_ontology:cs_axiom('861d2a63-335e-4ef0-9b48-4f59adf079ba', secondary, democratic_control_of_production_is_prerequisite_for_legitimate_redistribution).
narrative_ontology:cs_axiom_status(democratic_control_of_production_is_prerequisite_for_legitimate_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('861d2a63-335e-4ef0-9b48-4f59adf079ba', democratic_control_of_production_is_prerequisite_for_legitimate_redistribution, deontological).
narrative_ontology:cs_reference_frame('861d2a63-335e-4ef0-9b48-4f59adf079ba', degrowth_transformation_as_necessary_climate_response).
narrative_ontology:cs_drift_state('861d2a63-335e-4ef0-9b48-4f59adf079ba', contemporary_2024_onwards, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('861d2a63-335e-4ef0-9b48-4f59adf079ba', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations_in_all_regions).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, wealthy_nation_middle_class).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owners_in_developed_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, wealthy_nation_middle_class).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_movement_organizations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, political_economy_critics).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owners_wealthy_nations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, ecological_limits_to_growth).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_justice_principle).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, decoupling_myth_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face mandatory income reduction through working time cuts (moving from full-time employment toward 20-30 hour work weeks), redistribution via universal basic services, and loss of career advancement pathways under growth economy logic. Simultaneously benefit from reduced working hours, expanded free social services, and climate stabilization that protects their communities. Their exit options are constrained by the national-scale character of the transformation — they cannot opt out while remaining in the wealthy-nation labor market. Professional identity tied to growth-economy employment creates additional identity lock.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers, beneficiary).

% Experience mandatory restructuring of firm ownership (transition from investor-owned to democratic/cooperative ownership models) and reduction in dividend streams due to degrowth constraints on accumulation. They have geographic exit options (capital flight, reinvestment in growth economies) and the resources to organize opposition. Their structural power remains substantial but is consciously constrained by the transformation's institutional rules.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, capital_owners_wealthy_nations, payer,
    powerful, biographical, arbitrage, global).

% Receive a climate system stabilized by wealthy-nation consumption reduction, avoiding scenarios of 3+ degree warming and cascading ecosystem collapse. They gain the benefit without bearing the implementation costs, which are distributed across current wealthy-nation workers and capital owners. They cannot participate in or resist the arrangement being made on their behalf — their interests are represented only through intergenerational justice framing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations_in_all_regions, beneficiary,
    powerless, civilizational, trapped, global).

% Currently bear disproportionate climate impacts from wealthy-nation emissions (sea-level rise, crop failure, resource scarcity, heat stress) despite minimal contribution to the problem. Degrowth in wealthy nations reduces cumulative warming that would otherwise impact them catastrophically by late 21st century. They benefit from a coordination solution they did not design and cannot directly enforce; their power derives from coalition with wealthy-nation climate movements.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_vulnerable_populations, beneficiary,
    organized, generational, constrained, global).

% Lose consumption access tied to growth (smaller homes, single vehicles, reduced discretionary spending), educational advancement signaling through credential inflation, and status positioning within growth hierarchies. Simultaneously gain time through working-time reduction, economic security through universal basic services, and climate stabilization. Exit is constrained by national adoption; they cannot opt into growth while the economy degrows around them. Identity fusion with consumption patterns and status-competition creates psychological suppression beyond the material constraint.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, wealthy_nation_middle_class, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, wealthy_nation_middle_class, beneficiary).

% Document the physical limits to decoupling emissions from growth and the inadequacy of technological solutions to meet Paris targets while maintaining wealthy-nation growth trajectories. They provide the empirical substrate for the constraint's legitimacy claim but do not enforce it or collect from it. Their institutional voice shapes public understanding but is structurally separate from the constraint's operation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_scientists_and_ecologists, observer,
    institutional, civilizational, analytical, global).

% Advocate the degrowth reading as the legitimate response framework and shape discourse that positions growth-economy alternatives as inadequate and delay-based. They operate as the primary institutional carriers of this reading's authority, translating scientific findings into political demands. They benefit from the constraint's adoption through organizational expansion and cultural authority, though they do not directly collect extraction rents.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_movement_organizations, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, climate_movement_organizations, beneficiary).

% Represent growth-preserving technological and carbon-pricing approaches as the legitimate response; they are structurally outside this constraint's framing. Adopting the degrowth reading forecloses their proposed solutions by redefining legitimacy away from technological decoupling toward structural transformation. Their exclusion is built into the kernel contest — they hold a competing reading of the same fundamental question (what counts as legitimate climate response), not an alternative constraint.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mitigation_priority_advocates, excluded,
    powerful, generational, mobile, global).

% Intellectuals, policy advocates, and institutional actors who argue that growth capitalism's ecological impossibility makes degrowth transformation mandatory on both climate and justice grounds. They collect authority and influence through the constraint's adoption; their exit is mobile (they can relocate arguments to other domains) but their power is located in the institutional spaces where legitimacy gets defined.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, political_economy_critics, beneficiary,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, future_generations_in_all_regions).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of anthropogenic climate change by aligning wealthy-nation consumption reduction with global emissions targets and intergenerational justice. Coordinates current redistribution (working time, income, ownership structure) with future climate stability through mandatory structural transformation rather than voluntary technological solutions and market mechanisms.
% TRANSFER_FUNCTION: Moves consumption access, working hours, and ownership authority from wealthy-nation capital owners and middle-class consumers (high-consumption, full-time employment, investor-owned firms) toward universal basic services, reduced working time, democratic ownership, and climate-stabilized outcomes for future generations and currently vulnerable populations.
% ABSENT_VOICES: Growth-economy defenders and technological-optimization advocates are systemically excluded from the legitimacy framing this constraint instantiates. They would argue that decoupling is possible, that consumer preference for growth should be honored, that disruption costs exceed climate benefits, and that adaptation rather than mitigation is more effective. Wealthy-nation workers most dependent on growth-economy employment (financial, real estate, advertising, fossil fuel sectors) face structural barriers to organizing resistance. Populations in developing economies whose growth trajectories would be constrained by wealthy-nation degrowth are partially excluded.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if degrowth transformation were abandoned and wealthy nations returned to growth-maximization — climate trajectories would shift toward 2.7–3.2°C warming (IPCC scenarios SSP3-7.0 forward), forcing massive adaptive spending and potentially triggering ecosystem tipping points. The absence of the constraint removes a primary institutional pathway for aligning climate physics with economic structure; alternative legitimacy frames (mitigation-priority, adaptation-priority) would dominate, structuring different cost distributions and different feasibility conditions.
% FOUNDING_PROBLEM: Anthropogenic climate change driven by growth-economy production and consumption in wealthy nations creates existential risk to ecosystems and human societies, particularly vulnerable populations and future generations. Technological decoupling and market-based solutions have failed to bend the emissions curve despite decades of deployment; wealthy-nation consumption remains the primary empirical driver of exceeding planetary boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists (IPCC synthesis reports, peer-reviewed literature on consumption-based emissions and decoupling failure), ecological economists (empirical studies on rebound effects and absolute decoupling impossibility at scale), and global vulnerable population representatives (testimony on current climate impacts and future risk) all corroborate the founding problem. Growth-economy advocates contest the decoupling-failure claim and the necessity framing, arguing that technological and efficiency improvements remain viable — corroboration is provided by sources outside the beneficiary set (scientific institutions, vulnerable-population representatives) but remains empirically contested.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.68 because the constraint redistributes substantial material access (consumption, working hours, ownership authority) from wealthy-nation actors to future generations and vulnerable populations, but the legitimacy framing positions this as justice and ecological necessity rather than pure transfer. Suppression is higher (0.72) because the constraint's persistence requires active enforcement of consumption limits, working-time ceilings, and firm-ownership transitions that market actors would otherwise revert — the constraint is not self-sustaining through preference alignment. Theater is moderate (0.41) because the underlying ecological and justice claims are real, but institutional operationalization requires performative adoption of degrowth metrics and lifestyle signals that gradually hollow out substantive redistributive content. The measurement trajectory shows extraction rising sharply in the first 20 years (policy adoption phase, peak resistance from capital owners) then stabilizing as the constraint becomes institutionalized and resistance costs become accepted as normal. Theater ratio rises in parallel as institutions ceremonialize compliance while diffusing actual implementation. Suppression plateaus after year 25 as the constraint becomes structural rather than requiring active enforcement — the economy has reorganized around the new rules. Coercion-grid shows differential pressure: individual-level resistance starts high (people resist consumption reduction) but falls as working-time and basic-services benefits materialize; organizational-level resistance (capital owners, growth-dependent firms) remains high throughout; class-level (workers as a collective) shows rising awareness of shared interest in climate-stabilized outcomes, moderating resistance; structural-level suppression rises as rule-making institutions embed the constraint in taxation, labor law, and corporate governance.
 *
 * PERSPECTIVAL GAP:
 *   Same-seat analysis: wealthy_nation_workers and wealthy_nation_middle_class hold identical power (organized) and time_horizon (biographical) but differ in exit_options and situation. Workers have constrained exit (labor-market dependence) while middle class has constrained+identity-locked exit (status signaling, consumption identity). Both are payers but differ in how identity fusion affects their perceived options. Capital_owners have arbitrage exit (geographic mobility) that workers lack — this structural difference should compute into different directionalities and different type perceptions. Mitigation_priority_advocates are excluded entirely from this constraint's legitimacy framework, creating inter-institutional contestation between readings of the same kernel rather than lateral agent dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are required; the derivation chain (beneficiary/victim + exit + power) produces the asymmetries correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anthropogenic climate change exceeding planetary boundaries) is LIVE and empirically corroborated (IPCC, ecological economics, vulnerable-population testimony). The disappearance verdict is WORLD_REARRANGES (climate trajectories shift toward 2.7–3.2°C if the constraint is abandoned). This alignment (live problem + world-rearranges) indicates the constraint solves a real coordination problem: without the degrowth transformation, wealthy-nation consumption remains the primary driver of emissions, making the constraint's framing of mandatory structural change as necessary (not optional) empirically justified. There is no mandatrophy signal — the constraint's legitimacy framing matches its functional necessity. The contestation lies not in mandate obsolescence but in whether the founding problem can be solved through alternative means (mitigation_priority, adaptation_priority readings) that avoid the extraction costs the degrowth reading imposes on current-generation wealthy-nation actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_possibility_empirical,
    'Can absolute decoupling of emissions from growth be achieved at scale in wealthy nations, making the degrowth constraint''s framing of mandatory structural transformation incorrect?',
    'Empirical observation of wealthy-nation emissions trajectories under continued growth conditions; meta-analysis of rebound-effect studies; technological development enabling renewable energy dominance and circular economy operations.',
    'If absolute decoupling is demonstrable, the degrowth reading becomes unnecessary and shifts classification toward mitigation_priority. If decoupling remains elusive despite technological advancement, the degrowth reading''s framing of mandatory transformation becomes the structurally necessary position. This is the primary empirical fault line between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_possibility_empirical, empirical, 'Whether technological decoupling makes degrowth transformation necessary or optional.').

omega_variable(
    intergenerational_substitutability,
    'Can technological and adaptive capacity investments adequately substitute for emissions reductions, making future-generation welfare achievable through adaptation rather than degrowth?',
    'Modeling of 2.5°C+ warming scenarios with adaptive capacity at different levels of investment; empirical observation of ecological tipping points and irreversibilities (ice-sheet collapse, Amazon dieback); intergenerational utility analysis.',
    'If substitution is feasible, adaptation_priority becomes structurally more legitimate than degrowth_transformation, and the extraction costs the degrowth reading imposes on current-generation workers become unnecessary. If tipping points are irreversible and adaptation is inadequate, degrowth becomes the only reading that delivers intergenerational justice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_substitutability, empirical, 'Whether future-generation welfare can be achieved through adaptive capacity without emissions reduction.').

omega_variable(
    political_implementation_feasibility,
    'Can the degrowth transformation achieve democratic legitimacy and stable implementation in wealthy nations, or does the suppression required to overcome capital-owner resistance make it structurally unstable?',
    'Natural experiments from jurisdictions attempting major redistributive transformation (working-time reduction, firm ownership restructuring, universal basic services); longitudinal study of capital outflows and political backlash under degrowth policy; labor-movement organizing and coalition-building outcomes.',
    'If implementation proves infeasible (high capital flight, political collapse, inability to maintain suppression without authoritarianism), the degrowth reading''s framing collapses into a utopian ideal with low legitimacy for actual policy. If even partial implementation succeeds, it validates the reading''s empirical claimability. This is the primary practical-viability fault line.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_implementation_feasibility, empirical, 'Whether degrowth transformation can achieve stable democratic implementation.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression measured in this constraint primarily structural (external barriers to consumption, capital controls, labor-market restructuring) or internalized (psychological identity fusion with growth-economy values, learned helplessness, false consciousness)?',
    'Longitudinal study of workers'' psychological adjustment under working-time reduction and consumption limits; measurement of preference-drift toward non-growth values; observation of whether suppression persists or decays after structural barriers are removed (post-transformation stability).',
    'If suppression is primarily structural, it decays as the constraint becomes normal and internalized as cultural practice — the constraint remains Tangled Rope but theater_ratio falls. If suppression is primarily internalized, it persists independently of structural barriers, potentially indicating deeper identity-capture mechanisms and higher true extraction costs than structural measurement suggests — the constraint might reclassify toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural or internalized, affecting long-term stability of the constraint.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the degrowth reading logically foreclose the mitigation_priority reading (technological decoupling), or do they remain coexisting positions that differ in empirical claims but not in core normative premises?',
    'Rigorous logical analysis of the core axioms in each reading; determination of whether acceptance of ''growth is possible under decoupling'' in mitigation_priority directly contradicts ''growth is ecologically impossible'' in degrowth_transformation, or whether the readings differ only on empirical contingencies.',
    'If foreclosure is genuine (the readings are logically incompatible within any single framework), the kernel contest is adversarial and zero-sum — adopting degrowth eliminates mitigation as a legitimate framework. If the readings coexist (they differ on empirical claims about decoupling, not on incompatible normative axioms), the contest is about which reading better matches reality, not which is more legitimate in principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether degrowth and mitigation readings are logically incompatible or empirically divergent coexisting positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_degrowth_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_degrowth_tr_t0, projected).
narrative_ontology:measurement(clim_degrowth_tr_t5, climate_response_legitimacy__degrowth_transformation, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(clim_degrowth_tr_t5, projected).
narrative_ontology:measurement(clim_degrowth_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(clim_degrowth_tr_t10, projected).
narrative_ontology:measurement(clim_degrowth_tr_t15, climate_response_legitimacy__degrowth_transformation, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(clim_degrowth_tr_t15, projected).
narrative_ontology:measurement(clim_degrowth_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_degrowth_tr_t20, projected).
narrative_ontology:measurement(clim_degrowth_tr_t25, climate_response_legitimacy__degrowth_transformation, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_degrowth_tr_t25, projected).
narrative_ontology:measurement(clim_degrowth_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_degrowth_tr_t30, projected).
narrative_ontology:measurement(clim_degrowth_tr_t35, climate_response_legitimacy__degrowth_transformation, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(clim_degrowth_tr_t35, projected).
narrative_ontology:measurement(clim_degrowth_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_degrowth_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_degrowth_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(clim_degrowth_be_t0, projected).
narrative_ontology:measurement(clim_degrowth_be_t5, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(clim_degrowth_be_t5, projected).
narrative_ontology:measurement(clim_degrowth_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_degrowth_be_t10, projected).
narrative_ontology:measurement(clim_degrowth_be_t15, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_degrowth_be_t15, projected).
narrative_ontology:measurement(clim_degrowth_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_degrowth_be_t20, projected).
narrative_ontology:measurement(clim_degrowth_be_t25, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_degrowth_be_t25, projected).
narrative_ontology:measurement(clim_degrowth_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_degrowth_be_t30, projected).
narrative_ontology:measurement(clim_degrowth_be_t35, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_degrowth_be_t35, projected).
narrative_ontology:measurement(clim_degrowth_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_degrowth_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_degrowth_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_degrowth_su_t0, projected).
narrative_ontology:measurement(clim_degrowth_su_t5, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(clim_degrowth_su_t5, projected).
narrative_ontology:measurement(clim_degrowth_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(clim_degrowth_su_t10, projected).
narrative_ontology:measurement(clim_degrowth_su_t15, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(clim_degrowth_su_t15, projected).
narrative_ontology:measurement(clim_degrowth_su_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(clim_degrowth_su_t20, projected).
narrative_ontology:measurement(clim_degrowth_su_t25, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_degrowth_su_t25, projected).
narrative_ontology:measurement(clim_degrowth_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_degrowth_su_t30, projected).
narrative_ontology:measurement(clim_degrowth_su_t35, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(clim_degrowth_su_t35, projected).
narrative_ontology:measurement(clim_degrowth_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_degrowth_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_degrowth_grid_01, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(clim_degrowth_grid_02, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(class), 40, 0.78).
narrative_ontology:measurement(clim_degrowth_grid_03, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(clim_degrowth_grid_04, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(individual), 40, 0.58).
narrative_ontology:measurement(clim_degrowth_grid_05, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(clim_degrowth_grid_06, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(clim_degrowth_grid_07, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(clim_degrowth_grid_08, climate_response_legitimacy__degrowth_transformation, accessibility_collapse(structural), 40, 0.82).
narrative_ontology:measurement(clim_degrowth_grid_09, climate_response_legitimacy__degrowth_transformation, resistance(class), 0, 0.82).
narrative_ontology:measurement(clim_degrowth_grid_10, climate_response_legitimacy__degrowth_transformation, resistance(class), 40, 0.72).
narrative_ontology:measurement(clim_degrowth_grid_11, climate_response_legitimacy__degrowth_transformation, resistance(individual), 0, 0.62).
narrative_ontology:measurement(clim_degrowth_grid_12, climate_response_legitimacy__degrowth_transformation, resistance(individual), 40, 0.55).
narrative_ontology:measurement(clim_degrowth_grid_13, climate_response_legitimacy__degrowth_transformation, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(clim_degrowth_grid_14, climate_response_legitimacy__degrowth_transformation, resistance(organizational), 40, 0.68).
narrative_ontology:measurement(clim_degrowth_grid_15, climate_response_legitimacy__degrowth_transformation, resistance(structural), 0, 0.75).
narrative_ontology:measurement(clim_degrowth_grid_16, climate_response_legitimacy__degrowth_transformation, resistance(structural), 40, 0.62).
narrative_ontology:measurement(clim_degrowth_grid_17, climate_response_legitimacy__degrowth_transformation, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(clim_degrowth_grid_18, climate_response_legitimacy__degrowth_transformation, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(clim_degrowth_grid_19, climate_response_legitimacy__degrowth_transformation, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(clim_degrowth_grid_20, climate_response_legitimacy__degrowth_transformation, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(clim_degrowth_grid_21, climate_response_legitimacy__degrowth_transformation, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(clim_degrowth_grid_22, climate_response_legitimacy__degrowth_transformation, stakes_inflation(organizational), 40, 0.75).
narrative_ontology:measurement(clim_degrowth_grid_23, climate_response_legitimacy__degrowth_transformation, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(clim_degrowth_grid_24, climate_response_legitimacy__degrowth_transformation, stakes_inflation(structural), 40, 0.78).
narrative_ontology:measurement(clim_degrowth_grid_25, climate_response_legitimacy__degrowth_transformation, suppression(class), 0, 0.62).
narrative_ontology:measurement(clim_degrowth_grid_26, climate_response_legitimacy__degrowth_transformation, suppression(class), 40, 0.76).
narrative_ontology:measurement(clim_degrowth_grid_27, climate_response_legitimacy__degrowth_transformation, suppression(individual), 0, 0.48).
narrative_ontology:measurement(clim_degrowth_grid_28, climate_response_legitimacy__degrowth_transformation, suppression(individual), 40, 0.64).
narrative_ontology:measurement(clim_degrowth_grid_29, climate_response_legitimacy__degrowth_transformation, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(clim_degrowth_grid_30, climate_response_legitimacy__degrowth_transformation, suppression(organizational), 40, 0.74).
narrative_ontology:measurement(clim_degrowth_grid_31, climate_response_legitimacy__degrowth_transformation, suppression(structural), 0, 0.68).
narrative_ontology:measurement(clim_degrowth_grid_32, climate_response_legitimacy__degrowth_transformation, suppression(structural), 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, intergenerational_justice__future_welfare_discount_rate).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, wealthy_nation_consumption_production_model).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, working_time_reduction__employment_stability).

% DUAL FORMULATION NOTE:
% This constraint is one reading (degrowth_transformation) of the kernel climate_response_legitimacy. Sibling readings climate_response_legitimacy__mitigation_priority and climate_response_legitimacy__adaptation_priority are structurally distinct constraints with different beneficiary/victim allocations. All three readings constitute a constraint family linked by shared kernel origin and mutually affecting network relationships. The degrowth reading uniquely enters wealthy-nation workers into the cost-bearer set and maximizes intergenerational/global-equity redistribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
