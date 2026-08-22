% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Structural Transformation Imperative for Climate Response
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The degrowth reading of the climate response imperative asserts that
 *   stabilizing the climate system requires Global North economies to undergo
 *   structural transformation: absolute reductions in material throughput and
 *   energy use, redistributive policies to maintain wellbeing at lower
 *   consumption, and institutional redesign around post-growth objectives
 *   (universal basic services, working-time reduction, job guarantees,
 *   decommodification). This reading claims to solve both mitigation (by
 *   shrinking the emissions source) and adaptation (by freeing ecological
 *   space and finance for the Global South) without relying on unproven,
 *   large-scale carbon dioxide removal technologies. The constraint is
 *   claimed as tangled_rope: it coordinates a genuine collective-action
 *   problem (planetary boundary compliance + intergenerational justice) while
 *   extracting from present Global North populations through reduced
 *   consumption and working-time restructuring. The claimed_type and metrics
 *   are authored independently — the engine computes per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (civilizational/analytical) — receives stabilized climate and ecological space
 *   - global_south_populations: Primary beneficiary (generational/constrained) — receives atmospheric space, climate finance, technology transfer, reduced climate damages
 *   - global_north_consumers: Primary victim (organized/constrained) — bears consumption reduction, lifestyle transformation, status loss
 *   - global_north_workers: Primary victim (organized/constrained) — bears working-time reduction, sectoral transition, income adjustment
 *   - fossil_fuel_intensive_workers: Concentrated victim (powerless/trapped) — bears full sectoral elimination, community dissolution
 *   - growth_dependent_financial_assets: Institutional victim (institutional/constrained) — bears devaluation, stranded assets, regime change
 *   - post_growth_institutions_advocates: Agenda setter/beneficiary (organized/mobile) — designs and administers the transition, gains institutional legitimacy
 *   - climate_science_consensus: Observer (analytical/analytical) — provides the carbon budget and overshoot evidence that grounds the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.62).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.71).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.43).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Structural Transformation Imperative for Climate Response").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '88999e56-836e-40d0-a2f6-63c91700de35').
narrative_ontology:cs_kernel_codification('88999e56-836e-40d0-a2f6-63c91700de35', distributed).
narrative_ontology:cs_authority_grounding('88999e56-836e-40d0-a2f6-63c91700de35', distributed).
narrative_ontology:cs_reading_relation('88999e56-836e-40d0-a2f6-63c91700de35', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('88999e56-836e-40d0-a2f6-63c91700de35', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('88999e56-836e-40d0-a2f6-63c91700de35', foundational, carbon_budget_requires_absolute_north_contraction).
narrative_ontology:cs_axiom_status(carbon_budget_requires_absolute_north_contraction, holdable).
narrative_ontology:cs_axiom_grounding('88999e56-836e-40d0-a2f6-63c91700de35', carbon_budget_requires_absolute_north_contraction, empirically_contingent).
narrative_ontology:cs_axiom('88999e56-836e-40d0-a2f6-63c91700de35', foundational, cdr_is_moral_hazard_not_solution).
narrative_ontology:cs_axiom_status(cdr_is_moral_hazard_not_solution, holdable).
narrative_ontology:cs_axiom_grounding('88999e56-836e-40d0-a2f6-63c91700de35', cdr_is_moral_hazard_not_solution, deontological).
narrative_ontology:cs_axiom('88999e56-836e-40d0-a2f6-63c91700de35', secondary, wellbeing_decoupled_from_throughput).
narrative_ontology:cs_axiom_status(wellbeing_decoupled_from_throughput, holdable).
narrative_ontology:cs_axiom_grounding('88999e56-836e-40d0-a2f6-63c91700de35', wellbeing_decoupled_from_throughput, empirically_contingent).
narrative_ontology:cs_reference_frame('88999e56-836e-40d0-a2f6-63c91700de35', carbon_budget_justice_framework).
narrative_ontology:cs_drift_state('88999e56-836e-40d0-a2f6-63c91700de35', post_paris_agreement_implementation_gap, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('88999e56-836e-40d0-a2f6-63c91700de35', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, post_growth_institutions_advocates).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_fuel_intensive_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, growth_dependent_financial_assets).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, carbon_budget_finitude).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, ecological_overshoot_reality).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, intergenerational_equity_principle).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, common_but_differentiated_responsibilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a stabilized climate system and intact ecological life-support systems. They cannot negotiate, exit, or resist — they inherit whatever atmospheric concentration results from present decisions. Their benefit is the avoidance of catastrophic climate damages that would foreclose flourishing.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% Receive atmospheric space for development, climate finance and technology transfer enabled by Global North contraction, and reduced climate damages from stabilized warming. Their exit is constrained by global economic structures and historical emissions debt. They gain from both mitigation (avoided damages) and adaptation (resources freed by Northern contraction).
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Bear absolute reductions in material consumption (meat, aviation, private vehicles, living space, disposable goods). Experience status loss, lifestyle disruption, and cultural dislocation. Exit is constrained by infrastructure lock-in, social norms, and the collective-action nature of the transition — individual opt-out is ineffective. Political voice is organized but contested by growth lobbies.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumers, payer,
    organized, biographical, constrained, national).

% Bear working-time reduction (with wage compensation), sectoral transition out of high-carbon industries, retraining costs, and potential income reduction. Exit is constrained by skill specificity, geographic immobility, and the macroeconomic coordination required. Organized labor is split: some unions support just transition policies, others defend growth-dependent employment.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_workers, payer,
    organized, biographical, constrained, national).

% Bear the concentrated, immediate elimination of their livelihoods, communities, and identity. Coal miners, oil rig workers, refinery communities in Global North regions face total sectoral phase-out within 10-15 years. Exit is trapped by geographic concentration, skill non-transferability, age, and community ties. They are the constraint's most exposed victims.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_intensive_workers, payer,
    powerless, immediate, trapped, local).

% Bear devaluation of assets whose returns depend on perpetual GDP growth: pension funds, sovereign debt, equity markets, real estate in growth corridors. The constraint requires financial regime change (debt jubilees, monetary reform, capital controls) that extracts from these asset holders. Exit is constrained by systemic interconnectedness — they cannot individually opt out of the financial system.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_financial_assets, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__degrowth_reading, growth_dependent_financial_assets).

% Design, advocate, and administer the transition institutions: wellbeing economy frameworks, universal basic services, job guarantees, working-time reduction policies, participatory planning bodies. They gain institutional authority, research funding, and policy influence as the climate crisis deepens. Their exit is mobile — they can shift between academic, NGO, governmental, and international roles.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_institutions_advocates, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, post_growth_institutions_advocates, beneficiary).

% Provides the carbon budget quantification, overshoot risk assessment, and CDR feasibility evaluation that ground the constraint's epistemic legitimacy. They neither collect nor pay; they observe the biophysical boundary conditions. Their authority is epistemic, not institutional.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_science_consensus, observer,
    analytical, civilizational, analytical, universal).

% Would need to enact and enforce the transformation: carbon rationing, border adjustments, capital controls, industrial policy, working-time legislation. They are constrained by electoral cycles, corporate capture, geopolitical competition, and the growth imperative built into state legitimacy. They sit between the agenda-setting function and the payer seat — they administer extraction from their own populations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Are structurally excluded from the transition governance because their asset stranding is the policy objective. They would object to the constraint's existence and deploy lobbying, litigation, media influence, and political capture to block or dilute it. Their exclusion is what the enforcement machinery must maintain against their resistance.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_capital, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dual collective-action problem of (a) staying within a finite global carbon budget to avoid catastrophic warming, and (b) enabling Global South development within that budget, without relying on unproven CDR technologies that may fail or create moral hazard. The degrowth transformation coordinates Global North contraction to make atmospheric space for the South while building resilient, low-throughput provisioning systems for both.
% TRANSFER_FUNCTION: Moves material throughput, energy use, and ecological footprint from Global North populations (present) to future generations and Global South populations (future/present) via reduced consumption, working-time reduction, and redistributive institutions. Simultaneously moves financial claims from growth-dependent assets to public provisioning systems (universal basic services, job guarantees). The transfer is from high-consuming, high-emitting populations to the atmospheric commons and to those historically excluded from it.
% ABSENT_VOICES: Global South diaspora communities in the Global North (who may bear disproportionate transition costs), Indigenous peoples in the Global North (whose land rights may conflict with renewable infrastructure), children and youth (who inherit both the transition costs and the climate benefits but have no formal voice), non-human species (who bear climate damages without representation). These voices are absent from the policy design process but would object to specific distributional outcomes.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished overnight, Global North emissions would continue on green growth/CDR trajectories, the carbon budget would be exceeded with high probability, CDR deployment would become a forced necessity at massive scale, Global South would face uncompensated climate damages, and the window for managed transformation would close — the world would rearrange into a higher-temperature, higher-overshoot, more unequal trajectory with CDR lock-in.
% FOUNDING_PROBLEM: The founding problem is the biophysical impossibility of universalizing Global North material living standards within planetary boundaries, combined with the historical injustice of Global North emissions crowding out Global South development space. The arrangement was built (in discourse) to solve: how to achieve rapid mitigation AND just adaptation AND Global South development within a shrinking carbon budget, without gambling on unproven CDR.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) Chapter 5 acknowledges degrowth/post-growth literature as a mitigation pathway; UNEP Emissions Gap Reports document the widening gap between pledges and budgets; Global South negotiating blocs (AOSIS, LDCs, African Group) consistently demand developed country leadership on consumption emissions; climate justice movements (Climate Justice Alliance, La Via Campesina, Fridays for Future) articulate the dual mitigation/adaptation/justice problem. No major institutional actor outside the degrowth advocacy network has implemented the full transformation — corroboration is discursive, not institutional.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the magnitude of required material throughput reduction in the Global North (~60-80% by 2050 in degrowth literature) and the concentrated costs on specific populations. Suppression (0.71) reflects the enforcement infrastructure needed: carbon rationing, border carbon adjustments, capital controls, advertising bans, working-time mandates, and the political suppression of growth-dependence. Theater ratio (0.28) is moderate — the coordination function (solving the dual mitigation/adaptation problem without CDR) is genuine, but a growing share of policy energy goes to managing political resistance rather than the biophysical problem. Accessibility collapse (0.43) is partial: alternatives (green growth, CDR, solar geoengineering) remain discursively available but are rejected by this reading's epistemic criteria. Resistance (0.58) is substantial: incumbent industries, growth-dependent financial systems, consumer culture, and geopolitical competition all resist. The measurement series track the rising enforcement intensity and extraction as carbon budgets tighten and the window for voluntary transition closes.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute dramatically different effective extraction across seats: future_generations and global_south_populations (d ≈ 0.0-0.15) experience subsidy/coordination; global_north_consumers/workers (d ≈ 0.7-0.85) experience high extraction; fossil_fuel_intensive_workers (d ≈ 0.95, trapped) experience near-total extraction. Post_growth_institutions_advocates (d ≈ 0.15, agenda_setter) experience the constraint as coordination they administer. The perspectival gap is the core of the tangled_rope classification: genuine coordination for some, genuine extraction for others, held together by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: future_generations (no exit, civilizational horizon, pure beneficiary of stabilized climate), global_south_populations (constrained exit, generational horizon, receive atmospheric space and finance), post_growth_institutions_advocates (mobile exit, organized power, gain institutional authority). Victims: global_north_consumers (constrained exit, organized power, bear consumption reduction), global_north_workers (constrained exit, organized power, bear working-time and sectoral transition), fossil_fuel_intensive_workers (trapped exit, powerless, bear elimination), growth_dependent_financial_assets (constrained exit, institutional power, bear devaluation). The directionality derivation from beneficiary/victim declarations plus exit options produces the steep d-gradient that drives the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing climate within carbon budgets while enabling Global South development) remains live and worsening — the carbon budget has shrunk, not expanded. However, the degrowth arrangement itself has not been implemented at scale; it persists as a marginal but growing policy discourse. The mandatrophy risk is not that the founding problem is dead, but that the arrangement may be captured by growth-dependent institutions (green growth co-optation) or abandoned for CDR reliance (mitigation_priority_reading). The theater ratio rise (0.12→0.28) suggests increasing performative adoption of degrowth language without structural implementation — a potential mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''climate_response_imperative'', and how does it structurally differ from the mitigation_priority_reading and adaptation_priority_reading siblings?',
    'Compare the victim/beneficiary sets, CDR reliance, and institutional prescriptions across the three constraint stories generated from this kernel. The degrowth_reading eliminates CDR reliance and places present Global North populations in the victim set; the other readings do not.',
    'If the three readings produce materially different ε values and beneficiary/victim structures, the kernel is genuinely contested and each reading instantiates a distinct constraint. If they converge, the kernel label may be obscuring a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee structure: this reading''s structural identity as one of three sibling readings from the same kernel.').

omega_variable(
    cdr_elimination_feasibility,
    'Is the elimination of reliance on unproven carbon dioxide removal (CDR) technologies structurally feasible within the degrowth transformation, or does the transformation itself implicitly require CDR-scale negative emissions?',
    'Integrated assessment model comparison: run degrowth scenarios with and without CDR assumptions; assess whether post-growth pathways meet temperature targets without CDR.',
    'If CDR-free pathways are infeasible, the reading''s victim set (Global North populations bearing transformation costs) expands without the promised mitigation payoff, shifting toward snare. If feasible, the coordination function (mitigation+adaptation without CDR) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_elimination_feasibility, empirical, 'Whether the reading''s core coordination claim (mitigation+adaptation without CDR) is physically realizable.').

omega_variable(
    global_north_victim_set_cohesion,
    'Do present-day Global North populations constitute a coherent victim set, or does the extraction fall disproportionately on specific subclasses (workers in fossil sectors, renters, Global South diaspora in the North) while professional-managerial classes capture transition rents?',
    'Distributional incidence analysis of degrowth policy packages: carbon taxes with dividend, working-time reduction, universal basic services, job guarantees. Measure net welfare change by income decile, sector, and demographic.',
    'If the victim set fractures along class lines with a captured beneficiary subclass inside the Global North, the constraint operates as tangled_rope with intra-North extraction. If the victim set is broadly coherent, the extraction is more cleanly intergenerational/geographic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_north_victim_set_cohesion, empirical, 'Whether the declared victim set ''global_north_consumers/workers'' masks intra-North class extraction.').

omega_variable(
    enforcement_mechanism_nature,
    'What enforcement mechanism holds the degrowth transformation? Democratic deliberation, state planning, international treaty, or social movement pressure — and does the mechanism itself extract from the declared victims?',
    'Institutional ethnography of existing degrowth-aligned policies (e.g., wellbeing economy budgets, working-time reductions, ecological tax reforms). Trace who designs, who implements, who resists, who pays.',
    'If enforcement is democratic and participatory with low suppression, the constraint leans toward rope. If enforcement requires authoritarian state capacity or international coercion with high suppression, it leans toward snare. The current suppression score (0.71) assumes substantial enforcement infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, conceptual, 'The nature of the active enforcement required by this tangled_rope claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__degrowth_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__degrowth_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__degrowth_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, global_carbon_budget_enforcement).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, international_climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, global_supply_chain_governance).

% DUAL FORMULATION NOTE:
% This constraint is one member of the climate_response_imperative constraint family. The three sibling readings (degrowth, mitigation_priority, adaptation_priority) decompose the single natural-language concept 'adequate climate response' into structurally distinct constraints with different ε values, beneficiary/victim sets, and CDR dependencies. The degrowth_reading has the highest base extractiveness (0.62) because it distributes costs to present Global North populations rather than deferring via CDR or adaptation. The mitigation_priority_reading likely has lower ε for present populations but higher systemic risk from CDR failure. The adaptation_priority_reading likely has lowest ε for Global North but highest for Global South. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.75).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
