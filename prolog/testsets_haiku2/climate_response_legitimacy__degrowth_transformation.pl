% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth transformation reading claims that legitimate climate
 *   response requires wealthy nations to structurally dismantle
 *   growth-dependent economic systems through universal basic services,
 *   working-time reduction, and democratic firm ownership. This reading
 *   treats current-generation workers and capital-owners in wealthy nations
 *   as cost-bearers (reduced incomes/accumulation capacity) and future
 *   generations plus vulnerable populations as beneficiaries (stabilized
 *   climate without technological lock-in). The constraint is claimed as
 *   Tangled Rope (real coordination function for emission reduction +
 *   asymmetric extraction via cost incidence); the authored metrics describe
 *   substantial extractiveness and suppression rising over implementation
 *   phases, capturing the political-resistance reality. This is one reading
 *   of the climate-response-legitimacy kernel; sibling readings
 *   (mitigation_priority treating technological decoupling as sufficient,
 *   adaptation_priority treating resilience as primary) are separate
 *   constraints with different beneficiary structures and ε values.
 *
 * KEY AGENTS:
 *   - wealthy_nation_workers_current_generation (payer, organized, biographical time horizon, constrained exit) — bear structural costs via income reduction and private-consumption loss
 *   - future_generations_reduced_warming (beneficiary, powerless, civilizational horizon, trapped) — receive stabilized climate but do not participate in design
 *   - globally_vulnerable_populations (beneficiary, powerless, biographical horizon, trapped) — receive aggressive mitigation avoiding uninhabitability
 *   - owner_class_of_extractive_firms (payer, institutional, biographical, constrained) — face dissolved dividend streams and wealth redistribution
 *   - democratic_planning_institutions (agenda_setter, institutional, generational, mobile) — execute transformation; political empowerment is the operative barrier
 *   - climate_scientists (observer, analytical) — establish founding problem empirically but do not advocate degrowth specifically
 *   - mitigation_technology_proponents (excluded) — would contest the reading's premise of technological insufficiency
 *   - adaptation_specialists (excluded) — would contest prioritization of mitigation over adaptation
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
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '4f82ccaf-8b6c-4f7b-bb76-c805b61a2600').
narrative_ontology:cs_kernel_codification('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', formalized).
narrative_ontology:cs_authority_grounding('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', extraction).
narrative_ontology:cs_interpretation_layer_present('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600').
narrative_ontology:cs_reading_relation('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', foundational, technological_decoupling_insufficient).
narrative_ontology:cs_axiom_status(technological_decoupling_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', technological_decoupling_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', foundational, growth_dismantling_in_wealthy_nations_mandatory).
narrative_ontology:cs_axiom_status(growth_dismantling_in_wealthy_nations_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', growth_dismantling_in_wealthy_nations_mandatory, instrumental).
narrative_ontology:cs_reference_frame('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', growth_imperative_legitimacy).
narrative_ontology:cs_drift_state('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', contemporary_climate_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4f82ccaf-8b6c-4f7b-bb76-c805b61a2600', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations_reduced_warming).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, globally_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers_current_generation).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, owner_class_of_extractive_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the structural costs of transformation: reduced working hours mean compressed incomes unless subsidized; shift to universal basic services means tax restructuring that may reduce disposable income; loss of private-consumption status markers as cultural weight shifts to reduced material throughput. Exit involves relocating to non-transforming economies or political opposition that blocks implementation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, wealthy_nation_workers_current_generation, payer,
    organized, biographical, constrained, national).

% Inherit a climate system stabilized by current-generation emission cuts rather than dependent on speculative technological fixes or perpetual adaptation infrastructure maintenance. They do not participate in the constraint's operation; they receive the benefit of reduced warming trajectory without bargaining power over the transformation's design.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations_reduced_warming, beneficiary,
    powerless, civilizational, trapped, global).

% Benefit from aggressive emissions reduction that prevents warming scenarios where their regions face uninhabitability or mass displacement. Currently bear disproportionate warming impacts despite minimal historical emissions; degrowth in wealthy nations shifts the burden-sharing toward responsibility-proportional mitigation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, globally_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Face constraints on capital accumulation models: democratic firm ownership dissolves extractive dividend streams; reduced working time compression removes a primary mechanism for labor cost control; universal basic services funded by wealth redistribution directly diminishes ownership class purchasing power over political authority. Their agenda-setting capacity exists through current political influence, which the transformation explicitly targets for redistribution.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, owner_class_of_extractive_firms, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, owner_class_of_extractive_firms, agenda_setter).

% Document that warming trajectories under current growth models exceed safety thresholds; provide empirical grounding for the constraint's founding problem. They do not advocate for degrowth specifically but their data establishes the warming ceiling that makes the constraint's founding problem live.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_scientists_and_ippcc_consensus, observer,
    analytical, civilizational, analytical, global).

% Would advocate that technological decoupling permits growth continuation; excluded from the degrowth reading's core argument because the reading's founding premise is that technological decoupling has proven insufficient under tested timelines and requires suppressed demand as a backstop.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mitigation_technology_proponents, excluded,
    institutional, biographical, constrained, global).

% Would argue adaptation capacity building should be the primary response vector; excluded because the degrowth reading treats adaptation as economically unsustainable at high warming levels and politically inadequate without mitigation as the primary strategy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, adaptation_specialists, excluded,
    institutional, biographical, constrained, global).

% Would administer the transformation: redesigning tax systems, managing firm ownership transitions, administering working-time and basic-services structures. Their capacity to implement the constraint is the operational bottleneck; political feasibility of their empowerment is the implementation barrier.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, democratic_planning_institutions, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational allocation problem: current wealthy-nation consumption above sustainable per-capita ceilings must be reduced collectively to avoid warming that locks future generations and vulnerable present populations into uninhabitable or perpetually defensive states. The constraint coordinates a shift from individual consumption choice (limited by income) to collective consumption structuring (limited by emission budget) and distributes the reduction burden through reduced working time, publicly provisioned basics, and ownership democratization rather than through market-price compression alone.
% TRANSFER_FUNCTION: Moves material provisioning from private consumption (funded by individual income) to universal basic services (funded by collective tax/wealth); moves labor time from income-generating work to reduced working hours (compressed income offset by service provision); moves firm ownership from concentrated shareholder class to democratic governance structures (shifts surplus allocation from owners to workers/communities). The transfer is bidirectional: current-generation wealthy nations transfer reduced emissions to future generations and vulnerable populations; current-generation organized labor transfers some consumption purchasing power to ensure survival of the habitable climate.
% ABSENT_VOICES: Extractive-firm shareholders who would argue for technological-decoupling-based growth preservation are structurally excluded by the reading's core premise; their objection is pre-adjudicated as empirically inadequate (decoupling rates insufficient, timelines compressed). Non-wealthy nations' development aspirations are present but subordinated: the reading prioritizes wealthy-nation transformation; it does not address whether non-wealthy nations must also transform (a divergence between this reading and others). Workers in fossil-fuel and high-carbon sectors whose livelihoods depend on extraction are present as cost-bearers but not as voice-holders in the design (a political-feasibility gap the constraint acknowledges).
% DISAPPEARANCE_RATIONALE: If the degrowth transformation constraint and its structural enforcement vanished, wealthy nations would return to growth-maximization models; global emissions would follow current trajectories toward 2.7–3.2°C warming; future generations and vulnerable populations would face dramatically higher climate impacts. The constraint's disappearance would be catastrophic in the reading's own terms because it treats the transformation as the only feasible path to emissions reductions deep enough to avoid catastrophic warming.
% FOUNDING_PROBLEM: Current-generation wealthy-nation consumption and production models generate emissions exceeding the atmosphere's capacity to absorb without triggering irreversible warming; technological decoupling of growth from emissions has proven insufficient in tested timelines (2000–2024 data show no consistent absolute decoupling in wealthy nations); therefore, reducing material throughput in wealthy nations is structurally necessary to reach climate-stabilization emission budgets before tipping points lock in uninhabitable warming.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 and peer-reviewed climate science establish the emission-gap problem as live (current policies inadequate to stabilization targets). Ecological-economics research documents insufficient technological decoupling rates. The degrowth reading's distinctive claim — that structural transformation is the ONLY viable mitigation path — is contested: mitigation-priority and adaptation-priority readings deny this necessity. Corroboration from outside the degrowth beneficiary set: climate scientists (consensus on warming problem and decoupling insufficiency, but not on degrowth necessity); labor economists (document that working-time reduction is technically feasible with public-service expansion, but contest political feasibility); adaptation specialists (contest that mitigation alone suffices without adaptive capacity building). No neutral external seat affirms the reading's full causal chain (current-generation workers MUST bear these specific costs, or warming is inevitable).
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.68 at interval end, rising from 0.48 at start, because the constraint requires sustained income/wealth transfers and loss of consumption status markers for current-generation cost-bearers, with no compensating gain to them in their own time horizon. The measurement trajectory (0.48 → 0.54 → 0.61 → 0.66 → 0.68 → 0.68) shows extraction rising steeply during implementation phases (0–32) then plateauing as steady-state structures stabilize. Suppression requirement (0.72 at end) is high because the transformation must overcome organized resistance from wealth-holders and working-class constituencies skeptical of promised service-adequacy; without active enforcement the constraint collapses into mitigation_priority or status-quo alternatives. Theater ratio (0.41) captures rhetorical performance around 'just transition' and 'workers' participation' that obscures the reality of enforced income compression and ownership restructuring. Accessibility collapse (0.58) is moderate because alternative economic models exist (tech-driven decoupling, adaptation-first) and exit remains possible (capital flight, political opposition) even under the constraint; the constraint does not collapse alternatives completely. Resistance (0.71) is high because current-generation organized labor, capital owners, and middle-class beneficiaries of growth all have incentive to oppose; the constraint must actively suppress these organized pressure points.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (democratic planning institutions), the constraint is a necessary coordination mechanism for survival — it solves a genuine collective-action problem (individual incentives cannot generate adequate emission reductions) and distributes the burden toward proportionality (those who benefited from high-carbon growth bear the restructuring costs). From the working-class payer seat, the constraint is coercive burden-shifting masked as 'just transition' — the promised service adequacy may not materialize, exit into non-transforming economies is foreclosed by political coordination, and the lived experience is income compression and status-marker loss. From the owner-class seat, the constraint is asset confiscation justified by climate emergency. The engine computes these divergent seats from the structural data (power atom differences, exit-option constraints, beneficiary/victim declarations); the authored claim (Tangled Rope) does not pre-adjudicate the divergence — it names the structural type that contains the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-generation wealthy-nation workers occupy a structurally ambiguous seat: they benefit from future emission reductions (d shifted toward beneficiary for intergenerational spillover) but bear immediate material costs (d shifted toward target for income loss and consumption restructuring). The net directionality is d ≈ 0.65 (moderately target-side) — they are primarily cost-bearers relative to the constraint's operation, though they also capture some organizational benefits (reduced working hours, public services) that soften the extraction. Owner class sits at d ≈ 0.82 (strongly target-side) — they lose capital accumulation capacity and face ownership restructuring with no compensating gain under this reading. Future generations and vulnerable populations sit at d ≈ 0.15 (strongly beneficiary-side) — they receive the constraint's primary benefit (reduced warming) but have no exit options and no voice in implementation design. The asymmetry (organized workers at d~0.65, owners at d~0.82 vs. future populations at d~0.15) is the structural marker of Tangled Rope: genuine coordination (emission reduction) coupled with asymmetric extraction (current-generation wealth-holders bear most of the implementation cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live in this reading (warming trajectories exceed stabilization thresholds under current growth models) but contested as to solution (mitigation_priority reading denies that degrowth is necessary; adaptation_priority reading accepts warming and prioritizes resilience instead). The disappearance verdict (world_rearranges) confirms the constraint has stakeholders and operates on actual arrangements. The potential mandatrophy signal: if empirical evaluation shows that decoupling rates are sufficient or political implementation proves feasible at lower cost (via different mechanisms), the founding problem's necessity claim erodes, and the constraint reverts to Scaffold (transitional aspiration) or Rope (optional coordination). The theater ratio (0.41, rising) is the operational warning: if the ratio rises above 0.60 without corresponding emission reductions, the constraint has become performative and the founding problem resolution has stalled — a classic mandatrophy signature. The classification (Tangled Rope) correctly names the structure: real coordination function (emission reduction) + asymmetric extraction (current workers and owners pay, future generations benefit) + active enforcement requirement (suppression = 0.72). If empirical drift shows the coordination function is being displaced by pure redistribution theater, the classification would shift toward Snare — the engine would detect this from the measurement trajectory and the omega ambiguities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_decoupling_ceiling,
    'Is technological decoupling fundamentally limited by physical constraints and tested adoption rates, or do current decoupling estimates understate feasible acceleration under strong policy signals?',
    'Long-term empirical tracking of decoupling rates in jurisdictions with aggressive carbon pricing and green-industrial policy (EU, UK, Scandinavia 2020–2035); comparison of realized vs. modeled decoupling trajectories; physical-limit assessments from thermodynamic and material-flow analyses.',
    'If decoupling proves faster than current baseline assumptions, the necessity of structural degrowth is reduced; the constraint becomes optional coordination rather than mandatory transformation. If decoupling stalls, the constraint''s core premise hardens and the transformation becomes unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_ceiling, empirical, 'Whether technological emissions reduction can occur fast enough without demand suppression.').

omega_variable(
    political_feasibility_of_implementation,
    'Can wealthy-nation democratic institutions execute a structural transformation of this scope — tax restructuring, firm ownership transitions, working-time reductions, service expansion — without authoritarian enforcement or collapse into chaos/capital flight?',
    'Natural experiments from jurisdictions attempting partial transformations (France working-time reduction, Scandinavia tax-funded services, cooperative firm expansions); political-economy analysis of barrier coalitions and their countermeasures; historical case studies of comparable economic transitions (post-war European reconstruction, Nordic labor-capital settlements).',
    'High feasibility strengthens the constraint as a live option and hardens its mandate via incentive structures; low feasibility converts the constraint into a Scaffold (transitional aspiration) or Snare (elite-imposed mandate that collapses under resistance). Current evidence suggests moderate-to-low feasibility, which generates the constraint''s suppression burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_implementation, empirical, 'Whether the transformation can be politically implemented in wealthy democracies.').

omega_variable(
    intergenerational_cost_incidence,
    'Who actually bears the cost of degrowth transformation: current-generation organized workers (via reduced incomes), or wealthy-nation elites (via wealth redistribution and ownership loss), or some other distribution?',
    'Tax incidence analysis of proposed transformation schemes; historical cost distribution from labor-market shifts and welfare-state expansions; empirical evaluation of whether universal basic services funded by wealth taxes actually preserve working-class material standards.',
    'If costs fall primarily on working classes despite rhetoric, the constraint operates as a Snare (extraction with coordination cover). If costs fall on wealth-holders, the constraint remains Tangled Rope (genuine coordination with asymmetric extraction from different seats). If cost-sharing is genuinely symmetric, the constraint becomes closer to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_incidence, empirical, 'The distribution of transformation costs across class positions.').

omega_variable(
    alternative_readings_logical_status,
    'Are the sibling readings (mitigation_priority and adaptation_priority) logically incompatible with this degrowth reading, or can they coexist in a framework that admits both as valid strategies for different contexts?',
    'Structural analysis of the claims: does rejecting technological-decoupling-based growth necessarily foreclose accepting adaptation as a complement? Can a framework accept both mitigation via degrowth AND mitigation via tech, with priority sequencing rather than mutual exclusion?',
    'If readings are logically incompatible (forecloses), the constraint carries a higher burden of proof and faces stronger sibling competition. If coexistent (coexists_with), the constraint is one legitimate option among others, reducing its mandatory character. This omega names the committer-frame uncertainty: which structural relationship obtains?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_logical_status, conceptual, 'Whether the degrowth reading logically precludes or merely contests the sibling readings.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.72) primarily structural (institutional barriers, enforcement apparatus, legal exclusions) or internalized (workers believe degrowth is undesirable, owners internalize legitimacy of growth, cultural dominance of consumption makes exit unthinkable)?',
    'Post-implementation suppression trajectory: if suppression persists after structural barriers are removed (through pilot programs or jurisdictional experiments), reclassify as substantially internalized; if suppression disappears when barriers fall, it was structural. Survey data from high-working-time-reduction and high-service-provision countries (Scandinavia, Netherlands) on satisfaction and aspiration shifts.',
    'If substantially internalized, the effective suppression is higher than the structural measure indicates — the constraint carries the suppression with it across populations even if enforcement mechanisms weaken. If structural, removing the enforcement apparatus would reduce suppression and potentially unlock political support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized cultural dominance of growth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, projected).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__degrowth_transformation, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(clim_tr_t8, projected).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__degrowth_transformation, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(clim_tr_t16, projected).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__degrowth_transformation, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(clim_tr_t24, projected).
narrative_ontology:measurement(clim_tr_t32, climate_response_legitimacy__degrowth_transformation, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(clim_tr_t32, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, projected).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(clim_be_t8, projected).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(clim_be_t16, projected).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(clim_be_t24, projected).
narrative_ontology:measurement(clim_be_t32, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(clim_be_t32, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(clim_su_t0, projected).
narrative_ontology:measurement(clim_su_t8, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(clim_su_t8, projected).
narrative_ontology:measurement(clim_su_t16, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(clim_su_t16, projected).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(clim_su_t24, projected).
narrative_ontology:measurement(clim_su_t32, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(clim_su_t32, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel. The kernel unites three structurally distinct claims about what constitutes legitimate climate response. The degrowth reading measures extraction against the standing arrangement (growth-dependent wealthy-nation economies); the mitigation_priority reading measures extraction against a different referent (technological decoupling models); the adaptation_priority reading measures extraction against yet another referent (current-warming adaptive infrastructure). Their ε values differ not because of measurement basis but because they identify different constraints operating on different premises. Decomposition is justified by ε-invariance: changing from degrowth to mitigation_priority changes ε because the constraint identity changes (the claim being measured changes from 'growth-dismantling-is-necessary' to 'decoupling-is-sufficient'). All three readings are linked via network.affects_constraints and share the kernel_id; their sibling relationships are declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
