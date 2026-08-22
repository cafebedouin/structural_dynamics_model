% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Harm Prevention: Adaptation-Priority Reading
 *   domain: climate_policy/intergenerational_ethics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the adaptation-priority reading of the
 *   contested climate harm prevention kernel. Governments and development
 *   institutions adopt the reading that rapid mitigation is politically and
 *   economically infeasible, and that legitimate climate response therefore
 *   prioritizes near-term resilience building for present vulnerable
 *   populations through adaptation infrastructure. This reading accepts a
 *   higher warming trajectory (2.5–3.5°C+) as the cost of political
 *   feasibility. The sibling readings — mitigation-priority (emphasizing
 *   emissions reduction) and degrowth (requiring planned economic
 *   contraction) — offer different framings of the same kernel (climate harm
 *   prevention), but this constraint instantiates only the
 *   adaptation-priority reading. The core asymmetry: present vulnerable
 *   populations receive priority benefit, while future generations and
 *   low-adaptation-capacity regions bear residual climate costs. Extraction
 *   accumulates (theater ratio rises) as the initial coordination function
 *   (solving near-term resource allocation) yields to enforcement maintenance
 *   (defending the feasibility frame against mitigation alternatives).
 *
 * KEY AGENTS:
 *   - Present vulnerable populations (trapped, immediate time horizon): coastal communities, island nations, arid-margin agriculture. Receive prioritized adaptation funding; their survival 2–3 decades is the reading's moral anchor.
 *   - Global North governments (institutional, agenda-setter): Set the feasibility frame. Justify adaptation-priority as the best achievable response; manage political cost of deprioritizing mitigation.
 *   - Future generations (powerless, civilizational horizon, trapped exit): Inherit higher warming, cumulative impacts, and exhausted adaptation budgets. No voice in present choice.
 *   - Low-adaptation-capacity regions (moderate power, constrained exit): Africa, South Asia, island states. Receive some adaptation funding but insufficient for the warming trajectory; residual climate impacts unavoidable.
 *   - Fossil fuel sector (powerful beneficiary): Retains market share under adaptation-priority because mitigation is deprioritized. No direct threat from adaptation spending.
 *   - Development finance institutions (institutional agenda-setter): Deploy adaptation finance; benefit from expanded infrastructure mandate. Claim to serve vulnerable populations while administering the constraint.
 *   - Mitigation advocates (excluded): Environmental organizations, climate scientists, future-focused ethicists. Present in discourse but excluded from decision authority. Argue adaptation alone is insufficient and legitimizes harm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Harm Prevention: Adaptation-Priority Reading").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/intergenerational_ethics/political_economy").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'a242fbff-f839-48dc-87f4-3047f5b69c52').
narrative_ontology:cs_kernel_codification('a242fbff-f839-48dc-87f4-3047f5b69c52', distributed).
narrative_ontology:cs_authority_grounding('a242fbff-f839-48dc-87f4-3047f5b69c52', distributed).
narrative_ontology:cs_reading_relation('a242fbff-f839-48dc-87f4-3047f5b69c52', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('a242fbff-f839-48dc-87f4-3047f5b69c52', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('a242fbff-f839-48dc-87f4-3047f5b69c52', foundational, mitigation_infeasibility_within_growth).
narrative_ontology:cs_axiom_status(mitigation_infeasibility_within_growth, holdable).
narrative_ontology:cs_axiom_grounding('a242fbff-f839-48dc-87f4-3047f5b69c52', mitigation_infeasibility_within_growth, empirically_contingent).
narrative_ontology:cs_axiom('a242fbff-f839-48dc-87f4-3047f5b69c52', foundational, present_vulnerable_populations_moral_priority).
narrative_ontology:cs_axiom_status(present_vulnerable_populations_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('a242fbff-f839-48dc-87f4-3047f5b69c52', present_vulnerable_populations_moral_priority, deontological).
narrative_ontology:cs_reference_frame('a242fbff-f839-48dc-87f4-3047f5b69c52', climate_feasibility_via_adaptation).
narrative_ontology:cs_drift_state('a242fbff-f839-48dc-87f4-3047f5b69c52', contemporary_tipping_point_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a242fbff-f839-48dc-87f4-3047f5b69c52', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, near_term_adaptation_beneficiaries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, development_finance_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coastal communities, island nations, arid-margin agricultural zones, and low-income urban centers facing imminent climate hazards (sea-level rise, drought, flooding, heat extremes). They receive priority funding and infrastructure deployment for seawalls, water systems, early warning networks, and disaster response capacity. The adaptation-priority reading foregrounds their survival in the next 2–3 decades as the primary moral and political legitimacy condition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Set the policy frame defining adaptation as the feasible response pathway. They manage the political and budgetary trade-off between mitigation (costly, requires economic restructuring) and adaptation (deployable within growth-compatible models). They justify the choice through cost-benefit analysis, technological feasibility studies, and political-economy constraints on carbon pricing or energy transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, global_north_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Inherit a warmer climate (2.5–3.5°C+ warming trajectory under adaptation-priority models) with accumulated climate impacts beyond near-term adaptation capacity: ecosystem collapse, forced migration, resource scarcity, and infrastructure failures at scale. They have no voice in present policy choice and no ability to exit or alter the constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Sub-Saharan Africa, South Asian agricultural economies, and island states with limited capital and technology access for adaptation infrastructure. While they receive some adaptation funding, the aggregate adaptation budget is insufficient for the warming trajectory the reading accepts; they face residual climate impacts (crop failure, water stress, displacement) that adaptation cannot prevent.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    moderate, generational, constrained, global).

% Retains market share and production under the adaptation-priority frame because mitigation is deprioritized. The reading's acceptance of higher warming trajectories means delayed or weakened carbon pricing, continued hydrocarbon demand growth, and extended asset lives. Adaptation funding does not directly threaten their operations.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_sector, beneficiary,
    powerful, biographical, mobile, global).

% Documents the warming trajectory implications of adaptation-priority policy (tipping points, irreversible changes, cumulative impacts). Provides evidence on which warming levels are compatible with which adaptation strategies. Holds no direct power but shapes the epistemic legitimacy of the policy frame.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_science_community, observer,
    analytical, generational, analytical, global).

% Environmental movements, climate scientists emphasizing mitigation, and ethical philosophers prioritizing future interests argue that adaptation alone is insufficient and that the reading legitimizes climate harm. They are present in policy discourse but structurally excluded from decision-making authority; governments and capital-holders set the feasibility frame that constrains their position.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, civilizational, constrained, global).

% World Bank, regional development banks, and bilateral development agencies deploy adaptation finance. They benefit from the expanded infrastructure mandate (engineering contracts, project deployment, institutional relevance) while also claiming to serve vulnerable populations. They administer the constraint's implementation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, development_finance_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, development_finance_institutions, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, global_north_governments).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates limited climate finance to near-term adaptation infrastructure for vulnerable populations, solving the present resource-allocation problem when rapid mitigation is believed politically infeasible.
% TRANSFER_FUNCTION: Moves adaptation finance from global development budgets and climate funds toward present-vulnerable coastal, island, and arid-margin communities. Simultaneously transfers climate risk (higher warming trajectory, cumulative impacts) from present to future generations and from high-adaptation-capacity to low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations have no seat at present policy choice; they cannot contest the accepted warming trajectory. Low-adaptation-capacity countries have formal participation in climate conferences but lack capital leverage to veto the adaptation-priority frame. Mitigation advocates (climate scientists emphasizing tipping points, environmental movements, future-focused ethicists) are present in discourse but excluded from decision authority in most governments and capital allocation institutions.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint and its enforcement vanished overnight, governments would face immediate pressure to reallocate climate finance toward mitigation (carbon pricing, energy transition research, technology deployment). Present vulnerable populations would lose prioritized adaptation funding and face both accelerating near-term hazards AND higher long-term warming. Development institutions would lose expanded infrastructure mandates. The political economy of climate policy would reorganize around competing mitigation vs. adaptation priorities.
% FOUNDING_PROBLEM: In the early 2020s, rapid global mitigation appeared politically infeasible: carbon pricing lacked consensus, energy transition was capital-intensive, developed economies were reluctant to accept decarbonization costs. Meanwhile, present vulnerable populations faced immediate climate hazards (sea-level rise, drought, flooding). Adaptation offered a way to reduce urgent suffering without requiring politically difficult mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Governments and development institutions cite political economy analyses and cost-benefit studies showing mitigation as economically disruptive and politically infeasible at the speed required. Climate scientists, renewable energy economists (outside government advisory circles), and environmental organizations contest this claim, pointing to accelerating technology cost declines, successful rapid renewable deployment in Denmark/Costa Rica/Uruguay, and carbon pricing mechanisms in EU/Canada as evidence of feasibility. The founding problem's status is actively disputed in peer-reviewed climate economics, policy analysis, and international climate negotiations. No consensus corroboration exists outside the benefiting institutional coalition.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the 40-year interval because the constraint accumulates intergenerational transfer: present vulnerable populations gain near-term protection, but each year of delayed mitigation locks in higher warming and shifts costs to future populations that cannot contest the choice. Suppression is moderate (0.52) but rises steadily: governments must actively defend the 'infeasibility' narrative against mitigation alternatives; climate scientists emphasizing tipping points and irreversibility are present but structurally excluded from decision authority. Theater ratio starts low (0.28) and rises toward 0.44 because adaptation projects generate visible infrastructure (seawalls, water systems) that satisfy accountability demands, while the underlying mitigation-avoidance becomes harder to defend—the constraint begins performing its legitimacy more theatrically as impacts accumulate. Accessibility collapse is low (0.47): alternatives to the adaptation-priority frame exist (mitigation-priority, degrowth) and retain analytical coherence, but they are politically suppressed rather than logically impossible. Resistance is moderate-high (0.58): mitigation advocates, climate scientists, and future-interested constituencies actively contest the reading, but lack institutional power to override government choice. The measurement series is authored on a single shared time grid: every metric is valued at each time point (8 points × 3 metrics = 24 measurements). Early observations (t=0–20) are empirically grounded in actual policy trajectories; t=25–40 are projected under the assumption the adaptation-priority frame persists. The rising trajectory models how the constraint's initial coordination function (solving near-term allocation) is overtaken by enforcement activity defending against mitigation alternatives as impacts accumulate.
 *
 * PERSPECTIVAL GAP:
 *   Present vulnerable populations and near-term adaptation beneficiaries experience the constraint as legitimate coordination: it provides funding and priority attention when they face imminent threats. From their seat, the adaptation-priority reading solves a real problem (near-term climate risk) and delivers material benefit (seawalls, early warning, capacity). Future generations and low-adaptation-capacity regions experience the same constraint as extractive transfer: the reading legitimizes the present's choice to accept higher warming rather than pay the near-term political cost of mitigation. They inherit constrained options and unavoidable impacts. From the government and development-institution seat, the constraint is feasible coordination (solvable with growth-compatible infrastructure spending, implementable without economic disruption). From the mitigation-advocate and climate-scientist seats, it is extraction disguised as problem-solving (the 'infeasibility' claim is overstated; mitigation was always available but politically convenient to deprioritize). The engine computes each seat's directionality (d) from power + time_horizon + exit_options: present vulnerable populations are low-power, immediate-horizon, trapped → high beneficiary character; future generations are powerless, civilizational-horizon, trapped → pure target (d near 1.0); governments are institutional, biographical-horizon, constrained-but-mobile → mixed (set the rules but bound by political economy). The per-seat type divergence follows from these directionalities feeding the extraction computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations: low power, immediate horizon, trapped exit. They are beneficiaries (receive adaptation funding) with no mobility. Directionality d is driven strongly toward 0.0 (beneficiary end) by the explicit beneficiary declaration and trapped exit, but moderated upward by their powerlessness and dependence on government allocation. d ≈ 0.15–0.25 (beneficiary-leaning). Future generations: powerless, civilizational horizon, trapped exit (no ability to exit the warming they inherit). Not declared as beneficiaries; declared as victims. No mobility. d → 1.0 (pure target). Low-adaptation-capacity regions: moderate power, generational horizon, constrained exit. Declared as victims (bear residual costs). Have some institutional voice but limited capital leverage. d ≈ 0.65–0.75 (target-leaning). Governments: institutional power, biographical horizon, constrained exit (bound by political economy but can choose policy direction). Declared as beneficiary via fossil fuel sector benefits + authority to set the feasibility frame. d ≈ 0.35–0.45 (mixed, slightly beneficiary-leaning). Fossil fuel sector: powerful, biographical horizon, mobile exit (can divest, relocate, invest in new sectors). Declared beneficiary. d ≈ 0.20 (beneficiary). Development finance institutions: institutional power, generational horizon, constrained exit. Declared beneficiary (expanded mandate, projects) with secondary agenda-setter role. d ≈ 0.30 (beneficiary-leaning). Mitigation advocates: organized power, civilizational horizon, constrained exit (politically excluded). Not declared beneficiary or victim but role=excluded asserts their structural absence from decision authority. d ≈ 0.70 (outsider position). The directionality spread reflects structural asymmetry: the reading benefits short-term actors and the present, targets long-term actors and the future. No overrides are needed; the derivation from declarations and exit produces appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits incipient mandatrophy: the founding problem (political infeasibility of rapid mitigation in early 2020s) was arguably valid then but is contested now. As renewable technology costs fall, carbon pricing becomes viable in more jurisdictions, and energy transition examples accumulate, the 'infeasibility' claim becomes harder to defend. Yet the adaptation-priority constraint persists because: (1) governments benefit from avoiding carbon pricing and energy restructuring; (2) development institutions benefit from expanded adaptation budgets and infrastructure mandates; (3) present vulnerable populations benefit from adaptation funding and fear losing it if priorities shift. The founding problem's status is live for present populations (imminent climate hazards remain real) but dead or contested for future populations and low-adaptation-capacity regions. The constraint does NOT yet exhibit full piton characteristics (complete function atrophy + pure theater) because adaptation genuinely reduces near-term risk. But the measurement series shows theater ratio rising (from 0.28 to 0.44), indicating growing performative defense of the infeasibility narrative. Mandatrophy is emerging: the founding problem is weakening, but the constraint persists due to institutional inertia and distributional benefit to present decision-makers. The constraint could resolve mandatrophy in two directions: (a) foundational mitigation acceleration, reframing the problem as solvable and redirecting climate finance toward emissions reduction (constraint dissolves or reclassifies); (b) full piton realization, where the constraint becomes pure performance (high theater, low actual risk reduction) as impacts exceed adaptation capacity. Neither has occurred yet (theater is rising but not dominant; risk reduction is still material). The reading keeps the constraint in tangled_rope territory: real coordination (near-term adaptation) bound with real extraction (deferral of mitigation, transfer of warming risk to future).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_feasibility_premise,
    'Is rapid, large-scale mitigation genuinely infeasible within present political and economic structures, or does the adaptation-priority reading understate mitigation possibility to justify near-term extraction?',
    'Historical analysis of policy moments where mitigation acceleration occurred (rapid renewable deployment in Denmark, Costa Rica, Uruguay; carbon pricing mechanisms in EU, Canada). Comparative assessment of political will narratives vs. actual policy flexibility in other domains (pandemic response, financial crisis stimulus). Empirical observation of technology cost curves and deployment timelines.',
    'If mitigation is underestimated as infeasible, the reading''s core justification collapses and the constraint reclassifies as pure extraction (snare) rather than coordination-with-asymmetry. If mitigation is genuinely constrained, the tangled-rope classification holds but the moral weight shifts to questioning whether accepting higher warming is a legitimate policy response to constrained feasibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_feasibility_premise, empirical, 'The empirical status of mitigation feasibility claims underlying the reading''s legitimacy.').

omega_variable(
    adaptation_saturation_point,
    'At what warming level does adaptation capacity saturate? Is 2.5–3.5°C warming compatible with maintaining livable conditions for present vulnerable populations past the near-term horizon?',
    'Climate impact modeling at specific warming levels for vulnerable regions (crop yield at 3°C warming, water availability, habitability thresholds). Empirical observation as warming accumulates: do adaptation investments prevent harm, or do impacts exceed adaptation capacity despite funding?',
    'If adaptation saturates below 2.5°C, the constraint is not actually protecting present vulnerable populations but merely postponing harm—it becomes a snare (deferral, not prevention). If adaptation holds to 3.5°C+, the tangled-rope classification is supported but the reading accepts significant unpreventable harm to future populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_saturation_point, empirical, 'Whether the constraint''s promised near-term protection holds at the warming trajectory it accepts.').

omega_variable(
    intergenerational_normativity,
    'Is the implicit discount rate applied to future harm (treating it as less important than present suffering) normatively defensible, or does it embed an extractive transfer hidden in cost-benefit arithmetic?',
    'Philosophical analysis: comparison to discount rates applied in other long-timescale policy domains (nuclear waste, financial obligations, species extinction). Empirical measurement of whose welfare is discounted (future humans vs. present humans; wealthy vs. poor; high-adaptation capacity vs. low-adaptation capacity). Audit of intergenerational justice frameworks that the reading claims to honor.',
    'If the discount rate is unjustifiably high, the reading violates intergenerational justice and the constraint reclassifies toward snare (systematic transfer of welfare from future to present). If the rate is defensible, the tangled-rope framing holds but requires explicit normative justification, not mere economic convenience or political feasibility claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_normativity, preference, 'Whether the adaptation-priority reading''s treatment of future harm is normatively justified or represents hidden intergenerational extraction.').

omega_variable(
    reading_genealogy_vs_rationalization,
    'Is the adaptation-priority reading a genuine analytical position developed through independent scholarship, or a post-hoc rationalization of constrained political choice by governments and fossil interests?',
    'Historical tracing of the reading''s intellectual origins (peer-reviewed climate economics, ethical philosophy) vs. institutional adoption timelines (government policy documents, corporate adaptation narratives). Analysis of who holds the reading and what they benefit from (actors with mitigation-avoidance incentives vs. independent analysts). Audit of citations and intellectual lineage.',
    'If the reading is rationalization, the constraint exhibits higher suppression (of genuine mitigation-priority arguments) and lower accessibility collapse (alternatives exist but are politically suppressed). The classification might shift toward snare (pure extraction using a post-hoc cover story). If the reading is independent analysis with genuine merit, it retains analytical legitimacy despite asymmetric outcomes and tangled-rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_genealogy_vs_rationalization, empirical, 'The epistemic genealogy of the adaptation-priority reading: whether independent analysis or institutional rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__adaptation_priority, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__adaptation_priority, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__adaptation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__adaptation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__adaptation_priority, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.53).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__adaptation_priority, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel decomposes into three structurally distinct constraints, one per reading. adaptation_priority frames the climate problem as requiring near-term resilience with accepted higher warming (this story); mitigation_priority frames it as requiring emissions reduction with lower warming outcome; degrowth_reading frames it as requiring economic contraction. All three share the kernel (climate harm must be addressed) but differ fundamentally in ε (what harm is acceptable), beneficiary structure (who benefits from this response), and time-horizon priorities (present vs. future). The readings exhibit different directionalities, power distributions, and exit options for stakeholders. Each story carries its own ε-invariant assessment. Sibling constraints are linked via network.affects_constraints so the corpus recognizes them as a family and tracks how policy choice between readings alters distributional outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, powerless, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
