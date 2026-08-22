% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-26
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative â Degrowth Reading
 *   domain: climate_policy_political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth reading of the
 *   climate_response_imperative kernel. It holds that climate response
 *   requires structural economic transformation in the Global Northâreduced
 *   consumption, redistribution, and post-growth institutionsâto enable
 *   both mitigation and adaptation. Present-day Global North populations
 *   enter the victim set through consumption and working-time reductions;
 *   future generations and Global South populations are the beneficiaries.
 *   The reading eliminates reliance on unproven carbon dioxide removal and
 *   technological optimism.
 *
 * KEY AGENTS:
 *   - Present-day Global North populations: Primary target (organized/constrained) â bear extraction via reduced material throughput.
 *   - Future generations: Primary beneficiary (powerless/trapped) â receive climate stability but have no present voice.
 *   - Global South populations: Secondary beneficiary (moderate/constrained) â gain ecological space and redistribution.
 *   - Global North institutions: Agenda-setter (institutional/constrained) â administer the transition.
 *   - Fossil fuel incumbents: Secondary target (powerful/constrained) â face stranded assets and phase-out.
 *   - Climate scientists: Analytical observer (organized/analytical) â supply the planetary boundaries evidence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.78).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative â Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy_political_economy").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d').
narrative_ontology:cs_kernel_codification('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', distributed).
narrative_ontology:cs_authority_grounding('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', expertise).
narrative_ontology:cs_interpretation_layer_present('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d').
narrative_ontology:cs_reading_relation('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', foundational, eliminate_unproven_cdr_reliance).
narrative_ontology:cs_axiom_status(eliminate_unproven_cdr_reliance, holdable).
narrative_ontology:cs_axiom_grounding('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', eliminate_unproven_cdr_reliance, empirically_contingent).
narrative_ontology:cs_axiom('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', foundational, global_north_contraction_obligation).
narrative_ontology:cs_axiom_status(global_north_contraction_obligation, holdable).
narrative_ontology:cs_axiom_grounding('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', global_north_contraction_obligation, deontological).
narrative_ontology:cs_reference_frame('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', planetary_boundaries_steady_state).
narrative_ontology:cs_drift_state('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21eaf1b3-15d1-4c9e-a127-82fafc7e8b2d', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of reduced consumption, altered labor markets, and lower material throughput as wealthy economies are restructured away from growth. They vote in democracies but face constrained exit from the global climate regime and embedded carbon-intensive infrastructure.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_day_global_north_populations, payer,
    organized, biographical, constrained, global).

% Face stranded assets, declining social license, and regulatory phase-out under the transformation imperative. They deploy capital and lobbying capacity to resist the constraint but cannot easily exit the political economy in which their reserves and infrastructure are valued.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, global).

% Receive a stabilized climate and preserved ecological capacity but hold no present vote or market power. They cannot exit the temporal arrangement and their benefit is entirely contingent on present-day transformation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Benefit from reduced climate impacts, freed ecological space, and redistributive transfers that enable development without replicating Northern emissions trajectories. Constrained by existing global economic structures but gaining relative to business-as-usual.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, global).

% Administer the post-growth transition through fiscal, regulatory, and redistribution regimes. They set the formal rules but are constrained by domestic political economy, democratic cycles, and the threat of capital flight if transformation is too abrupt.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_institutions, agenda_setter,
    institutional, biographical, constrained, global).

% Argue that technological innovation, efficiency, and market mechanisms can achieve climate goals without contraction of economic output. Excluded from the core framing of this reading, which treats their premise as insufficient and potentially dangerous.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, green_growth_advocates, excluded,
    organized, biographical, mobile, global).

% Produce and synthesize Earth system evidence on emissions trajectories and planetary boundaries. They do not collect or pay within the constraint but provide the empirical substrate that makes the imperative legible.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_scientists, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational and global collective-action problem of climate change by aligning wealthy economies with planetary boundaries and freeing ecological and policy space for Global South development.
% TRANSFER_FUNCTION: Moves consumption, productive capacity, and accumulated wealth from present-day Global North populations and fossil fuel incumbents to future generations and Global South populations via reduced throughput, working-time reduction, and redistributive post-growth institutions.
% ABSENT_VOICES: Green growth advocates, tech-optimist economists, and present-day Global North political majorities who reject consumption reduction are structurally sidelined in this reading; they would argue for innovation and market mechanisms over contraction.
% DISAPPEARANCE_RATIONALE: If the imperative for structural economic transformation disappeared, climate governance would revert to mitigation-priority or adaptation-priority frames, preserving Global North consumption patterns and leaving future generations and Global South populations exposed to accumulating damages.
% FOUNDING_PROBLEM: Anthropogenic climate change driven by historical Global North emissions has created an intergenerational and global justice crisis in which current economic structures exceed planetary boundaries and lock in catastrophic warming.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group reports attest to the physical and impact dimensions from a scientific seat outside the beneficiary set. Global South climate negotiators and youth movements corroborate the justice dimension. Mainstream growth economists contest the specific framing that structural contraction is required, supporting the live/contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint demands fundamental restructuring of wealthy economies, extensive redistribution, and abandonment of growth as a policy objective. Suppression (0.72) reflects the active suppression of growth-as-usual alternatives and fossil fuel development. Theater ratio (0.45) captures the performative gap in international climate negotiations where contraction is discussed but not implemented. Resistance (0.80) is high because incumbent growth coalitions and populations actively oppose the arrangement. Accessibility collapse (0.60) indicates that alternatives such as green growth are rhetorically marginalized but remain technically accessible. The measurement series use a single shared time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Global North populations, fossil incumbents) experience the constraint as extractive sacrifice imposed by an external imperative. The beneficiary seats (future generations, Global South) experience it as necessary justice and coordination. The agenda-setter seat experiences it as a difficult but legitimate transition to administer. The engine computes this divergence from structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-day Global North populations and fossil fuel incumbents are declared victims; their directionality sits near the full-target end, amplifying effective extraction. Future generations and Global South populations are declared beneficiaries; their directionality sits near the full-beneficiary end, damping or inverting effective extraction. Global North institutions are agenda-setters with constrained exit; their directionality is mixed but administratively closer to the beneficiary end because they coordinate the transition rather than paying its material costs. Climate scientists occupy an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) because it possesses a genuine coordination function: stabilizing the climate for future generations and freeing ecological space. It prevents mislabeling as pure coordination (rope) because the victim set is non-empty and asymmetric. If the founding problem were solved and the constraint persisted without a sunset clause, it would drift toward piton; presently it lacks a sunset and carries mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_validity,
    'Is the degrowth reading structurally distinct from its siblings, or merely a policy preference within the same climate response framework?',
    'Compare beneficiary/victim invariance across readings: if the same agents are victims and beneficiaries across all three readings with only policy instruments differing, merge into one constraint; if the victim/beneficiary sets differ structurally, maintain separate epsilon values.',
    'If preference-only, this story should be merged with its siblings; if structurally distinct, the separate epsilon and directionality profiles are warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_validity, conceptual, 'Whether degrowth is a distinct constraint or a preference variant').

omega_variable(
    present_suffering_vs_future_benefit,
    'Do present-day Global North populations experience net harm or net benefit from the transformation, considering avoided climate damages within their own lifetimes?',
    'Integrated assessment models comparing damage functions under immediate transformation versus business-as-usual within a biographical time horizon for current adult cohorts.',
    'If net benefit, their directionality shifts toward symmetric and the extraction profile weakens; if net harm, the target classification and high extraction hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_suffering_vs_future_benefit, empirical, 'Net welfare effect of transformation on present Global North').

omega_variable(
    enforcement_feasibility,
    'Can post-growth institutions actually enforce reduced consumption and redistribution against incumbent growth coalitions in democratic contexts?',
    'Historical case studies of economic restructuring under democratic constraints and analysis of policy lock-in by incumbent industry.',
    'If infeasible, the constraint may be largely theatrical or extractive without effective coordination; if feasible, the tangled_rope classification with active enforcement holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_feasibility, empirical, 'Feasibility of enforcing post-growth transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__degrowth_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__degrowth_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__degrowth_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth reading of the climate_response_imperative kernel, distinct from mitigation-priority and adaptation-priority readings which share the same kernel but assign different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
