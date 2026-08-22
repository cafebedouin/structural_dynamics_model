% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Adaptation-Priority Reading of Climate Harm Prevention
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation-priority reading of the
 *   contested climate-harm-prevention kernel: legitimate climate response, on
 *   this reading, prioritizes near-term resilience-building because
 *   aggressive mitigation is treated as politically and economically
 *   infeasible, and accepts a correspondingly higher long-run warming
 *   trajectory as the residual cost. The reading front-loads expenditure on
 *   present protection and back-loads physical damage onto later,
 *   less-consulted parties. This is a distinct constraint from the
 *   mitigation-priority reading (which claims emissions reduction is feasible
 *   and prioritizes preventing future harm) and the degrowth reading (which
 *   claims mitigation within a growth framework is impossible and requires
 *   planned contraction) — each reading has its own beneficiary/victim
 *   structure and its own epsilon, and none is a measurement of the same
 *   underlying arrangement from a different angle. They are three different
 *   constraints sharing a kernel.
 *
 * KEY AGENTS:
 *   - present_vulnerable_coastal_populations: primary near-term beneficiary of resilience spending
 *   - incumbent_energy_producers: institutional beneficiary and agenda-setter shaping the infeasibility framing
 *   - future_generations: primary payer, structurally voiceless in the decision
 *   - small_island_states: payer facing a physical adaptation ceiling below the accepted trajectory
 *   - climate_policy_analysts: analytical observers assessing whether infeasibility is genuine or constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.62).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.48).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '5a85c912-f5a1-4669-a919-4b8e09c3a2de').
narrative_ontology:cs_kernel_codification('5a85c912-f5a1-4669-a919-4b8e09c3a2de', distributed).
narrative_ontology:cs_authority_grounding('5a85c912-f5a1-4669-a919-4b8e09c3a2de', distributed).
narrative_ontology:cs_reading_relation('5a85c912-f5a1-4669-a919-4b8e09c3a2de', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('5a85c912-f5a1-4669-a919-4b8e09c3a2de', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('5a85c912-f5a1-4669-a919-4b8e09c3a2de', foundational, present_political_economic_constraints_bind_climate_choice).
narrative_ontology:cs_axiom_status(present_political_economic_constraints_bind_climate_choice, holdable).
narrative_ontology:cs_axiom_grounding('5a85c912-f5a1-4669-a919-4b8e09c3a2de', present_political_economic_constraints_bind_climate_choice, empirically_contingent).
narrative_ontology:cs_axiom('5a85c912-f5a1-4669-a919-4b8e09c3a2de', foundational, near_term_visible_harm_prevention_takes_priority_over_diffuse_future_harm).
narrative_ontology:cs_axiom_status(near_term_visible_harm_prevention_takes_priority_over_diffuse_future_harm, holdable).
narrative_ontology:cs_axiom_grounding('5a85c912-f5a1-4669-a919-4b8e09c3a2de', near_term_visible_harm_prevention_takes_priority_over_diffuse_future_harm, conventional).
narrative_ontology:cs_reference_frame('5a85c912-f5a1-4669-a919-4b8e09c3a2de', growth_compatible_incremental_policy_baseline).
narrative_ontology:cs_drift_state('5a85c912-f5a1-4669-a919-4b8e09c3a2de', post_paris_agreement_shortfall_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5a85c912-f5a1-4669-a919-4b8e09c3a2de', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_coastal_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_dependent_economies).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_energy_producers).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_generation_electorates).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_climate_migrants).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_economic_feasibility_constraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive sea walls, early-warning systems, relocation subsidies, and resilient infrastructure funded now. The adaptation-priority framing directs present budget toward their protection rather than toward emissions reductions whose benefits would arrive decades later and elsewhere.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_coastal_populations, beneficiary,
    moderate, immediate, constrained, national).

% National economies whose growth models depend on continued fossil fuel extraction or use are relieved of near-term mitigation mandates. They can continue current industrial pathways while redirecting some revenue toward resilience projects at home.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_dependent_economies, beneficiary,
    powerful, biographical, mobile, national).

% Fossil fuel companies and allied industries actively lobby for the feasibility framing that declares mitigation politically/economically impossible, since it preserves their asset base and defers stranded-asset risk. They fund think tanks and policy research supporting adaptation-first framings and shape which options are presented as 'realistic.'
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, incumbent_energy_producers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, incumbent_energy_producers, agenda_setter).

% Voters in wealthy nations avoid the near-term costs of aggressive decarbonization — carbon pricing, industrial transition, lifestyle change — in exchange for visible local resilience spending. Their political horizon rarely extends past the next election cycle.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_generation_electorates, beneficiary,
    organized, immediate, mobile, national).

% Inherit a higher-warming trajectory locked in by today's deferred mitigation. They bear compounding physical damages — more frequent extreme weather, ecosystem collapse, agricultural disruption — that adaptation investment made today cannot retroactively prevent, and have no vote or voice in the decision that produces their inheritance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Poorer regions lacking fiscal capacity to build seawalls, irrigation systems, or heat-resilient infrastructure absorb rising physical damages from a warming trajectory set largely by decisions made in wealthier emitting nations. Adaptation financing pledged to them is chronically underfunded relative to need.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Face existential territorial loss from sea-level rise that no feasible level of local adaptation spending can resolve — for some, adaptation has a physical ceiling below the warming trajectory this reading accepts. Their diplomatic advocacy for aggressive mitigation is heard but structurally outvoted in international forums dominated by larger emitters.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, regional).

% People not yet displaced who will be forced to migrate as regions become uninhabitable under the accepted warming trajectory. They have no legal standing, no international framework recognizing their status, and no representation in the negotiations that set the trajectory they will live under.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_climate_migrants, payer,
    powerless, generational, trapped, global).

% Publish assessments quantifying the harm differential between mitigation and adaptation-priority pathways, but their findings enter a policy process where the 'political/economic infeasibility' premise is treated as a fixed constraint rather than a contestable political choice, limiting their influence to caveats within an already-adopted frame.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists_and_ipcc_bodies, excluded,
    institutional, generational, analytical, global).

% Evaluate whether the claimed infeasibility of mitigation is a genuine physical/economic constraint or a constructed political outcome reflecting incumbent lobbying power and short electoral horizons, and trace how the adaptation-priority framing redistributes cost across time and geography.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs scarce present-day public resources toward protecting current populations from climate impacts that are already locked in or imminent, rather than diffusing spending across a mitigation program whose payoff is diffuse, delayed, and politically costly to enact.
% TRANSFER_FUNCTION: Moves fiscal and political capital from emissions-reduction programs into resilience infrastructure for present populations, and correspondingly transfers physical climate risk and cost from the present generation and adaptation-capable regions onto future generations and low-capacity regions who did not choose the trajectory.
% ABSENT_VOICES: Future generations and future climate migrants have no seat in the political process that sets the accepted warming trajectory; small island states and low-adaptation-capacity regions participate in international forums but are structurally outvoted by larger emitters who benefit from deferring mitigation costs.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing were abandoned overnight in favor of mitigation-priority policy, near-term fiscal allocations would shift sharply toward decarbonization infrastructure and carbon pricing, incumbent fossil-asset valuations would fall, present electorates would face immediate cost increases, and the warming trajectory imposed on future generations and vulnerable regions would be reduced — a substantial rearrangement of who bears cost and when.
% FOUNDING_PROBLEM: Governments faced immediate, visible climate damages (floods, heatwaves, storms) alongside a genuine political economy problem: rapid mitigation requires costs concentrated on identifiable present actors (workers, industries, voters) for benefits that are diffuse, delayed, and accrue mostly to people not yet born or not yet voting.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent energy producers and present-generation electorates attest the infeasibility premise is a live, binding political-economic constraint. IPCC assessment bodies, small island state coalitions, and independent climate economists — parties outside the beneficiary set — attest that mitigation remains physically and economically feasible at declining cost, and that the 'infeasibility' claim substantially reflects incumbent political resistance rather than a hard constraint, making the founding problem's status genuinely contested rather than settled.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects a substantial, growing transfer of climate risk from present, politically enfranchised populations to future and geographically distant populations who bear residual warming costs without having set the trajectory. Suppression (0.48) is moderate rather than high: no one is coercively barred from advocating mitigation, but incumbent lobbying, electoral short-termism, and the political packaging of 'feasibility' as an objective constraint rather than a contestable choice function as a softer suppressive mechanism against alternative framings. Theater ratio (0.40) is elevated because a meaningful share of adaptation spending functions as visible present-tense political performance (ribbon-cutting resilience projects) layered atop a genuine underlying need for protective infrastructure. Accessibility collapse (0.50) is moderate: mitigation pathways remain technically available (unlike a true mountain), but political-economic framing has substantially narrowed which options are treated as legitimate. Resistance (0.60) is real and rising — climate scientists, youth movements, and vulnerable-state coalitions actively contest the infeasibility premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable coastal populations, present-generation electorates, incumbent energy producers, and fossil-fuel-dependent economies are declared beneficiaries: they receive either direct protective investment now or continuity of current economic arrangements without near-term disruption — low d, benefit-side. Future generations, low-adaptation-capacity regions, small island states, and future climate migrants are declared victims: they bear the compounding physical costs of a higher warming trajectory they had no part in choosing and cannot exit — high d, target-side, amplified further by trapped/generational exit options and (for future generations and migrants) the absence of any present standing at all. The engine's directionality derivation should place these groups near the full-target end precisely because temporal and civic exclusion compound the structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visible present climate damage colliding with a genuine short-term political economy constraint on rapid decarbonization) was real when adaptation-first framings emerged. Whether it remains live is genuinely contested: incumbent beneficiaries treat mitigation infeasibility as a stable fact, while scientific bodies and excluded future/vulnerable parties argue the infeasibility premise has become a self-serving political construction that outlives any period in which it was strictly true, given falling renewable costs and demonstrated rapid-transition precedents elsewhere. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (real present protection is delivered, not fabricated) while registering the asymmetric extraction from those with no voice in the decision — collapsing it to pure extraction would erase the real resilience benefits present populations receive; treating it as pure coordination would erase the compounding cost transferred forward and outward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infeasibility_premise_genuineness,
    'Is the claimed political/economic infeasibility of near-term mitigation a genuine structural constraint, or a constructed political outcome sustained by incumbent lobbying and electoral short-termism that could be otherwise?',
    'Comparative policy analysis of jurisdictions that have executed rapid decarbonization at scale (e.g., wartime-style industrial mobilization precedents, national renewable buildouts) against jurisdictions claiming infeasibility, controlling for resource endowment and political system.',
    'If infeasibility is substantially constructed rather than physically/economically binding, this reading functions closer to a snare dressed as feasible coordination; if infeasibility is a genuine hard constraint given present political institutions, the tangled_rope classification (real coordination plus asymmetric extraction) is the accurate read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infeasibility_premise_genuineness, conceptual, 'Whether the adaptation-priority reading''s founding premise is a real or constructed constraint.').

omega_variable(
    kernel_reading_selection_authority,
    'Who has the standing authority to determine which reading of the climate_harm_prevention kernel counts as the legitimate one — present national electorates, international scientific bodies, or the future/vulnerable parties who cannot vote in the process that selects a reading?',
    'Track whether international climate governance mechanisms (UNFCCC processes, loss-and-damage funds, ICJ advisory proceedings) shift decision authority toward parties currently excluded from the reading-selection process.',
    'If authority remains concentrated in present national electorates and incumbent-influenced legislatures, the adaptation-priority reading will likely persist regardless of its harm profile; if authority shifts toward affected future/vulnerable parties or binding international adjudication, the mitigation_priority reading gains structural leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Where the locus of legitimate authority over kernel-reading selection actually sits.').

omega_variable(
    adaptation_ceiling_for_territorial_loss,
    'For populations facing physical territorial loss (small island states, low-lying deltas), does any level of adaptation investment actually substitute for avoided warming, or is there a hard physical ceiling beyond which adaptation cannot compensate for the accepted trajectory?',
    'Sea-level rise and habitability modeling cross-referenced against maximum feasible engineered adaptation (seawalls, managed retreat capacity, freshwater systems) for specific island and delta geographies.',
    'If a hard ceiling exists, the ''residual costs'' this reading assigns to low-adaptation-capacity regions are not merely delayed or diminished but categorically unavoidable under the accepted trajectory — sharpening the victim classification for those groups specifically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_ceiling_for_territorial_loss, empirical, 'Whether adaptation has a physical substitution limit for territorial-loss-exposed populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.27).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.34).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.37).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__adaptation_priority, theater_ratio, 25, 0.39).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__adaptation_priority, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__adaptation_priority, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_harm_prevention kernel, each authored as a separate story with its own epsilon and stakeholder structure per the epsilon-invariance principle: adaptation_priority (this file), mitigation_priority, and degrowth_reading. The three are linked bidirectionally via affects_constraints because each reading's political dominance structurally affects the resource and legitimacy environment available to the others — funding committed to adaptation infrastructure is not simultaneously available to mitigation infrastructure, and the political framing of 'infeasibility' that grounds this reading is precisely what the mitigation_priority reading contests as false.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
