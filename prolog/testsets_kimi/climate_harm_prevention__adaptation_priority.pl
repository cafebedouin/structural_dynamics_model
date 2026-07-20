% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Adaptation-Priority Climate Response (Higher Warming Trajectory)
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   The adaptation_priority reading of the climate_harm_prevention kernel
 *   treats near-term resilience building as the legitimate climate response,
 *   accepting a higher warming trajectory because mitigation is deemed
 *   politically and economically infeasible. Present vulnerable populations
 *   receive adaptation infrastructure and finance, while future generations
 *   and low-adaptation-capacity regions bear the residual climate costs that
 *   adaptation cannot prevent. The constraint operates as a policy regime
 *   that requires active political maintenanceâbudget allocation,
 *   institutional priority-setting, and the marginalization of mitigation
 *   advocates. It claims coordination (protecting present life) while
 *   structurally transferring uncompensated risk to temporally and
 *   geographically distant parties. This is authored as a tangled_rope:
 *   genuine coordination function for present vulnerable populations,
 *   asymmetric extraction onto future generations and weak regions, actively
 *   enforced through political economy capture.
 *
 * KEY AGENTS:
 *   - climate_policy_authorities: Agenda-setter (institutional/mobile) â administers the prioritization and allocates finance
 *   - present_vulnerable_populations: Beneficiary (powerless/trapped) â receives immediate resilience infrastructure
 *   - incumbent_economic_actors: Beneficiary (institutional/arbitrage) â avoids mitigation costs and captures rents
 *   - future_generations: Payer (powerless/trapped) â bears locked-in warming trajectory without representation
 *   - low_adaptation_capacity_regions: Payer (powerless/trapped) â bears uncompensated residual damages
 *   - mitigation_advocates: Excluded (organized/constrained) â structurally sidelined from finance and policy priority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.6).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Climate Response (Higher Warming Trajectory)").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '5ec73fcb-039f-4a0e-b8a3-2a2460478455').
narrative_ontology:cs_kernel_codification('5ec73fcb-039f-4a0e-b8a3-2a2460478455', distributed).
narrative_ontology:cs_authority_grounding('5ec73fcb-039f-4a0e-b8a3-2a2460478455', distributed).
narrative_ontology:cs_reading_relation('5ec73fcb-039f-4a0e-b8a3-2a2460478455', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('5ec73fcb-039f-4a0e-b8a3-2a2460478455', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('5ec73fcb-039f-4a0e-b8a3-2a2460478455', foundational, near_term_resilience_overrides_mitigation_priority).
narrative_ontology:cs_axiom_status(near_term_resilience_overrides_mitigation_priority, holdable).
narrative_ontology:cs_axiom_grounding('5ec73fcb-039f-4a0e-b8a3-2a2460478455', near_term_resilience_overrides_mitigation_priority, deontological).
narrative_ontology:cs_axiom('5ec73fcb-039f-4a0e-b8a3-2a2460478455', foundational, political_economic_feasibility_bounds_climate_action).
narrative_ontology:cs_axiom_status(political_economic_feasibility_bounds_climate_action, holdable).
narrative_ontology:cs_axiom_grounding('5ec73fcb-039f-4a0e-b8a3-2a2460478455', political_economic_feasibility_bounds_climate_action, empirically_contingent).
narrative_ontology:cs_reference_frame('5ec73fcb-039f-4a0e-b8a3-2a2460478455', pragmatic_resilience_governance).
narrative_ontology:cs_drift_state('5ec73fcb-039f-4a0e-b8a3-2a2460478455', contemporary_climate_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ec73fcb-039f-4a0e-b8a3-2a2460478455', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_economic_actors).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, climate_adaptation_imperative).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, mitigation_infeasibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer climate finance and policy design, allocating budgets toward adaptation infrastructure and resilience programs while deferring stringent mitigation regulation. They justify this prioritization as pragmatic given perceived political constraints and immediate human needs. They retain formal policy mobility but operate within institutional capture by incumbent economic interests.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_policy_authorities, agenda_setter,
    institutional, generational, mobile, global).

% Receive adaptation funding, early-warning systems, and resilient infrastructure designed to protect against floods, droughts, and storms. They benefit from immediate material protection but cannot easily exit their climate-vulnerable geographic and economic circumstances and depend on state-delivered resilience.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Avoid stranded assets, carbon pricing, and regulatory transition costs by framing emissions mitigation as politically and economically infeasible. They capture continued rents from high-carbon infrastructure and supply chains while adaptation spending is channeled through their contracting networks.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, incumbent_economic_actors, beneficiary,
    institutional, biographical, arbitrage, global).

% Inherit the higher warming trajectory accepted under the adaptation-priority framework. They bear residual damages that adaptation cannot fully preventâsea level rise, ecosystem loss, and compounded climate impactsâwithout having been present in the policy negotiations that locked in the trajectory.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Small island states, arid regions, and least-developed countries that lack fiscal and technical resources to implement effective adaptation. They bear uncompensated residual climate costsâincluding loss and damageâwhile adaptation finance often flows to middle-income countries with greater implementation capacity.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Climate scientists, activists, and international lawyers who argue for rapid emissions reduction. They are formally present in UNFCCC processes but structurally sidelined by the infeasibility framing that prioritizes adaptation in finance allocation and political discourse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, incumbent_economic_actors).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides immediate protection and resilience infrastructure to present vulnerable populations already experiencing climate harms, bypassing political deadlock on emissions reduction.
% TRANSFER_FUNCTION: Moves climate finance and political capital from mitigation pathways to adaptation programs, transferring residual climate risks and uncompensated damages to future generations and low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations cannot speak in present policy fora; mitigation advocates and small island delegations are nominally present but structurally overridden by finance allocation and infeasibility framing.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished, climate finance and political capital would reallocate toward mitigation and emissions reduction; present vulnerable populations would lose adaptation support unless mitigation happened rapidly enough to prevent impacts; the political economy of fossil fuel incumbency would face stronger transition pressure.
% FOUNDING_PROBLEM: Political deadlock preventing adequate emissions mitigation combined with present vulnerable populations already suffering climate harms requiring immediate protection.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II and international development organizations attest to the urgency of present climate harms from outside the beneficiary set. However, the claim that mitigation is politically infeasibleâthe core premise justifying the priority shiftâis primarily attested by incumbent economic actors and governments benefiting from the status quo, with weak independent corroboration. Climate justice movements outside the beneficiary set contest the founding problem status, arguing that rapid mitigation remains feasible.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint locks in a warming trajectory that imposes large uncompensated costs on non-present, non-powerful agents. Suppression (0.60) reflects the active political marginalization of mitigation advocates and the narrative framing of emissions reduction as infeasible. Theater_ratio (0.52) indicates that a growing share of adaptation activity serves performative functionsâclimate action theater that obscures the lack of mitigationâwhile still delivering some genuine resilience. Accessibility_collapse (0.52) captures that alternatives (rapid mitigation) are technically available but politically collapsed through the infeasibility framing. Resistance (0.55) reflects sustained pushback from climate justice movements and vulnerable nations. The temporal series shows rising extraction and theater as the mitigation window narrows and warming commitments harden.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (climate policy authorities) and beneficiary seats (present vulnerable populations, incumbents) experience the constraint as necessary coordination responding to political reality and immediate suffering. The payer seats (future generations, low-adaptation-capacity regions) experience it as structural extractionâan intergenerational and geopolitical risk transfer. The engine will compute this divergence: low directionality for beneficiaries and agenda-setters, high directionality for trapped temporal and spatial victims. Mitigation_advocates, though present-era actors, compute as high-d because their exclusion is the enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (present_vulnerable_populations, incumbent_economic_actors) sit at low directionality: the constraint subsidizes their present security or present profits. Victims (future_generations, low_adaptation_capacity_regions) sit at high directionality: they are the structural targets of the accepted warming trajectory. The climate_policy_authorities agenda-setter sits near the beneficiary end because they control the constraint and do not personally bear its costs. The mitigation_advocates excluded seat sits near the target end because the constraint's persistence depends on suppressing their preferred alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint risks misclassification as a rope if one focuses only on the genuine adaptation coordination, or as a snare if one treats all adaptation as cover. The tangled_rope classification is warranted because: (1) present vulnerable populations do receive genuine coordination (resilience infrastructure), satisfying the coordination-function test; (2) future generations and weak regions bear identifiable residual costs that are not internalized by beneficiaries, satisfying the asymmetric-extraction test; and (3) the constraint requires active political enforcement to maintain the prioritization against mitigation alternatives, satisfying the active-enforcement test. If the founding problem (present suffering plus mitigation deadlock) were resolvedâif mitigation became politically feasibleâthe constraint would become a scaffold or dissolve; instead, it persists as the deadlock is treated as permanent, suggesting drift toward piton if the coordination function atrophies further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_infeasibility_constructed,
    'Is the political and economic infeasibility of emissions mitigation a genuine structural constraint, or a narrative manufactured by incumbent economic actors to protect existing rents?',
    'Comparative political economy analysis of jurisdictions that have achieved rapid mitigation transitions, and investigation of incumbent lobbying expenditures against climate regulation.',
    'If manufactured, the constraint''s extraction from future generations is deliberate rent-seeking rather than pragmatic coordination, pushing classification toward snare. If genuine, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_infeasibility_constructed, empirical, 'Whether mitigation infeasibility is real or constructed').

omega_variable(
    residual_damage_quantification,
    'What share of climate damages under an adaptation-priority regime falls on future generations and low-adaptation-capacity regions that cannot be offset by resilience investments?',
    'Integrated assessment modeling comparing residual damages under adaptation-priority versus mitigation-priority pathways, with explicit regional and intergenerational accounting.',
    'Higher residual damages concentrated on excluded seats would raise extractiveness and confirm asymmetric extraction; lower damages would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_damage_quantification, empirical, 'Quantification of uncompensated residual damages on non-beneficiaries').

omega_variable(
    adaptation_finance_capture,
    'Does adaptation finance flow primarily to vulnerable populations, or is it captured by incumbent contractors and middle-income countries with greater access?',
    'Trace adaptation finance flows from multilateral funds to ultimate beneficiaries; audit project procurement and geographic allocation.',
    'If captured by incumbents, the beneficiary structure is misaligned and the coordination function is weaker than claimed, increasing extraction. If it reaches vulnerable populations, coordination is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_finance_capture, empirical, 'Whether adaptation finance reaches intended beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.27).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.34).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.41).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.47).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__adaptation_priority, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__adaptation_priority, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__adaptation_priority, suppression_requirement, 25, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% One reading of the climate_harm_prevention kernel. The adaptation_priority reading accepts a higher warming trajectory and prioritizes near-term resilience, while sibling readings differ on feasibility and growth assumptions. This decomposition follows the epsilon-invariance principle: the three readings have different beneficiary/victim structures and different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
