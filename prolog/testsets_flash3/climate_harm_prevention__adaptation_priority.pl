% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Adaptation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes a climate policy framework that prioritizes
 *   adaptation and resilience building in the near term, implicitly accepting
 *   a higher global warming trajectory due to the perceived political and
 *   economic infeasibility of aggressive mitigation. It is a reading of the
 *   'climate_harm_prevention' kernel, focusing on managing present harms
 *   rather than preventing future ones. While it provides immediate benefits
 *   to vulnerable populations, it imposes significant unmitigated costs on
 *   future generations and regions with limited adaptive capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation Priority").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'e6cb505b-6f53-45bb-a65c-5ec5e2508aae').
narrative_ontology:cs_kernel_codification('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', formalized).
narrative_ontology:cs_authority_grounding('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', extraction).
narrative_ontology:cs_interpretation_layer_present('e6cb505b-6f53-45bb-a65c-5ec5e2508aae').
narrative_ontology:cs_reading_relation('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', foundational, present_generation_wellbeing_priority).
narrative_ontology:cs_axiom_status(present_generation_wellbeing_priority, holdable).
narrative_ontology:cs_axiom_grounding('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', present_generation_wellbeing_priority, deontological).
narrative_ontology:cs_axiom('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', foundational, mitigation_politically_economically_infeasible).
narrative_ontology:cs_axiom_status(mitigation_politically_economically_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', mitigation_politically_economically_infeasible, empirically_contingent).
narrative_ontology:cs_reference_frame('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', pragmatic_adaptation_framework).
narrative_ontology:cs_drift_state('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e6cb505b-6f53-45bb-a65c-5ec5e2508aae', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, developed_nations_economic_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_feasibility_doctrine).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, economic_growth_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive immediate, tangible benefits from resilience projects (e.g., sea walls, early warning systems). Their immediate safety and livelihoods are improved, but they remain exposed to unmitigated long-term climate change impacts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from avoiding costly, disruptive mitigation efforts in the near term, allowing continued economic growth and resource consumption. They fund adaptation projects, which can also create new markets and industries.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, developed_nations_economic_sectors, beneficiary,
    institutional, biographical, mobile, global).

% Bear the long-term, unmitigated costs of higher warming trajectories, including increased frequency and intensity of extreme weather, ecosystem collapse, and resource scarcity. They have no voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Experience disproportionate harm from climate change due to limited resources and infrastructure for adaptation. They receive some adaptation aid but face existential threats from impacts that cannot be locally adapted to.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Provide the scientific basis for understanding climate change impacts and mitigation/adaptation strategies. Their warnings about long-term warming are acknowledged but often deprioritized in policy decisions based on political feasibility.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% Shape global climate policy, often balancing competing national interests and political realities. They advocate for adaptation funding while navigating the political difficulties of securing ambitious mitigation commitments.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, international_climate_negotiators, agenda_setter,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates immediate responses to climate impacts, directing resources to build resilience in vulnerable areas and manage unavoidable climate harms, thereby stabilizing societies in the face of ongoing change.
% TRANSFER_FUNCTION: Transfers resources (funding, technology, expertise) from developed nations to vulnerable populations for adaptation projects, while transferring the burden of unmitigated future climate harms to future generations and low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations and non-human species are entirely absent from the policy-making process, bearing the highest costs without representation. Radical mitigation advocates and degrowth proponents are marginalized as 'politically infeasible'.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the immediate focus on adaptation would collapse, leaving present vulnerable populations exposed. The political and economic calculus would shift dramatically, potentially forcing a more aggressive mitigation agenda, but also creating significant near-term disruption as existing adaptation funding streams ceased.
% FOUNDING_PROBLEM: The immediate and growing threat of climate impacts on vulnerable populations, coupled with the perceived political and economic infeasibility of rapid, deep decarbonization.
% FOUNDING_PROBLEM_CORROBORATION: International development agencies, disaster relief organizations, and many national governments attest to the live problem of immediate climate vulnerability. Economic analyses and political science studies from outside the direct beneficiaries corroborate the perceived infeasibility of rapid mitigation, though this is contested by other scientific and advocacy groups.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high because the policy choice effectively extracts future well-being from future generations and vulnerable regions to preserve present economic and political stability. Suppression (0.70) is also high, as it actively suppresses alternative, more mitigation-focused policies by framing them as 'infeasible' or 'too costly'. The theater ratio (0.20) is moderate; while adaptation efforts are genuinely functional, a portion of the discourse around 'feasibility' serves to deflect from more fundamental structural changes. The claimed type is 'tangled_rope' because it genuinely coordinates immediate harm reduction (benefiting present vulnerable populations) but does so through an asymmetric extraction from future generations.
 *
 * PERSPECTIVAL GAP:
 *   Present vulnerable populations experience this as a beneficial, life-saving intervention, while future generations and low-adaptation-capacity regions would experience it as a highly extractive and unjust imposition. Developed nations' economic sectors perceive it as a pragmatic, economically rational approach, whereas climate scientists often view it as a dangerous deferral of necessary action.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are beneficiaries due to immediate aid and resilience projects (low d). Developed nations' economic sectors are also beneficiaries, as they avoid disruptive mitigation costs (low d). Future generations and low-adaptation-capacity regions are clear victims, bearing the unmitigated long-term costs (high d). Climate scientists are observers, providing data but not directly benefiting or paying. International climate negotiators are agenda-setters, balancing these competing interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (coordination) by highlighting the significant, asymmetric extraction from future generations. It also avoids mislabeling it as a 'snare' by acknowledging the genuine, immediate coordination function of adaptation for present populations. The 'tangled_rope' classification captures the hybrid nature, where a real coordination problem is solved through a structure that simultaneously extracts from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_economic_infeasibility_ambiguity,
    'Is the ''political/economic infeasibility'' of aggressive mitigation an objective constraint (mountain) or a constructed narrative serving present interests (snare)?',
    'Comparative analysis of policy outcomes in jurisdictions that have pursued more aggressive mitigation, or a shift in political will/economic models demonstrating alternative pathways.',
    'If constructed, the suppression metric would be higher, reflecting active suppression of alternatives, and the constraint would lean more towards a Snare. If objective, the Mountain aspect would be stronger, reducing the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economic_infeasibility_ambiguity, conceptual, 'Uncertainty regarding the true nature of mitigation ''infeasibility''.').

omega_variable(
    intergenerational_equity_framing,
    'How would the classification change if intergenerational equity were the primary normative frame, rather than present-day political economy?',
    'Adoption of legal frameworks granting standing to future generations or explicit intergenerational cost-benefit analysis in policy decisions.',
    'Under an intergenerational equity frame, the extractiveness and suppression would be perceived as significantly higher, and the constraint would likely be reclassified as a Snare, as the coordination benefits for the present would be outweighed by the costs imposed on the future.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, preference, 'Impact of shifting the primary ethical lens on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__adaptation_priority, theater_ratio, 2025, 0.17).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.19).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__adaptation_priority, theater_ratio, 2035, 0.2).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__adaptation_priority, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(clim_tr_t2045, climate_harm_prevention__adaptation_priority, theater_ratio, 2045, 0.2).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__adaptation_priority, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__adaptation_priority, base_extractiveness, 2025, 0.59).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.62).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__adaptation_priority, base_extractiveness, 2035, 0.65).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__adaptation_priority, base_extractiveness, 2040, 0.67).
narrative_ontology:measurement(clim_be_t2045, climate_harm_prevention__adaptation_priority, base_extractiveness, 2045, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__adaptation_priority, base_extractiveness, 2050, 0.69).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__adaptation_priority, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.66).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__adaptation_priority, suppression_requirement, 2035, 0.68).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__adaptation_priority, suppression_requirement, 2040, 0.69).
narrative_ontology:measurement(clim_su_t2045, climate_harm_prevention__adaptation_priority, suppression_requirement, 2045, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__adaptation_priority, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
