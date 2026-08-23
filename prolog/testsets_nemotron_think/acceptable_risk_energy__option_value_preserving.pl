% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Acceptable Risk in Energy Pathway Selection
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The option-value-preserving reading of acceptable risk treats energy
 *   pathway diversity as a real options portfolio: under deep uncertainty
 *   (unknown probability distributions over future states), maintaining
 *   multiple viable pathways has intrinsic value beyond expected-cost
 *   minimization. This reading instantiates a specific constraint: planning
 *   standards and capacity mechanisms must keep nuclear, fossil-with-CCS, and
 *   renewables+storage all technically and economically viable, suppressing
 *   both anti-nuclear absolutism and unconstrained fossil expansion. The
 *   constraint is claimed as tangled_rope — it coordinates a genuine
 *   collective-action problem (no single actor can optimally hedge deep
 *   uncertainty) but extracts asymmetrically (ratepayers and host communities
 *   pay for capacity that may never run). The engine will compute per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Acceptable Risk in Energy Pathway Selection").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '61a44d15-df9e-43d0-90aa-6c159418f10c').
narrative_ontology:cs_kernel_codification('61a44d15-df9e-43d0-90aa-6c159418f10c', distributed).
narrative_ontology:cs_authority_grounding('61a44d15-df9e-43d0-90aa-6c159418f10c', practice).
narrative_ontology:cs_interpretation_layer_present('61a44d15-df9e-43d0-90aa-6c159418f10c').
narrative_ontology:cs_reading_relation('61a44d15-df9e-43d0-90aa-6c159418f10c', acceptable_risk_energy__catastrophic_tail_dominant, influences).
narrative_ontology:cs_reading_relation('61a44d15-df9e-43d0-90aa-6c159418f10c', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('61a44d15-df9e-43d0-90aa-6c159418f10c', foundational, option_value_dominates_expected_value_under_deep_uncertainty).
narrative_ontology:cs_axiom_status(option_value_dominates_expected_value_under_deep_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('61a44d15-df9e-43d0-90aa-6c159418f10c', option_value_dominates_expected_value_under_deep_uncertainty, empirically_contingent).
narrative_ontology:cs_axiom('61a44d15-df9e-43d0-90aa-6c159418f10c', secondary, irreversible_pathway_closure_warrants_insurance_premium).
narrative_ontology:cs_axiom_status(irreversible_pathway_closure_warrants_insurance_premium, holdable).
narrative_ontology:cs_axiom_grounding('61a44d15-df9e-43d0-90aa-6c159418f10c', irreversible_pathway_closure_warrants_insurance_premium, instrumental).
narrative_ontology:cs_reference_frame('61a44d15-df9e-43d0-90aa-6c159418f10c', post_oil_crisis_portfolio_planning).
narrative_ontology:cs_drift_state('61a44d15-df9e-43d0-90aa-6c159418f10c', post_renewables_storage_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('61a44d15-df9e-43d0-90aa-6c159418f10c', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_system_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, policy_makers_flexibility_faction).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_generations_under_uncertainty).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers_bearing_capacity_costs).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, communities_near_maintained_unneeded_infrastructure).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, clean_energy_innovators_crowded_out).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, decision_flexibility_has_intrinsic_value_under_deep_uncertainty).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, irreversibility_warrants_pathway_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the grid and long-term capacity planning. They design and enforce the requirement to maintain multiple generation pathways (nuclear, fossil with CCS, renewables+storage) as a hedge against deep uncertainty. They justify this as prudent risk management. They bear operational complexity costs but control the planning process and resource adequacy standards.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_system_operators, agenda_setter,
    institutional, generational, constrained, national).

% Legislators and regulators who champion 'technology-neutral' or 'all-of-the-above' energy policies. They gain political cover from preserving options — avoiding commitment to any single pathway that could prove wrong. They collect support from diverse energy constituencies. Their exit is mobile: they can shift emphasis as political winds change.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, policy_makers_flexibility_faction, beneficiary,
    organized, biographical, mobile, national).

% Electricity customers who pay for maintaining capacity that may rarely or never run (e.g., nuclear plants kept online as hedge, gas peakers maintained for reliability). Costs appear in rates and capacity payments. Exit is constrained: they cannot individually opt out of the grid, and collective action (municipalization, distributed generation) faces high barriers.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers_bearing_capacity_costs, payer,
    organized, biographical, constrained, regional).

% Host communities for power plants kept operational primarily as option-value hedges rather than economic merit. They bear local environmental and health externalities (air quality, water use, waste) for a plant whose system-level justification is flexibility, not utilization. Exit is trapped: relocation is economically infeasible and identity-locked to place.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, communities_near_maintained_unneeded_infrastructure, payer,
    moderate, generational, trapped, local).

% Developers of novel storage, advanced renewables, and grid integration technologies who find market entry slowed because capacity markets and planning reserves are partially committed to maintaining incumbent pathways as hedges. They pay opportunity cost in delayed deployment and capital access. Exit is constrained: they can pivot markets but the energy system is the primary arena.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, clean_energy_innovators_crowded_out, payer,
    moderate, biographical, constrained, national).

% Advocacy groups and policy actors who argue nuclear must be phased out immediately regardless of flexibility value. They are structurally excluded from the option-value framework because their premise (nuclear is categorically unacceptable) contradicts the core coordination function (maintain all viable pathways). They would object to nuclear's inclusion in the preserved set.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, anti_nuclear_absolutists, excluded,
    organized, biographical, constrained, national).

% Industry actors and political allies who argue for unrestricted fossil expansion and reject climate constraints. They are excluded because the option-value framework conditions fossil's inclusion on CCS-readiness and eventual phase-down — a constraint they reject. They would object to any suppression of fossil pathway expansion.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_maximalists, excluded,
    organized, biographical, constrained, national).

% Scholars of robust decision-making, info-gap theory, and real options analysis who evaluate whether preserving pathways genuinely improves outcomes under deep uncertainty versus merely increasing system cost. They neither collect nor pay; they assess the epistemic warrant for the coordination claim.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, decision_theory_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of energy planning under deep uncertainty: no single actor can bear the risk of committing to one pathway that may prove catastrophically wrong (e.g., renewables-only failing storage scaling, nuclear-only facing cost overruns, fossil-only facing climate liability). The constraint coordinates by mandating a diversified portfolio, preventing any faction from forcing premature closure of a pathway that might be needed.
% TRANSFER_FUNCTION: Moves capital and risk from ratepayers and host communities (who pay for maintaining underutilized capacity and bear local externalities) to the system as a whole (which gains resilience against unknown futures). The transfer is not a simple rent flow to a concentrated beneficiary — it is a diffuse insurance premium paid by identifiable parties for a collective option value.
% ABSENT_VOICES: Future generations who would inherit the consequences of premature pathway closure are not present to object or consent. Communities in the Global South facing energy poverty are excluded from the calculus — the option-value framework assumes a wealthy-system capacity to pay for hedges. Both would likely argue for different weightings of flexibility vs. immediate access.
% DISAPPEARANCE_RATIONALE: If the option-value preservation constraint vanished overnight, energy planning would collapse into single-pathway advocacy wars. One faction would force closure of nuclear; another would block fossil phase-down; a third would gamble entirely on renewables+storage. The system would lose its hedge against deep uncertainty, and the first major surprise (storage breakthrough failure, climate tipping point, nuclear cost collapse) would cause system-wide disruption. The world rearranges because arrangements depend on the hedge.
% FOUNDING_PROBLEM: Post-1970s energy crises and early climate awareness created a planning environment where no energy pathway had proven reliable at scale: nuclear faced cost and waste crises, fossil faced climate and geopolitical risk, renewables faced intermittency and storage gaps. The option-value framework emerged to prevent betting the energy system on any single unproven horse.
% FOUNDING_PROBLEM_CORROBORATION: Energy historians (e.g., Vaclav Smil, Daniel Yergin) document the genuine uncertainty of the 1970s-1990s period from outside the benefiting parties. However, contemporary analysts (e.g., IEA, IPCC WGIII) argue the founding problem has shifted: renewables+storage have substantially solved the intermittency gap, changing the option-value calculus. The benefiting parties (system operators, flexibility-faction policymakers) attest the problem remains live; independent analysts contest this.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects real but diffuse opportunity costs: maintaining hedging capacity costs ~10-15% above least-cost portfolios, borne by identifiable payers. Suppression (0.48) is moderate: the constraint actively excludes categorical exclusions (no-nuclear, no-fossil) but does not silence debate — advocates remain free to argue, just not to enforce closure. Theater ratio (0.28) captures that the coordination function is genuine (diversified portfolios demonstrably reduce worst-case regret) but a growing share of maintained capacity (esp. aging nuclear, unabated gas) may serve incumbency more than option value. The trajectory shows extractiveness and theater rising through 2005 then plateauing as renewables+storage matured — the hedge became less necessary but the constraint persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the operator's seat, the constraint is coordination: they see the genuine collective-action problem and the hedge working. From ratepayer and host community seats, it extracts: they pay for insurance they didn't choose and may not need. From clean innovator seats, it suppresses: their technologies face artificial barriers. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) asserts the coordination function is real and the extraction is asymmetric, which the metrics and stakeholders support.
 *
 * DIRECTIONALITY LOGIC:
 *   Energy system operators are agenda_setters with constrained exit (they cannot abandon planning responsibility) — they sit near symmetric (d~0.5), bearing complexity costs but controlling the process. Policy_makers_flexibility_faction are beneficiaries with mobile exit — they gain political cover, d~0.2. Ratepayers and host communities are payers with constrained/trapped exit — they bear concentrated costs, d~0.7-0.85. Clean energy innovators are payers with constrained exit — they face market distortion, d~0.65. Excluded groups (anti-nuclear, fossil maximalists) are structurally excluded because their premises contradict the coordination function; their exclusion is the enforcement object. Analytical observers sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s-1990s deep uncertainty across all pathways) has partially resolved: renewables+storage have substantially proven viability. Yet the constraint persists with near-identical scope. This is a mandatrophy signal — the mandate (preserve all pathways) outlives its original function (hedge against total pathway failure). However, the constraint is not yet a piton: the coordination function remains live for residual uncertainties (long-duration storage, grid integration at >80% renewables, climate tail risks). The theater ratio plateau at 0.28 suggests the performative share has stabilized but not dominated. The classification as tangled_rope (not piton) captures this: genuine coordination persists alongside asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the option-value-preserving reading of acceptable risk structurally differ from its sibling readings in the contested kernel?',
    'Compare the three readings'' beneficiary/victim sets, suppression targets, and coordination functions. This reading keeps both nuclear and fossil viable; catastrophic_tail_dominant suppresses nuclear; expected_value_dominant suppresses fossil. The victim sets differ: this reading''s victims bear option-premium costs; tail-dominant''s victims bear catastrophic risk; expected-value''s victims bear higher aggregate harm.',
    'If the readings are structurally distinct constraints (different ε, different victims, different suppression), they must be separate stories linked by network.affects_constraints. If they are merely interpretive variants of one constraint, the ε-invariance principle is violated. This omega documents the committer-frame decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committers structure: this is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    option_value_epistemic_warrant,
    'Does preserving pathway diversity under deep uncertainty genuinely reduce worst-case regret, or has the epistemic warrant decayed as renewables+storage resolved the founding uncertainty?',
    'Robust decision-making analysis comparing minmax-regret portfolios with and without pathway diversity constraints, using updated technology cost and performance distributions (post-2020). If the option value of maintaining nuclear/fossil pathways has fallen below their maintenance cost, the coordination function has atrophied.',
    'If option value has decayed below cost, the constraint shifts from tangled_rope toward piton (atrophied coordination, persistent extraction). If option value remains positive, tangled_rope holds. This is the central empirical question for classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_epistemic_warrant, empirical, 'Whether the coordination function''s epistemic warrant persists given technology learning curves.').

omega_variable(
    suppression_mechanism_asymmetry,
    'Is the moderate suppression of both extremes (anti-nuclear absolutism, fossil maximalism) symmetric in practice, or does the enforcement machinery disproportionately target one side?',
    'Track policy outcomes: count of nuclear plant closures blocked vs. fossil expansions blocked under the option-value framework; regulatory interventions citing ''fuel diversity'' or ''resource adequacy'' by technology type.',
    'If suppression is asymmetric (e.g., primarily blocks nuclear closures while allowing fossil retirements), the constraint''s extraction profile shifts — the coordination cover may favor one incumbent technology. This would alter victim/beneficiary structure and potentially reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_asymmetry, empirical, 'Whether suppression enforcement is symmetric across the two excluded extremes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 1975, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1975, acceptable_risk_energy__option_value_preserving, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(acce_tr_t1985, acceptable_risk_energy__option_value_preserving, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(acce_tr_t1995, acceptable_risk_energy__option_value_preserving, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_energy__option_value_preserving, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_energy__option_value_preserving, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_energy__option_value_preserving, theater_ratio, 2025, 0.28).
narrative_ontology:measurement(acce_tr_t2030, acceptable_risk_energy__option_value_preserving, theater_ratio, 2030, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t1975, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(acce_be_t1985, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement(acce_be_t1995, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement(acce_be_t2030, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2030, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1975, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(acce_su_t1985, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(acce_su_t1995, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2025, 0.48).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2030, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'acceptable risk in energy policy' into three structurally distinct readings with different ε, victims, and suppression targets. The option_value_preserving reading keeps all pathways viable (moderate extraction from ratepayers/communities); catastrophic_tail_dominant closes nuclear (extracts from nuclear communities/workers); expected_value_dominant closes fossil (extracts from fossil communities/workers). They share a kernel but are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, institutional, 0.45).
constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
