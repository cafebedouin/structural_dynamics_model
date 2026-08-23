% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The catastrophic-tail-dominant reading of acceptable risk in energy
 *   policy treats low-probability, high-consequence nuclear accidents as
 *   carrying infinite or near-infinite weight, while discounting the
 *   distributed, reversible harms of fossil fuel combustion (air pollution,
 *   climate change). This reading structures nuclear regulation (ALARA, LNT,
 *   defense-in-depth) to suppress the nuclear pathway even when
 *   expected-value analysis favors it. The constraint is a tangled rope: it
 *   coordinates around a genuine societal desire to avoid catastrophe, but
 *   extracts asymmetric costs from the nuclear industry, energy consumers,
 *   and climate-vulnerable populations, while benefiting fossil fuel
 *   incumbents and risk-averse policymakers. Active enforcement (licensing,
 *   inspections, legal challenges) is required to maintain the suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'decf2998-59f3-48fa-90c4-e0f3d6972773').
narrative_ontology:cs_kernel_codification('decf2998-59f3-48fa-90c4-e0f3d6972773', distributed).
narrative_ontology:cs_authority_grounding('decf2998-59f3-48fa-90c4-e0f3d6972773', expertise).
narrative_ontology:cs_interpretation_layer_present('decf2998-59f3-48fa-90c4-e0f3d6972773').
narrative_ontology:cs_reading_relation('decf2998-59f3-48fa-90c4-e0f3d6972773', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('decf2998-59f3-48fa-90c4-e0f3d6972773', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('decf2998-59f3-48fa-90c4-e0f3d6972773', foundational, catastrophic_risk_aversion_override).
narrative_ontology:cs_axiom_status(catastrophic_risk_aversion_override, holdable).
narrative_ontology:cs_axiom_grounding('decf2998-59f3-48fa-90c4-e0f3d6972773', catastrophic_risk_aversion_override, deontological).
narrative_ontology:cs_axiom('decf2998-59f3-48fa-90c4-e0f3d6972773', secondary, fossil_harm_reversibility_assumption).
narrative_ontology:cs_axiom_status(fossil_harm_reversibility_assumption, holdable).
narrative_ontology:cs_axiom_grounding('decf2998-59f3-48fa-90c4-e0f3d6972773', fossil_harm_reversibility_assumption, empirically_contingent).
narrative_ontology:cs_reference_frame('decf2998-59f3-48fa-90c4-e0f3d6972773', precautionary_principle_framework).
narrative_ontology:cs_drift_state('decf2998-59f3-48fa-90c4-e0f3d6972773', contemporary_climate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('decf2998-59f3-48fa-90c4-e0f3d6972773', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, general_public).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_policymakers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, catastrophic_risk_aversion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce radiation protection standards and licensing requirements that embody the catastrophic-tail-dominant logic. They justify stringent rules as necessary to prevent low-probability high-consequence accidents. Their authority derives from statutory mandate and technical expertise; they face political pressure but have high job security and institutional inertia.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced probability of nuclear catastrophe (radiation release, evacuation, long-term contamination). The protection is diffuse and probabilistic; individuals cannot opt out of the regulatory regime. They also bear indirect costs through higher electricity prices and continued fossil fuel pollution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, general_public, beneficiary,
    organized, biographical, constrained, national).

% Gain market share and policy protection when nuclear deployment is suppressed by tail-dominant regulation. They also pay for their own emissions regulations and face stranded-asset risk. Their exit options include diversification into renewables and political lobbying to shape competing risk frameworks.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, payer).

% Bears the full cost of compliance with tail-dominant standards (design margins, emergency planning, waste disposal, insurance). These costs are structurally higher than for other energy sources per unit of output. Exit means abandoning sunk capital and specialized workforce; international diversification is possible but limited by harmonized regulatory regimes.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry, payer,
    organized, generational, constrained, global).

% Pay higher electricity prices due to nuclear's regulatory cost premium and the system-level cost of foregoing a low-carbon baseload option. They have limited ability to switch providers or generate their own power; demand response is marginal. The cost is regressive, falling disproportionately on low-income households.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers, payer,
    moderate, biographical, constrained, national).

% Suffer accelerated climate impacts because tail-dominant regulation slows nuclear deployment, extending fossil fuel use. They have no voice in the risk-assessment frameworks of major emitter nations and no exit from the resulting climate damages. Their situation is structural: the constraint operates at a scale they cannot influence.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Develop and debate the methodological foundations (probabilistic risk assessment, value of statistical life, discount rates, tail-risk metrics). They do not directly set policy but their models and testimony shape the regulatory agenda. Their professional standing depends on the legitimacy of the field; they can move between frameworks but the dominant paradigm constrains publication and funding.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_assessment_experts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legitimate decision rule for energy infrastructure that bounds worst-case outcomes, enabling political consensus around high-stakes technologies by offering a clear 'safety first' heuristic.
% TRANSFER_FUNCTION: Transfers economic surplus from nuclear developers and ratepayers (higher capital and operating costs, foregone low-carbon generation) to the general public (reduced catastrophic risk) and fossil fuel incumbents (preserved market share). The transfer is mediated by regulatory stringency that is not calibrated to expected harm.
% ABSENT_VOICES: Future generations who would benefit from deeper decarbonization enabled by nuclear; communities hosting fossil fuel extraction and combustion who bear localized pollution; engineers and scientists who argue for risk-informed rather than risk-averse regulation. They are excluded because the tail-dominant framing treats their concerns as secondary to the overriding imperative of catastrophe avoidance.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant constraint vanished overnight, nuclear licensing would shift to expected-value or risk-informed frameworks, deployment costs would drop sharply, and nuclear would likely expand its share of low-carbon generation. Fossil fuel use would decline faster, altering climate trajectories. The regulatory apparatus would lose its central organizing principle and be restructured.
% FOUNDING_PROBLEM: Early nuclear regulation (1950s-1970s) faced genuine uncertainty about reactor safety and public fear of radiation. The tail-dominant approach was adopted to bound worst-case consequences when probability distributions were poorly known, and to build public trust in a novel technology.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear historians (e.g., Perrow, Wellock) document the shift from deterministic to probabilistic safety goals, showing the founding problem was uncertainty, not a settled preference for tail dominance. Climate scientists and energy modelers (IPCC, IEA) attest that the founding problem is now substantially solved for light-water reactors, and the constraint persists as a barrier to decarbonization. The nuclear industry's own probabilistic risk assessments demonstrate that core damage frequencies are orders of magnitude below early estimates.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the large and growing gap between regulatory cost and marginal safety benefit. Suppression (0.75) is high because the constraint actively blocks nuclear deployment through licensing barriers, waste policy, and public opposition amplified by the regulatory frame. Theater ratio (0.42) is moderate: safety culture and training are real, but an increasing fraction of regulatory activity serves to maintain the tail-dominant frame rather than improve actual safety. Accessibility collapse (0.62) is significant: alternative risk frameworks (expected value, option value) are institutionally marginalized. Resistance (0.55) comes from nuclear advocates, climate hawks, and some regulators pushing for risk-informed regulation.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear regulator's seat, the constraint is a rope: it coordinates safety culture and provides a defensible standard. From the nuclear industry and energy consumer seats, it is a snare: extraction without proportional benefit. From the fossil fuel incumbent seat, it is a mountain: an immutable barrier to their main competitor. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (general_public, fossil_fuel_incumbents, risk_averse_policymakers) are declared because they receive net subsidy from the constraint: the public gets catastrophe avoidance, incumbents get market protection, policymakers get political cover. Victims (nuclear_industry, energy_consumers, climate_vulnerable_populations) bear the costs: compliance burden, higher prices, climate damages. Exit options differentiate the seats: nuclear_industry is constrained (high sunk costs), climate_vulnerable_populations are trapped (no voice, no exit), fossil_fuel_incumbents have arbitrage (can diversify).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early uncertainty about nuclear risk) is contested: experts agree it is largely resolved for existing designs, but the constraint persists and has expanded to new designs (SMRs, advanced reactors). The mandate has atrophied: the coordination function (bounding worst-case under uncertainty) has been replaced by an extraction function (maintaining regulatory stringency as an end in itself). The theater ratio rise tracks this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_weight_justification,
    'Does the infinite/near-infinite weight assigned to nuclear catastrophic tails reflect a genuine societal preference or a constructed regulatory artifact that benefits identifiable actors?',
    'Stated-preference surveys with realistic tradeoffs (e.g., "accept 0.1% higher electricity cost to reduce nuclear core damage frequency from 1e-4 to 1e-5 per reactor-year") compared to revealed-preference in energy markets and voting behavior.',
    'If constructed, the constraint is a snare masquerading as a rope; if genuine, it is a tangled rope with a coordination function that society explicitly endorses despite the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_weight_justification, preference, 'Origin of the tail-dominance weighting: social preference vs. regulatory capture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression of the nuclear pathway primarily structural (regulatory barriers, licensing delays) or internalized (industry self-censorship, investor risk aversion, public fear amplified by the regulatory frame)?',
    'Counterfactual analysis: if structural barriers were removed (e.g., risk-informed licensing), would deployment accelerate? Track investor and developer behavior in jurisdictions with different regulatory regimes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase the effective extraction for the nuclear_industry seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for nuclear pathway.').

omega_variable(
    fossil_fuel_beneficiary_intent,
    'Do fossil fuel incumbents actively lobby for tail-dominant nuclear regulation, or is their benefit an unintended consequence of a genuinely risk-averse public?',
    'Lobbying records, campaign finance, think-tank funding, and regulatory comment analysis tracing fossil fuel industry influence on nuclear safety standards and radiation protection guidelines.',
    'Active lobbying would establish the constraint as a snare with fossil fuel incumbents as covert agenda-setters; passive benefit would keep them as incidental beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_fuel_beneficiary_intent, empirical, 'Whether fossil fuel industry strategically supports tail-dominant regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.28).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.35).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.38).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.4).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the acceptable_risk_energy kernel. The catastrophic_tail_dominant reading assigns infinite weight to nuclear accident tails and suppresses nuclear deployment. The expected_value_dominant reading minimizes aggregate expected harm and favors nuclear. The option_value_preserving reading maintains multiple pathways. All three share the kernel but instantiate different constraints with distinct ε, beneficiary/victim structures, and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, powerful, 0.25).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, organized, 0.8).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, moderate, 0.75).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
