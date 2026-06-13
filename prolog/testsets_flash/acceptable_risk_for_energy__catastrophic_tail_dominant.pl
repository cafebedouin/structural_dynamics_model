% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint describes a risk assessment framework in energy policy
 *   where the potential for low-probability, high-consequence events (e.g.,
 *   nuclear meltdowns, long-term waste storage) and their intergenerational
 *   burden are given disproportionate weight, overriding traditional
 *   expected-value optimization. This framing effectively constrains certain
 *   energy technologies, particularly nuclear power, by emphasizing
 *   irreversibility and the burden on future generations. It is a specific
 *   reading of the broader 'acceptable_risk_for_energy' kernel.
 *
 * KEY AGENTS:
 *   - environmental_advocacy_groups: Primary beneficiary (institutional/arbitrage) — benefits from the constraint's policy outcomes.
 *   - future_generations: Primary beneficiary (powerless/trapped) — abstract beneficiary of long-term risk aversion.
 *   - nuclear_energy_developers: Primary target (organized/constrained) — bears the costs of heightened regulatory hurdles and public opposition.
 *   - energy_consumers: Payer (moderate/constrained) — bears costs of higher energy prices or reduced energy options.
 *   - risk_analysts: Observer (analytical/analytical) — provides technical input but often finds their probabilistic framing suppressed.
 *   - regulatory_bodies: Agenda setter (institutional/constrained) — enforces the risk calculus, balancing public safety with energy needs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '9c36e297-5e91-437e-b9a5-5f1482d8c327').
narrative_ontology:cs_kernel_codification('9c36e297-5e91-437e-b9a5-5f1482d8c327', implicit).
narrative_ontology:cs_authority_grounding('9c36e297-5e91-437e-b9a5-5f1482d8c327', distributed).
narrative_ontology:cs_reading_relation('9c36e297-5e91-437e-b9a5-5f1482d8c327', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('9c36e297-5e91-437e-b9a5-5f1482d8c327', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('9c36e297-5e91-437e-b9a5-5f1482d8c327', foundational, irreversibility_outweighs_probability).
narrative_ontology:cs_axiom_status(irreversibility_outweighs_probability, holdable).
narrative_ontology:cs_axiom_grounding('9c36e297-5e91-437e-b9a5-5f1482d8c327', irreversibility_outweighs_probability, deontological).
narrative_ontology:cs_axiom('9c36e297-5e91-437e-b9a5-5f1482d8c327', foundational, intergenerational_burden_is_primary).
narrative_ontology:cs_axiom_status(intergenerational_burden_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('9c36e297-5e91-437e-b9a5-5f1482d8c327', intergenerational_burden_is_primary, deontological).
narrative_ontology:cs_reference_frame('9c36e297-5e91-437e-b9a5-5f1482d8c327', precautionary_principle_dominance).
narrative_ontology:cs_drift_state('9c36e297-5e91-437e-b9a5-5f1482d8c327', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c36e297-5e91-437e-b9a5-5f1482d8c327', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_developers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote policies that prioritize catastrophic tail risks and intergenerational equity, benefiting from the constraint's influence on energy policy. They leverage public concern and scientific findings to shape regulatory outcomes.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups, beneficiary,
    institutional, generational, arbitrage, global).

% Are abstract beneficiaries of policies that aim to prevent irreversible environmental damage or long-term burdens from energy choices made today. They have no direct voice or agency.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear significant costs due to the stringent regulatory requirements, public opposition, and financial risks associated with the catastrophic tail-risk framing. Their ability to deploy new technologies is severely constrained.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_developers, payer,
    organized, generational, constrained, national).

% May face higher energy costs or reduced energy choices if policies driven by this risk calculus limit the deployment of cost-effective, low-carbon energy sources. Their options are limited by the available energy mix.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_consumers, payer,
    moderate, biographical, constrained, national).

% Provide technical expertise on probabilistic risk assessment but often find their expected-value methodologies marginalized in policy debates dominated by catastrophic risk aversion. They can analyze the constraint but not directly change it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_analysts, observer,
    analytical, biographical, analytical, global).

% Are tasked with implementing energy and safety policies. They enforce the risk calculus, balancing public safety mandates (often influenced by catastrophic risk framing) with energy supply needs. They are subject to political and public pressure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and political action around the imperative to avoid low-probability, high-consequence events and to protect future generations from irreversible burdens, particularly in energy infrastructure decisions.
% TRANSFER_FUNCTION: Transfers the burden of extreme caution and long-term risk management onto specific energy technologies (e.g., nuclear) and their developers, and indirectly onto energy consumers, while transferring perceived safety and intergenerational equity benefits to the public and future generations.
% ABSENT_VOICES: Proponents of purely expected-value risk assessment and those advocating for a more aggressive deployment of low-carbon technologies (like nuclear) based on comparative risk are often marginalized or excluded from the core policy-setting discussions, as their framing is suppressed by the catastrophic tail-risk narrative.
% DISAPPEARANCE_RATIONALE: If this risk calculus vanished overnight, the policy landscape for energy would fundamentally shift. Nuclear power projects would likely become more viable, regulatory hurdles would decrease, and public discourse would re-center on cost-benefit analysis and comparative risk, leading to a significant reorganization of energy investment and deployment strategies.
% FOUNDING_PROBLEM: The problem of managing risks from technologies with potentially catastrophic but rare failure modes, and ensuring intergenerational equity for long-lived hazards like radioactive waste.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by environmental scientists, ethicists, and public safety advocates, who point to the ongoing challenges of nuclear waste disposal and the potential for severe, albeit rare, accidents. Industry groups acknowledge the problem but contest the severity and the appropriate risk weighting, arguing for a more balanced approach. The corroboration for the 'live' status comes from outside the direct beneficiaries, particularly from scientific bodies and ethical frameworks.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial, as it imposes significant costs on nuclear energy development and potentially on energy consumers through higher prices or reliance on less optimal sources. Suppression (0.78) is high due to the active exclusion of alternative risk assessment methodologies (like pure expected-value optimization) from policy discourse, often through public campaigns and regulatory capture. Theater ratio (0.20) is low, indicating that the constraint is genuinely enforced, not merely performative. Accessibility collapse (0.60) is moderate, as alternative energy sources exist, but the specific framing makes certain options (like nuclear) less accessible. Resistance (0.45) comes from industry and some economists, but is often outmatched by public and advocacy pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of environmental advocacy groups and those concerned with future generations, this constraint is a necessary safeguard (beneficiary seat). From the perspective of nuclear energy developers and some economists, it is an overly cautious and extractive barrier to a viable energy source (payer/victim seat). The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Environmental advocacy groups and future generations are beneficiaries (low d) as the constraint aligns with their goals of minimizing long-term, catastrophic risks. Nuclear energy developers and energy consumers are victims (high d) as they bear the direct and indirect costs of this risk calculus. Regulatory bodies act as agenda setters, enforcing the framework, and their directionality is complex, balancing public safety with economic development.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates public safety concerns and intergenerational equity (beneficiary side) while simultaneously extracting from specific industries and consumers through an asymmetric risk weighting (victim side). It requires active enforcement to maintain this specific risk calculus against competing economic and probabilistic arguments. It prevents mislabeling as a Snare by acknowledging the genuine coordination function around catastrophic risk aversion, but also prevents mislabeling as a Rope by highlighting the asymmetric extraction and suppression of alternative framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_catastrophic_tail_dominant,
    'Is this constraint a genuine reflection of irreducible risk, or a constructed framing that benefits specific advocacy groups?',
    'Long-term empirical data on actual catastrophic event probabilities and consequences, coupled with a re-evaluation of intergenerational equity principles.',
    'If genuinely irreducible, the constraint is a Mountain; if constructed, it is a Snare or Tangled Rope, with identifiable beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_catastrophic_tail_dominant, conceptual, 'This constraint is the ''catastrophic_tail_dominant'' reading of the ''acceptable_risk_for_energy'' kernel. It prioritizes low-probability, high-consequence events and intergenerational burden over expected-value optimization. Sibling readings (''expected_value_dominant'', ''comparative_risk_dominant'') would shift the victim set and suppression mechanisms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (regulatory barriers, legal challenges) or internalized (public fear, media narratives)?',
    'Post-policy-shift analysis: if public opposition persists after regulatory barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — public perception carries the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public risk perception.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'acceptable_risk_for_energy' kernel. Its high weighting of catastrophic tail risks and intergenerational burden differs significantly from 'expected_value_dominant' (which uses probability-weighted averages) and 'comparative_risk_dominant' (which assesses risks relative to alternatives). Each reading constitutes a distinct constraint with different extractiveness and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
