% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment as Near-Term Harm Prevention for Marginalized Populations
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the 'nearterm_harms_reading' of the broader
 *   'ai_alignment_priority' kernel. It defines AI alignment as the imperative
 *   to prevent and mitigate present discriminatory and extractive harms
 *   arising from deployed AI systems, with a primary focus on achieving
 *   justice for marginalized populations. This framing emphasizes
 *   sociotechnical audits, bias mitigation, and redress mechanisms,
 *   contrasting with other readings that prioritize long-term existential
 *   risks or attempt to integrate multiple concerns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.75).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.8).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment as Near-Term Harm Prevention for Marginalized Populations").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '3f2b9a02-b564-48dd-b3e6-f397147d1328').
narrative_ontology:cs_kernel_codification('3f2b9a02-b564-48dd-b3e6-f397147d1328', distributed).
narrative_ontology:cs_authority_grounding('3f2b9a02-b564-48dd-b3e6-f397147d1328', expertise).
narrative_ontology:cs_interpretation_layer_present('3f2b9a02-b564-48dd-b3e6-f397147d1328').
narrative_ontology:cs_reading_relation('3f2b9a02-b564-48dd-b3e6-f397147d1328', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f2b9a02-b564-48dd-b3e6-f397147d1328', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('3f2b9a02-b564-48dd-b3e6-f397147d1328', foundational, present_harms_are_primary_alignment_concern).
narrative_ontology:cs_axiom_status(present_harms_are_primary_alignment_concern, holdable).
narrative_ontology:cs_axiom_grounding('3f2b9a02-b564-48dd-b3e6-f397147d1328', present_harms_are_primary_alignment_concern, empirically_contingent).
narrative_ontology:cs_axiom('3f2b9a02-b564-48dd-b3e6-f397147d1328', foundational, justice_for_marginalized_populations_is_priority).
narrative_ontology:cs_axiom_status(justice_for_marginalized_populations_is_priority, holdable).
narrative_ontology:cs_axiom_grounding('3f2b9a02-b564-48dd-b3e6-f397147d1328', justice_for_marginalized_populations_is_priority, deontological).
narrative_ontology:cs_reference_frame('3f2b9a02-b564-48dd-b3e6-f397147d1328', justice_oriented_ai_ethics_framework).
narrative_ontology:cs_drift_state('3f2b9a02-b564-48dd-b3e6-f397147d1328', contemporary_ai_development_acceleration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3f2b9a02-b564-48dd-b3e6-f397147d1328', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, advocacy_organizations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_affected_by_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_affected_by_ai).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_system_developers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_system_deployers).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, algorithmic_bias_is_systemic).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, justice_is_a_primary_ai_governance_goal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drive the research agenda, develop methodologies for identifying and mitigating harms, and often receive funding and recognition for these efforts. They benefit from the prioritization of this alignment framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_ethics_researchers, agenda_setter,
    organized, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, ai_ethics_researchers, beneficiary).

% Represent the interests of marginalized populations, push for policy changes, and raise public awareness. They benefit from the focus and resources directed towards addressing near-term harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, advocacy_organizations, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, advocacy_organizations, beneficiary).

% Disproportionately bear the discriminatory and extractive harms from deployed AI systems (e.g., biased loan algorithms, facial recognition errors, unfair content moderation). They are the primary victims, but also the ultimate beneficiaries of successful mitigation efforts.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_affected_by_ai, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_affected_by_ai, beneficiary).

% Bear the costs of auditing, redesigning, and mitigating biases in their AI systems. While they have significant market power, they face increasing pressure from regulators and public opinion to address harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Organizations that implement and use AI systems in critical domains (e.g., healthcare, finance, criminal justice). They bear the costs of compliance, audits, and potential legal liabilities related to discriminatory outcomes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_system_deployers, payer,
    institutional, biographical, constrained, national).

% Develop and enforce regulations aimed at preventing AI harms, mandating audits, and ensuring accountability. They shape the institutional environment for this alignment reading.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulators_and_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Their primary concern is with long-term, catastrophic risks from advanced AI. While their work is important, their priorities are largely excluded from this specific alignment framing, which focuses on present harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_focused_researchers, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts across AI ethics researchers, advocacy organizations, and regulators to identify, measure, and mitigate discriminatory and extractive harms from currently deployed AI systems, ensuring a focus on justice for marginalized populations.
% TRANSFER_FUNCTION: Transfers resources (funding, labor, attention, and compliance costs) from AI system developers and deployers towards sociotechnical audits, bias mitigation strategies, redress mechanisms, and policy development for affected marginalized populations.
% ABSENT_VOICES: Researchers and organizations primarily focused on long-term existential risks from advanced AI are largely excluded from the priority-setting of this framing, as are those who deny the existence or severity of present AI harms.
% DISAPPEARANCE_RATIONALE: If this framing of AI alignment vanished, the dedicated resources, research, and policy efforts aimed at mitigating present discriminatory and extractive harms would significantly diminish. This would lead to an increase in unaddressed harms, further entrenchment of biased AI systems, and a severe setback for justice for marginalized populations, fundamentally reorganizing the landscape of AI governance and its societal impact.
% FOUNDING_PROBLEM: Deployed AI systems were found to perpetuate and amplify existing societal biases, leading to discriminatory outcomes and disproportionate harm for marginalized groups, with insufficient mechanisms for accountability or redress.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities and civil rights organizations consistently corroborate the ongoing nature and severity of these harms. These sources provide independent verification from outside the immediate beneficiaries of AI development.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the ongoing and often unaddressed harms borne by marginalized populations due to biased AI. Suppression (0.80) is high because the systemic nature of these harms, combined with the power imbalance between affected communities and AI developers/deployers, makes it difficult to resist or exit. The theater ratio (0.40) indicates that while genuine efforts are made, a significant portion of 'alignment' activity in this domain can be performative, lacking deep structural change. The claimed type is Tangled Rope because it genuinely coordinates efforts among advocates, researchers, and regulators to address these harms, but simultaneously involves asymmetric extraction from marginalized groups and requires active enforcement to shift resources and power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized populations, the constraint is highly extractive, as they continue to bear harms. From the perspective of AI ethics researchers and advocacy organizations, it is a necessary coordination mechanism to address these harms. The engine's per-seat classification will reflect this divergence, showing different effective extraction values for different stakeholders based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are the primary targets (victims) of the harms, placing them at the high end of directionality, though they are also the ultimate beneficiaries of this alignment framing's success. AI ethics researchers and advocacy organizations are beneficiaries and agenda-setters, driving the discourse and receiving resources for mitigation efforts. AI system developers and deployers are payers, bearing the costs of compliance and mitigation. Existential risk-focused researchers are structurally excluded from this specific framing's priority-setting.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_priority,
    'To what extent does the prioritization of near-term harms in AI alignment divert resources and attention from long-term existential risks, and vice-versa?',
    'Empirical analysis of funding flows, research publication trends, and policy agendas across different AI alignment communities over time.',
    'If resource diversion is significant, it suggests a zero-sum competition for attention and funding, potentially weakening efforts in one domain to strengthen another. If not, the framings may coexist with less direct competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_priority, empirical, 'Competition for resources between near-term and long-term AI alignment priorities.').

omega_variable(
    framing_legitimacy_contest,
    'Is the ''near-term harms'' framing of AI alignment sufficiently robust to address the full spectrum of AI risks, or does it risk being dismissed as a ''lesser'' concern by those focused on existential risks?',
    'Analysis of discourse patterns, policy adoption rates, and institutional influence of different alignment framings in major AI governance bodies and research institutions.',
    'If dismissed, this reading''s ability to secure resources and influence policy may be undermined, leading to less effective mitigation of present harms. If it gains equal legitimacy, it strengthens the overall AI governance landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_legitimacy_contest, conceptual, 'Legitimacy contest between different AI alignment framings.').

omega_variable(
    integrated_approach_viability,
    'Can the ''near-term harms'' reading be effectively integrated with ''existential risk'' concerns into a coherent and actionable ''integrated_reading'' without diluting either priority?',
    'Development and evaluation of governance frameworks and research programs that explicitly attempt to address both categories of risk simultaneously, assessing their practical efficacy and stakeholder buy-in.',
    'If integration is viable, it could lead to a more comprehensive and robust approach to AI alignment. If not, the separate framings may remain necessary, or one may dominate at the expense of the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integrated_approach_viability, empirical, 'Feasibility of integrating near-term and existential AI alignment concerns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2015, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ai_a_tr_t2017, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(ai_a_tr_t2019, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(ai_a_tr_t2021, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(ai_a_tr_t2023, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2023, 0.39).
narrative_ontology:measurement(ai_a_tr_t2025, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2015, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_a_be_t2017, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(ai_a_be_t2019, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(ai_a_be_t2021, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement(ai_a_be_t2023, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2023, 0.74).
narrative_ontology:measurement(ai_a_be_t2025, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2015, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(ai_a_su_t2017, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement(ai_a_su_t2019, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2019, 0.75).
narrative_ontology:measurement(ai_a_su_t2021, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2021, 0.78).
narrative_ontology:measurement(ai_a_su_t2023, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2023, 0.79).
narrative_ontology:measurement(ai_a_su_t2025, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_alignment_priority' kernel, focusing on present harms. It is linked to 'ai_alignment_priority__existential_risk_reading' and 'ai_alignment_priority__integrated_reading' as sibling framings of the same core concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
