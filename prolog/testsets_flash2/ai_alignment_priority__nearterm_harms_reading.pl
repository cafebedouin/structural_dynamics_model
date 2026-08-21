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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment: Prioritizing Near-Term Harms and Justice for Marginalized Populations
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'AI Alignment' that
 *   prioritizes the prevention of present discriminatory and extractive harms
 *   from deployed AI systems, with a focus on justice for marginalized
 *   populations. It emphasizes sociotechnical audits and resource allocation
 *   to bias mitigation. This reading is one of several competing
 *   interpretations of AI alignment, each with different priorities and
 *   proposed solutions. The constraint is claimed as a Rope by its
 *   proponents, but its metrics reflect a Tangled Rope due to the active
 *   enforcement required to shift resources and the identifiable victims of
 *   current AI systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.7).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment: Prioritizing Near-Term Harms and Justice for Marginalized Populations").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '35349a69-b494-4c25-acaf-9f319fd95779').
narrative_ontology:cs_kernel_codification('35349a69-b494-4c25-acaf-9f319fd95779', distributed).
narrative_ontology:cs_authority_grounding('35349a69-b494-4c25-acaf-9f319fd95779', practice).
narrative_ontology:cs_interpretation_layer_present('35349a69-b494-4c25-acaf-9f319fd95779').
narrative_ontology:cs_reading_relation('35349a69-b494-4c25-acaf-9f319fd95779', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('35349a69-b494-4c25-acaf-9f319fd95779', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('35349a69-b494-4c25-acaf-9f319fd95779', foundational, justice_is_primary_ai_alignment_goal).
narrative_ontology:cs_axiom_status(justice_is_primary_ai_alignment_goal, holdable).
narrative_ontology:cs_axiom_grounding('35349a69-b494-4c25-acaf-9f319fd95779', justice_is_primary_ai_alignment_goal, deontological).
narrative_ontology:cs_axiom('35349a69-b494-4c25-acaf-9f319fd95779', foundational, present_harms_are_tractable_and_urgent).
narrative_ontology:cs_axiom_status(present_harms_are_tractable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('35349a69-b494-4c25-acaf-9f319fd95779', present_harms_are_tractable_and_urgent, empirically_contingent).
narrative_ontology:cs_reference_frame('35349a69-b494-4c25-acaf-9f319fd95779', ai_for_social_good_framework).
narrative_ontology:cs_drift_state('35349a69-b494-4c25-acaf-9f319fd95779', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('35349a69-b494-4c25-acaf-9f319fd95779', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, sociotechnical_auditors).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_impacted_by_ai).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_system_developers_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct and indirect costs of discriminatory or extractive AI systems, experiencing harms related to age, race, disability, gender, or socioeconomic status. Their ability to opt-out of AI-driven systems (e.g., hiring, credit, policing) is severely limited.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_impacted_by_ai, payer,
    powerless, immediate, trapped, global).

% Are responsible for designing, developing, and deploying AI systems. They face pressure to integrate fairness and bias mitigation, but also have incentives to prioritize rapid deployment and profit, sometimes at the expense of thorough harm assessment. Compliance with near-term harm mitigation often requires significant resource allocation and process changes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_system_developers_deployers, agenda_setter,
    institutional, biographical, constrained, global).

% Benefit from the focus on mitigating immediate harms, as resources are directed towards identifying and rectifying discriminatory outcomes in deployed AI. This includes advocacy groups and communities actively working to address AI bias.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, beneficiary,
    organized, biographical, constrained, global).

% Their expertise in identifying and measuring AI harms, particularly those affecting marginalized groups, is central to this alignment approach. They receive funding and influence as their methodologies become standard for assessing AI systems.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, sociotechnical_auditors, beneficiary,
    moderate, biographical, mobile, global).

% Their work on fairness, accountability, and transparency in AI is directly supported and prioritized by this alignment framework. They contribute to the methodologies and theoretical underpinnings for identifying and mitigating near-term harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_ethics_researchers, beneficiary,
    moderate, generational, mobile, global).

% Are largely excluded from the primary resource allocation and policy focus of this reading, as their concerns about long-term, catastrophic AI risks are deemed secondary or distracting from immediate justice issues. They would argue for a different prioritization of alignment resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_advocates, excluded,
    powerful, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts across AI developers, policymakers, and affected communities to identify, measure, and mitigate present-day discriminatory and extractive harms from deployed AI systems, ensuring resources are directed towards justice for marginalized populations.
% TRANSFER_FUNCTION: Transfers resources (funding, research focus, regulatory attention) from general AI development and long-term speculative risks towards sociotechnical audits, bias mitigation, and redress mechanisms for marginalized populations impacted by AI.
% ABSENT_VOICES: Advocates for existential AI risk are often sidelined, arguing that focusing solely on near-term harms distracts from more fundamental, long-term threats to humanity. Their concerns are not central to the policy and research agenda of this reading.
% DISAPPEARANCE_RATIONALE: If this alignment priority vanished, the focus on near-term harms would dissipate, resources for bias mitigation and justice for marginalized populations would diminish, and AI systems would likely continue to perpetuate and exacerbate existing societal inequalities without dedicated intervention.
% FOUNDING_PROBLEM: Deployed AI systems were found to be perpetuating and amplifying existing societal biases and discrimination, leading to concrete harms for marginalized communities in areas like hiring, credit, and criminal justice.
% FOUNDING_PROBLEM_CORROBORATION: Numerous independent academic studies, investigative journalism reports, and testimony from civil rights organizations and affected communities consistently corroborate the ongoing nature and severity of these harms, providing strong evidence from outside the direct beneficiaries of this alignment approach.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) reflects the significant costs borne by marginalized populations due to AI harms, and the 'extraction' of resources and attention from other AI concerns. Suppression (0.6) is moderate, as there is active resistance from affected communities, but also significant institutional inertia and power imbalances that suppress effective redress. Theater ratio (0.2) is low, indicating that efforts are genuinely directed towards harm mitigation, though some performative 'ethics washing' may occur. The metrics reflect the ongoing struggle to re-prioritize AI development towards justice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized populations, this constraint is a necessary but often insufficient effort to mitigate ongoing harms, experiencing it as a Snare or Tangled Rope. From the perspective of sociotechnical auditors and AI ethics researchers, it is a crucial Rope or Scaffold, enabling their work and directing resources effectively. AI developers may experience it as a Tangled Rope, balancing compliance costs with market pressures. The engine's classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are the primary victims (high d), bearing the brunt of AI harms. AI system developers/deployers are agenda-setters (moderate d), facing pressure to comply but also benefiting from the existing system. Sociotechnical auditors and AI ethics researchers are beneficiaries (low d), as their work is prioritized and funded. Present vulnerable populations are also beneficiaries, as the focus is on their protection. Existential risk advocates are excluded, as their concerns are not the primary focus.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_efficacy,
    'Are the resources directed towards near-term harm mitigation actually effective in reducing harms for marginalized populations, or are they primarily absorbed by administrative overhead and performative compliance?',
    'Longitudinal studies tracking the actual reduction in AI-induced harms for specific marginalized groups, correlated with resource allocation to mitigation efforts.',
    'If resources are ineffective, the constraint''s true extractiveness (from marginalized groups) is higher, and its theater_ratio is underestimated, potentially reclassifying it closer to a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficacy, empirical, 'Effectiveness of harm mitigation resource allocation.').

omega_variable(
    scope_of_marginalized_populations,
    'How broadly is ''marginalized populations'' defined and applied in practice? Does it encompass all relevant groups (e.g., age, disability, linguistic minorities) or is it narrowly focused on a few prominent categories?',
    'Analysis of policy documents, funding allocations, and audit reports to determine the explicit and implicit scope of ''marginalized populations'' addressed by mitigation efforts.',
    'A narrow scope would mean the constraint''s coordination function is less effective and its victim set is larger than acknowledged, increasing its effective extractiveness for unaddressed groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_marginalized_populations, conceptual, 'Definition and practical application of ''marginalized populations''.').

omega_variable(
    near_vs_long_term_tradeoff,
    'Is the prioritization of near-term harms genuinely complementary to, or actively in tension with, efforts to address long-term catastrophic AI risks? Does resource allocation to one necessarily detract from the other?',
    'Empirical analysis of funding flows and research output across both domains, and qualitative assessment of policy debates for evidence of zero-sum framing or synergistic effects.',
    'If the tradeoff is zero-sum, this reading''s classification might shift towards a Snare from the perspective of existential risk advocates, as it actively suppresses their concerns. If synergistic, it reinforces its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_vs_long_term_tradeoff, empirical, 'Tradeoff dynamics between near-term and long-term AI alignment priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is the 'nearterm_harms_reading' of the 'ai_alignment_priority' kernel. It focuses on present harms and justice, distinct from the 'existential_risk_reading' (catastrophic risks) and 'integrated_reading' (both). Each reading instantiates a different constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
