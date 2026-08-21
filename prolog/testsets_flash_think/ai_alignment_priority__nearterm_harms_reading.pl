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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: AI Alignment Mandate: Prioritizing Near-Term Harms
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the 'AI alignment
 *   priority' kernel, focusing on preventing present discriminatory and
 *   extractive harms from deployed AI systems, with a strong priority on
 *   justice for marginalized populations. It functions as a mandate or
 *   normative framework that coordinates efforts to mitigate these harms,
 *   while simultaneously imposing compliance costs on AI developers and
 *   deployers. The 'high ε on deployed system audits' mentioned in the prompt
 *   refers to the severity of the problem this constraint aims to address,
 *   not the intrinsic extractiveness of the mandate itself. The mandate
 *   itself is a Tangled Rope, coordinating efforts for beneficiaries
 *   (marginalized populations) but extracting compliance costs from payers
 *   (AI developers/deployers).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.55).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment Mandate: Prioritizing Near-Term Harms").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '781cfd80-30a8-4d19-9222-9d810df81287').
narrative_ontology:cs_kernel_codification('781cfd80-30a8-4d19-9222-9d810df81287', formalized).
narrative_ontology:cs_authority_grounding('781cfd80-30a8-4d19-9222-9d810df81287', expertise).
narrative_ontology:cs_interpretation_layer_present('781cfd80-30a8-4d19-9222-9d810df81287').
narrative_ontology:cs_reading_relation('781cfd80-30a8-4d19-9222-9d810df81287', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('781cfd80-30a8-4d19-9222-9d810df81287', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('781cfd80-30a8-4d19-9222-9d810df81287', foundational, justice_for_marginalized_populations_is_primary).
narrative_ontology:cs_axiom_status(justice_for_marginalized_populations_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('781cfd80-30a8-4d19-9222-9d810df81287', justice_for_marginalized_populations_is_primary, deontological).
narrative_ontology:cs_axiom('781cfd80-30a8-4d19-9222-9d810df81287', secondary, ai_harms_are_present_and_amplifying_existing_inequalities).
narrative_ontology:cs_axiom_status(ai_harms_are_present_and_amplifying_existing_inequalities, holdable).
narrative_ontology:cs_axiom_grounding('781cfd80-30a8-4d19-9222-9d810df81287', ai_harms_are_present_and_amplifying_existing_inequalities, empirically_contingent).
narrative_ontology:cs_reference_frame('781cfd80-30a8-4d19-9222-9d810df81287', human_rights_framework_for_ai).
narrative_ontology:cs_drift_state('781cfd80-30a8-4d19-9222-9d810df81287', contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('781cfd80-30a8-4d19-9222-9d810df81287', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, regulatory_bodies).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_developers_deployers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, legacy_ai_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary targets of discriminatory and extractive harms from deployed AI systems. This alignment mandate aims to protect them and ensure justice, reducing their exposure to harm and providing avenues for redress.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations, beneficiary,
    powerless, immediate, trapped, global).

% Their expertise in identifying and mitigating sociotechnical harms is prioritized and funded under this mandate. They contribute to auditing methodologies and policy recommendations, gaining influence and resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ethics_researchers, beneficiary,
    organized, biographical, mobile, global).

% Tasked with implementing and enforcing the mandate, they develop and apply standards for AI system audits, bias mitigation, and accountability. They gain authority and resources to oversee AI development and deployment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% They bear the costs of compliance, including investing in sociotechnical audits, bias detection tools, and redesigning systems to prevent harms. Their ability to deploy AI systems without extensive ethical review is suppressed.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_developers_deployers, payer,
    powerful, immediate, constrained, global).

% Existing AI systems that perpetuate harms are subject to remediation or decommissioning under this mandate. Their continued operation in their current form is constrained or suppressed.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, legacy_ai_systems, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__nearterm_harms_reading, legacy_ai_systems).

% Their primary concern for catastrophic, long-term AI risks is deprioritized by this mandate's focus on near-term harms. They are often excluded from the central policy discussions and resource allocation under this reading.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, mobile, global).

% They observe the debate, advocating for a broader approach that addresses both near-term harms and long-term risks. While their perspective is acknowledged, it is not the primary driver of this specific mandate.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, integrated_alignment_advocates, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts and resources across AI developers, researchers, and regulators to identify, mitigate, and prevent discriminatory and extractive harms from deployed AI systems, ensuring a focus on justice for marginalized populations.
% TRANSFER_FUNCTION: Transfers resources (funding, labor, attention) from AI developers/deployers (who must invest in audits, bias mitigation) and potentially from other AI safety priorities (e.g., existential risk) towards sociotechnical auditing, bias detection, and remediation efforts for marginalized populations.
% ABSENT_VOICES: AI systems themselves (cannot speak to their own harms), future generations (whose existential risks are deprioritized), and those advocating for a purely technical, non-sociotechnical approach to alignment.
% DISAPPEARANCE_RATIONALE: If this mandate vanished, the focus on near-term harms and justice for marginalized populations would diminish, leading to increased deployment of harmful AI systems without adequate mitigation, and a shift of resources to other priorities, exacerbating existing inequalities.
% FOUNDING_PROBLEM: Widespread evidence of AI systems perpetuating and amplifying existing societal biases, leading to discriminatory outcomes and exacerbating extraction from marginalized communities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimony from affected communities and civil society organizations consistently corroborate the ongoing nature of these harms, independent of the AI industry's self-assessments.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint's extractiveness (0.55) reflects the significant compliance costs borne by AI developers and deployers for audits, redesigns, and mitigation efforts. Suppression (0.7) is high because it actively constrains the alternative of deploying AI systems without rigorous ethical review and harm prevention. The theater ratio (0.3) indicates that while some efforts might be performative, a substantial portion of the activity is genuinely aimed at preventing harms. Accessibility collapse (0.6) reflects that while developers' options for unconstrained deployment are reduced, marginalized populations still face challenges in fully accessing justice or freedom from AI harms. Resistance (0.6) comes from both developers pushing back on compliance costs and advocates pushing for stronger enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized populations, this mandate is a crucial, albeit imperfect, mechanism for justice and protection. From the perspective of AI developers/deployers, it represents a significant regulatory burden and cost. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations, ethics researchers, and regulatory bodies are structural beneficiaries, gaining protection, influence, and authority from this mandate. AI developers/deployers and legacy AI systems are targets/payers, bearing the costs of compliance and remediation. Existential risk advocates are excluded, as their concerns are deprioritized by this reading. Integrated alignment advocates serve as observers, advocating for a broader approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_tradeoffs,
    'To what extent does the prioritization of near-term harms divert resources and attention from long-term or existential AI risks, and what are the net societal consequences of this allocation?',
    'Comprehensive, long-term impact assessments comparing outcomes in jurisdictions with different alignment priorities, alongside expert elicitation on the probability and severity of various AI risks.',
    'If the diversion of resources significantly increases long-term risks without adequately addressing near-term harms, the overall societal benefit of this reading could be negative, leading to a re-evaluation of its claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoffs, empirical, 'Tradeoffs in resource allocation between near-term and long-term AI alignment priorities.').

omega_variable(
    efficacy_of_sociotechnical_audits,
    'How effective are current sociotechnical auditing methodologies in genuinely preventing and mitigating discriminatory/extractive harms, rather than merely identifying them post-deployment or creating a compliance facade?',
    'Independent, longitudinal studies tracking the real-world impact of audited AI systems on marginalized populations, comparing outcomes before and after audit-driven interventions.',
    'If audits are largely ineffective or performative, the constraint''s ''theater_ratio'' would be higher, and its ''extractiveness'' (from developers for compliance costs) would be less justified, potentially reclassifying it closer to a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_sociotechnical_audits, empirical, 'Effectiveness of sociotechnical audits in preventing AI harms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative AI development paths (e.g., those without extensive harm mitigation) structural (regulatory barriers, market pressure) or internalized (developers self-censor due to ethical norms)?',
    'Post-regulatory change analysis: if developers continue to prioritize harm mitigation even after external pressures are reduced, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as developers carry the suppression with them. If purely structural, removing the external barriers would lead to rapid shifts in development practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for AI development practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ai_a_tr_t18, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(ai_a_tr_t30, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ai_a_be_t18, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(ai_a_be_t30, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(ai_a_su_t18, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(ai_a_su_t30, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_auditing_standards).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_alignment_priority' kernel, focusing on near-term harms. It is linked to sibling readings (existential_risk_reading, integrated_reading) which represent alternative or complementary priorities within AI alignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
