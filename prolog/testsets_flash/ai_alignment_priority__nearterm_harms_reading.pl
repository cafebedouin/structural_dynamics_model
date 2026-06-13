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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment: Prioritizing Near-Term Harms and Justice
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint defines AI alignment as primarily focused on preventing
 *   present discriminatory and extractive harms from deployed AI systems,
 *   with a specific priority on justice for marginalized populations. It
 *   mandates sociotechnical audits and bias mitigation, shifting resources
 *   towards these efforts. The constraint is framed as a 'tangled_rope'
 *   because it genuinely coordinates efforts to address real harms
 *   (benefiting marginalized groups) but also extracts resources and imposes
 *   compliance burdens on AI developers and deploying organizations, often
 *   through active enforcement and regulatory pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.65).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment: Prioritizing Near-Term Harms and Justice").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'b8d179e5-edc4-4902-a1c1-9533be8d6e88').
narrative_ontology:cs_kernel_codification('b8d179e5-edc4-4902-a1c1-9533be8d6e88', formalized).
narrative_ontology:cs_authority_grounding('b8d179e5-edc4-4902-a1c1-9533be8d6e88', lineage).
narrative_ontology:cs_interpretation_layer_present('b8d179e5-edc4-4902-a1c1-9533be8d6e88').
narrative_ontology:cs_reading_relation('b8d179e5-edc4-4902-a1c1-9533be8d6e88', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8d179e5-edc4-4902-a1c1-9533be8d6e88', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('b8d179e5-edc4-4902-a1c1-9533be8d6e88', foundational, justice_as_primary_ai_alignment_goal).
narrative_ontology:cs_axiom_status(justice_as_primary_ai_alignment_goal, holdable).
narrative_ontology:cs_axiom_grounding('b8d179e5-edc4-4902-a1c1-9533be8d6e88', justice_as_primary_ai_alignment_goal, deontological).
narrative_ontology:cs_axiom('b8d179e5-edc4-4902-a1c1-9533be8d6e88', foundational, present_harms_as_tractable_and_urgent).
narrative_ontology:cs_axiom_status(present_harms_as_tractable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('b8d179e5-edc4-4902-a1c1-9533be8d6e88', present_harms_as_tractable_and_urgent, empirically_contingent).
narrative_ontology:cs_reference_frame('b8d179e5-edc4-4902-a1c1-9533be8d6e88', human_centered_ai_ethics).
narrative_ontology:cs_drift_state('b8d179e5-edc4-4902-a1c1-9533be8d6e88', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b8d179e5-edc4-4902-a1c1-9533be8d6e88', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, human_rights_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_system_developers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, deploying_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the primary intended beneficiaries, as the constraint aims to prevent and mitigate harms (e.g., discrimination, exploitation) from AI systems that disproportionately affect them. Their 'exit' from these harms is often structural and difficult.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations, beneficiary,
    powerless, generational, trapped, global).

% Advocate for the rights of marginalized groups and benefit from policies that prioritize justice and harm prevention in AI. They contribute to shaping the constraint and monitor its enforcement.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear the costs of implementing bias mitigation, conducting sociotechnical audits, and redesigning systems to comply with harm prevention mandates. Their ability to deploy systems is constrained by adherence to these principles.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Are responsible for ensuring that AI systems they deploy do not cause discriminatory or extractive harms. They face legal, reputational, and financial risks for non-compliance, incurring significant costs for due diligence and oversight.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, deploying_organizations, payer,
    institutional, biographical, constrained, global).

% Are tasked with developing, implementing, and enforcing policies that operationalize this alignment priority. They set standards, conduct investigations, and impose penalties for non-compliance, acting as the primary enforcers of the constraint.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Focus on preventing catastrophic risks from advanced AI and argue that prioritizing near-term harms diverts critical resources from existential safety. They are often excluded from the primary discourse and resource allocation for this specific reading of alignment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts across AI developers, deploying organizations, and regulatory bodies to identify, prevent, and mitigate discriminatory and extractive harms from AI systems, ensuring a more just and equitable deployment of AI.
% TRANSFER_FUNCTION: Transfers resources (time, money, personnel) from AI developers and deploying organizations towards sociotechnical audits, bias mitigation research, and compliance efforts, ultimately aiming to transfer safety and justice to marginalized populations.
% ABSENT_VOICES: Researchers and advocates primarily focused on existential AI risks are often marginalized in this discourse, arguing that an exclusive focus on near-term harms neglects potentially catastrophic long-term threats. Their perspective is often not fully integrated into the policy-making and resource allocation for this specific alignment priority.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, AI systems would likely be deployed with less scrutiny for bias and harm, leading to an increase in discriminatory and extractive outcomes for marginalized populations. The current efforts towards ethical AI development would significantly diminish, and the landscape of AI governance would shift dramatically towards unchecked deployment.
% FOUNDING_PROBLEM: The widespread deployment of AI systems was observed to perpetuate and amplify existing societal biases and inequalities, leading to discriminatory outcomes in areas like hiring, lending, and criminal justice, disproportionately harming marginalized populations.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities and human rights organizations consistently corroborate the ongoing nature of AI-driven harms. Regulatory bodies and international organizations also acknowledge the problem, providing corroboration from outside the immediate beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).

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
 *   The extractiveness (0.65) reflects the significant costs imposed on AI developers and deploying organizations for audits, redesigns, and compliance, which are often passed on. Suppression (0.70) is high due to the active enforcement mechanisms (e.g., regulatory fines, public shaming, legal action) required to ensure compliance and prevent the deployment of harmful systems. The theater ratio (0.20) is relatively low, indicating that while some performative compliance exists, there's a genuine effort towards mitigation, driven by the explicit justice priority. Accessibility collapse (0.40) is moderate, as developers still have options but face significant hurdles, and resistance (0.55) is present from those bearing the compliance costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized populations and human rights advocates, this constraint is a necessary 'rope' for justice, coordinating efforts to protect vulnerable groups. From the perspective of AI system developers and deploying organizations, it can feel like a 'snare' due to the high compliance costs, potential for reputational damage, and the perceived burden of addressing complex societal issues through technical means.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are the primary beneficiaries (d=0.0-0.1) as the constraint aims to prevent harms directly affecting them. Human rights advocates also benefit by seeing their advocacy translated into policy (d=0.1-0.2). AI system developers and deploying organizations are the primary targets (d=0.8-1.0) as they bear the direct costs of compliance, audits, and potential penalties. Regulatory bodies act as agenda-setters, enforcing the constraint (d=0.4-0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling genuine efforts to address social harms as pure extraction by acknowledging the real coordination function of protecting vulnerable groups. However, it also guards against 'virtue signaling' by requiring active enforcement and measurable outcomes, preventing it from becoming a 'piton' of performative ethics without real impact. The 'tangled_rope' classification captures the dual nature of coordination and extraction inherent in regulating complex sociotechnical systems for justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine focus on near-term harms, or a rhetorical framing to divert resources from other alignment priorities?',
    'Resource allocation audits: track funding and personnel deployment to near-term bias mitigation vs. long-term safety research. If resources disproportionately flow to rhetorical rather than practical mitigation, reclassify as a form of performative compliance.',
    'If primarily rhetorical, the effective extractiveness from marginalized populations is higher due to unaddressed harms, and the constraint''s classification shifts towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''nearterm_harms_reading'' of the ''ai_alignment_priority'' kernel. Sibling readings (''existential_risk_reading'', ''integrated_reading'') would shift the victim/beneficiary sets and the primary focus of enforcement.').

omega_variable(
    sociotechnical_audit_efficacy,
    'Are current sociotechnical audit methodologies sufficiently robust to detect and mitigate all forms of discriminatory and extractive harms from deployed AI systems?',
    'Empirical validation of audit outcomes: track the long-term impact of audited systems on marginalized populations. If harms persist or new ones emerge post-audit, the efficacy is low.',
    'If audit efficacy is low, the constraint''s actual suppression of harms is lower than intended, and the ''theater_ratio'' might be higher, indicating performative rather than functional mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sociotechnical_audit_efficacy, empirical, 'Uncertainty regarding the effectiveness of current tools to achieve the stated goal of harm prevention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_ethics_guidelines_enforcement).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_bias_auditing_standards).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'ai_alignment_priority' constraint family, representing the 'nearterm_harms_reading'. It is linked to other readings of the same kernel, which focus on different aspects of AI alignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
