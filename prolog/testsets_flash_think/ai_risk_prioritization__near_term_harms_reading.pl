% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Prioritization of AI Near-Term Harms
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of the broader
 *   'AI risk prioritization' kernel. It asserts that the primary risks from
 *   AI are immediate and measurable, manifesting as discrimination,
 *   displacement, and surveillance, particularly affecting marginalized
 *   populations. Consequently, justice-oriented interventions are deemed
 *   paramount. This reading directly contrasts with the
 *   'existential_risk_reading', which focuses on speculative, long-term
 *   threats. The constraint is classified as a Snare because the deployed
 *   systems actively extract from identifiable victims (marginalized
 *   communities) through their discriminatory and surveilling operations,
 *   with the 'coordination story' of general AI safety often serving as cover
 *   for these ongoing harms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.8).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.75).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, snare).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Prioritization of AI Near-Term Harms").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '3ee29441-5c8b-4054-b550-52e8e9ba858e').
narrative_ontology:cs_kernel_codification('3ee29441-5c8b-4054-b550-52e8e9ba858e', distributed).
narrative_ontology:cs_authority_grounding('3ee29441-5c8b-4054-b550-52e8e9ba858e', practice).
narrative_ontology:cs_interpretation_layer_present('3ee29441-5c8b-4054-b550-52e8e9ba858e').
narrative_ontology:cs_reading_relation('3ee29441-5c8b-4054-b550-52e8e9ba858e', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('3ee29441-5c8b-4054-b550-52e8e9ba858e', foundational, ai_harms_are_present_and_measurable).
narrative_ontology:cs_axiom_status(ai_harms_are_present_and_measurable, holdable).
narrative_ontology:cs_axiom_grounding('3ee29441-5c8b-4054-b550-52e8e9ba858e', ai_harms_are_present_and_measurable, empirically_contingent).
narrative_ontology:cs_axiom('3ee29441-5c8b-4054-b550-52e8e9ba858e', foundational, justice_requires_addressing_current_inequities).
narrative_ontology:cs_axiom_status(justice_requires_addressing_current_inequities, holdable).
narrative_ontology:cs_axiom_grounding('3ee29441-5c8b-4054-b550-52e8e9ba858e', justice_requires_addressing_current_inequities, deontological).
narrative_ontology:cs_reference_frame('3ee29441-5c8b-4054-b550-52e8e9ba858e', social_justice_framework).
narrative_ontology:cs_drift_state('3ee29441-5c8b-4054-b550-52e8e9ba858e', contemporary_ai_governance_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ee29441-5c8b-4054-b550-52e8e9ba858e', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_system_developers_deployers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, institutions_using_surveillance).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, civil_society_advocates).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__near_term_harms_reading, technological_determinism_critique).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__near_term_harms_reading, social_justice_advocacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy AI systems that, often unintentionally, perpetuate or amplify existing societal biases, leading to discriminatory outcomes. They benefit from rapid deployment and market dominance, often externalizing the costs of these harms onto others. They can shift focus or modify systems but face market pressure to deploy quickly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_system_developers_deployers, agenda_setter,
    institutional, biographical, mobile, global).

% Utilize AI-powered surveillance and decision-making systems (e.g., in policing, hiring, credit scoring) that can disproportionately target and disadvantage marginalized communities. They benefit from perceived efficiency or control, often resisting calls for transparency or accountability.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, institutions_using_surveillance, beneficiary,
    institutional, generational, constrained, national).

% Bear the direct and indirect costs of AI-driven discrimination, displacement, and surveillance. Their access to housing, employment, justice, and public services is often negatively impacted. They have limited individual exit options from these systems and rely on collective action and advocacy.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Conduct research to identify, measure, and document the near-term harms of AI systems, developing methods for bias detection and mitigation. They advocate for policy changes and greater accountability from developers and deployers.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, observer,
    organized, biographical, analytical, global).

% Organize and mobilize to resist harmful AI deployments, raise public awareness, and lobby for regulatory interventions. They bear the costs of sustained advocacy against powerful institutional actors.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_society_advocates, payer,
    organized, biographical, constrained, national).

% Focus on speculative, long-term risks from advanced AI, such as superintelligence misalignment. From the perspective of this 'near-term harms' reading, their concerns are often seen as a distraction from urgent, present-day injustices, leading to their marginalization in policy discussions focused on immediate impacts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, ai_system_developers_deployers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate immediate, tangible harms from AI systems, ensuring that resources and policy attention are directed towards present-day injustices rather than solely speculative future risks.
% TRANSFER_FUNCTION: Transfers the costs of unchecked AI deployment onto marginalized communities, while simultaneously transferring attention, funding, and policy focus towards addressing these present-day harms and away from purely speculative future risks.
% ABSENT_VOICES: Voices primarily concerned with speculative, long-term AI risks (e.g., existential risk advocates) are often structurally excluded or marginalized in discussions focused on near-term harms, as their framing is seen as diverting resources and attention from urgent present-day injustices.
% DISAPPEARANCE_RATIONALE: If the prioritization of near-term harms vanished overnight, the focus would shift predominantly to speculative future risks, leaving present harms unaddressed and potentially exacerbated. This would lead to significant social and economic reorganization for affected communities, likely worsening existing inequalities as AI systems continue to be deployed without adequate oversight for current impacts.
% FOUNDING_PROBLEM: Unchecked deployment of powerful technologies has historically led to disproportionate harm to vulnerable populations, and early AI systems already demonstrated measurable biases and discriminatory outcomes, necessitating a framework to address these immediate injustices.
% FOUNDING_PROBLEM_CORROBORATION: Social science research, investigative journalism, and direct testimony from affected communities consistently corroborate the ongoing nature and severity of these near-term harms. Independent reports from human rights organizations and some government agencies also support this assessment, providing corroboration from outside the direct beneficiaries of AI deployment.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the significant and measurable costs borne by marginalized communities due to biased algorithms and unchecked surveillance. Suppression (0.75) is high because the systems are often opaque, and efforts to resist or mitigate harms are met with technical barriers, institutional inertia, and the marginalization of alternative framings. The theater ratio (0.4) indicates that while some efforts towards 'ethical AI' or 'responsible innovation' exist, a substantial portion of activity may be performative, failing to address root causes of harm. The increasing trends in all metrics over the interval reflect the growing recognition of these harms and the intensifying struggle to address them.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of AI system developers and institutions, the constraint might be framed as a necessary cost of innovation or a complex technical challenge. However, from the perspective of marginalized communities, it is a clear mechanism of ongoing extraction and harm. The engine's computation of per-seat classifications will highlight this divergence, showing a Snare for victims and a more benign type (or even a subsidy) for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   AI system developers and deployers, along with institutions using surveillance, are structural beneficiaries (low directionality) as they profit from or gain control through the deployment of these systems, often externalizing costs. Marginalized communities are the primary targets (high directionality), bearing the direct costs of discrimination and surveillance. Fairness and accountability researchers, along with civil society advocates, act as payers (moderate directionality) as they expend significant resources to identify and resist these harms. Existential risk advocates are excluded, as their concerns are actively suppressed within this framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine prioritization of near-term harms, or is it primarily a rhetorical counter-framing to the existential risk narrative?',
    'Analysis of resource allocation: if funding and policy consistently flow to concrete interventions addressing present harms, it''s a genuine prioritization. If it primarily serves to dismiss other concerns without substantive action, it''s a rhetorical counter-framing.',
    'If a rhetorical counter-framing, its effective extractiveness (from marginalized communities) might be higher due to continued inaction, and its coordination function lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''ai_risk_prioritization'' kernel, specifically the ''near_term_harms_reading''. The core disagreement with the ''existential_risk_reading'' is the referent of ''AI risk'' (present vs. future) and the locus of responsibility (developers/deployers vs. abstract future AGI).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative risk framings structural (e.g., institutional funding priorities) or internalized (e.g., cognitive patterns among advocates)?',
    'Post-exit suppression trajectory: if the ''existential risk'' framing persists and gains traction after institutional barriers are removed, reclassify as partially internalized. If it remains marginalized, the suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making it harder for alternative framings to gain traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_r_tr_t18, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(ai_r_be_t18, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(ai_r_su_t18, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_regulatory_frameworks).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel, focusing on present-day harms. It is linked to the 'existential_risk_reading' as a competing framework for resource and policy allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
