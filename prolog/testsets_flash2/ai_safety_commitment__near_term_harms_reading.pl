% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Preventing Near-Term Harms
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI safety,
 *   which prioritizes preventing documented present-day harms from deployed
 *   AI systems, such as bias, discrimination, labor exploitation, and
 *   misinformation. It is one reading of the broader 'ai_safety_commitment'
 *   kernel. This reading emphasizes accountability, transparency, and
 *   immediate regulatory action, contrasting with framings that focus on
 *   speculative future risks. The high extractiveness reflects the ongoing
 *   costs borne by affected communities and the resistance from industry to
 *   implement costly safeguards.
 *
 * KEY AGENTS:
 *   - marginalized_communities_and_workers: Primary beneficiaries (powerless/trapped) — bear existing harms, benefit from mitigation.
 *   - human_rights_advocates: Agenda-setters (organized/constrained) — champion this reading, push for regulation.
 *   - tech_companies_deploying_ai: Primary payers (institutional/constrained) — bear compliance costs, resist regulation.
 *   - existential_risk_researchers: Excluded (organized/identity_locked) — view this as a distraction from long-term risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Preventing Near-Term Harms").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '915e929c-83c3-4dde-8320-556227a149cd').
narrative_ontology:cs_kernel_codification('915e929c-83c3-4dde-8320-556227a149cd', distributed).
narrative_ontology:cs_authority_grounding('915e929c-83c3-4dde-8320-556227a149cd', practice).
narrative_ontology:cs_interpretation_layer_present('915e929c-83c3-4dde-8320-556227a149cd').
narrative_ontology:cs_reading_relation('915e929c-83c3-4dde-8320-556227a149cd', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('915e929c-83c3-4dde-8320-556227a149cd', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('915e929c-83c3-4dde-8320-556227a149cd', foundational, present_harms_are_primary_moral_imperative).
narrative_ontology:cs_axiom_status(present_harms_are_primary_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('915e929c-83c3-4dde-8320-556227a149cd', present_harms_are_primary_moral_imperative, deontological).
narrative_ontology:cs_axiom('915e929c-83c3-4dde-8320-556227a149cd', foundational, accountability_for_deployed_systems_is_feasible).
narrative_ontology:cs_axiom_status(accountability_for_deployed_systems_is_feasible, holdable).
narrative_ontology:cs_axiom_grounding('915e929c-83c3-4dde-8320-556227a149cd', accountability_for_deployed_systems_is_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('915e929c-83c3-4dde-8320-556227a149cd', human_rights_and_social_justice_framework).
narrative_ontology:cs_drift_state('915e929c-83c3-4dde-8320-556227a149cd', contemporary_ai_policy_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('915e929c-83c3-4dde-8320-556227a149cd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, marginalized_communities_and_workers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, human_rights_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, tech_companies_deploying_ai).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are disproportionately affected by algorithmic bias, discrimination, and labor exploitation. This reading of AI safety aims to protect them by demanding accountability and mitigation of present-day harms. Their 'benefit' is the reduction of extraction they currently face.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_communities_and_workers, beneficiary,
    powerless, immediate, trapped, global).

% Actively champion this reading of AI safety, pushing for regulations, audits, and legal frameworks that address documented harms. They organize resistance and provide a voice for affected communities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, human_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Bear the costs of compliance, auditing, and redesigning systems to mitigate bias and discrimination. They often resist this framing, preferring to focus on future, speculative risks that require less immediate, costly action.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies_deploying_ai, payer,
    institutional, biographical, constrained, global).

% Must integrate ethical design principles, conduct impact assessments, and address bias in their models, incurring development costs and potential delays. Their careers are tied to the industry, limiting their exit options.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_developers, payer,
    moderate, biographical, constrained, global).

% Are tasked with translating this reading into enforceable laws and standards. They face pressure from both advocates for stronger protections and industry for less burdensome regulation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators_and_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Focus on long-term, catastrophic risks from advanced AI. They view this near-term harms reading as a distraction from the 'real' problem, or as insufficient to address the scale of future threats. They are excluded from the immediate policy agenda set by this reading.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate concrete, observable harms from AI systems, ensuring that ethical considerations are integrated into development and deployment practices.
% TRANSFER_FUNCTION: Transfers resources (time, money, attention) from speculative, long-term AI risk research and unchecked AI deployment to immediate harm reduction, accountability mechanisms, and protective measures for affected populations.
% ABSENT_VOICES: Those focused exclusively on existential risk are often marginalized in this discourse, arguing that focusing on present harms diverts resources from preventing a greater, future catastrophe. Their concerns are not directly addressed by this reading's policy priorities.
% DISAPPEARANCE_RATIONALE: If this commitment to addressing near-term harms vanished, the existing harms (bias, discrimination, labor exploitation) would likely intensify without accountability, and the affected communities would face increased vulnerability. The regulatory landscape would shift dramatically towards less oversight of deployed systems.
% FOUNDING_PROBLEM: The rapid deployment of AI systems without adequate safeguards led to documented instances of algorithmic bias, discrimination, privacy violations, and exacerbation of social inequalities, creating immediate and tangible harm to individuals and communities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected individuals and advocacy groups (outside of the direct beneficiaries of this reading) consistently corroborate the ongoing nature and severity of these harms. International human rights organizations also attest to the problem's persistence.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.78) because the costs of unmitigated harms are substantial and borne by vulnerable populations, while the costs of mitigation are resisted by powerful industry players. Suppression (0.65) reflects the difficulty in holding powerful tech companies accountable and the systemic nature of algorithmic harms. Theater ratio (0.20) is relatively low, as this reading is focused on tangible, verifiable outcomes rather than performative gestures, though some 'ethics washing' by industry exists. The metrics reflect the ongoing struggle to enforce this reading against powerful interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of affected communities and human rights advocates, this is a critical, overdue coordination mechanism to protect fundamental rights. From the perspective of tech companies, it is an extractive regulatory burden that stifles innovation. The engine's per-seat classification will reflect these divergent experiences based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and workers are structural beneficiaries (d near 0.0) as the constraint aims to reduce harms they currently face. Tech companies and AI developers are targets (d near 1.0) as they bear the costs of compliance and regulation. Human rights advocates and regulators are agenda-setters, aiming to shift the burden of harm from victims to perpetrators, thus acting as beneficiaries of the constraint's function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading directly addresses a live problem (ongoing harms) and seeks to establish a functional coordination mechanism for mitigation. It prevents mislabeling genuine harm reduction as mere extraction by clearly identifying the victims and beneficiaries of the current state of affairs. The 'live' status of the founding problem and 'world_rearranges' verdict for disappearance indicate it is not a piton or a snare masquerading as coordination, but a contested effort to establish a necessary constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''ai_safety_commitment'' kernel, or merely a subset of a broader, dual-priority approach?',
    'Analysis of policy proposals and resource allocation: if policies derived from this reading consistently de-prioritize or actively oppose existential risk research funding, it confirms a distinct reading. If they are integrated into a larger framework, it suggests a subset.',
    'If a distinct reading, its classification stands. If a subset, its classification might be re-evaluated as a component of a ''dual_priority_reading'', potentially altering its extractiveness and suppression metrics based on the broader context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishes this reading from a mere component of a broader AI safety strategy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., lack of legal recourse, industry lobbying power) or internalized (e.g., affected communities'' lack of awareness or capacity to organize)?',
    'Post-intervention analysis: if legal reforms or funding for community organizing significantly reduce harms, it suggests structural suppression. If harms persist despite structural changes, internalized factors may be more dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as affected parties carry the suppression with them. If structural, targeted policy interventions are more likely to be effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in addressing AI harms.').

omega_variable(
    industry_resistance_sincerity,
    'Is industry''s resistance to this reading primarily due to genuine technical difficulty and cost, or is it a strategic maneuver to avoid accountability and maintain unchecked deployment?',
    'Comparative analysis of industry compliance in jurisdictions with strong vs. weak regulation: if compliance costs are consistently high even with good-faith efforts, it suggests genuine difficulty. If compliance is minimal where enforcement is weak, it suggests strategic resistance.',
    'If strategic, the ''payer'' role for tech companies is more extractive, and the constraint''s ''tangled_rope'' classification is more robust. If genuine, the extractiveness might be partially re-attributed to the inherent difficulty of the problem, rather than pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_resistance_sincerity, empirical, 'Assesses the true nature of industry''s resistance to near-term AI harm mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__near_term_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_safety_commitment' kernel, focusing on near-term harms. It coexists with 'existential_risk_reading' and 'dual_priority_reading', which represent alternative framings of AI safety.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
