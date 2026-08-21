% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics/Justice Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents one reading of the broader 'AI alignment'
 *   kernel, specifically focusing on preventing the reproduction of social
 *   bias and present-day harm in AI systems. It prioritizes the immediate,
 *   demonstrable impacts on marginalized communities over more speculative,
 *   long-term risks. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates efforts to address a collective problem (bias) but
 *   also involves asymmetric extraction from those prioritizing other forms
 *   of alignment or raw performance. The metrics reflect a growing cost to
 *   those whose priorities are displaced, and increasing enforcement to
 *   maintain this specific focus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.4).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics/Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '02d0de4b-2747-4baf-b078-eefd682ed251').
narrative_ontology:cs_kernel_codification('02d0de4b-2747-4baf-b078-eefd682ed251', formalized).
narrative_ontology:cs_authority_grounding('02d0de4b-2747-4baf-b078-eefd682ed251', expertise).
narrative_ontology:cs_interpretation_layer_present('02d0de4b-2747-4baf-b078-eefd682ed251').
narrative_ontology:cs_reading_relation('02d0de4b-2747-4baf-b078-eefd682ed251', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('02d0de4b-2747-4baf-b078-eefd682ed251', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('02d0de4b-2747-4baf-b078-eefd682ed251', foundational, present_day_harm_priority).
narrative_ontology:cs_axiom_status(present_day_harm_priority, holdable).
narrative_ontology:cs_axiom_grounding('02d0de4b-2747-4baf-b078-eefd682ed251', present_day_harm_priority, deontological).
narrative_ontology:cs_axiom('02d0de4b-2747-4baf-b078-eefd682ed251', foundational, social_bias_amplification_is_unjust).
narrative_ontology:cs_axiom_status(social_bias_amplification_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('02d0de4b-2747-4baf-b078-eefd682ed251', social_bias_amplification_is_unjust, deontological).
narrative_ontology:cs_reference_frame('02d0de4b-2747-4baf-b078-eefd682ed251', ai_ethics_principles_framework).
narrative_ontology:cs_drift_state('02d0de4b-2747-4baf-b078-eefd682ed251', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('02d0de4b-2747-4baf-b078-eefd682ed251', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, social_justice_advocates).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_developers_prioritizing_performance).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_system_users).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_system_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities are the primary beneficiaries, as the constraint aims to prevent AI systems from reproducing and amplifying existing social biases and harms that disproportionately affect them. Their 'exit' from harm is dependent on the constraint's effective enforcement.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, global).

% These researchers define the scope of 'social bias' and 'present-day harm,' develop methodologies for detection and mitigation, and advocate for their integration into AI development. They gain influence and resources from this prioritization.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ethics_researchers, agenda_setter,
    organized, biographical, mobile, global).

% Advocate for the implementation and enforcement of this alignment reading, ensuring that AI development aligns with broader social justice goals. They gain legitimacy and a platform for their concerns.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, social_justice_advocates, agenda_setter,
    organized, generational, constrained, national).

% Bear the costs of implementing bias detection, mitigation, and fairness audits, which can slow down development, increase costs, and potentially reduce raw performance metrics. Their 'exit' is to develop less impactful, non-AI systems or operate in less regulated jurisdictions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_developers_prioritizing_performance, payer,
    powerful, immediate, constrained, global).

% Experience a diversion of resources, attention, and funding towards immediate bias and harm prevention, potentially at the expense of research into catastrophic, long-term AI control problems. Their 'exit' is to pursue funding outside mainstream AI alignment discourse.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, constrained, global).

% Benefit from AI systems that are less likely to perpetuate bias or cause direct harm. However, they may also experience slower innovation or higher costs if developers pass on compliance expenses.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_system_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_system_users, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development efforts to collectively address and mitigate the reproduction of social biases and present-day harms, ensuring a more equitable and just deployment of AI systems.
% TRANSFER_FUNCTION: Transfers resources (time, money, attention) from optimizing raw AI performance and long-term speculative risks towards immediate ethical auditing, bias mitigation, and fairness-focused development. It also transfers legitimacy and influence to ethics researchers and social justice advocates.
% ABSENT_VOICES: AI developers focused solely on rapid deployment and performance optimization, who would argue for less stringent ethical constraints to accelerate innovation. Also, some long-term safety advocates who believe immediate bias concerns distract from existential risks.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, AI development would likely revert to prioritizing performance and efficiency, potentially exacerbating existing social biases and harms. Marginalized communities would face increased risks, and the influence of ethics researchers would diminish.
% FOUNDING_PROBLEM: The historical and ongoing deployment of AI systems has demonstrated a tendency to embed and amplify existing social biases (e.g., in hiring, lending, policing), leading to discriminatory outcomes and present-day harm for vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities consistently corroborate the existence and persistence of AI-driven social bias and harm. This is widely acknowledged by independent researchers and civil society organizations, not just the direct beneficiaries.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it imposes significant costs on AI developers and diverts resources from other research areas, particularly long-term safety. Suppression (0.40) is moderate but growing, as this reading actively suppresses alternative alignment priorities in funding and discourse. Theater ratio (0.20) is low, indicating that efforts are largely genuine, though some 'ethics washing' may occur. Resistance (0.70) is high from those whose priorities are displaced. Accessibility collapse (0.30) is low, as alternative approaches to AI alignment still exist but face increasing pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and ethics advocates, this constraint is a necessary coordination mechanism to ensure justice. From the perspective of long-term safety researchers or performance-driven developers, it is an extractive mechanism that diverts crucial resources and attention from what they perceive as more critical problems. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities are the primary beneficiaries (d=0.0), as the constraint directly addresses harms they experience. Ethics researchers and social justice advocates are also beneficiaries (d low) as their agenda gains prominence. AI developers prioritizing performance and long-term safety researchers are the primary targets (d high), as they bear the costs of compliance and resource diversion. AI system users are mixed, benefiting from less biased systems but potentially paying higher costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_impact,
    'What is the precise impact of resource diversion from long-term safety research to immediate bias mitigation on the overall risk profile of advanced AI systems?',
    'Longitudinal studies tracking funding allocations, research output, and risk assessments across different alignment paradigms over several decades.',
    'If diversion significantly increases long-term catastrophic risk, the extractiveness from long-term safety researchers is more severe than currently estimated, potentially shifting the constraint''s classification for that seat towards a Snare. If not, the current extractiveness is justified as a necessary rebalancing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Uncertainty about the net effect of prioritizing present-day harms over future catastrophic risks.').

omega_variable(
    scope_of_harm_definition,
    'Is the definition of ''social bias'' and ''present-day harm'' sufficiently robust and inclusive, or does it inadvertently exclude certain forms of harm or marginalized groups?',
    'Ongoing participatory design and auditing processes involving a broader range of affected communities and interdisciplinary experts, coupled with independent impact assessments.',
    'If the definition is too narrow, the constraint''s claimed beneficiary set is incomplete, and its coordination function is less effective than claimed, potentially increasing its effective extractiveness from unaddressed victims. If robust, its legitimacy is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, conceptual, 'Ambiguity in the precise scope and definition of the harms being addressed by this alignment reading.').

omega_variable(
    mandatrophy_risk_of_ethics_washing,
    'To what extent is the commitment to ''ethics and justice'' becoming a performative exercise (''ethics washing'') rather than genuine, impactful change, particularly among large AI developers?',
    'Independent audits of AI development pipelines, public reporting on bias mitigation outcomes, and tracking of resource allocation to genuine ethical review versus public relations efforts.',
    'If ''ethics washing'' is prevalent, the theater_ratio is significantly higher than currently estimated, and the constraint''s effective extractiveness from marginalized communities (who receive less genuine benefit) is higher, pushing it closer to a Piton or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_risk_of_ethics_washing, empirical, 'Risk that the commitment becomes performative without substantive impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI alignment commitment' kernel. It focuses on social bias and present-day harm, influencing (and being influenced by) readings focused on safety control and integrated approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
