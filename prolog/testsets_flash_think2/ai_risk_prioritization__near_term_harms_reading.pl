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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: AI Risk Prioritization: Near-Term Harms Reading
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of AI risk prioritization,
 *   asserting that the primary focus should be on immediate, measurable harms
 *   caused by deployed AI systems, such as discrimination, displacement, and
 *   surveillance. It advocates for justice interventions as paramount. This
 *   reading actively frames alternative prioritizations, particularly those
 *   focused on speculative existential risks, as distractions from urgent
 *   present harms. The constraint is modeled as a Tangled Rope because it
 *   coordinates efforts towards justice but does so by extracting resources
 *   and attention from those who would prefer other priorities, and by
 *   suppressing alternative risk framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.7).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.8).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Risk Prioritization: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, 'b305eedd-5ca9-4508-b90e-a1aeb95b0db8').
narrative_ontology:cs_kernel_codification('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', implicit).
narrative_ontology:cs_authority_grounding('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', practice).
narrative_ontology:cs_interpretation_layer_present('b305eedd-5ca9-4508-b90e-a1aeb95b0db8').
narrative_ontology:cs_reading_relation('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', ai_risk_prioritization__existential_risk_reading, influences).
narrative_ontology:cs_axiom('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', foundational, measurable_harms_demand_immediate_action).
narrative_ontology:cs_axiom_status(measurable_harms_demand_immediate_action, holdable).
narrative_ontology:cs_axiom_grounding('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', measurable_harms_demand_immediate_action, empirically_contingent).
narrative_ontology:cs_axiom('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', foundational, justice_is_primary_metric_for_ai_governance).
narrative_ontology:cs_axiom_status(justice_is_primary_metric_for_ai_governance, holdable).
narrative_ontology:cs_axiom_grounding('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', justice_is_primary_metric_for_ai_governance, deontological).
narrative_ontology:cs_reference_frame('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', justice_oriented_ai_governance).
narrative_ontology:cs_drift_state('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', contemporary_ai_governance_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b305eedd-5ca9-4508-b90e-a1aeb95b0db8', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, civil_society_organizations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_developers_companies).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly experience the harms (discrimination, displacement, surveillance) from deployed AI systems. They benefit from interventions that prioritize these harms and seek justice.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, local).

% Their work focuses on identifying, measuring, and proposing solutions for near-term AI harms. This prioritization validates their research agenda and directs resources towards their field.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for the rights of affected communities and push for regulatory and policy changes to address immediate AI harms. This prioritization aligns with their mission and empowers their advocacy.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_society_organizations, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of implementing justice interventions, conducting bias audits, and complying with new regulations. They may resist this prioritization due to increased development costs and slower deployment cycles.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_developers_companies, payer,
    institutional, biographical, constrained, global).

% Focus on long-term, speculative risks like misaligned AGI. This prioritization diverts attention and resources away from their research, framing their concerns as less urgent or even a distraction.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, payer,
    organized, generational, constrained, global).

% Are responsible for setting regulatory agendas and allocating public resources. They are influenced by competing risk framings and decide which priorities to enshrine in policy and law.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts and resources towards identifying, measuring, and mitigating immediate, measurable harms of AI systems, such as discrimination, displacement, and surveillance.
% TRANSFER_FUNCTION: Transfers resources (funding, regulatory attention, developer effort) from unchecked AI development and speculative long-term risk mitigation to immediate justice interventions, bias audits, and worker protections.
% ABSENT_VOICES: Those who benefit from unchecked AI development or who are solely focused on long-term speculative risks without acknowledging present harms are often marginalized in this discourse, as their concerns are framed as less urgent or distracting.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, resources and attention would likely revert to unchecked AI development or purely speculative long-term risks, leaving present harms unaddressed and marginalized communities unprotected. The landscape of AI governance and advocacy would fundamentally shift.
% FOUNDING_PROBLEM: AI systems were being developed and deployed without adequate consideration for their immediate social impacts, leading to measurable harms like discrimination, job displacement, and increased surveillance, particularly for vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities and civil rights organizations consistently corroborate the existence and persistence of these harms, providing strong evidence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because implementing justice interventions and regulatory compliance imposes significant costs on AI developers and companies. Suppression (0.8) is high due to the active rhetorical framing of existential risk as a 'speculative distraction,' which aims to delegitimize and divert resources from that agenda. The theater ratio is low (0.2) because the proposed interventions are intended to be real and impactful, not merely performative. Resistance is high (0.75) from those who benefit from the status quo of unchecked AI development or who advocate for other risk priorities. The temporal measurements reflect a growing intensity of this prioritization over time, as its advocates gain traction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and their advocates, this prioritization is a necessary and just coordination mechanism. From the perspective of AI developers or existential risk researchers, it may be seen as an extractive and suppressive force that misdirects resources or stifles innovation. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities, fairness/accountability researchers, and civil society organizations are the primary beneficiaries, as this prioritization directly addresses their concerns and empowers their work. AI developers/companies and existential risk researchers are the primary targets/payers, as they bear the costs of compliance or the diversion of resources/attention. Policy makers act as agenda-setters, mediating between competing framings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_proportionality,
    'Is the diversion of resources from existential risk research, implied by this prioritization, proportional to the urgency and scale of near-term harms?',
    'Comprehensive, independent cost-benefit analysis comparing the societal impact of near-term harms versus the probability-weighted impact of existential risks, and the efficacy of interventions for each.',
    'If disproportionate, the suppression of existential risk discourse would be reclassified as more purely extractive; if proportional, it would be seen as a necessary cost of effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_proportionality, empirical, 'Assesses whether resource reallocation is justified by comparative risk assessment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative risk framings structural (e.g., institutional funding biases) or internalized (e.g., cognitive patterns that dismiss long-term risks)?',
    'Post-policy trajectory: if suppression of x-risk discourse persists even after policy changes address near-term harms, it suggests internalized suppression. If it dissipates, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the dismissal of x-risk persists even without active external pressure. If structural, removing the external barriers would reduce effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.79).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel. Its sibling, 'existential_risk_reading', offers an alternative prioritization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
