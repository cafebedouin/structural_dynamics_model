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
 *   human_readable: AI Risk Prioritization: Near-Term Harms Focus
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of AI risk prioritization,
 *   focusing on immediate, measurable harms from deployed AI systems
 *   (discrimination, displacement, surveillance). It asserts that justice
 *   interventions are paramount. This reading directs resources and policy
 *   attention towards addressing these present harms, often at the expense of
 *   concerns about long-term or existential AI risks. The constraint is a
 *   'tangled rope' because it genuinely coordinates efforts to address real
 *   harms (a coordination function) but also involves asymmetric extraction
 *   by redirecting resources and suppressing alternative risk framings (the
 *   extractive function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Risk Prioritization: Near-Term Harms Focus").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '765995d5-fef2-43d1-b2b0-e9f50ba4d3ce').
narrative_ontology:cs_kernel_codification('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', distributed).
narrative_ontology:cs_authority_grounding('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', practice).
narrative_ontology:cs_interpretation_layer_present('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce').
narrative_ontology:cs_reading_relation('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', foundational, present_suffering_demands_priority).
narrative_ontology:cs_axiom_status(present_suffering_demands_priority, holdable).
narrative_ontology:cs_axiom_grounding('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', present_suffering_demands_priority, deontological).
narrative_ontology:cs_axiom('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', foundational, measurable_harms_are_actionable_risks).
narrative_ontology:cs_axiom_status(measurable_harms_are_actionable_risks, holdable).
narrative_ontology:cs_axiom_grounding('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', measurable_harms_are_actionable_risks, empirically_contingent).
narrative_ontology:cs_reference_frame('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', justice_oriented_risk_assessment).
narrative_ontology:cs_drift_state('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('765995d5-fef2-43d1-b2b0-e9f50ba4d3ce', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, civil_society_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_developers_deployers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, policymakers_prioritizing_x_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly experience the harms of discriminatory AI systems (e.g., biased facial recognition, predictive policing, hiring algorithms). This reading prioritizes interventions that directly address their present suffering and systemic disadvantages.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, local).

% Their research focuses on identifying, measuring, and mitigating near-term AI harms. This prioritization directs funding and policy attention to their areas of expertise, validating their work and increasing their influence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for policy interventions like bias audits, worker protections, and surveillance regulation. This reading aligns with their advocacy goals, empowering their efforts and directing resources towards their proposed solutions.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_society_advocates, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of implementing new regulations, conducting bias audits, and redesigning systems to mitigate discrimination and ensure transparency. They face increased scrutiny and potential legal liabilities under this prioritization.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_developers_deployers, payer,
    powerful, immediate, constrained, global).

% Are pressured to reallocate resources and attention from long-term, speculative AI risks (like AGI alignment) to immediate, tangible harms. This shift challenges their established agendas and funding streams.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policymakers_prioritizing_x_risk, payer,
    institutional, generational, constrained, global).

% Their work on AGI alignment and long-term safety is de-prioritized and framed as a distraction from urgent present issues. They find it harder to secure funding and influence policy under this dominant narrative.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts and resources towards addressing immediate, tangible harms caused by AI systems, ensuring that policy and research focus on measurable impacts on human populations.
% TRANSFER_FUNCTION: Transfers attention, funding, and regulatory pressure from speculative, long-term AI risks to concrete, near-term issues like algorithmic bias, job displacement, and surveillance, from AI developers/deployers and x-risk advocates to marginalized communities and justice advocates.
% ABSENT_VOICES: Researchers and policymakers focused on existential AI risk are often excluded from the primary discourse, their concerns framed as abstract or a diversion from real-world suffering. They would argue for a balanced approach or even a primary focus on preventing catastrophic future scenarios.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, resources would likely shift back towards long-term, speculative AI risks, potentially leaving immediate harms unaddressed and exacerbating existing inequalities. The policy landscape for AI governance would fundamentally reorient.
% FOUNDING_PROBLEM: The initial deployment of AI systems led to documented cases of algorithmic discrimination, job displacement, and increased surveillance, disproportionately affecting vulnerable populations, while much of the AI safety discourse focused on hypothetical future scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Numerous civil rights organizations, academic studies, and investigative journalists (outside the direct beneficiaries of this prioritization) consistently document ongoing near-term harms, corroborating the continued urgency of the founding problem.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) stems from the redirection of significant resources (funding, policy attention, research focus) away from other AI risk concerns, imposing costs on those who prioritize long-term risks. Suppression (0.70) is high because this reading actively frames alternative prioritizations (e.g., existential risk) as speculative, distracting, or even harmful, thereby suppressing their influence and legitimacy. The theater ratio is low (0.20) as the focus on near-term harms is largely genuine and driven by real-world impacts, not mere performance. Accessibility collapse (0.40) is moderate, as alternative framings still exist but face significant barriers to gaining traction. Resistance (0.75) is high, reflecting the ongoing and often intense debate between different AI risk prioritization camps.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and civil society advocates, this prioritization is a necessary 'rope' for collective action against injustice. From the perspective of existential risk researchers, it functions as a 'snare' that traps resources and attention, preventing focus on what they see as more critical, long-term threats. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities, fairness researchers, and civil society advocates are beneficiaries (d near 0.0) as this prioritization directly addresses their concerns and empowers their work. AI developers/deployers and policymakers prioritizing x-risk are payers (d near 1.0) as they bear the costs of compliance and the redirection of resources. Existential risk researchers are excluded, experiencing suppression of their agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled rope' prevents mislabeling it as a 'pure rope' (ignoring the suppression of alternative framings) or a 'pure snare' (ignoring the genuine coordination function around real, present harms). It highlights the dual nature where a legitimate coordination problem (addressing injustice) is intertwined with an extractive mechanism (resource redirection and suppression of competing priorities).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of resources (funding, talent, policy attention) under this prioritization optimal for mitigating the full spectrum of AI risks, considering both near-term harms and long-term catastrophic risks?',
    'Comprehensive, independent risk assessment models that quantify the expected value of mitigating different types of AI risks across various timescales, accounting for interdependencies and feedback loops.',
    'If the allocation is found to be suboptimal, it would suggest that the ''extractive'' component of this prioritization (redirecting resources) is less justified, potentially shifting the classification closer to a ''snare'' for those bearing the costs of redirection. If optimal, it would strengthen the ''coordination'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Assesses the efficiency of resource allocation under the near-term harms prioritization.').

omega_variable(
    suppression_legitimacy,
    'To what extent is the ''suppression'' of existential risk concerns a legitimate consequence of focusing on urgent present harms, versus an unjustified silencing of valid alternative perspectives?',
    'Analysis of the rhetorical strategies and institutional mechanisms used to de-legitimize x-risk concerns, alongside a philosophical evaluation of the moral weight of present suffering versus potential future catastrophe.',
    'If suppression is found to be largely unjustified, it would increase the perceived ''snare'' characteristics of this constraint, highlighting the coercive aspect. If deemed a necessary consequence of ethical prioritization, it would reinforce the ''tangled rope'' classification by emphasizing the coordination function''s moral grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_legitimacy, conceptual, 'Examines the ethical and rhetorical legitimacy of suppressing alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.63).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel, focusing on near-term harms. It is structurally distinct from the 'existential_risk_reading' which focuses on long-term threats, with different victim sets, resource allocations, and timescales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
