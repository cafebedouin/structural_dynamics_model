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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: AI Risk Prioritization: Near-Term Harms Reading
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI risk
 *   prioritization. It asserts that the primary focus of AI governance should
 *   be on measurable, present-day harms like discrimination, displacement,
 *   and surveillance, rather than speculative future risks. This reading
 *   drives resource allocation towards justice-oriented interventions and
 *   away from long-term alignment research. The constraint is framed as a
 *   Tangled Rope because it genuinely coordinates efforts to address real
 *   harms (beneficiaries: marginalized communities, fairness researchers) but
 *   also extracts resources and suppresses alternative risk framings
 *   (victims: AI developers, long-term safety advocates).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.75).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Risk Prioritization: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '5c313fbc-a264-4306-a589-cbddfad45ab5').
narrative_ontology:cs_kernel_codification('5c313fbc-a264-4306-a589-cbddfad45ab5', distributed).
narrative_ontology:cs_authority_grounding('5c313fbc-a264-4306-a589-cbddfad45ab5', practice).
narrative_ontology:cs_interpretation_layer_present('5c313fbc-a264-4306-a589-cbddfad45ab5').
narrative_ontology:cs_reading_relation('5c313fbc-a264-4306-a589-cbddfad45ab5', ai_risk_prioritization__existential_risk_reading, influences).
narrative_ontology:cs_axiom('5c313fbc-a264-4306-a589-cbddfad45ab5', foundational, present_suffering_is_paramount).
narrative_ontology:cs_axiom_status(present_suffering_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('5c313fbc-a264-4306-a589-cbddfad45ab5', present_suffering_is_paramount, deontological).
narrative_ontology:cs_axiom('5c313fbc-a264-4306-a589-cbddfad45ab5', foundational, measurable_harms_demand_immediate_action).
narrative_ontology:cs_axiom_status(measurable_harms_demand_immediate_action, holdable).
narrative_ontology:cs_axiom_grounding('5c313fbc-a264-4306-a589-cbddfad45ab5', measurable_harms_demand_immediate_action, empirically_contingent).
narrative_ontology:cs_reference_frame('5c313fbc-a264-4306-a589-cbddfad45ab5', justice_first_ai_governance).
narrative_ontology:cs_drift_state('5c313fbc-a264-4306-a589-cbddfad45ab5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c313fbc-a264-4306-a589-cbddfad45ab5', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_developers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_deployers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, long_term_ai_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly experience the harms of deployed AI systems (discrimination in hiring/lending, algorithmic surveillance, job displacement). This reading prioritizes interventions that directly address their present suffering and seek justice for past harms.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, local).

% Advocate for and develop methods to identify, measure, and mitigate near-term AI harms. Their work provides the intellectual and policy framework for this prioritization, influencing regulatory bodies and public discourse.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, agenda_setter,
    organized, biographical, constrained, national).

% Bear the costs of implementing bias audits, explainability requirements, and other regulatory burdens aimed at mitigating near-term harms. They face increased scrutiny and potential legal liability for discriminatory outcomes.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_developers, payer,
    powerful, biographical, constrained, global).

% Organizations that integrate AI systems into their operations (e.g., government agencies, corporations). They incur costs for compliance, system redesign, and public relations management related to near-term harm mitigation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_deployers, payer,
    institutional, biographical, constrained, national).

% Focus on existential risks from advanced AI, arguing that near-term harms, while real, distract from the more fundamental and catastrophic threat of misaligned AGI. This reading suppresses their agenda by diverting resources and attention to present-day issues.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, long_term_ai_safety_advocates, excluded,
    organized, civilizational, constrained, global).

% Responsible for drafting and implementing regulations. They are influenced by both near-term and long-term risk narratives, but this reading pushes them towards immediate legislative action on discrimination, privacy, and labor displacement.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate tangible, present-day harms caused by AI systems, ensuring that justice and equity are central to AI development and deployment.
% TRANSFER_FUNCTION: Transfers resources (funding, research attention, policy focus) from speculative, long-term AI risks to immediate, measurable harms affecting marginalized populations, from AI developers/deployers to affected communities and accountability researchers.
% ABSENT_VOICES: Advocates for long-term AI safety and existential risk mitigation are often marginalized in this discourse, their concerns framed as speculative or a distraction from urgent present injustices. They would argue for a more balanced or even primary focus on preventing catastrophic future scenarios.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the focus on immediate AI harms would dissipate, leading to less regulation, fewer audits, and a likely increase in discriminatory or exploitative AI deployments. Resources would shift towards other risk framings, potentially leaving marginalized communities more vulnerable.
% FOUNDING_PROBLEM: The rapid deployment of AI systems without adequate ethical oversight led to documented instances of algorithmic bias, job displacement, and increased surveillance, disproportionately affecting vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Numerous civil rights organizations, academic studies, and investigative journalists corroborate the ongoing nature and severity of these near-term harms, providing extensive empirical evidence from outside the immediate beneficiary group of researchers.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the significant costs imposed on AI developers and deployers for compliance, audits, and potential liability, as well as the opportunity cost for long-term safety research. Suppression (0.75) is high because this reading actively marginalizes and de-legitimizes the 'existential risk' narrative, framing it as a distraction. Theater ratio (0.20) is relatively low, as the interventions (bias audits, regulatory enforcement) are largely functional, though some performative aspects may exist in public relations. Accessibility collapse (0.40) is moderate, as alternative risk framings still exist but are less resourced. Resistance (0.80) is high, reflecting ongoing debates and lobbying from those who advocate for different prioritization schemes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and fairness researchers, this constraint is a necessary Rope or even a Scaffold, providing crucial support and coordination for justice. From the perspective of long-term AI safety advocates, it functions as a Snare, trapping resources and attention in a less critical problem while a catastrophic future risk grows unchecked. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities are full beneficiaries (d=0.0) as the constraint directly addresses their harms. Fairness and accountability researchers are also beneficiaries (d low) as their work is prioritized and funded. AI developers and deployers are targets (d high) as they bear the costs of compliance and regulation. Long-term AI safety advocates are also targets (d high) as their concerns are suppressed and resources diverted. Policy makers are agenda-setters, navigating these competing priorities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_impact,
    'Does the prioritization of near-term harms significantly impede progress on long-term AI safety, or are the two agendas largely orthogonal?',
    'Empirical analysis of funding flows, research output, and policy attention over time, comparing jurisdictions with different prioritization schemes.',
    'If significant impedance is found, the ''near-term harms'' reading''s suppressive effect on the ''existential risk'' reading is more severe than currently estimated, potentially reclassifying it as more extractive for long-term safety advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Assesses the actual impact of resource diversion on long-term AI safety efforts.').

omega_variable(
    causal_link_between_harms,
    'Are near-term harms (e.g., bias) causally linked to or predictive of long-term existential risks (e.g., misalignment), such that addressing one inherently contributes to addressing the other?',
    'Theoretical and empirical research establishing a robust causal chain or shared underlying mechanisms between different scales of AI risk.',
    'If a strong causal link is established, the ''suppression'' metric for long-term safety advocates would decrease, as addressing near-term harms would be seen as a necessary component of long-term safety, reducing the perceived trade-off.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_link_between_harms, conceptual, 'Examines whether different scales of AI risk are fundamentally interconnected or distinct.').

omega_variable(
    framing_under_determination,
    'Is the ''near-term harms'' framing the only defensible way to conceptualize AI risk, or are there alternative framings that would lead to different classifications?',
    'Analysis of philosophical arguments, public discourse, and policy proposals that offer alternative categorizations or prioritizations of AI risk.',
    'If alternative coherent framings exist that yield different classifications (e.g., a ''sociotechnical systems risk'' framing), it highlights the constructed nature of this constraint and the role of power in establishing dominant narratives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Documents the possibility of alternative conceptualizations of AI risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.69).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI risk prioritization' kernel. It focuses on near-term harms, while 'ai_risk_prioritization__existential_risk_reading' focuses on long-term risks. Both are distinct constraints arising from the same underlying societal debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
