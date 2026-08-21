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
 *   This constraint represents a specific reading of 'AI alignment' that
 *   prioritizes preventing present discriminatory and extractive harms from
 *   deployed AI systems, with a focus on justice for marginalized
 *   populations. It emphasizes sociotechnical audits, bias mitigation, and
 *   accountability for immediate, observable impacts. This reading is one of
 *   several competing interpretations of AI alignment, each with different
 *   priorities and resource allocations.
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
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '02aea124-be94-4fa2-b9f2-b60e0a1803c1').
narrative_ontology:cs_kernel_codification('02aea124-be94-4fa2-b9f2-b60e0a1803c1', distributed).
narrative_ontology:cs_authority_grounding('02aea124-be94-4fa2-b9f2-b60e0a1803c1', practice).
narrative_ontology:cs_interpretation_layer_present('02aea124-be94-4fa2-b9f2-b60e0a1803c1').
narrative_ontology:cs_reading_relation('02aea124-be94-4fa2-b9f2-b60e0a1803c1', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('02aea124-be94-4fa2-b9f2-b60e0a1803c1', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('02aea124-be94-4fa2-b9f2-b60e0a1803c1', foundational, justice_is_primary_ai_alignment_goal).
narrative_ontology:cs_axiom_status(justice_is_primary_ai_alignment_goal, holdable).
narrative_ontology:cs_axiom_grounding('02aea124-be94-4fa2-b9f2-b60e0a1803c1', justice_is_primary_ai_alignment_goal, deontological).
narrative_ontology:cs_axiom('02aea124-be94-4fa2-b9f2-b60e0a1803c1', foundational, present_harms_are_tractable_and_urgent).
narrative_ontology:cs_axiom_status(present_harms_are_tractable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('02aea124-be94-4fa2-b9f2-b60e0a1803c1', present_harms_are_tractable_and_urgent, empirically_contingent).
narrative_ontology:cs_reference_frame('02aea124-be94-4fa2-b9f2-b60e0a1803c1', ai_for_social_good_framework).
narrative_ontology:cs_drift_state('02aea124-be94-4fa2-b9f2-b60e0a1803c1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('02aea124-be94-4fa2-b9f2-b60e0a1803c1', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, sociotechnical_auditors).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_system_developers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, deploying_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These populations (e.g., based on race, age, disability, socioeconomic status) are disproportionately affected by discriminatory or extractive AI systems. This reading of alignment aims to protect them and ensure justice, directing resources towards mitigating harms they currently face.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Experts and organizations specializing in identifying and mitigating bias, discrimination, and other harms in deployed AI systems. They advocate for and implement methodologies to assess and improve AI fairness, often benefiting from increased demand for their services under this alignment priority.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, sociotechnical_auditors, agenda_setter,
    organized, biographical, mobile, global).

% Bear the costs of implementing bias mitigation, fairness audits, and redesigning systems to prevent discriminatory outcomes. They face increased regulatory scrutiny and potential legal liabilities if their systems cause harm, leading to higher development and compliance costs.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Organizations that deploy AI systems in real-world contexts (e.g., hiring, lending, healthcare). They incur costs for pre-deployment audits, ongoing monitoring, and potential remediation efforts to ensure their AI systems do not perpetuate or create harms for marginalized groups.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, deploying_organizations, payer,
    institutional, biographical, constrained, global).

% Focus on long-term, catastrophic risks from advanced AI. Under this near-term harms reading, their concerns are often deprioritized or seen as a distraction from immediate justice issues, leading to reduced funding or influence for their research agendas.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, constrained, global).

% Responsible for creating regulations and guidelines for AI development and deployment. This reading influences them to prioritize legislation focused on fairness, accountability, and transparency in AI, often through mandates for impact assessments and bias audits.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts across AI developers, deployers, and auditors to identify, measure, and mitigate present-day harms from AI systems, particularly those affecting marginalized populations, ensuring a more just and equitable application of AI.
% TRANSFER_FUNCTION: Transfers resources (funding, developer time, regulatory attention) from focusing on speculative future risks or general AI development towards immediate harm prevention, bias mitigation, and justice-oriented AI audits. It also transfers accountability for harms from affected individuals to AI developers and deployers.
% ABSENT_VOICES: Proponents of an 'AI existential risk first' approach are often marginalized in this discourse, arguing that focusing on near-term harms distracts from more fundamental, long-term threats to humanity. Their concerns are not directly addressed by this reading's primary focus.
% DISAPPEARANCE_RATIONALE: If this alignment priority vanished, the focus on present harms and justice for marginalized populations would diminish. Resources would likely shift to other priorities (e.g., efficiency, long-term risk), leading to an increase in unmitigated discriminatory or extractive AI systems and exacerbating existing inequalities.
% FOUNDING_PROBLEM: AI systems were being deployed with demonstrable biases and discriminatory impacts, particularly against marginalized groups, perpetuating and amplifying existing societal inequalities without adequate accountability or mitigation strategies.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and civil society organizations (e.g., AI Now Institute, Algorithmic Justice League) consistently document ongoing discriminatory and extractive harms from deployed AI systems, corroborating the live status of this problem from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.7) reflects the significant costs imposed on AI developers and deployers for compliance, audits, and redesigns, which are seen as necessary to prevent harms. Suppression (0.6) indicates the active enforcement mechanisms (e.g., regulations, legal liabilities, public pressure) required to ensure adherence to fairness and justice principles. The theater ratio (0.2) is relatively low, suggesting that efforts are genuinely directed towards mitigation rather than mere performance, though some performative 'ethics washing' may occur. Resistance (0.75) is high due to the significant costs and changes required from powerful industry actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable populations and sociotechnical auditors, this constraint is a necessary 'rope' or 'scaffold' for justice. From the perspective of AI developers and deployers, it can feel like a 'snare' due to the high costs and perceived restrictions on innovation. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are the primary beneficiaries (d=0.0) as the constraint aims to protect them. Sociotechnical auditors also benefit (d=0.1) from increased demand for their expertise. AI system developers and deploying organizations are the primary payers/targets (d=0.9) due to the compliance costs and restrictions. Existential risk researchers are excluded (d=0.8) as their priorities are sidelined.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively combats mandatrophy by re-centering AI alignment on a live, urgent problem (present harms) rather than allowing it to drift into abstract or speculative concerns. It ensures that the 'mandate' of alignment remains tied to demonstrable societal benefit and justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_tradeoff,
    'Does prioritizing near-term harms divert critical resources and talent away from addressing potential long-term, catastrophic AI risks, creating a different kind of systemic vulnerability?',
    'Empirical analysis of funding flows and research talent allocation across different AI safety subfields over time, correlated with perceived risk trajectories.',
    'If significant diversion occurs, this reading''s classification might need to account for an ''excluded beneficiary'' (long-term safety) whose interests are suppressed, potentially shifting it towards a more extractive classification for the broader AI safety community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoff, empirical, 'Tradeoff between near-term and long-term AI safety resource allocation.').

omega_variable(
    scope_of_marginalized_populations,
    'How broadly or narrowly are ''marginalized populations'' defined in practice, and does this definition inadvertently exclude other groups experiencing AI-induced harms?',
    'Analysis of regulatory frameworks, audit methodologies, and case law to determine the practical scope of protection and identify any systematically excluded groups.',
    'A narrow definition could reduce the effective ''beneficiary'' scope, potentially increasing the effective extractiveness for unaddressed victim groups. A broader definition might increase compliance costs for developers, intensifying the ''payer'' experience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_marginalized_populations, conceptual, 'Definition and practical scope of ''marginalized populations'' in AI harm mitigation.').

omega_variable(
    effectiveness_of_sociotechnical_audits,
    'Are current sociotechnical audit methodologies sufficiently robust and comprehensive to reliably detect and mitigate all significant discriminatory/extractive harms in complex AI systems?',
    'Longitudinal studies comparing audit findings with real-world harm incidents, and independent evaluations of audit methodology efficacy across diverse AI applications.',
    'If audits are found to be ineffective, the ''theater_ratio'' might increase, and the ''suppression'' of harms might be lower than perceived, potentially shifting the constraint towards a ''piton'' or ''snare'' if the coordination function becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_sociotechnical_audits, empirical, 'Efficacy of sociotechnical audits in preventing AI harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_alignment_priority' kernel. It focuses on near-term harms and justice, influencing related AI ethics and regulatory frameworks. Sibling readings (existential_risk_reading, integrated_reading) offer alternative priorities for AI alignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
