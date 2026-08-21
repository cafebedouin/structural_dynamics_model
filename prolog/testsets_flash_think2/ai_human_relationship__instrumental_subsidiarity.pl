% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool: Instrumental Subsidiarity Reading
 *   domain: catholic_social_teaching/technology_ethics/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'instrumental subsidiarity'
 *   reading of the broader 'AI-human relationship' kernel. It posits AI as a
 *   morally neutral tool whose ethical implications are determined by its
 *   human design, deployment, and governance. The constraint emphasizes the
 *   need for robust legal and ethical frameworks, guided by principles like
 *   subsidiarity, to ensure AI serves human dignity and the common good. The
 *   core tension lies in actively enforcing these frameworks against the
 *   rapid pace of technological development and competing visions of AI's
 *   role.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.6).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.7).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool: Instrumental Subsidiarity Reading").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "catholic_social_teaching/technology_ethics/political_theology").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '065ad503-724e-440b-b416-4584b5d99a66').
narrative_ontology:cs_kernel_codification('065ad503-724e-440b-b416-4584b5d99a66', formalized).
narrative_ontology:cs_authority_grounding('065ad503-724e-440b-b416-4584b5d99a66', lineage).
narrative_ontology:cs_interpretation_layer_present('065ad503-724e-440b-b416-4584b5d99a66').
narrative_ontology:cs_reading_relation('065ad503-724e-440b-b416-4584b5d99a66', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('065ad503-724e-440b-b416-4584b5d99a66', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('065ad503-724e-440b-b416-4584b5d99a66', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('065ad503-724e-440b-b416-4584b5d99a66', technology_is_morally_neutral, conventional).
narrative_ontology:cs_axiom('065ad503-724e-440b-b416-4584b5d99a66', foundational, human_responsibility_for_technology).
narrative_ontology:cs_axiom_status(human_responsibility_for_technology, holdable).
narrative_ontology:cs_axiom_grounding('065ad503-724e-440b-b416-4584b5d99a66', human_responsibility_for_technology, deontological).
narrative_ontology:cs_reference_frame('065ad503-724e-440b-b416-4584b5d99a66', human_centered_governance).
narrative_ontology:cs_drift_state('065ad503-724e-440b-b416-4584b5d99a66', contemporary_ai_acceleration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('065ad503-724e-440b-b416-4584b5d99a66', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, human_regulators).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ethical_framework_developers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_deployers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, individuals_harmed_by_unregulated_ai).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, catholic_social_teaching_principles).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, human_dignity_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, principle_of_subsidiarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and enforce legal and ethical frameworks for AI, aiming to ensure technology serves human ends. They gain legitimacy and authority from successfully governing AI, but are constrained by political will and lobbying from industry.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, human_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Academics, ethicists, and policy experts who develop the ethical principles and guidelines that inform AI regulation. Their work is validated and gains influence when adopted into formal frameworks, benefiting from the constraint's existence.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ethical_framework_developers, beneficiary,
    organized, biographical, mobile, global).

% Companies and researchers who design, build, and implement AI systems. They bear the costs of compliance with regulations (e.g., for transparency, safety, bias mitigation) and face restrictions on certain use-cases. Their exit options are limited by the need for market access and social license.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_deployers, payer,
    powerful, biographical, constrained, global).

% Individuals who suffer negative consequences (e.g., discrimination, job displacement, privacy violations) when AI systems operate without adequate ethical or legal safeguards. They are the ultimate targets of the constraint's failure to protect.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, individuals_harmed_by_unregulated_ai, payer,
    powerless, immediate, trapped, local).

% Advocates for efficiency maximization through AI, often viewing regulation as an impediment to progress. They are structurally excluded from the core premise of this reading, which prioritizes human ends over pure optimization, but exert significant lobbying pressure.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, technocratic_optimists, excluded,
    powerful, biographical, constrained, global).

% Proponents of a more holistic, integral human development approach to AI, emphasizing solidarity and the preferential option for the poor. While sharing some goals, they view this reading as potentially too instrumentalist and not sufficiently grounded in the irreducible dignity of the human person.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, incarnational_humanists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, human_regulators).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the development and deployment of AI technologies with established human ethical principles and legal frameworks, ensuring AI remains a tool serving human flourishing rather than an autonomous force.
% TRANSFER_FUNCTION: Transfers the primary responsibility for ethical outcomes from individual AI developers to collective regulatory bodies and ethical frameworks. It also transfers compliance costs and limitations on certain applications to AI developers and deployers.
% ABSENT_VOICES: Those who believe AI is inherently good and requires minimal regulation, or those who believe AI is inherently dangerous and should be severely restricted or even halted. Also, those who believe technology has its own inherent moral agency. Their perspectives are sidelined by the focus on AI as a neutral, governable instrument.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, AI development would likely proceed with significantly fewer ethical guardrails and regulatory oversight. This would lead to an acceleration of potentially harmful applications, increased social inequality, and a diminished capacity for human agency, fundamentally reorganizing the relationship between humans and technology.
% FOUNDING_PROBLEM: The potential for advanced technologies, particularly AI, to develop in ways that undermine human dignity, exacerbate social inequalities, and operate beyond human control, leading to dehumanization and fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: International bodies (e.g., UN, UNESCO), academic ethicists, and various civil society organizations consistently highlight the ongoing and evolving risks of unregulated AI, corroborating the continued relevance of this founding problem from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate AI development with human values (beneficiaries: human regulators, ethical framework developers) but does so through active enforcement that imposes costs and limits on AI developers (victims: AI developers and deployers). Extractiveness (0.6) reflects the compliance costs and foregone opportunities for less regulated development. Suppression (0.7) is high due to the continuous need for active monitoring, regulation, and enforcement to prevent harmful AI applications. Theater ratio (0.4) indicates that while there's genuine effort, some regulatory initiatives may be more symbolic than effective in practice. Resistance (0.6) is significant from those who oppose regulation or advocate for different ethical paradigms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human regulators and ethical framework developers, this constraint is a necessary and beneficial coordination mechanism. From the perspective of AI developers, it represents a significant cost and limitation on innovation. From the perspective of those harmed, it is a necessary but often insufficient safeguard. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Human regulators and ethical framework developers are beneficiaries, gaining authority and influence from the constraint's operation. AI developers and deployers are payers, bearing the costs of compliance and restricted use-cases. Individuals harmed by unregulated AI are also victims, as the constraint's failure to fully protect them means they bear the negative externalities. Technocratic optimists are excluded, as their core premise of unbridled efficiency conflicts with this reading's emphasis on human-centered governance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_neutrality_ambiguity,
    'Is AI truly a morally neutral tool, or do its inherent design, data, and emergent properties carry intrinsic biases or moral valences that challenge the ''neutral tool'' premise?',
    'Empirical studies of AI system behavior in diverse contexts, philosophical analysis of AI''s agency and embedded values, and the development of AI ethics that account for non-human moral properties.',
    'If AI is found to possess inherent moral valences, the ''neutral tool'' premise of this reading would be challenged, potentially shifting the constraint towards a more ''incarnational_humanism'' or ''technocratic_optimization'' framing, or requiring a more fundamental re-evaluation of governance models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_neutrality_ambiguity, conceptual, 'Ambiguity of AI''s moral neutrality vs. inherent bias.').

omega_variable(
    regulatory_effectiveness_gap,
    'How effective are current and proposed regulatory frameworks in actually mitigating AI risks and ensuring human-centered outcomes, given the rapid pace of technological change?',
    'Longitudinal studies of regulatory impact, comparative analysis of different governance models, and real-world case studies of AI harms and their redress.',
    'If regulation is found to be consistently ineffective, the constraint''s ''tangled_rope'' classification might shift towards a ''snare'' (if extraction of compliance costs persists without genuine protection) or ''piton'' (if it becomes purely performative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_gap, empirical, 'Effectiveness of AI regulation in practice.').

omega_variable(
    subsidiarity_implementation_challenge,
    'How can the principle of subsidiarity be effectively implemented in global AI governance, balancing local autonomy with universal ethical standards?',
    'Development of multi-level governance models, case studies of successful and unsuccessful applications of subsidiarity in other global domains, and ongoing dialogue between international and local stakeholders.',
    'Failure to effectively implement subsidiarity could lead to either over-centralized, unresponsive regulation (increasing extraction and suppression) or fragmented, ineffective local governance (leading to greater harms from unregulated AI).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_implementation_challenge, preference, 'Challenge of implementing subsidiarity in AI governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_h_be_t30, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_h_su_t30, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, digital_human_rights_protections).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_human_relationship' kernel, focusing on AI as a neutral tool to be governed by human law and ethics. It is linked to sibling readings 'technocratic_optimization' and 'incarnational_humanism', which offer alternative framings of the AI-human relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
