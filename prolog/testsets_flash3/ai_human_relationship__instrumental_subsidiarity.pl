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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool: Instrumental Subsidiarity Reading
 *   domain: ethics/technology/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'instrumental subsidiarity' reading of the
 *   AI-human relationship kernel, prevalent in certain Catholic Social
 *   Teaching and technology ethics circles. It frames AI as a morally neutral
 *   tool whose ethical implications depend entirely on its human design and
 *   use, advocating for robust legal and ethical governance guided by the
 *   principle of subsidiarity. The constraint aims to ensure AI serves human
 *   ends through proper regulation and transparency, protecting human dignity
 *   via procedural safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.3).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.2).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.3).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool: Instrumental Subsidiarity Reading").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "ethics/technology/political_theology").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'e970c918-ead3-4c76-bc76-0d00c05b2304').
narrative_ontology:cs_kernel_codification('e970c918-ead3-4c76-bc76-0d00c05b2304', formalized).
narrative_ontology:cs_authority_grounding('e970c918-ead3-4c76-bc76-0d00c05b2304', lineage).
narrative_ontology:cs_interpretation_layer_present('e970c918-ead3-4c76-bc76-0d00c05b2304').
narrative_ontology:cs_reading_relation('e970c918-ead3-4c76-bc76-0d00c05b2304', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('e970c918-ead3-4c76-bc76-0d00c05b2304', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('e970c918-ead3-4c76-bc76-0d00c05b2304', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('e970c918-ead3-4c76-bc76-0d00c05b2304', technology_is_morally_neutral, deontological).
narrative_ontology:cs_axiom('e970c918-ead3-4c76-bc76-0d00c05b2304', foundational, human_dignity_protected_by_law_and_ethics).
narrative_ontology:cs_axiom_status(human_dignity_protected_by_law_and_ethics, holdable).
narrative_ontology:cs_axiom_grounding('e970c918-ead3-4c76-bc76-0d00c05b2304', human_dignity_protected_by_law_and_ethics, deontological).
narrative_ontology:cs_reference_frame('e970c918-ead3-4c76-bc76-0d00c05b2304', human_centered_instrumentalism).
narrative_ontology:cs_drift_state('e970c918-ead3-4c76-bc76-0d00c05b2304', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e970c918-ead3-4c76-bc76-0d00c05b2304', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ethical_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_users).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, unregulated_ai_developers).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technological_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, principle_of_subsidiarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with drafting and enforcing laws and ethical guidelines for AI development and deployment, ensuring it aligns with human dignity and societal well-being. They benefit from the mandate to govern and shape the technological landscape.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Seek to develop AI systems that respect human values and operate within clear ethical and legal boundaries. They benefit from regulatory clarity and public trust, which this constraint aims to provide.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ethical_ai_developers, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from AI systems that are designed and deployed with ethical safeguards, transparency, and accountability, reducing risks of harm and misuse. Their trust in AI is fostered by this approach.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_users, beneficiary,
    moderate, biographical, constrained, global).

% Bear the costs of compliance with new regulations and ethical standards, which may slow down development or increase operational expenses. They are targeted by the enforcement mechanisms designed to ensure responsible AI.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, unregulated_ai_developers, payer,
    powerful, immediate, constrained, global).

% Analyze the underlying philosophical and theological assumptions of AI governance frameworks, assessing their coherence with broader principles of human flourishing and the common good. They provide critical commentary on the constraint's foundations.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, political_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for responsible AI development and deployment, coordinating diverse actors (developers, regulators, users) to ensure technology serves human ends, preventing unchecked technological determinism or harmful applications.
% TRANSFER_FUNCTION: Transfers responsibility for ethical outcomes from individual developers to a broader regulatory and ethical governance structure, aiming to transfer trust and safety to users while imposing compliance costs on developers.
% ABSENT_VOICES: Those who believe AI is inherently transformative and cannot be contained by 'neutral tool' framing, or those who advocate for a more radical re-ordering of technology towards integral human development, are often marginalized in policy discussions focused on instrumental regulation.
% DISAPPEARANCE_RATIONALE: If this framework vanished, AI development would likely proceed with fewer ethical safeguards, leading to increased risks of misuse, algorithmic bias, and societal disruption. The current trajectory of AI governance would collapse, and a more chaotic, less human-centered approach would likely emerge.
% FOUNDING_PROBLEM: The rapid advancement of AI technology without clear ethical or legal guidance, leading to fears of autonomous systems undermining human agency, dignity, and societal values.
% FOUNDING_PROBLEM_CORROBORATION: International bodies (e.g., UNESCO, UN), academic ethicists, and civil society organizations consistently corroborate the ongoing challenge of governing AI responsibly, highlighting the need for frameworks that prioritize human well-being. This corroboration comes from outside the direct beneficiaries of the regulatory process.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is relatively low, reflecting the view that regulation imposes necessary, not exploitative, costs for the common good. Suppression (0.2) is also low, as the framework primarily seeks to guide rather than coercively restrict innovation, relying on legal and ethical frameworks. Theater ratio (0.1) is minimal, as the stated goals of ethical governance are genuinely pursued. The constraint is claimed as a 'rope' because it aims for genuine coordination and mutual benefit through shared ethical principles and legal structures.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for broad societal benefit, other readings (e.g., 'technocratic optimization' or 'incarnational humanism') would perceive its extractiveness and suppression differently. The 'technocratic optimization' reading might see any regulation as an extractive burden on efficiency, while 'incarnational humanism' might view the 'neutral tool' framing as a subtle form of suppression, failing to challenge deeper technological biases.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and ethical AI developers are beneficiaries, gaining legitimacy and a framework for responsible innovation. AI users also benefit from safer, more accountable systems. Unregulated AI developers are payers, bearing the costs of compliance. Political theologians act as observers, analyzing the framework's deeper implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to ensure AI serves human ends through regulation. This mandate is currently live and actively pursued, preventing it from becoming a piton. The focus on instrumental neutrality and subsidiarity aims to prevent the constraint from becoming a snare by ensuring that governance genuinely serves the common good rather than masking extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_neutrality_ambiguity,
    'Is AI truly a morally neutral tool, or does its inherent design, scale, and societal integration carry intrinsic moral biases or transformative power that cannot be fully contained by instrumental regulation?',
    'Longitudinal studies of AI''s societal impact, particularly in areas of power distribution and human agency, assessing whether ethical guidelines consistently mitigate unintended consequences or if systemic biases persist despite regulation.',
    'If AI is found to have intrinsic moral biases, the ''neutral tool'' framing would be challenged, potentially reclassifying the constraint towards a ''tangled_rope'' or ''snare'' if the framework fails to address these deeper issues, or towards ''mountain'' if the biases are irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_neutrality_ambiguity, conceptual, 'Ambiguity regarding the moral neutrality of AI itself.').

omega_variable(
    subsidiarity_implementation_effectiveness,
    'How effectively is the principle of subsidiarity being implemented in AI governance, ensuring decisions are made at the lowest appropriate level and avoiding centralized control that could become extractive?',
    'Audits of AI governance structures, examining decision-making processes, stakeholder participation, and the distribution of power in setting ethical and legal standards for AI.',
    'Ineffective implementation of subsidiarity could lead to centralized, top-down control, increasing extractiveness and suppression, potentially shifting the constraint towards a ''tangled_rope'' or ''snare'' if power concentrates unduly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_implementation_effectiveness, empirical, 'Effectiveness of subsidiarity in AI governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ai_h_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.09).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_h_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ai_h_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_human_relationship' kernel, focusing on instrumental neutrality and subsidiarity. It coexists with 'technocratic_optimization' and 'incarnational_humanism' readings, which offer alternative framings of AI's role and ethical governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
