% ============================================================================
% CONSTRAINT STORY: ai_dignity_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_dignity_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: ai_dignity_safeguarding__autonomy_rights_reading
 *   human_readable: AI Dignity Safeguarding: Autonomy and Rights Reading
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This constraint represents a reading of 'AI Dignity Safeguarding' that
 *   grounds dignity in human autonomy, rationality, and rights. It advocates
 *   for democratic regulation, transparency, labor and privacy protection,
 *   and algorithmic accountability, while allowing cautious enhancement
 *   within rights limits. The framework aims to coordinate technological
 *   development with ethical principles, imposing costs on developers
 *   (extraction) and requiring active enforcement (suppression) to protect
 *   individuals and democratic values. The claimed type is 'rope' as it
 *   represents an ongoing, evolving coordination effort with genuine
 *   benefits, despite the inherent costs and enforcement needs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_dignity_safeguarding__autonomy_rights_reading, 0.55).
domain_priors:suppression_score(ai_dignity_safeguarding__autonomy_rights_reading, 0.65).
domain_priors:theater_ratio(ai_dignity_safeguarding__autonomy_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_dignity_safeguarding__autonomy_rights_reading, rope).
narrative_ontology:human_readable(ai_dignity_safeguarding__autonomy_rights_reading, "AI Dignity Safeguarding: Autonomy and Rights Reading").
narrative_ontology:topic_domain(ai_dignity_safeguarding__autonomy_rights_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(ai_dignity_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_dignity_safeguarding__autonomy_rights_reading, 'fea7034d-8f0c-40a6-954c-356635a614f0').
narrative_ontology:cs_kernel_codification('fea7034d-8f0c-40a6-954c-356635a614f0', formalized).
narrative_ontology:cs_authority_grounding('fea7034d-8f0c-40a6-954c-356635a614f0', practice).
narrative_ontology:cs_interpretation_layer_present('fea7034d-8f0c-40a6-954c-356635a614f0').
narrative_ontology:cs_reading_relation('fea7034d-8f0c-40a6-954c-356635a614f0', ai_dignity_safeguarding__imago_dei_reading, forecloses).
narrative_ontology:cs_reading_relation('fea7034d-8f0c-40a6-954c-356635a614f0', ai_dignity_safeguarding__posthuman_continuity_reading, influences).
narrative_ontology:cs_axiom('fea7034d-8f0c-40a6-954c-356635a614f0', foundational, human_autonomy_is_foundational).
narrative_ontology:cs_axiom_status(human_autonomy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('fea7034d-8f0c-40a6-954c-356635a614f0', human_autonomy_is_foundational, deontological).
narrative_ontology:cs_axiom('fea7034d-8f0c-40a6-954c-356635a614f0', foundational, rights_based_limits_on_enhancement).
narrative_ontology:cs_axiom_status(rights_based_limits_on_enhancement, holdable).
narrative_ontology:cs_axiom_grounding('fea7034d-8f0c-40a6-954c-356635a614f0', rights_based_limits_on_enhancement, deontological).
narrative_ontology:cs_reference_frame('fea7034d-8f0c-40a6-954c-356635a614f0', enlightenment_humanism_framework).
narrative_ontology:cs_drift_state('fea7034d-8f0c-40a6-954c-356635a614f0', contemporary_ai_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('fea7034d-8f0c-40a6-954c-356635a614f0', '').
narrative_ontology:cs_kernel_id(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rational_agents).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, democratic_institutions).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, individuals_subjected_to_opaque_algorithms).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, displaced_labor).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, coercively_enhanced_individuals).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, ai_developers).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_tech_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose autonomy, rationality, and rights are protected by the regulatory framework. They benefit from transparency, accountability, and limits on coercive technologies, but may still face challenges from AI systems.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rational_agents, beneficiary,
    moderate, biographical, constrained, global).

% Governments and regulatory bodies tasked with enacting and enforcing laws, policies, and ethical guidelines to safeguard human dignity in the age of AI and enhancement technologies. They set the framework and ensure compliance.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, democratic_institutions, agenda_setter,
    institutional, generational, analytical, national).

% People whose lives are significantly impacted by AI systems (e.g., in hiring, credit, justice) that lack transparency or accountability, leading to unfair or discriminatory outcomes. They bear the costs of algorithmic bias and lack of recourse.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, individuals_subjected_to_opaque_algorithms, payer,
    powerless, immediate, trapped, local).

% Workers whose jobs are automated or significantly altered by AI, leading to economic insecurity and the need for retraining or new employment. They bear the costs of technological disruption without adequate social safety nets.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, displaced_labor, payer,
    powerless, biographical, constrained, regional).

% Individuals who undergo enhancement procedures under duress or without full informed consent, potentially compromising their bodily autonomy or personal identity. They bear the costs of exploitation in the pursuit of 'improvement'.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, coercively_enhanced_individuals, payer,
    powerless, biographical, identity_locked, local).

% Companies and researchers developing AI systems. They bear the costs of compliance with regulations, transparency requirements, and ethical guidelines, which may slow innovation or increase development expenses.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, ai_developers, payer,
    organized, biographical, mobile, global).

% Companies developing human enhancement technologies. They face regulatory hurdles, ethical scrutiny, and restrictions on certain types of enhancement or marketing practices, impacting their market access and product development.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_tech_companies, payer,
    organized, biographical, mobile, global).

% Lobbyists and organizations advocating for minimal regulation of AI and enhancement technologies, prioritizing rapid innovation and market freedom over ethical constraints. Their views are largely excluded from the democratic regulatory process this constraint embodies.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, unregulated_tech_advocates, excluded,
    powerful, immediate, mobile, global).

% Academics and researchers who analyze the ethical implications of AI and enhancement, contributing to the conceptual foundations of dignity and rights. They observe and critique the evolving regulatory landscape.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, philosophical_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_dignity_safeguarding__autonomy_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_dignity_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared ethical and legal framework for the development and deployment of AI and human enhancement technologies, ensuring that innovation proceeds in a manner consistent with human autonomy, rationality, and fundamental rights.
% TRANSFER_FUNCTION: Transfers the burden of ethical consideration, transparency, and accountability to developers and deployers of AI/enhancement technologies. It transfers protection, agency, and a degree of control over technological impact to individuals and democratic governance structures.
% ABSENT_VOICES: Those who advocate for purely market-driven or technologically deterministic approaches to AI and enhancement, or those who believe human nature is infinitely malleable without ethical guardrails, are largely excluded from the deliberative processes that shape this framework. They would argue for less regulation and fewer restrictions on technological development.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the rapid, unregulated advancement of AI and enhancement technologies would likely lead to significant erosion of human autonomy, privacy, and labor rights, increased algorithmic discrimination, and potentially coercive enhancement practices. Society would face profound ethical and social disruption as technological power concentrates without accountability.
% FOUNDING_PROBLEM: The rapid and accelerating development of artificial intelligence and biotechnologies without adequate ethical, legal, or social frameworks, posing significant risks to human autonomy, privacy, labor, and the very definition of human dignity.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, labor unions, privacy advocates, and a broad consensus among academic ethicists and many public policy experts corroborate the ongoing nature of these risks and the critical need for such a framework. Reports from UN bodies, national commissions on AI ethics, and civil society organizations consistently highlight these challenges.
narrative_ontology:disappearance_verdict(ai_dignity_safeguarding__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_dignity_safeguarding__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_dignity_safeguarding__autonomy_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_dignity_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_dignity_safeguarding__autonomy_rights_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_dignity_safeguarding__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) as it imposes significant compliance costs on AI and enhancement technology developers, but these costs are framed as necessary for safeguarding rights rather than pure rent extraction. Suppression is also moderate (0.65) due to the active enforcement required for democratic regulation, transparency, and accountability. The theater ratio is low (0.15) because the efforts to protect dignity and rights are largely genuine and functional, not merely performative. Accessibility collapse is moderate (0.55) as it constrains, but does not eliminate, alternatives like unregulated development or certain enhancement paths. Resistance is moderate (0.6) from those who prioritize unfettered innovation or reject the underlying ethical premises.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of democratic institutions and protected individuals, this framework is a necessary 'rope' for coordinating technological progress with human values. However, from the perspective of AI and enhancement tech companies, it may feel more extractive due to compliance burdens, and from unregulated tech advocates, it is seen as an illegitimate 'snare' stifling innovation. The engine's per-seat classification will capture these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous rational agents and democratic institutions are the primary beneficiaries, gaining protection and a framework for ethical governance. Individuals subjected to opaque algorithms, displaced labor, and coercively enhanced individuals are victims, bearing the costs of technological risks that the framework seeks to mitigate. AI and enhancement tech companies are also victims in the sense that they bear the costs of compliance and regulation. Unregulated tech advocates are excluded, as their preferred approach is incompatible with this framework's core tenets. Philosophical ethicists serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_autonomy_and_rationality,
    'How broadly or narrowly are ''autonomy'' and ''rationality'' defined in practice, and does this definition inadvertently exclude certain populations or forms of intelligence?',
    'Empirical studies of algorithmic bias and regulatory impact assessments on diverse populations, alongside ongoing philosophical debate on the nature of autonomy in technologically mediated environments.',
    'A narrow definition could lead to the framework inadvertently extracting from or suppressing groups deemed less ''autonomous'' or ''rational'', shifting its effective classification towards a Tangled Rope or Snare for those groups. A broader, more inclusive definition would reinforce its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_autonomy_and_rationality, conceptual, 'Ambiguity in the practical definition of foundational concepts like autonomy and rationality.').

omega_variable(
    enforcement_effectiveness_vs_capture,
    'To what extent can democratic regulation effectively safeguard dignity against powerful technological actors, or is it susceptible to regulatory capture by the very industries it seeks to govern?',
    'Longitudinal studies of regulatory outcomes, lobbying expenditures, and the revolving door phenomenon between industry and regulatory bodies. Analysis of enforcement actions and their impact on industry practices.',
    'If regulatory capture is substantial, the constraint''s effective extractiveness from individuals would be higher, and its claimed coordination function would be undermined, pushing its classification towards a Snare. Effective, uncaptured enforcement would maintain its Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_capture, empirical, 'The risk of regulatory capture undermining the constraint''s stated goals.').

omega_variable(
    enhancement_rights_limits_slippage,
    'Will the ''cautious openness to enhancement within rights limits'' inevitably lead to a slippage where ''rights limits'' are gradually eroded under pressure for innovation or competitive advantage?',
    'Monitoring of legislative changes, ethical guidelines evolution, and public discourse around enhancement technologies over time. Analysis of case law related to enhancement and consent.',
    'If rights limits erode, the constraint''s protective function diminishes, and the potential for coercive or exploitative enhancement increases, shifting its classification towards a Snare for those subjected to such practices. Robust defense of rights limits would maintain its Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_rights_limits_slippage, preference, 'The potential for ''rights limits'' on enhancement to erode over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_dignity_safeguarding__autonomy_rights_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_d_tr_t0, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_d_tr_t2, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 2, 0.11).
narrative_ontology:measurement(ai_d_tr_t4, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(ai_d_tr_t6, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(ai_d_tr_t8, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(ai_d_tr_t10, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 10, 0.145).
narrative_ontology:measurement(ai_d_tr_t12, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 12, 0.148).
narrative_ontology:measurement(ai_d_tr_t14, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 14, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_d_be_t0, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_d_be_t2, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(ai_d_be_t4, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(ai_d_be_t6, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ai_d_be_t8, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ai_d_be_t10, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ai_d_be_t12, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(ai_d_be_t14, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 14, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_d_su_t0, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_d_su_t2, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 2, 0.53).
narrative_ontology:measurement(ai_d_su_t4, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(ai_d_su_t6, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(ai_d_su_t8, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(ai_d_su_t10, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(ai_d_su_t12, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(ai_d_su_t14, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 14, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_dignity_safeguarding__autonomy_rights_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_dignity_safeguarding' kernel, focusing on autonomy and rights. It is structurally distinct from the 'imago_dei_reading' (which grounds dignity in divine image and rejects enhancement) and the 'posthuman_continuity_reading' (which sees enhancement as continuous with flourishing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
