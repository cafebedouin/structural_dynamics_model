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
 *   human_readable: AI as Neutral Tool: Instrumental Subsidiarity
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   This constraint represents the view that AI is a morally neutral tool
 *   whose ethical implications are determined by its human application and
 *   governance. It emphasizes the role of law, ethics, and the principle of
 *   subsidiarity in ensuring AI serves human ends. This is one reading of the
 *   broader 'AI-human relationship' kernel, focusing on instrumental control
 *   and responsible regulation.
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
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool: Instrumental Subsidiarity").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '497d2713-40e6-466c-8df2-994a2f1f4545').
narrative_ontology:cs_kernel_codification('497d2713-40e6-466c-8df2-994a2f1f4545', formalized).
narrative_ontology:cs_authority_grounding('497d2713-40e6-466c-8df2-994a2f1f4545', lineage).
narrative_ontology:cs_interpretation_layer_present('497d2713-40e6-466c-8df2-994a2f1f4545').
narrative_ontology:cs_reading_relation('497d2713-40e6-466c-8df2-994a2f1f4545', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('497d2713-40e6-466c-8df2-994a2f1f4545', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('497d2713-40e6-466c-8df2-994a2f1f4545', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('497d2713-40e6-466c-8df2-994a2f1f4545', technology_is_morally_neutral, deontological).
narrative_ontology:cs_axiom('497d2713-40e6-466c-8df2-994a2f1f4545', foundational, human_dignity_protected_by_law_and_ethics).
narrative_ontology:cs_axiom_status(human_dignity_protected_by_law_and_ethics, holdable).
narrative_ontology:cs_axiom_grounding('497d2713-40e6-466c-8df2-994a2f1f4545', human_dignity_protected_by_law_and_ethics, deontological).
narrative_ontology:cs_reference_frame('497d2713-40e6-466c-8df2-994a2f1f4545', human_centered_instrumentalism).
narrative_ontology:cs_drift_state('497d2713-40e6-466c-8df2-994a2f1f4545', contemporary_ai_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('497d2713-40e6-466c-8df2-994a2f1f4545', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ethical_framework_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, citizens_and_users).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with developing and enforcing legal and ethical frameworks for AI. They benefit from the mandate to govern AI, positioning themselves as essential for guiding technological development in line with human values.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Academics, think tanks, and NGOs who develop ethical guidelines and principles for AI. They benefit from the demand for their expertise and frameworks, which are seen as crucial for ensuring AI serves human ends.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ethical_framework_developers, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of compliance with regulations and ethical guidelines, including implementing transparency measures, conducting impact assessments, and adhering to data privacy laws. They view these as necessary costs for market access and public trust.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_corporations, payer,
    powerful, biographical, constrained, global).

% Monitor AI development and deployment to ensure it respects human dignity and fundamental rights. They advocate for robust legal and ethical safeguards, viewing the constraint as a necessary, though often insufficient, step.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Are intended beneficiaries of AI governance, receiving protections against misuse, discrimination, and privacy violations. Their benefits are often indirect, mediated through regulatory action, and their ability to influence the constraint is diffuse.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, citizens_and_users, beneficiary,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and deployment of AI technologies by establishing common legal and ethical boundaries, ensuring that diverse actors (developers, users, regulators) operate within a shared understanding of acceptable and desirable AI applications.
% TRANSFER_FUNCTION: Transfers responsibility for ethical outcomes from individual technological determinism to collective governance through legal and ethical frameworks. It transfers compliance costs to AI developers and corporations, and (intended) safety/rights benefits to citizens and users.
% ABSENT_VOICES: Those who believe AI is inherently transformative and cannot be contained by instrumental regulation, or those who see technology as a purely self-organizing force, are often marginalized in policy discussions focused on 'proper governance.'
% DISAPPEARANCE_RATIONALE: If the belief in AI as a neutral tool to be regulated, along with its associated governance structures, vanished, the development of AI would likely become far more chaotic and driven purely by commercial or military imperatives, leading to rapid and potentially harmful societal shifts without ethical guardrails.
% FOUNDING_PROBLEM: The rapid advancement of AI raised concerns about its potential to undermine human autonomy, dignity, and societal well-being if left unregulated, leading to a need for frameworks to ensure technology serves human ends.
% FOUNDING_PROBLEM_CORROBORATION: International organizations (e.g., UNESCO, OECD), numerous national governments, and a broad consensus among ethicists and civil society groups corroborate the ongoing need for ethical and legal governance of AI to prevent harm and ensure alignment with human values.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.3) because the primary goal is coordination and protection, not direct extraction, though compliance costs are borne by developers. Suppression is also low (0.2) as it relies on legal and ethical frameworks rather than overt coercion, with resistance primarily coming from those who find compliance burdensome. Theater ratio is low (0.1) as the efforts to regulate AI are largely genuine, though some 'ethics washing' may occur. The constraint is claimed as a Rope because it aims for genuine coordination and mutual benefit through shared rules.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory bodies and ethical framework developers perceive this as a necessary and beneficial coordination mechanism, ensuring responsible innovation. AI developers and corporations, while acknowledging the need for trust, may experience it as an extractive burden due to compliance costs and limitations on innovation. Human rights advocates see it as a foundational step, but often argue for more robust protections.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and ethical framework developers are beneficiaries, as the constraint legitimizes their roles and expertise. AI developers and corporations are payers, bearing the direct costs of compliance. Citizens and users are intended beneficiaries, receiving protections, though their influence is indirect. There are no direct 'victims' in this reading, as the framework is designed to prevent harm, not cause it.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Rope prevents mislabeling genuine efforts at ethical governance as pure extraction. It acknowledges the coordination function of establishing shared rules for AI development. However, the omegas highlight the ongoing contestation over whether this instrumental approach is sufficient or if it masks deeper, more structural issues of AI's impact on human dignity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_instrumental_approach,
    'Is the ''AI as neutral tool'' framing sufficient to address the full range of ethical challenges posed by advanced AI, or does it overlook inherent biases or emergent properties?',
    'Empirical studies on the long-term societal impacts of regulated AI, particularly concerning systemic biases, autonomy erosion, or the redefinition of human capabilities. Conceptual analysis of AI''s ''agency'' and its implications for moral responsibility.',
    'If insufficient, the constraint''s effective extractiveness (e.g., in terms of human dignity or autonomy) might be higher than currently measured, leading to a reclassification towards a Tangled Rope or Snare if the ''neutrality'' claim serves to obscure deeper harms. If sufficient, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_instrumental_approach, conceptual, 'Whether AI''s neutrality is a valid premise for its governance.').

omega_variable(
    subsidiarity_implementation_effectiveness,
    'How effectively is the principle of subsidiarity being implemented in AI governance to ensure decisions are made at the lowest appropriate level, empowering local communities and individuals?',
    'Audits of AI governance structures, case studies of community-led AI initiatives, and analysis of power distribution in AI policy-making processes. Comparison with other governance models.',
    'If subsidiarity is poorly implemented, leading to centralized control and disempowerment, the constraint''s suppression could be higher, and its coordination function could be undermined, pushing it towards a Tangled Rope. Effective implementation would strengthen its Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_implementation_effectiveness, empirical, 'Effectiveness of subsidiarity in AI governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 2018, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t2018, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2018, 0.05).
narrative_ontology:measurement(ai_h_tr_t2020, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(ai_h_tr_t2022, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2022, 0.09).
narrative_ontology:measurement(ai_h_tr_t2024, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2018, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2018, 0.2).
narrative_ontology:measurement(ai_h_be_t2020, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(ai_h_be_t2022, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2022, 0.28).
narrative_ontology:measurement(ai_h_be_t2024, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2018, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2018, 0.15).
narrative_ontology:measurement(ai_h_su_t2020, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2020, 0.18).
narrative_ontology:measurement(ai_h_su_t2022, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2022, 0.19).
narrative_ontology:measurement(ai_h_su_t2024, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
