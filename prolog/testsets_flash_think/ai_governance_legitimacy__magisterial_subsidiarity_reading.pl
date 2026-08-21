% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy: Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint describes the 'magisterial_subsidiarity_reading' of the
 *   'ai_governance_legitimacy' kernel. It posits that AI governance
 *   legitimacy derives from conformity to Catholic Social Doctrine principles
 *   (common good, subsidiarity, solidarity, universal destination of goods)
 *   as authoritatively interpreted by the Magisterium. Technology must be
 *   subordinated to these principles through transparent accountability,
 *   participatory governance, and protection of the vulnerable. The
 *   constraint is claimed as a Tangled Rope because it genuinely seeks to
 *   coordinate AI development towards ethical ends (benefiting vulnerable
 *   populations) but also imposes significant, actively enforced burdens on
 *   powerful actors (private tech monopolies, military-industrial complex)
 *   who resist these constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.6).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy: Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '75a6975f-38a4-4b67-b78a-b51708cec56c').
narrative_ontology:cs_kernel_codification('75a6975f-38a4-4b67-b78a-b51708cec56c', formalized).
narrative_ontology:cs_authority_grounding('75a6975f-38a4-4b67-b78a-b51708cec56c', lineage).
narrative_ontology:cs_interpretation_layer_present('75a6975f-38a4-4b67-b78a-b51708cec56c').
narrative_ontology:cs_reading_relation('75a6975f-38a4-4b67-b78a-b51708cec56c', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('75a6975f-38a4-4b67-b78a-b51708cec56c', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('75a6975f-38a4-4b67-b78a-b51708cec56c', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('75a6975f-38a4-4b67-b78a-b51708cec56c', foundational, human_dignity_is_non_negotiable).
narrative_ontology:cs_axiom_status(human_dignity_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('75a6975f-38a4-4b67-b78a-b51708cec56c', human_dignity_is_non_negotiable, deontological).
narrative_ontology:cs_axiom('75a6975f-38a4-4b67-b78a-b51708cec56c', foundational, technology_must_serve_the_common_good).
narrative_ontology:cs_axiom_status(technology_must_serve_the_common_good, holdable).
narrative_ontology:cs_axiom_grounding('75a6975f-38a4-4b67-b78a-b51708cec56c', technology_must_serve_the_common_good, deontological).
narrative_ontology:cs_reference_frame('75a6975f-38a4-4b67-b78a-b51708cec56c', catholic_social_doctrine_framework).
narrative_ontology:cs_drift_state('75a6975f-38a4-4b67-b78a-b51708cec56c', contemporary_ai_development, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75a6975f-38a4-4b67-b78a-b51708cec56c', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_nations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, human_dignity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets and promulgates Catholic Social Doctrine, guiding ethical AI development. It seeks to subordinate technology to these principles through moral suasion and advocacy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from principles like solidarity and the universal destination of goods, which advocate for technology serving human labor, fair distribution, and protection from automation's negative impacts.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    powerless, biographical, constrained, global).

% Benefit from principles of solidarity and universal destination of goods, advocating for equitable access to AI benefits and preventing exploitation by AI-driven systems or data colonialism.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Benefit from AI governance that protects human dignity, privacy, and community, rather than disrupting social bonds or undermining parental authority through pervasive AI systems.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, biographical, constrained, global).

% Protected by the principles of solidarity and universal destination of goods, ensuring AI does not exacerbate existing inequalities, discrimination, or vulnerability.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, constrained, global).

% Bear costs of compliance with principles that challenge profit maximization and demand accountability, transparency, participatory governance, and ethical limits on AI development. Their business models are often in tension with these demands.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, constrained, global).

% Faces constraints on developing autonomous weapons and surveillance technologies that violate human dignity, subsidiarity, and the common good, requiring ethical oversight and limitations on use.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Challenged by principles that prioritize the common good and universal destination of goods over purely financial returns from AI-driven investment, algorithmic trading, or data monetization.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, constrained, global).

% Actively promote and advocate for the application of Catholic Social Doctrine in AI governance, exerting moral and political pressure on states, corporations, and international bodies.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates, agenda_setter,
    organized, biographical, mobile, global).

% Analyze and critique the application of Catholic Social Doctrine to AI, often finding common ground on human dignity but differing on the theological grounding or specific policy prescriptions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_ethicists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the development and deployment of AI technologies towards the common good, ensuring they serve human dignity, subsidiarity, and solidarity, rather than purely economic or technocratic ends.
% TRANSFER_FUNCTION: Transfers moral authority and interpretive power over AI ethics to the Magisterium and associated civil society actors, while imposing ethical constraints and accountability burdens on tech developers and states.
% ABSENT_VOICES: Pure market fundamentalists or radical transhumanists, who would reject any external ethical constraints on technological development or human enhancement, are structurally excluded from this framework's foundational premises.
% DISAPPEARANCE_RATIONALE: If this framework vanished, AI governance would likely default more heavily to technocratic optimization or market-driven logics, potentially leading to increased inequality, exploitation, and erosion of human dignity in AI development, as the counter-narrative and advocacy would be absent.
% FOUNDING_PROBLEM: The perceived ethical vacuum and potential for AI to exacerbate social inequalities, undermine human dignity, and concentrate power in unaccountable hands, driven by purely utilitarian or profit-driven motives.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and Catholic civil society organizations consistently attest to the ongoing urgency of these ethical challenges through encyclicals, pastoral letters, and advocacy. Independent human rights organizations and some secular ethicists also corroborate the existence of these problems, even if they propose different solutions or groundings.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.50) is substantial because adherence to these principles often requires significant changes to profit-driven or power-maximizing AI development models, imposing real costs on those who would otherwise operate without such ethical limits. Suppression (0.60) is moderate, reflecting the reliance on moral suasion, civil society pressure, and international advocacy rather than direct state coercion, but it is still active in shaping discourse and policy. The theater ratio (0.30) indicates that while some performative adherence may occur, the core principles are genuinely held and advocated for. Resistance (0.70) is high due to the direct challenge these principles pose to powerful economic and military interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and civil society advocates, this framework is a necessary ethical guide for AI, ensuring technology serves humanity. From the perspective of private tech monopolies or the military-industrial complex, it represents an external imposition that restricts innovation or strategic advantage, seen as an extractive burden.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and civil society advocates are agenda-setters, benefiting from the moral authority and influence this framework provides. Workers, Global South nations, families, and marginalized populations are beneficiaries, as the principles are designed to protect and empower them. Private tech monopolies, the military-industrial complex, and extractive finance are victims, as they bear the costs of compliance and face restrictions on their operations. The Magisterium's exit option is identity_locked, as its identity is fused with upholding these doctrines.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately captured as one reading of the ''ai_governance_legitimacy'' kernel, or is it a distinct, independent constraint?',
    'Analysis of whether the core principles (common good, subsidiarity, solidarity) are universally recognized as part of AI governance discourse, or if their ''Magisterial interpretation'' fundamentally alters the constraint''s structure.',
    'If independent, it would not participate in kernel-level drift analysis; if a reading, its classification contributes to the overall contest over AI governance legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a specific reading within a broader kernel.').

omega_variable(
    structural_delta_with_siblings,
    'How do the structural properties (ε, suppression, beneficiaries/victims) of this ''magisterial_subsidiarity_reading'' quantitatively differ from its sibling readings (technocratic_optimization, democratic_pluralist, market_libertarian)?',
    'Comparative analysis of generated constraint stories for each sibling reading, focusing on metric values and stakeholder declarations.',
    'Quantifies the divergence in how each reading structures AI governance, revealing which readings are more extractive or suppressive from different seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_with_siblings, empirical, 'Measures the structural differences between this reading and its siblings.').

omega_variable(
    location_of_disagreement,
    'Is the primary disagreement between this reading and its siblings located in the foundational principles (axioms), the authority structure, or the practical application of governance?',
    'Detailed textual analysis of each reading''s core claims and their implications for governance mechanisms and enforcement.',
    'Clarifies whether the contest is over ''what is right'' (axioms), ''who decides'' (authority), or ''how it''s done'' (practice), informing potential pathways for convergence or irreconcilable conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(location_of_disagreement, conceptual, 'Identifies the core point of contention within the AI governance legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(ai_g_su_t18, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
