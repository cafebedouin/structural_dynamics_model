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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy (Magisterial Subsidiarity Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint defines AI governance legitimacy through the lens of
 *   Catholic Social Doctrine (CSD), as interpreted by the Magisterium. It
 *   asserts that AI must serve the common good, respect subsidiarity and
 *   solidarity, and ensure the universal destination of goods. This reading
 *   explicitly subordinates technology to human dignity and social justice,
 *   rejecting purely technocratic or market-driven approaches. It is one
 *   reading of the broader 'AI governance legitimacy' kernel, with specific
 *   beneficiaries (workers, vulnerable populations) and victims (private tech
 *   monopolies, military-industrial complex) identified by its ethical
 *   demands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.65).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy (Magisterial Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '7d501a93-b4ac-4eea-bc25-392be0aa3cf8').
narrative_ontology:cs_kernel_codification('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', formalized).
narrative_ontology:cs_authority_grounding('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', lineage).
narrative_ontology:cs_interpretation_layer_present('7d501a93-b4ac-4eea-bc25-392be0aa3cf8').
narrative_ontology:cs_reading_relation('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_reading_relation('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', foundational, technology_subordinated_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinated_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', technology_subordinated_to_human_dignity, deontological).
narrative_ontology:cs_axiom('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', foundational, common_good_as_primary_governance_aim).
narrative_ontology:cs_axiom_status(common_good_as_primary_governance_aim, holdable).
narrative_ontology:cs_axiom_grounding('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', common_good_as_primary_governance_aim, deontological).
narrative_ontology:cs_reference_frame('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', magisterial_social_teaching_tradition).
narrative_ontology:cs_drift_state('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', contemporary_ai_acceleration_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7d501a93-b4ac-4eea-bc25-392be0aa3cf8', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_doctrine_advocates).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_entities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Catholic Social Doctrine principles and applies them to AI governance, providing moral guidance and advocating for specific policy frameworks. Its authority is rooted in theological tradition and moral suasion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from principles like the universal destination of goods and solidarity, which advocate for AI's use to enhance human labor and dignity, not replace or exploit it. They are often vulnerable to technological displacement without such protections.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Benefit from principles advocating for equitable access to technology and preventing its use for further exploitation or digital colonialism. They are often targets of extractive AI practices without strong ethical governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Bear the costs of conforming to principles that demand transparent accountability, participatory governance, and subordination of technology to human dignity, which may limit profit maximization and require sharing control. They resist external ethical mandates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, immediate, constrained, global).

% Faces constraints on the development and deployment of autonomous weapons and surveillance technologies that violate human dignity or subsidiarity. They bear the cost of ethical limitations on their operational freedom and profit motives.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Are constrained by principles that challenge purely profit-driven AI applications, especially those that exacerbate inequality or exploit vulnerable populations through algorithmic lending or speculative trading. They bear the cost of ethical limits on financial innovation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_entities, payer,
    powerful, biographical, mobile, global).

% Are called upon to implement policies reflecting these principles, balancing them with other governance priorities. They observe the moral arguments and consider their integration into secular legal frameworks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, democratic_governments, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent ethical framework for AI development and deployment, coordinating diverse actors (governments, industry, civil society) around shared moral principles to ensure technology serves the common good and human dignity.
% TRANSFER_FUNCTION: Transfers moral authority and legitimacy from purely technical or market-driven AI development to a framework grounded in human dignity and social justice, shifting power towards vulnerable populations and away from unchecked corporate or state control.
% ABSENT_VOICES: Purely utilitarian ethicists, radical transhumanists, and those who believe technology is inherently neutral or self-regulating are excluded from the foundational premise of this discourse. They would argue against the imposition of pre-determined moral frameworks on technological development.
% DISAPPEARANCE_RATIONALE: If this ethical framework vanished, the moral landscape of AI governance would lose a significant voice advocating for human dignity, subsidiarity, and solidarity. The default would likely revert to more technocratic or market-driven approaches, leading to different outcomes for workers, vulnerable populations, and the distribution of AI's benefits.
% FOUNDING_PROBLEM: The problem of ensuring that rapidly advancing technology, particularly AI, serves human flourishing and the common good, rather than exacerbating inequality, exploitation, or dehumanization, especially in the absence of robust ethical guidance.
% FOUNDING_PROBLEM_CORROBORATION: Numerous international bodies, civil society organizations, and academic ethicists (outside the Magisterium's direct influence) corroborate the ongoing and urgent nature of the problem of ethical AI governance, even if they propose different solutions or foundational principles.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.52) reflects the significant demands placed on powerful actors to reorient AI development away from pure profit or power, incurring costs for compliance. Suppression (0.65) is moderate, as enforcement relies on moral suasion, civil society pressure, and advocacy in international law, rather than direct coercive power. However, it actively suppresses alternative, less ethically constrained approaches. Theater ratio (0.20) is low, indicating a genuine commitment to the stated principles, with minimal performative maintenance. The claimed type is 'tangled_rope' because it genuinely seeks to coordinate AI development for the common good while simultaneously extracting concessions and reorienting power from those who would pursue unconstrained technological advancement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and its beneficiaries, this framework is a necessary coordination mechanism to ensure AI serves humanity. From the perspective of the 'payer' stakeholders, it is an extractive imposition that limits innovation and economic freedom. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium acts as the agenda-setter, defining the ethical framework. Workers, Global South populations, families, and marginalized groups are beneficiaries, as the principles advocate for their protection and empowerment in the face of AI. Private tech monopolies, the military-industrial complex, and extractive finance entities are victims, as they bear the costs of ethical limitations on their operations and profit motives. Democratic governments are observers, tasked with considering and potentially implementing these principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_efficacy_ambiguity,
    'How effective is moral suasion and civil society pressure in enforcing these principles against powerful institutional actors like tech monopolies and states?',
    'Empirical studies tracking compliance rates, policy changes, and industry shifts in response to advocacy efforts over time.',
    'If efficacy is low, the constraint''s effective suppression is lower than measured, and its classification might drift towards a ''piton'' (theatrical maintenance) or a ''snare'' (if the coordination story is pure cover). If high, the ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy_ambiguity, empirical, 'Uncertainty regarding the actual coercive power of moral and civil society enforcement.').

omega_variable(
    subsidiarity_vs_solidarity_tension,
    'How are the principles of subsidiarity (decentralization) and solidarity (collective responsibility) balanced in practice, especially when they appear to conflict in AI governance?',
    'Case studies of AI governance initiatives that explicitly attempt to apply both principles, analyzing the practical compromises and interpretive frameworks used.',
    'If solidarity consistently overrides subsidiarity, the constraint might lean more towards centralized control, potentially increasing extraction from local initiatives. If subsidiarity dominates, it might diffuse responsibility, weakening collective protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_vs_solidarity_tension, conceptual, 'Ambiguity in the practical application and potential tension between core CSD principles.').

omega_variable(
    secular_reception_of_theological_ethics,
    'To what extent can a theological ethical framework gain traction and be implemented in pluralistic, secular governance contexts without being reinterpreted or diluted?',
    'Analysis of policy documents, legislative debates, and international agreements to identify direct or indirect incorporation of CSD principles, and the extent of their transformation in secular discourse.',
    'If the framework is consistently diluted or ignored, its effective scope and impact on ''victims'' would be lower, potentially shifting its classification towards a ''piton'' or a less effective ''tangled_rope''. If it gains significant traction, its influence and extractiveness would be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_reception_of_theological_ethics, empirical, 'The challenge of translating a theological ethical framework into secular policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_governance_legitimacy' kernel. Its ethical demands influence, and are influenced by, other readings of AI governance legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
