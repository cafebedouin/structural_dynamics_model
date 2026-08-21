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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: AI Governance Legitimacy: Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint describes the Magisterium's reading of AI governance
 *   legitimacy, grounded in Catholic Social Doctrine. It asserts that AI must
 *   be subordinated to principles like the common good, subsidiarity,
 *   solidarity, and the universal destination of goods, as authoritatively
 *   interpreted by the Church. This framework demands transparent
 *   accountability, participatory governance, and protection of the
 *   vulnerable, explicitly rejecting purely technocratic or market-driven
 *   logics. The constraint is classified as a Tangled Rope because it
 *   genuinely seeks to coordinate ethical development for the common good
 *   while simultaneously imposing significant costs and restrictions on
 *   powerful actors who would prefer unconstrained technological development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.65).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy: Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20').
narrative_ontology:cs_kernel_codification('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', fixed_text).
narrative_ontology:cs_authority_grounding('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', lineage).
narrative_ontology:cs_interpretation_layer_present('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20').
narrative_ontology:cs_reading_relation('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', foundational, common_good_supremacy).
narrative_ontology:cs_axiom_status(common_good_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', common_good_supremacy, deontological).
narrative_ontology:cs_axiom('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', foundational, subsidiarity_principle).
narrative_ontology:cs_axiom_status(subsidiarity_principle, holdable).
narrative_ontology:cs_axiom_grounding('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', subsidiarity_principle, deontological).
narrative_ontology:cs_reference_frame('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', catholic_social_doctrine_tradition).
narrative_ontology:cs_drift_state('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', contemporary_ai_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('70f5cc6c-ed58-4b7b-b44d-d3da2daa2a20', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The authoritative interpretive body of Catholic Social Doctrine, which articulates and promotes the principles for AI governance. It seeks to guide ethical development through moral suasion, advocacy, and education.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Benefit from principles that advocate for human-centered AI, protection against automation-induced unemployment, and fair labor practices in the AI economy. They are often vulnerable to unchecked technological change.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    powerless, biographical, constrained, global).

% Benefit from principles promoting equitable access to AI benefits, prevention of digital colonialism, and technology transfer that respects local contexts and sovereignty. They are often targets of extractive practices.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Benefit from AI governance that supports human flourishing, protects privacy, and strengthens community bonds, rather than eroding social structures or fostering isolation. They bear diffuse costs of unchecked AI.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, biographical, constrained, local).

% Benefit from AI governance that actively combats bias, ensures equitable access to essential services, and prevents further marginalization through algorithmic discrimination. They are often the first victims of technological harms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, local).

% Bear the costs of compliance with ethical guidelines that prioritize the common good over profit maximization, requiring transparent accountability and participatory governance. They resist external ethical constraints on their operations.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, mobile, global).

% Bears the costs of ethical constraints on autonomous weapons systems and surveillance technologies, which challenge their operational autonomy and profit motives. They resist external ethical oversight.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Bears the costs of principles that challenge purely speculative or exploitative uses of AI in financial markets, advocating for economic justice and the universal destination of goods. They seek to maximize returns without ethical limits.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, arbitrage, global).

% Actively promote and apply Catholic Social Doctrine principles in AI policy debates, forming coalitions and pressuring governments and corporations. They benefit from the moral authority and framework provided by the Magisterium.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates, beneficiary).

% Would prioritize efficiency, innovation, and aggregate welfare as primary governance goals, viewing ethical constraints as secondary optimization parameters. Their purely technical framing is explicitly rejected by this reading's foundational principles.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, technocratic_experts, excluded,
    powerful, biographical, mobile, global).

% Would prioritize voluntary exchange, property rights, and minimal collective mandates, viewing solidarity and the common good as illegitimate coercion. Their market-first framing is explicitly rejected by this reading's foundational principles.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, market_libertarians, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure AI development and deployment serves the common good, respects human dignity, and prioritizes the vulnerable, preventing purely profit-driven or power-driven technological trajectories by providing a coherent ethical framework.
% TRANSFER_FUNCTION: Transfers moral authority and decision-making power from purely technical or market actors to a broader, ethically guided participatory framework, reallocating benefits towards the vulnerable and costs towards those who would otherwise externalize them.
% ABSENT_VOICES: Purely technocratic experts and market libertarians, who would argue for different foundational principles for AI governance, are structurally excluded from the Magisterium's interpretive framework, as their core premises are seen as incompatible.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the moral and ethical guidance for AI governance would lose a significant, globally influential voice, allowing purely utilitarian or market-driven approaches to dominate, leading to different societal outcomes and power distributions, particularly for the vulnerable.
% FOUNDING_PROBLEM: The historical tendency of technological development to proceed without sufficient ethical guidance, leading to exploitation, inequality, and disregard for human dignity, particularly exacerbated by the scale and potential impact of AI.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium itself, various Catholic social justice organizations, and independent ethicists and human rights advocates attest to the ongoing nature of this problem, citing current trends in AI development and deployment. This corroboration comes from outside the direct beneficiaries of the Magisterium's institutional power.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.52) is high because it demands a fundamental reorientation of powerful economic and military interests, imposing costs on those who benefit from current AI trajectories. Suppression (0.65) is moderate-high, relying on moral suasion, civil society pressure, and international advocacy to enforce its principles against strong resistance. The theater ratio (0.25) is relatively low, as the Magisterium's engagement is generally substantive, though some performative adherence to ethical principles by corporations may occur. The increasing trend in metrics reflects the growing power of the tech sector and the escalating challenge of subordinating it to ethical principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and its beneficiaries, this framework is a necessary coordination mechanism for ethical AI. From the perspective of the payers and excluded parties, it is an extractive imposition that limits innovation and economic freedom. The engine's classification will reflect this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and civil society advocates act as agenda-setters, promoting the framework. Workers, Global South populations, families, and marginalized populations are primary beneficiaries, as the principles are designed to protect and empower them. Private tech monopolies, the military-industrial complex, and extractive finance are the primary payers, as they bear the costs of ethical compliance and limitations on their autonomy. Technocratic experts and market libertarians are excluded, as their foundational premises are deemed incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing one distinct reading of the ''ai_governance_legitimacy'' kernel, or does it conflate elements of other readings?',
    'Comparative analysis with detailed structural descriptions of sibling readings to ensure clear boundaries and non-overlapping core premises.',
    'If conflated, the classification of this reading would be inaccurate, potentially obscuring the true nature of the contestation within the kernel. A clearer distinction would sharpen the analysis of inter-reading relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensures this constraint is a clean, ε-invariant representation of a single kernel reading.').

omega_variable(
    subordination_empirical_test,
    'To what extent is technology actually being ''subordinated'' to these principles in practice, beyond rhetorical adherence?',
    'Empirical studies tracking AI development outcomes, corporate accountability reports, and legislative changes in jurisdictions influenced by this framework, measuring actual shifts in power and resource allocation.',
    'If subordination is largely rhetorical, the effective extractiveness from payers might be lower than stated, and the benefits to vulnerable populations less tangible, potentially shifting the classification towards a Piton or a less effective Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_empirical_test, empirical, 'Assesses the real-world impact of the constraint''s ethical demands on technological practice.').

omega_variable(
    enforcement_efficacy_of_moral_suasion,
    'How effective is ''moral suasion, civil society pressure, and international law advocacy'' as an enforcement mechanism against powerful, profit-driven actors?',
    'Analysis of historical cases where similar moral/ethical frameworks have attempted to constrain powerful economic or military actors, evaluating the success rate of non-coercive enforcement.',
    'If these enforcement mechanisms are consistently weak, the constraint''s effective suppression might be lower than stated, leading to a re-evaluation of its ability to impose costs and thus its classification as a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_efficacy_of_moral_suasion, empirical, 'Evaluates the practical power of the declared enforcement mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.25).

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

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
