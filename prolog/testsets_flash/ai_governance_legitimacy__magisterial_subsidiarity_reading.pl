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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   Catholic Social Doctrine, as interpreted by the Magisterium. It asserts
 *   that AI must serve the common good, respect subsidiarity and solidarity,
 *   and prioritize the universal destination of goods, with a strong emphasis
 *   on protecting the vulnerable. This reading explicitly subordinates
 *   technology to ethical principles and demands transparent, participatory
 *   governance. It is a 'tangled_rope' because it genuinely seeks to
 *   coordinate societal efforts towards ethical AI while simultaneously
 *   extracting compliance from powerful technological actors who might prefer
 *   unconstrained development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.3).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy (Magisterial Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'ed8ddcc2-5ae4-44ee-83d9-aea508581942').
narrative_ontology:cs_kernel_codification('ed8ddcc2-5ae4-44ee-83d9-aea508581942', formalized).
narrative_ontology:cs_authority_grounding('ed8ddcc2-5ae4-44ee-83d9-aea508581942', lineage).
narrative_ontology:cs_interpretation_layer_present('ed8ddcc2-5ae4-44ee-83d9-aea508581942').
narrative_ontology:cs_reading_relation('ed8ddcc2-5ae4-44ee-83d9-aea508581942', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('ed8ddcc2-5ae4-44ee-83d9-aea508581942', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed8ddcc2-5ae4-44ee-83d9-aea508581942', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('ed8ddcc2-5ae4-44ee-83d9-aea508581942', foundational, technology_subordinated_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinated_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('ed8ddcc2-5ae4-44ee-83d9-aea508581942', technology_subordinated_to_human_dignity, deontological).
narrative_ontology:cs_axiom('ed8ddcc2-5ae4-44ee-83d9-aea508581942', foundational, common_good_as_primary_ethical_criterion).
narrative_ontology:cs_axiom_status(common_good_as_primary_ethical_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ed8ddcc2-5ae4-44ee-83d9-aea508581942', common_good_as_primary_ethical_criterion, deontological).
narrative_ontology:cs_reference_frame('ed8ddcc2-5ae4-44ee-83d9-aea508581942', catholic_social_doctrine_tradition).
narrative_ontology:cs_drift_state('ed8ddcc2-5ae4-44ee-83d9-aea508581942', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ed8ddcc2-5ae4-44ee-83d9-aea508581942', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_nations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_doctrine_advocates).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, human_dignity_as_foundational).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Catholic Social Doctrine and applies it to AI governance, issuing encyclicals and guidance. Seeks to shape global discourse and policy through moral suasion and advocacy. Benefits from the moral authority and influence derived from this interpretation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Benefit from principles like the universal destination of goods and solidarity, which advocate for AI's use to enhance human labor and dignity, not replace it or concentrate wealth. Their protection is a core aim of this governance framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Benefit from the subsidiarity principle, which advocates for local control and appropriate technology, and the universal destination of goods, which challenges technological colonialism and unequal access. They are often victims of unchecked technological power.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_nations, beneficiary,
    moderate, generational, constrained, global).

% Bear the costs of demands for transparent accountability, participatory governance, and subordination of technology to ethical principles, which challenge their profit motives and control. They resist external ethical constraints on innovation and market expansion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, constrained, global).

% Faces ethical challenges regarding autonomous weapons and surveillance technologies under principles of human dignity and solidarity. Subordination of technology to human flourishing limits their operational scope and profit models.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Act as agents of enforcement and dissemination for these principles, translating them into policy proposals and public pressure. They gain legitimacy and a coherent framework for their advocacy from the Magisterium's interpretation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates, beneficiary,
    organized, generational, mobile, global).

% Engage with these principles as one ethical framework among many in the broader discourse on AI governance. They may adopt elements that align with their own policy goals but are not bound by Magisterial authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_policy_makers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive ethical framework for AI development and deployment, coordinating diverse actors (developers, policymakers, civil society) around shared principles of human dignity, common good, and social justice, preventing purely technical or market-driven approaches from dominating.
% TRANSFER_FUNCTION: Transfers moral authority and legitimacy from Catholic Social Doctrine to AI governance, channeling advocacy efforts and public discourse towards specific ethical outcomes. It demands a transfer of power and accountability from private technological actors to broader societal oversight, particularly benefiting vulnerable populations.
% ABSENT_VOICES: Purely technocratic or market-libertarian voices, who prioritize efficiency, innovation, or individual property rights above collective ethical mandates, are structurally excluded from setting the foundational principles of this framework. They would argue for minimal regulation and maximal autonomy for developers and markets.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the ethical discourse around AI would lose a significant, globally influential voice advocating for human-centered principles. The balance of power would shift further towards technocratic and market-driven logics, potentially exacerbating inequalities and risks for vulnerable populations, as a major counter-narrative would be absent.
% FOUNDING_PROBLEM: The problem of technology developing without sufficient ethical guidance, leading to dehumanization, exploitation, and exacerbation of social inequalities, particularly in the context of powerful emerging technologies like AI.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium, civil society organizations, and numerous academic ethicists attest that the problem of ethically unguided technology is very much alive, citing ongoing concerns about AI bias, job displacement, surveillance, and autonomous weapons. This corroboration comes from outside the direct beneficiaries of the Magisterium's authority, reflecting broad societal concerns.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).

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
 *   The extractiveness (0.5) is moderate-high because it demands significant changes in how AI is developed and deployed, imposing costs on actors who prioritize profit or efficiency over social good. Suppression (0.3) is relatively low, relying more on moral suasion, advocacy, and civil society pressure rather than direct coercive enforcement. Theater ratio (0.2) is low, as the Magisterium's efforts are genuinely aimed at shaping ethical outcomes, not merely performing. The resistance (0.55) is moderate, reflecting pushback from powerful tech and financial interests. Accessibility collapse (0.4) is moderate, as alternative ethical frameworks exist, but this one offers a comprehensive and influential counter-narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, this framework is a necessary 'rope' for guiding technology towards human flourishing. From the perspective of private tech monopolies or the military-industrial complex, it is an 'snare' or 'tangled_rope' that imposes burdensome ethical limits on innovation and profit. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and civil society advocates are beneficiaries, as the framework provides them with moral authority and a coherent agenda. Workers, Global South nations, families, and marginalized populations are also beneficiaries, as the principles are designed to protect and empower them. Private tech monopolies, the military-industrial complex, and extractive finance institutions are victims, as the framework demands they subordinate their interests to broader ethical concerns, incurring costs in terms of profit or autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resists mandatrophy by continuously re-interpreting and applying foundational principles to new technological challenges (like AI). Its persistence is driven by a live founding problem (ethically unguided technology) and active advocacy, preventing it from becoming a 'piton'. The 'tangled_rope' classification acknowledges its dual function of genuine coordination (ethical guidance) and extraction (demanding compliance from powerful actors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_efficacy,
    'How effective is moral suasion and civil society pressure in compelling compliance from powerful technological actors?',
    'Empirical studies tracking changes in AI development practices and corporate policies in response to Magisterial guidance and related advocacy.',
    'If efficacy is low, the constraint''s effective suppression is lower than measured, and its ''tangled_rope'' classification might lean more towards a ''rope'' (less extraction) or even a ''piton'' (if its influence is purely performative). If high, its extractive force is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy, empirical, 'The actual power of non-coercive enforcement mechanisms.').

omega_variable(
    subsidiarity_vs_solidarity_tension,
    'How are the principles of subsidiarity (local autonomy) and solidarity (collective responsibility) balanced in practice, especially when they appear to conflict in AI governance?',
    'Case studies of AI governance initiatives that attempt to implement both principles, analyzing the trade-offs and interpretive choices made.',
    'If solidarity consistently overrides subsidiarity, the constraint might be more centralized and extractive than intended. If subsidiarity leads to fragmentation, the coordination function might be weaker. This impacts the balance of coordination vs. extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_vs_solidarity_tension, conceptual, 'Balancing local control with global ethical mandates in AI.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., through international law advocacy) or internalized (e.g., through moral conviction of developers)?',
    'Post-exit suppression trajectory: if ethical considerations persist after direct advocacy pressure is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — actors carry the ethical framework with them after direct external pressure subsides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ethical compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ai_g_tr_t2018, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(ai_g_tr_t2021, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(ai_g_tr_t2024, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(ai_g_be_t2018, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(ai_g_be_t2021, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement(ai_g_be_t2024, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2024, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(ai_g_su_t2018, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(ai_g_su_t2021, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2021, 0.3).
narrative_ontology:measurement(ai_g_su_t2024, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.08).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_governance_legitimacy' kernel, focusing on Catholic Social Doctrine. Other readings (democratic pluralist, market libertarian, technocratic optimization) are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
