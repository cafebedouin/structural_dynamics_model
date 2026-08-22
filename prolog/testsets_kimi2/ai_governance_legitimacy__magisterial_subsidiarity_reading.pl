% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Magisterial Subsidiarity Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the magisterial_subsidiarity_reading of the
 *   ai_governance_legitimacy kernel. It holds that AI governance legitimacy
 *   derives from conformity to Catholic Social Doctrineâcommon good,
 *   subsidiarity, solidarity, and universal destination of goodsâas
 *   authoritatively interpreted by the Magisterium. The constraint
 *   coordinates multi-level governance by subordinating technology to human
 *   dignity, while simultaneously extracting operational freedom from private
 *   tech monopolies, military-industrial actors, and extractive finance. The
 *   encyclical framework explicitly rejects pure technocratic optimization
 *   and market libertarianism, positioning the Magisterium as the
 *   interpretive authority. The authored metrics describe a tangled_rope
 *   structure: genuine coordination for the vulnerable entangled with
 *   asymmetric extraction from concentrated power, enforced through moral
 *   suasion, civil society pressure, and international law advocacy rather
 *   than direct coercion.
 *
 * KEY AGENTS:
 *   - magisterium (agenda_setter/institutional/civilizational): Sets the doctrinal framework authoritatively and interprets Catholic Social Doctrine for the AI age.
 *   - workers (beneficiary/powerless/biographical): Receive protective labor-dignity coordination against automation-driven displacement.
 *   - global_south (beneficiary/powerless/generational): Receive anti-colonial, anti-extractive coordination resisting algorithmic domination.
 *   - families (beneficiary/moderate/biographical): Receive social-fabric protection against surveillance consumerism.
 *   - marginalized_populations (beneficiary/powerless/biographical): Receive preferential option and algorithmic bias protection.
 *   - private_tech_monopolies (payer/powerful/biographical): Bear operational constraint and profit limitation.
 *   - military_industrial_complex (payer/institutional/generational): Bear restriction on autonomous weapons and surveillance development.
 *   - extractive_finance (payer/powerful/biographical): Bear restriction on speculative AI financial instruments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.6).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0').
narrative_ontology:cs_kernel_codification('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', formalized).
narrative_ontology:cs_authority_grounding('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', lineage).
narrative_ontology:cs_interpretation_layer_present('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0').
narrative_ontology:cs_reading_relation('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_reading_relation('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', foundational, magisterium_holds_authoritative_interpretation).
narrative_ontology:cs_axiom_status(magisterium_holds_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', magisterium_holds_authoritative_interpretation, theological).
narrative_ontology:cs_axiom('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', foundational, technology_subordinated_to_common_good).
narrative_ontology:cs_axiom_status(technology_subordinated_to_common_good, holdable).
narrative_ontology:cs_axiom_grounding('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', technology_subordinated_to_common_good, deontological).
narrative_ontology:cs_reference_frame('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', catholic_social_doctrine_framework).
narrative_ontology:cs_drift_state('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', contemporary_ai_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b2e0a5b-73e6-4561-b6fd-31eaf9b8a9c0', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from AI governance that prioritizes labor dignity and protection against automation-driven displacement, as framed by Catholic Social Doctrine. Their position in global supply chains offers limited exit from technologically imposed restructuring.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    powerless, biographical, constrained, global).

% Stand to gain from AI governance that respects subsidiarity and universal destination of goods, resisting extractive data colonialism and algorithmic exploitation by Northern tech powers. Structural geopolitical debt and trade regimes limit exit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south, beneficiary,
    powerless, generational, trapped, global).

% Benefit from governance constraints that protect relational and social fabric against AI-mediated fragmentation and surveillance consumerism. Exit is constrained by the ubiquity of digital platforms in everyday life.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, biographical, constrained, global).

% Receive protective priority under solidarity and preferential option for the poor principles, shielding them from algorithmic bias and exclusion. Structural exclusion from tech design and governance leaves them trapped in default systems.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% Bear the cost of governance constraints that limit unrestricted data extraction, monopolistic platform behavior, and algorithmic opacity. They experience the constraint as a direct limitation on operational freedom and profit maximization.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, arbitrage, global).

% Faces constraints on the development and deployment of autonomous weapons and surveillance systems under the demand that technology serve human dignity and the common good. Its institutional entanglement with state security apparatus limits exit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Encounters moral and legal pressure against speculative and extractive financial instruments powered by opaque AI systems. Regulatory arbitrage remains possible but civil-society and ecclesial scrutiny raises reputational and political costs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, arbitrage, global).

% Exercises doctrinal authority to interpret Catholic Social Doctrine for the AI age, setting the normative framework through encyclicals and magisterial teaching. Exit from this interpretive commitment is precluded by the institution's self-understanding as guardian of apostolic tradition.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders global AI governance across diverse nation-states, cultures, and economic systems by providing a shared moral vocabularyâcommon good, solidarity, subsidiarity, universal destination of goodsâthat resists reduction to either pure market exchange or technocratic efficiency, prioritizing the vulnerable in multi-level political oversight.
% TRANSFER_FUNCTION: Moves authority and accountability in AI governance from unaccountable technical and market actors toward politically accountable bodies that prioritize human dignity, with operational freedom and extractive capacity transferred away from private tech monopolies, military-industrial actors, and speculative finance.
% ABSENT_VOICES: Secular humanists, non-Catholic religious traditions, atheist technologists, and market-libertarian developers are not fully in the room; they would object to the Magisterium's claim to authoritative interpretation and to the subordination of innovation to doctrinal moral limits.
% DISAPPEARANCE_RATIONALE: If the magisterial reading vanished overnight, the specific moral-legal framework anchoring AI governance in human dignity through Catholic Social Doctrine would collapse; protective constraints on the vulnerable would lose their normative foundation, and governance would revert to technocratic or market default logics.
% FOUNDING_PROBLEM: Rapid technological change operating without moral orientation, threatening human dignity, solidarity, the common good, and the preferential option for the poor, as diagnosed in the tradition of Catholic social encyclicals applied to the digital age.
% FOUNDING_PROBLEM_CORROBORATION: Labor advocates and international development organizations outside the Catholic Church corroborate the problem of ungoverned AI extraction and algorithmic harm to vulnerable populations, though they do not uniformly endorse the Magisterium's doctrinal solution or its claim to authoritative interpretation.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.52 because the constraint genuinely protects vulnerable populations while substantially limiting the operational freedom of powerful actorsâan asymmetric transfer that reads as extraction from the payer seats. Suppression is 0.60: the constraint requires active enforcement (moral suasion, civil society mobilization, international law advocacy, ecclesial witness) to hold against the default power of market and technocratic logics; it is not self-enforcing. Theater_ratio at 0.40 reflects that ecclesial witness and encyclical discourse have performative dimensionsâmaintaining moral authority and institutional presenceâthat partly exceed direct governance efficacy. Accessibility_collapse is low (0.35) because market-libertarian and technocratic alternatives remain widely understood and institutionally available; understanding the magisterial framework does not eliminate those exits. Resistance is high (0.70) because the targeted payers mount substantial ideological, lobbying, and jurisdictional resistance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (workers, Global South, families, marginalized) experience the constraint as protective coordination or scaffold, receiving dignity and solidarity. The payer seats experience it as an externally imposed moral snare limiting their operational autonomy. The Magisterium experiences it as doctrinal fidelity. The engine computes these divergent seat classifications from the structural asymmetry in directionality and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups have low directionality (d near 0.0): the constraint structurally subsidizes their position by limiting predatory AI deployment against them. Payer groups have high directionality (d near 1.0): the constraint extracts operational freedom and profit potential from their activity. The Magisterium is not declared in beneficiaries or victims, so its directionality reverts to the canonical fallback for institutional actors; it sits near the agenda-setting center, neither fully subsidized nor targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids piton classification because its founding problemâungoverned AI threatening human dignity and the common goodâis contested but not dead; there is active corroboration from outside the benefiting parties (labor and development advocates attest to the problem). It avoids pure snare classification because the coordination function is genuine: without a normative framework prioritizing the vulnerable, AI governance defaults to efficiency and extraction. The tangled_rope classification captures this entanglement: the same structure that coordinates protection for the powerless extracts constraint from the powerful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_universalism,
    'Does the Magisterium''s claim to authoritative interpretation constitute a universal coordination mechanism for global AI governance, or does it function as an identity-locked extraction of moral authority that marginalizes non-Catholic and secular voices?',
    'Comparative uptake analysis: measure the degree to which non-Catholic governance bodies (UN agencies, secular NGOs, non-Christian states) adopt magisterial framings versus treat them as one input among many.',
    'If the latter, the constraint''s effective extraction is higher than measured because the exclusion of alternative moral frameworks is a structural cost borne by non-parties; if the former, the coordination function is broader than the Catholic base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_universalism, conceptual, 'Ambiguity over whether magisterial authority coordinates universally or extracts parochially.').

omega_variable(
    subsidiarity_solidarity_tension,
    'Can the simultaneous demands of subsidiarity (decentralized decision-making) and solidarity (centralized moral obligation to the common good) be operationalized without collapsing into either hollow localization or top-down imposition?',
    'Case-study analysis of AI governance implementations in Catholic-majority polities: do local decisions retain autonomy when they conflict with magisterial solidarity demands, or does solidarity override subsidiarity?',
    'If solidarity consistently overrides subsidiarity, the constraint functions more as a centralized enforcement mechanism (snare-like) than a distributed coordination; if subsidiarity holds, the tangled_rope characterization is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_tension, conceptual, 'Structural tension between decentralization and centralized moral claims.').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is enforcement limited to the declared mechanisms of moral suasion and ecclesial witness, or does it translate into structural coercion (development funding conditionalities, voting-bloc discipline, institutional access denial)?',
    'Trace resource flows and alliance structures: identify whether Catholic-institution-affiliated development banks, aid agencies, or political coalitions condition access on compliance with magisterial AI principles.',
    'If structural coercion is present, suppression should be revised upward and the constraint may approach snare classification for affected parties; if absent, the authored suppression score holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Gap between declared moral suasion and actual structural enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(ai_g_tr_t32, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(ai_g_tr_t40, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(ai_g_be_t32, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(ai_g_be_t40, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(ai_g_su_t32, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(ai_g_su_t40, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
