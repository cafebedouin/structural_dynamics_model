% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy: Market Libertarian Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates a market-libertarian reading of AI
 *   governance legitimacy, asserting that voluntary exchange and property
 *   rights are the pre-political foundations for innovation and dignity. It
 *   explicitly rejects collective mandates and centralized oversight as
 *   illegitimate coercion. The constraint is claimed as a 'mountain' due to
 *   its assertion of natural law principles, but its beneficiaries trigger
 *   False Summit Mountain detection. The metrics reflect a low but non-zero
 *   extractiveness and suppression, as even 'free' markets require
 *   enforcement of contracts and property, which can impose costs on those
 *   with less power.
 *
 * KEY AGENTS:
 *   - entrepreneurs: Primary beneficiary (powerful/arbitrage) — benefits from unencumbered markets.
 *   - investors: Primary beneficiary (institutional/arbitrage) — benefits from stable property rights.
 *   - high_autonomy_individuals: Beneficiary (moderate/mobile) — benefits from exit options.
 *   - low_market_power_individuals: Primary target (powerless/trapped) — bears costs of market failures.
 *   - communities_facing_coordination_failures: Target (organized/constrained) — struggles with collective action.
 *   - workers_in_monopsony_markets: Target (powerless/identity_locked) — vulnerable to exploitation.
 *   - market_libertarian_advocates: Agenda setter (institutional/analytical) — defines and promotes the framework.
 *   - regulatory_bodies: Excluded (institutional/constrained) — seen as illegitimate coercion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy: Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__market_libertarian_reading).
domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '613afea7-40c1-4ef6-a766-283f7211e19e').
narrative_ontology:cs_kernel_codification('613afea7-40c1-4ef6-a766-283f7211e19e', formalized).
narrative_ontology:cs_authority_grounding('613afea7-40c1-4ef6-a766-283f7211e19e', lineage).
narrative_ontology:cs_reading_relation('613afea7-40c1-4ef6-a766-283f7211e19e', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('613afea7-40c1-4ef6-a766-283f7211e19e', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('613afea7-40c1-4ef6-a766-283f7211e19e', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_axiom('613afea7-40c1-4ef6-a766-283f7211e19e', foundational, property_rights_are_pre_political).
narrative_ontology:cs_axiom_status(property_rights_are_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('613afea7-40c1-4ef6-a766-283f7211e19e', property_rights_are_pre_political, deontological).
narrative_ontology:cs_axiom('613afea7-40c1-4ef6-a766-283f7211e19e', foundational, voluntary_exchange_is_legitimacy_source).
narrative_ontology:cs_axiom_status(voluntary_exchange_is_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('613afea7-40c1-4ef6-a766-283f7211e19e', voluntary_exchange_is_legitimacy_source, deontological).
narrative_ontology:cs_reference_frame('613afea7-40c1-4ef6-a766-283f7211e19e', free_market_principles).
narrative_ontology:cs_drift_state('613afea7-40c1-4ef6-a766-283f7211e19e', contemporary_ai_governance_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('613afea7-40c1-4ef6-a766-283f7211e19e', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from minimal regulatory burden and clear property rights, allowing them to innovate and capture value without collective mandates. They can freely enter and exit markets based on opportunity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from stable property rights and voluntary exchange, which provide predictable conditions for capital allocation and return on investment in AI ventures. They can move capital to the most promising opportunities globally.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    institutional, generational, arbitrage, global).

% Experience dignity and freedom through abundant exit options and competitive markets, allowing them to choose employment, services, and lifestyles that align with their values without centralized control.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Bear the costs of market failures, lack of social safety nets, and limited bargaining power in competitive markets. Their dignity is often compromised by a lack of viable exit options from exploitative conditions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals, payer,
    powerless, immediate, trapped, local).

% Struggle to address collective action problems (e.g., environmental externalities, public goods provision) that AI development might exacerbate, as the market libertarian framework prioritizes individual rights over collective mandates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    organized, generational, constrained, local).

% Are vulnerable to exploitation due to limited employers and few alternatives, experiencing suppressed wages and poor working conditions. Their identity may be tied to their profession or location, making exit difficult despite market pressures.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets, payer,
    powerless, biographical, identity_locked, national).

% Actively promote and defend the principles of voluntary exchange and property rights as the foundation for legitimate AI governance, shaping policy debates and legal frameworks to resist collective mandates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, market_libertarian_advocates, agenda_setter,
    institutional, civilizational, analytical, global).

% Are viewed as illegitimate sources of coercion when they attempt to impose collective mandates or centralized oversight on AI development. Their authority is challenged by the market libertarian framework, limiting their ability to intervene.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, regulatory_bodies, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates innovation and efficient resource allocation in AI development by establishing clear property rights and enabling voluntary contracts, thereby minimizing transaction costs and the need for centralized planning.
% TRANSFER_FUNCTION: Transfers control and value from potential collective mandates and centralized oversight to individual actors, entrepreneurs, and investors, allowing them to capture the full value of their innovations and efforts.
% ABSENT_VOICES: Advocates for democratic deliberation, social solidarity, or magisterial authority in AI governance are structurally excluded, as their claims for collective mandates or common good are deemed illegitimate coercion within this framework.
% DISAPPEARANCE_RATIONALE: If the foundational principles of voluntary exchange and property rights vanished, the entire economic order, including AI innovation, would collapse. Capital allocation, investment, and contractual agreements would cease to function, leading to a complete reorganization of economic and social structures.
% FOUNDING_PROBLEM: The problem of inefficient resource allocation, stifled innovation, and infringement on individual liberty caused by centralized planning, collective mandates, and state intervention in economic affairs.
% FOUNDING_PROBLEM_CORROBORATION: Economists and legal scholars aligned with classical liberal and libertarian traditions corroborate this, citing historical examples of market failures due to intervention and the success of free-market economies. They argue that these problems are always present when markets are not sufficiently free.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects the reading's claim that market mechanisms are efficient and mutually beneficial, with costs primarily arising from necessary enforcement of property and contracts. Suppression (0.15) is low because it's not about overt coercion but the structural limitations imposed by market dynamics and legal frameworks. Theater ratio (0.05) is minimal, as the constraint is presented as a functional, natural order. The slight increases in metrics over the interval reflect the growing concentration of market power in the AI sector and the increasing need for enforcement to maintain the market order against calls for regulation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrepreneurs and investors, this framework is a pure 'rope' or 'mountain' that enables prosperity and innovation. From the perspective of low-market-power individuals or communities facing externalities, it operates as a 'snare' or 'tangled_rope' that extracts value and suppresses alternatives through structural market power and the enforcement of property rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy individuals are beneficiaries (low d) as the constraint directly enables their preferred mode of operation and value capture. Low-market-power individuals, communities, and workers are targets (high d) as they bear the costs of market failures and lack of collective action, with limited exit options. Market libertarian advocates are agenda-setters, actively shaping the discourse and legal environment to maintain this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a structurally extractive system as a 'natural law' by highlighting the identifiable beneficiaries and victims, and the active enforcement required. The 'mountain' claim is challenged by the presence of beneficiaries and the need for enforcement, triggering FSM detection. The low theater ratio suggests it's not a 'piton' but a actively defended (if subtly) framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_natural_vs_constructed,
    'Are property rights a pre-political natural law, or a social construct designed to facilitate economic organization?',
    'Philosophical and legal analysis of the historical development of property regimes, and cross-cultural comparison of legal systems that ground property differently.',
    'If property rights are a social construct, the constraint''s ''mountain'' claim is undermined, and its classification would shift towards a ''tangled_rope'' or ''snare'' depending on the degree of extraction and suppression involved in their enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_natural_vs_constructed, conceptual, 'Ambiguity of property rights as natural law vs. social construct.').

omega_variable(
    solidarity_as_coercion_ambiguity,
    'Is the encyclical''s solidarity principle genuinely illegitimate coercion, or a necessary coordination mechanism for the common good that market mechanisms fail to provide?',
    'Empirical analysis of social outcomes in jurisdictions with strong solidarity principles versus those with purely market-driven approaches, focusing on AI''s societal impacts.',
    'If solidarity is a necessary coordination, the market-libertarian reading''s rejection of it would be seen as a structural flaw, increasing its effective extractiveness and suppression for victims. If it is coercion, the reading''s claims are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_as_coercion_ambiguity, preference, 'Whether solidarity principles constitute illegitimate coercion or necessary coordination.').

omega_variable(
    market_failure_scope,
    'To what extent do competitive markets adequately address AI''s unique externalities and coordination failures (e.g., existential risk, bias, labor displacement) without collective mandates?',
    'Empirical studies and case analyses of AI''s societal impacts, identifying instances where market mechanisms alone have failed to mitigate harms or provide public goods related to AI.',
    'If market failures are widespread and severe, the constraint''s ''mountain'' claim of inherent efficiency is undermined, and its classification would shift towards a more extractive type, as it would be seen as failing to coordinate for the common good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_failure_scope, empirical, 'Adequacy of markets for AI''s unique challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 6, 0.03).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.03).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 18, 0.04).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 24, 0.04).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 18, 0.23).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 6, 0.11).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.12).
narrative_ontology:measurement(ai_g_su_t18, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 18, 0.13).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 24, 0.14).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
