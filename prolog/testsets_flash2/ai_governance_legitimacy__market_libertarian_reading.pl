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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This constraint represents the market libertarian reading of AI
 *   governance legitimacy, which posits that legitimacy derives from
 *   voluntary exchange and property rights. It asserts that innovation
 *   flourishes when unencumbered by collective mandates and that dignity is
 *   protected through exit options and competitive markets, not centralized
 *   oversight. It interprets the encyclical's subsidiarity principle as
 *   supporting decentralization but rejects its solidarity demands as
 *   illegitimate coercion. This is one reading of the 'AI governance
 *   legitimacy' kernel, distinct from magisterial, technocratic, or
 *   democratic pluralist readings.
 *
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

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'dd0c87b8-3da1-4be1-8f61-cd60694fb361').
narrative_ontology:cs_kernel_codification('dd0c87b8-3da1-4be1-8f61-cd60694fb361', formalized).
narrative_ontology:cs_authority_grounding('dd0c87b8-3da1-4be1-8f61-cd60694fb361', self_enforcing).
narrative_ontology:cs_reading_relation('dd0c87b8-3da1-4be1-8f61-cd60694fb361', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('dd0c87b8-3da1-4be1-8f61-cd60694fb361', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd0c87b8-3da1-4be1-8f61-cd60694fb361', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_axiom('dd0c87b8-3da1-4be1-8f61-cd60694fb361', foundational, property_rights_as_pre_political).
narrative_ontology:cs_axiom_status(property_rights_as_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('dd0c87b8-3da1-4be1-8f61-cd60694fb361', property_rights_as_pre_political, deontological).
narrative_ontology:cs_axiom('dd0c87b8-3da1-4be1-8f61-cd60694fb361', foundational, voluntary_exchange_as_primary_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_exchange_as_primary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('dd0c87b8-3da1-4be1-8f61-cd60694fb361', voluntary_exchange_as_primary_legitimacy, deontological).
narrative_ontology:cs_reference_frame('dd0c87b8-3da1-4be1-8f61-cd60694fb361', classical_liberal_order).
narrative_ontology:cs_drift_state('dd0c87b8-3da1-4be1-8f61-cd60694fb361', contemporary_ai_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dd0c87b8-3da1-4be1-8f61-cd60694fb361', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, free_market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, individual_liberty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from minimal regulatory burdens, allowing rapid innovation and market entry without needing to navigate complex collective mandates. Their dignity is protected by their ability to exit unfavorable markets or regulatory regimes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    powerful, biographical, mobile, global).

% Seek high returns in unencumbered markets, viewing collective mandates as risks that reduce investment opportunities. They benefit from the free flow of capital and minimal oversight.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Value personal freedom and the ability to make choices in competitive markets, including their engagement with AI technologies. They see dignity as deriving from self-ownership and voluntary association, not from protective oversight.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Bear the costs of market failures, lack of collective protection, and potential exploitation in competitive markets where they have limited bargaining power or exit options. Their dignity is vulnerable to market forces.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals, payer,
    powerless, immediate, constrained, local).

% Struggle to address collective action problems related to AI's societal impacts (e.g., environmental externalities, algorithmic bias, public safety) without centralized mandates or regulatory frameworks. Their ability to coordinate for common good is undermined.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    powerless, generational, trapped, local).

% Are vulnerable to AI-driven automation and algorithmic management in labor markets dominated by a few large employers. Without collective bargaining or regulatory protections, their wages, working conditions, and job security are dictated by market forces.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets, payer,
    powerless, immediate, constrained, regional).

% The Catholic Magisterium, which articulates principles like solidarity and the common good, is viewed as an illegitimate source of coercive mandates in AI governance by this reading. Its claims are rejected as infringing on economic freedom.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, identity_locked, global).

% Democratic processes that seek to establish collective mandates for AI governance are seen as illegitimate coercion, infringing on individual property rights and voluntary exchange. Their role in defining the common good is rejected.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_institutions, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economic activity and innovation in the AI sector by establishing clear property rights and enforcing contracts, allowing individuals and firms to pursue their interests through voluntary exchange.
% TRANSFER_FUNCTION: Transfers wealth and innovation benefits to entrepreneurs, investors, and high-autonomy individuals by minimizing regulatory costs and maximizing market freedom. It transfers the risks and costs of market failures to those with less market power.
% ABSENT_VOICES: Those advocating for collective mandates, social solidarity, or democratic oversight in AI governance are excluded from the legitimate discourse, as their proposals are framed as illegitimate coercion. This includes proponents of Catholic Social Doctrine's solidarity principle and democratic pluralists.
% DISAPPEARANCE_RATIONALE: This reading asserts that its principles (property rights, voluntary exchange) are fundamental, pre-political aspects of reality. If the 'constraint' of market libertarianism vanished, it would simply mean a return to a more fundamental, natural order of individual liberty and market dynamics, not a rearrangement of the world.
% FOUNDING_PROBLEM: The problem of ensuring individual liberty, protecting property rights, and fostering innovation by preventing illegitimate state or collective interference in economic and technological development.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (e.g., libertarian think tanks, certain tech entrepreneurs) attest that the problem of over-regulation and threats to individual liberty is perpetually live. Critics (e.g., social ethicists, labor advocates) argue that the 'problem' is a cover for rent-seeking and that the real problem is market failure and inequality, but within this reading's framework, the founding problem is considered self-evident and ongoing.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_unchanged).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The constraint is claimed as a Mountain because it asserts property rights and voluntary exchange as pre-political, natural laws. Extractiveness is low (0.25) because, by its own lights, it minimizes 'illegitimate' transfers and maximizes individual benefit. Suppression is low (0.15) as it primarily relies on contract law and private enforcement, not state coercion, to maintain its order. Accessibility collapse is high (0.88) because it views alternatives to market-based governance as fundamentally flawed or illegitimate. Resistance is low (0.10) because, within this framework, opposition to market principles is seen as misguided rather than legitimate. The metrics are stable over time, reflecting the reading's assertion of timeless principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a natural, efficient order. From the perspective of victims, it is a system that generates inequality and vulnerability. However, this reading frames these outcomes as natural consequences of individual liberty and market dynamics, not as extraction or suppression by the constraint itself. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy individuals are beneficiaries, as the constraint minimizes burdens and maximizes their freedom and potential for gain. Low-market-power individuals, communities facing coordination failures, and workers in monopsony labor markets are victims, as they bear the costs of market failures and lack of collective protection. Magisterial authority and democratic institutions are excluded, as their claims to legitimate collective mandates are rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy analysis by framing its principles as timeless and pre-political. The 'founding problem' (preventing illegitimate interference) is considered perpetually live, ensuring the constraint's continued justification. The classification prevents mislabeling by acknowledging the internal coherence of this 'mountain' claim while allowing the engine to detect its extractive effects on other seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Are property rights and voluntary exchange truly pre-political natural laws (Mountain), or are they social constructs (Snare/Tangled Rope) that benefit identifiable agents?',
    'Historical and anthropological analysis of diverse societies'' economic organization, and philosophical debate on the foundations of rights. If shown to be contingent social constructs, the constraint''s ''naturalness'' claim collapses.',
    'If property rights are social constructs, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), as its ''naturalness'' cover story would be revealed as a justification for asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the ontological status of market libertarian principles.').

omega_variable(
    coordination_failure_externalities,
    'Does the market libertarian framework adequately address AI-related coordination failures and negative externalities (e.g., algorithmic bias, environmental impact, systemic risks) without collective mandates?',
    'Empirical studies of AI''s societal impacts in unregulated or minimally regulated markets, comparing outcomes with jurisdictions employing collective mandates. Analysis of the efficacy of private solutions (e.g., self-regulation, private arbitration) for systemic problems.',
    'If market mechanisms consistently fail to address systemic AI risks, the ''efficiency'' and ''dignity protection'' claims of this reading would be undermined, increasing its effective extractiveness and potentially reclassifying it as a Snare or Tangled Rope due to unaddressed harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_failure_externalities, empirical, 'Whether market mechanisms alone can resolve AI''s collective action problems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_innovation_policy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, data_privacy_regulation__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI governance legitimacy' kernel. Its principles directly influence specific policy constraints like AI innovation and data privacy, which would also be interpreted through a market libertarian lens.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
