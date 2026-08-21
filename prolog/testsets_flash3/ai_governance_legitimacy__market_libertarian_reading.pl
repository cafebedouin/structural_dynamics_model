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
 *   This constraint represents a market-libertarian reading of AI governance
 *   legitimacy, asserting that voluntary exchange and property rights are the
 *   foundational principles. It claims that innovation flourishes when
 *   unencumbered by collective mandates and that dignity is protected through
 *   exit options and competitive markets, not centralized oversight. It
 *   selectively endorses the subsidiarity principle from Catholic Social
 *   Doctrine for decentralization but rejects solidarity demands as
 *   illegitimate coercion. This is one reading of the 'AI governance
 *   legitimacy' kernel, distinct from magisterial, technocratic, or
 *   democratic readings.
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
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '0f033ea6-8d02-461c-bc68-7328e73c4e0a').
narrative_ontology:cs_kernel_codification('0f033ea6-8d02-461c-bc68-7328e73c4e0a', implicit).
narrative_ontology:cs_authority_grounding('0f033ea6-8d02-461c-bc68-7328e73c4e0a', self_enforcing).
narrative_ontology:cs_reading_relation('0f033ea6-8d02-461c-bc68-7328e73c4e0a', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f033ea6-8d02-461c-bc68-7328e73c4e0a', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f033ea6-8d02-461c-bc68-7328e73c4e0a', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('0f033ea6-8d02-461c-bc68-7328e73c4e0a', foundational, property_rights_as_pre_political).
narrative_ontology:cs_axiom_status(property_rights_as_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('0f033ea6-8d02-461c-bc68-7328e73c4e0a', property_rights_as_pre_political, deontological).
narrative_ontology:cs_axiom('0f033ea6-8d02-461c-bc68-7328e73c4e0a', foundational, voluntary_exchange_as_dignity_protection).
narrative_ontology:cs_axiom_status(voluntary_exchange_as_dignity_protection, holdable).
narrative_ontology:cs_axiom_grounding('0f033ea6-8d02-461c-bc68-7328e73c4e0a', voluntary_exchange_as_dignity_protection, instrumental).
narrative_ontology:cs_reference_frame('0f033ea6-8d02-461c-bc68-7328e73c4e0a', unencumbered_market_order).
narrative_ontology:cs_drift_state('0f033ea6-8d02-461c-bc68-7328e73c4e0a', contemporary_regulatory_proposals, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0f033ea6-8d02-461c-bc68-7328e73c4e0a', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, free_market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, individual_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from minimal regulation, freedom to innovate, and the ability to capture value from their creations without collective mandates. They see market mechanisms as the most efficient allocators of resources and innovation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    powerful, biographical, mobile, global).

% Seek environments where capital can flow freely, property rights are secure, and returns are not diminished by taxes or regulations aimed at redistribution or collective mandates. They can move capital to jurisdictions that align with this reading.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    institutional, generational, arbitrage, global).

% Value personal freedom, self-reliance, and the ability to make choices unencumbered by state or collective oversight. They believe their dignity is best protected by their ability to exit undesirable situations through competitive markets.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Bear the costs of market failures, lack of social safety nets, and the inability to negotiate fair terms in highly concentrated markets. Their dignity is often compromised by a lack of genuine exit options and dependence on market forces.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, low_market_power_individuals, payer,
    powerless, immediate, constrained, local).

% Struggle to address collective action problems (e.g., environmental degradation, public goods provision) that markets alone cannot solve, leading to diffuse costs and reduced welfare. They lack the centralized authority to impose collective mandates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    organized, generational, constrained, local).

% Face limited employment options and suppressed wages due to a single dominant employer or a cartel of employers. Their ability to exit is severely constrained, leading to exploitation that market mechanisms fail to correct.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets, payer,
    powerless, immediate, trapped, regional).

% Its claims to define the common good and impose solidarity demands are rejected as illegitimate coercion by this reading. It is excluded from setting the terms of AI governance within this framework, despite its own claims to moral authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, identity_locked, global).

% Their attempts to impose collective mandates or centralized oversight on AI development are viewed as infringements on property rights and voluntary exchange, and thus illegitimate. They are excluded from the primary locus of governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_legislators, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates innovation and resource allocation through decentralized market signals, voluntary contracts, and individual choices, minimizing the need for centralized planning or collective mandates.
% TRANSFER_FUNCTION: Transfers wealth and innovation benefits to those who successfully navigate competitive markets and secure property rights, while externalizing costs onto those lacking market power or facing coordination failures.
% ABSENT_VOICES: Those advocating for collective mandates, social solidarity, or democratic oversight are excluded from the legitimate discourse on AI governance, as their proposals are deemed coercive and illegitimate infringements on individual liberty and property rights.
% DISAPPEARANCE_RATIONALE: This reading posits that market mechanisms and property rights are fundamental, pre-political realities. If the 'constraint' of market libertarian legitimacy vanished, it would simply mean a return to a more 'natural' state of voluntary exchange, not a collapse of a constructed order. Any attempt to impose collective mandates would be seen as an artificial imposition, not a necessary replacement.
% FOUNDING_PROBLEM: The problem of inefficient resource allocation, stifled innovation, and infringement on individual liberty caused by centralized planning, state intervention, and collective mandates.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including think tanks and advocacy groups, consistently argue that the threat of over-regulation and collectivism remains live, citing ongoing legislative efforts and international proposals for AI governance. Critics, however, argue that the 'problem' is a mischaracterization designed to justify unchecked market power.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_unchanged).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.25) because this reading frames market outcomes as natural and voluntary, thus not 'extracting' in the conventional sense, but rather reflecting fair exchange. Any 'costs' borne by victims are attributed to their lack of market power or poor choices, not to the constraint itself. Suppression is low (0.15) as the constraint is seen as a natural order, enforced primarily through contract law and private mechanisms, not overt coercion. Theater ratio is negligible (0.05) because the principles are held as fundamental truths, not performative. Accessibility collapse is high (0.88) because property rights and market logic are presented as universally applicable and unavoidable. Resistance is low (0.1) because any resistance is framed as illegitimate interference with natural rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a natural law (mountain) that enables prosperity and freedom. From the perspective of victims, it is a system that legitimizes their exploitation and disempowerment, effectively a snare. The engine's classification will highlight this divergence, as the claimed 'mountain' status will be challenged by the presence of identifiable victims and the low, but non-zero, extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy individuals are beneficiaries, as the constraint's operation directly aligns with their interests in freedom from regulation and wealth accumulation. Low-market-power individuals, communities facing coordination failures, and workers in monopsony markets are victims, bearing the costs of market failures and power imbalances that this reading deems legitimate or natural. Magisterial authority and democratic legislators are excluded, as their claims to impose collective mandates are rejected as illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, by framing property rights and voluntary exchange as pre-political and natural, inherently resists mandatrophy analysis. Its mandate is seen as eternal and self-justifying. The classification prevents mislabeling by highlighting the tension between the 'mountain' claim and the existence of victims, suggesting a potential 'false summit' where a constructed order is presented as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Are property rights and voluntary exchange truly pre-political natural laws, or are they social constructs that benefit identifiable groups?',
    'Historical and anthropological analysis of diverse societies, examining the emergence and enforcement of property regimes and market structures across different cultural and legal contexts.',
    'If found to be social constructs, the constraint''s ''mountain'' claim would be reclassified, likely as a ''tangled_rope'' or ''snare'', reflecting its constructed nature and the beneficiaries it serves. This would fundamentally alter the legitimacy of its claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the naturalness of market-libertarian principles.').

omega_variable(
    coercion_vs_voluntary_exchange,
    'At what point does a lack of genuine exit options in concentrated markets transform ''voluntary exchange'' into a form of structural coercion?',
    'Empirical studies of labor mobility, market concentration, and consumer choice in specific AI-related sectors, coupled with legal analysis of anti-trust enforcement and worker protections.',
    'If structural coercion is identified, the ''suppression'' and ''extractiveness'' metrics would be re-evaluated upwards, and the constraint would be reclassified from ''mountain'' to a more extractive type like ''snare'', particularly for victims with limited exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_voluntary_exchange, empirical, 'Distinction between voluntary exchange and structural coercion in market contexts.').

omega_variable(
    subsidiarity_solidarity_tension,
    'Is the rejection of solidarity demands as ''illegitimate coercion'' a consistent application of subsidiarity, or a selective interpretation that serves market interests?',
    'Comparative theological and ethical analysis of Catholic Social Doctrine, examining the historical and doctrinal relationship between subsidiarity and solidarity, and how different traditions interpret their interplay in economic and social policy.',
    'If found to be a selective interpretation, the intellectual coherence of this reading would be weakened, potentially leading to a re-evaluation of its ''mountain'' claim and its ability to withstand conceptual challenges from other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_tension, conceptual, 'Consistency of market-libertarian interpretation of Catholic Social Doctrine.').


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
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
