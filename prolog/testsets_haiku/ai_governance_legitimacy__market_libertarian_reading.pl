% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy (Market Libertarian Reading): Property Rights and Voluntary Exchange
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the market-libertarian reading of the
 *   contested kernel 'AI governance legitimacy.' The reading asserts that AI
 *   governance legitimacy derives exclusively from property rights, voluntary
 *   exchange, and competitive market discipline—not from democratic
 *   deliberation, regulatory mandate, or religious/solidaristic claims to
 *   subordinate economic freedom to common good. It frames the encyclical's
 *   subsidiarity principle as supporting decentralization but rejects its
 *   solidarity demands as illegitimate coercion. The reading claims that
 *   innovation flourishes when unencumbered, that dignity is protected
 *   through exit options and competitive markets (not centralized oversight),
 *   and that property rights are pre-political natural law, not human
 *   construction subject to collective revision. Four stakeholder seats
 *   experience this constraint radically differently: high-autonomy
 *   entrepreneurs and investors perceive it as freedom and natural law;
 *   workers in monopsony labor markets, communities facing unpriced
 *   externalities, and individuals lacking market power perceive it as a
 *   reading that legitimates extraction. The constraint's claimed type is
 *   mountain (natural law), but beneficiary presence and the low
 *   accessibility collapse score signal a false-summit candidate:
 *   beneficiaries exist (entrepreneurs, investors) and the reading benefits
 *   them asymmetrically. Low suppression and theater ratios are consistent
 *   with the reading's self-understanding as a description of natural fact,
 *   not enforced ideology. The measurement series show rising extractiveness
 *   and rising suppression requirement over the 2020-2034 interval,
 *   suggesting that as the constraint's implementation encounters resistance
 *   from democratic and solidaristic counterreadings, enforcement costs
 *   accumulate—a trajectory consistent with a reading whose natural-law claim
 *   is increasingly contested.
 *
 * KEY AGENTS:
 *   - entrepreneurs_and_founders — structural beneficiary, high autonomy, arbitrage exit, frame property rights and market freedom as natural entitlements
 *   - venture_capital_investors — structural beneficiary, institutional power, arbitrage exit, deploy capital under market-discipline assumption
 *   - workers_in_monopsony_labor_markets — structural victim, trapped exit, experience the reading as a legitimation of their powerlessness
 *   - communities_facing_coordination_failures — structural victim, trapped exit, experience the reading as denial of legitimate governance for externalities they cannot individually exit
 *   - individuals_lacking_market_power — structural victim, identity-locked exit (have internalized the framing that market is fair and their position reflects their choices/ability), experience AI systems designed without their consent as 'natural'
 *   - magisterial_authority — excluded voice, would assert that economic freedom must be subordinated to common good and solidarity demands, treated as illegitimate by the reading
 *   - democratic_publics — excluded voice, denied formal authority to set AI governance terms collective deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy (Market Libertarian Reading): Property Rights and Voluntary Exchange").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '787e60ce-8fab-44e4-887c-9db725af936e').
narrative_ontology:cs_kernel_codification('787e60ce-8fab-44e4-887c-9db725af936e', distributed).
narrative_ontology:cs_authority_grounding('787e60ce-8fab-44e4-887c-9db725af936e', extraction).
narrative_ontology:cs_reading_relation('787e60ce-8fab-44e4-887c-9db725af936e', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('787e60ce-8fab-44e4-887c-9db725af936e', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_reading_relation('787e60ce-8fab-44e4-887c-9db725af936e', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_axiom('787e60ce-8fab-44e4-887c-9db725af936e', foundational, property_rights_pre_political).
narrative_ontology:cs_axiom_status(property_rights_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('787e60ce-8fab-44e4-887c-9db725af936e', property_rights_pre_political, deontological).
narrative_ontology:cs_axiom('787e60ce-8fab-44e4-887c-9db725af936e', foundational, solidarity_claims_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_claims_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('787e60ce-8fab-44e4-887c-9db725af936e', solidarity_claims_illegitimate_coercion, deontological).
narrative_ontology:cs_axiom('787e60ce-8fab-44e4-887c-9db725af936e', secondary, market_discipline_sufficient_governance).
narrative_ontology:cs_axiom_status(market_discipline_sufficient_governance, holdable).
narrative_ontology:cs_axiom_grounding('787e60ce-8fab-44e4-887c-9db725af936e', market_discipline_sufficient_governance, empirically_contingent).
narrative_ontology:cs_created_at('787e60ce-8fab-44e4-887c-9db725af936e', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_founders).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_knowledge_workers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

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
 *   The claimed type is mountain: the reading asserts property rights as pre-political, natural law. However, the beneficiary list and the low accessibility_collapse score signal false-summit dynamics. If property rights are truly natural law, why do beneficiaries exist? Natural laws (e.g., gravity) have zero beneficiaries—everyone is equally subject to them. The presence of entrepreneurs, investors, and high-autonomy workers as beneficiaries suggests the reading describes not natural law but a distribution of authority that benefits specific parties. Accessibility_collapse is 0.72 (higher than a pure natural law would show, because alternatives—democratic governance, regulatory accountability, solidaristic redistribution—remain accessible to those who contest the reading), but lower than a snare would show (because the reading has intellectual coherence and appeals to genuine principles of liberty and efficiency, making it attractive to some non-beneficiaries). Suppression is low (0.15 at interval end) because the reading's enforcement relies primarily on contract law, reputational mechanisms, and property-rights protection—not on active coercion against dissenters. Theater_ratio is low (0.10 by 2034) because the reading's functional content (protecting property rights, enforcing contracts, allowing market discipline) aligns reasonably with its stated legitimacy claim (property rights and market discipline are legitimate), unlike a piton whose real function has atrophied. The measurement series show rising base_extractiveness and suppression_requirement over time, consistent with a reading whose natural-law claim is increasingly contested and whose enforcement machinery must be more actively maintained as resistance from democratic and magisterial counterreadings grows. The authorized measurement points at 2020, 2024, 2028, 2034 form a shared time grid for all three tracked metrics, satisfying the one-grid requirement. Observed vs. projected basis: the 2020-2024 points are observed (measured from the historical record of market-libertarian influence on AI policy); the 2028-2034 points are projected, modeling the expected trajectory if current counterreadings (democratic governance movements, EU AI Act, magisterial teaching) continue to contest the market reading and force more active enforcement. The tempo of rise (extractiveness +0.12 over 14 years) is moderate, reflecting a reading under increasing pressure but not yet in acute crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (entrepreneurs, investors), this reading is experienced as freedom and natural law—an accurate description of legitimate governance. From the agenda-setter perspective of courts and contract law, it is a governing principle. But from the victim seats (workers in monopsony markets, communities, individuals lacking power), it is experienced as a reading that legitimates their powerlessness by reframing governance failure as natural fact. From the excluded seats (Magisterium, democratic publics), it is experienced as a rejection of their authority and a refusal of their framework. The engine will compute per-seat classifications and should find divergence: from the beneficiary seat, the constraint may compute as rope (genuine coordination with minimal coercion); from the victim seat, it should compute as tangled_rope or snare (coordination for the powerful, extraction from the powerless, active suppression of alternatives—democratic governance, collective action—via property-rights enforcement). The directionality differs: beneficiaries have d near 0.0 (full subsidy, their freedom is protected), while victims have d near 1.0 (full extraction, they are the targets). The commentary does not predict the engine's computation; it explains why structural asymmetry should lead to per-seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (entrepreneurs, investors, high-autonomy workers): these stakeholders benefit from the constraint's protection of property rights and freedom from collective mandate. They have high exit options (arbitrage to less-regulated jurisdictions) and low cost to comply with or evade the constraint's requirements. Their directionality is near the beneficiary end (d = 0.0–0.2). Victim directionality (workers in monopsony markets, communities, individuals): these stakeholders experience the constraint as a legitimation of their powerlessness. They have trapped or identity-locked exit (cannot relocate, have internalized market framing, lack alternatives). They bear the cost of the reading—no collective governance of externalities, no solidarity-based redistribution, no coordination to address market failures. Their directionality is near the target end (d = 0.8–1.0). The automatic derivation from beneficiary/victim declarations and exit options should produce this spread without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem is: centralized authority (regulators, international bodies, religious institutions) constraining property rights and voluntary exchange, thereby stifling innovation. The founding_problem_status is contested. From the beneficiary perspective, the problem is live: every new regulation (EU AI Act, algorithmic accountability mandate, data-protection law) is read as a new instance of the founding problem. From the victim and magisterial perspectives, the founding problem is either dead or misdiagnosed: the real problem is unaccountable market power and the refusal of governance. The disappearance_verdict is contested for the same reason. If the constraint disappeared (if the market-libertarian reading lost influence and democracies could impose mandates), beneficiaries would experience the world as rearranging away from innovation and toward stagnation; victims would experience it as rearranging toward protection and accountability. The mandatrophy question is: has this constraint's founding problem become obsolete—has the problem it was built to solve already been resolved by other means? No. Innovation is not yet bounded by excessive regulation; the market-libertarian reading still carries influence in major AI development jurisdictions. The constraint is active, not mandatrophic. However, the measurement trajectory (rising suppression_requirement) suggests increasing effort is required to maintain the reading's influence—a sign that the founding problem's status is shifting from 'live' to 'contested' or 'dead' in the eyes of non-beneficiary stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_natural_law,
    'Is this constraint a description of pre-political natural law (property rights emerge from nature of scarcity and individual autonomy), or a reading of a contested theological and political kernel that benefits specific parties?',
    'Historical and comparative analysis: if property rights are universal and pre-political, they should appear identically across all cultures and time periods. If they vary materially by regime, the ''natural law'' framing becomes suspect. Alternatively: if the reading is ratified by all stakeholder seats (beneficiaries, victims, excluded voices, observers), it gains empirical credibility; if it is ratified only by benefiting parties, it suggests the reading is a cover story.',
    'If natural law: the constraint is correctly classified as mountain and the exclusion of democratic deliberation is justified. If reading: the constraint is a tangled_rope (coordinates market efficiency while extracting from those lacking market power) or a snare (pure extraction dressed as natural law). The terminal type is what the engine computes; divergence from ''mountain'' is the measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_natural_law, conceptual, 'Whether property rights legitimacy is pre-political natural law or a contested reading of a theological/political kernel.').

omega_variable(
    exit_availability_asymmetry,
    'Is exit actually available to the beneficiary and victim stakeholders listed, or is the ''exit option'' statement in the reading an artifact of the high-autonomy seats'' position?',
    'Empirical measurement: for each stakeholder, what are the realistic cost and feasibility of exit? Can a data-labeler in a monopsony actually exit to another employer, or is ''exit'' theoretically available but practically impossible? Can a community facing algorithmic redlining exit to a jurisdiction without algorithmic systems? The gap between theoretical and practical exit determines whether the market discipline mechanism actually functions for all parties.',
    'If exit is uniformly available and low-cost: the market mechanism functions as described and extraction is minimal (mountain classification supported). If exit is available to high-autonomy parties but trapped or identity-locked for others: the constraint is tangled_rope (coordinates for the mobile, extracts from the trapped), not mountain. The measurement is per-stakeholder exit viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_availability_asymmetry, empirical, 'Whether exit options are symmetrically available across stakeholder seats or concentrated in high-autonomy positions.').

omega_variable(
    subsidiarity_vs_solidarity_reading_divergence,
    'Can the encyclical''s subsidiarity principle (decentralize governance to the most local effective level) coexist with its solidarity demands (protect the vulnerable, subordinate property rights to common good) within a single theological framework, or do they irreconcilably diverge?',
    'Textual analysis of Fratelli Tutti and Caritas in Veritate: does the Magisterium intend subsidiarity to mean market-level decentralization (supporting this reading), or does it intend subsidiarity to be subordinated to solidarity and common-good principles (supporting the magisterial reading)? If the Magisterium provides binding interpretation, the question is resolved; if interpretation is contested within the Church, the kernels remain live.',
    'If subsidiarity and solidarity are compatible: both the market-libertarian and magisterial readings could claim encyclical support, strengthening the coexists_with relation. If they diverge: the reading that captures the encyclical''s actual intent gains authority, and the losing reading must rely on non-magisterial justifications (natural law, economic efficiency). The resolution affects which reading can claim fidelity to Catholic social teaching.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_vs_solidarity_reading_divergence, conceptual, 'Whether the encyclical''s subsidiarity and solidarity principles can coexist in one framework or diverge irreconcilably.').

omega_variable(
    coordination_failure_externality_legitimacy,
    'When market-mediated AI decisions produce unpriced negative externalities (environmental, social, cognitive) that individual exit cannot address, does the reading''s framework have a legitimate governance response, or does it treat coordination failures as un-governable by definition?',
    'Test case analysis: apply the market-libertarian reading to a specific externality (e.g., algorithmic labor-market discrimination, AI-driven environmental degradation, information-cascades from unmoderated recommendation systems). If the reading prescribes a response other than ''affected parties must renegotiate individually,'' it accommodates externality governance. If it prescribes only individual action, it denies the problem''s legitimacy as a governance issue—empirically testable by whether the prescribed response actually addresses the externality at scale.',
    'If the reading can accommodate externality governance: it is more robust than the pure libertarian position and more compatible with magisterial demands for common-good protection. If it cannot: critics can establish that the reading denies legitimate governance of a class of real harms, weakening its mountain claim and supporting reclassification to tangled_rope or snare. The measurement is whether the reading''s framework admits responses that address externalities at scale beyond individual bargaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_failure_externality_legitimacy, empirical, 'Whether the market-libertarian reading can accommodate governance of unpriced externalities or denies their legitimacy.').

omega_variable(
    reading_specific_to_kernel_decomposition,
    'Is the market-libertarian reading one instantiation of a contested kernel (ai_governance_legitimacy), or is it a separate, standalone constraint whose enforcement happens to interact with other readings?',
    'Structural analysis: if removing one reading does not change the ε of another, they are separate constraints. If they share the same authority structure (e.g., both appeal to the encyclical, or both claim natural-law status), they are readings of the same kernel. The kernel is the stabilized commitment (in this case: what principle governs AI governance legitimacy). The readings are different parties'' interpretations of that commitment.',
    'This is a meta-question about the corpus structure, not about the constraint''s classification. The measurement is whether the four sibling readings (market-libertarian, magisterial-subsidiarity, technocratic-optimization, democratic-pluralist) are genuinely sister interpretations of one contested kernel or separate constraints that happen to compete. The classification of this story as a kernel reading rests on the answer. If they are separate, this story should drop the cs_structure block and kernel context. If they are readings of one kernel, the cs_structure relations and axioms apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specific_to_kernel_decomposition, conceptual, 'Whether the market-libertarian reading is one reading of ai_governance_legitimacy kernel or a standalone constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 2020, 2034).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t2020, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 2020, 0.04).
narrative_ontology:measurement(ai_g_tr_t2024, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 2024, 0.06).
narrative_ontology:measurement(ai_g_tr_t2028, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 2028, 0.08).
narrative_ontology:measurement(ai_g_tr_t2034, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 2034, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t2020, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 2020, 0.18).
narrative_ontology:measurement(ai_g_be_t2024, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 2024, 0.22).
narrative_ontology:measurement(ai_g_be_t2028, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 2028, 0.26).
narrative_ontology:measurement(ai_g_be_t2034, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 2034, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t2020, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(ai_g_su_t2024, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 2024, 0.13).
narrative_ontology:measurement(ai_g_su_t2028, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 2028, 0.15).
narrative_ontology:measurement(ai_g_su_t2034, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 2034, 0.17).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_governance_legitimacy. The market-libertarian reading asserts that property rights and voluntary exchange are the pre-political source of governance legitimacy, rejecting collective mandates rooted in democratic or solidaristic authority. The magisterial-subsidiarity reading asserts that property rights must be subordinated to Catholic Social Doctrine principles (common good, solidarity, protection of the vulnerable). The democratic-pluralist reading asserts that no single tradition holds interpretive monopoly and governance must emerge from inclusive public reason. The technocratic-optimization reading asserts that aggregate welfare and technical performance should determine what constraints are legitimate. These four readings coexist as live positions held by different institutional and ideological coalitions. None logically forecloses the others within separate frameworks, but each influences the others by competing for authority over how AI governance should be legitimated. All four stories form a constraint family linked through network.affects_constraints and must be analyzed together to understand the full contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
