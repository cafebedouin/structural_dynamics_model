% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Immutable Monetary Policy
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'maximalist' reading of the Bitcoin
 *   whitepaper, which asserts that the monetary policy and core protocol
 *   rules are immutable and any deviation constitutes a violation of the
 *   founding covenant. This interpretation is actively enforced by a powerful
 *   ideological faction within the Bitcoin community, leading to high
 *   extraction from those seeking protocol changes and high suppression of
 *   dissenting views. The claimed type is 'snare' because the coordination
 *   story (sound money) serves as cover for the extraction of value from
 *   innovation and accessibility, enforced through social and technical
 *   means.
 *
 * KEY AGENTS:
 *   - long_term_bitcoin_holders: Primary beneficiary (powerful/arbitrage) — benefits from perceived scarcity.
 *   - early_adopters: Primary beneficiary (organized/arbitrage) — benefits from maintaining original vision.
 *   - maximalist_ideologues: Agenda setter (institutional/identity_locked) — enforces immutability narrative.
 *   - protocol_developers_seeking_scalability: Primary victim (moderate/constrained) — bears costs of resistance to change.
 *   - new_users_facing_high_fees: Primary victim (powerless/constrained) — bears costs of limited scalability.
 *   - alternative_layer_innovators: Victim (organized/mobile) — constrained by base layer immutability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.92).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, snare).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Immutable Monetary Policy").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '811e220e-6675-4a1a-85a5-640156f6c5b1').
narrative_ontology:cs_kernel_codification('811e220e-6675-4a1a-85a5-640156f6c5b1', fixed_text).
narrative_ontology:cs_authority_grounding('811e220e-6675-4a1a-85a5-640156f6c5b1', lineage).
narrative_ontology:cs_interpretation_layer_present('811e220e-6675-4a1a-85a5-640156f6c5b1').
narrative_ontology:cs_reading_relation('811e220e-6675-4a1a-85a5-640156f6c5b1', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('811e220e-6675-4a1a-85a5-640156f6c5b1', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('811e220e-6675-4a1a-85a5-640156f6c5b1', foundational, whitepaper_as_immutable_covenant).
narrative_ontology:cs_axiom_status(whitepaper_as_immutable_covenant, holdable).
narrative_ontology:cs_axiom_grounding('811e220e-6675-4a1a-85a5-640156f6c5b1', whitepaper_as_immutable_covenant, deontological).
narrative_ontology:cs_axiom('811e220e-6675-4a1a-85a5-640156f6c5b1', secondary, protocol_change_as_dilution).
narrative_ontology:cs_axiom_status(protocol_change_as_dilution, holdable).
narrative_ontology:cs_axiom_grounding('811e220e-6675-4a1a-85a5-640156f6c5b1', protocol_change_as_dilution, instrumental).
narrative_ontology:cs_reference_frame('811e220e-6675-4a1a-85a5-640156f6c5b1', original_whitepaper_vision).
narrative_ontology:cs_drift_state('811e220e-6675-4a1a-85a5-640156f6c5b1', contemporary_scalability_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('811e220e-6675-4a1a-85a5-640156f6c5b1', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_developers_seeking_scalability).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users_facing_high_fees).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, alternative_layer_innovators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the maximalist reading effectively 'taxes' any deviation from the original protocol, forcing innovation onto less secure or less adopted layers, or simply suppressing it. Suppression (0.92) is extremely high due to the strong social and ideological pressure against protocol changes, often framing them as attacks on Bitcoin itself. The theater ratio (0.1) is low, indicating that the enforcement is genuinely aimed at maintaining the perceived immutability, not merely for show. Accessibility collapse is high (0.9) because the maximalist narrative makes it nearly impossible to conceive of alternatives to the core protocol's immutability within the Bitcoin ecosystem.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of maximalist ideologues and long-term holders, this constraint is a 'mountain' or 'rope' – a natural law of sound money or a necessary coordination mechanism for decentralization. From the perspective of developers and new users, it operates as a 'snare', extracting value and suppressing innovation under the guise of immutability. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and early adopters are clear beneficiaries, as the immutability narrative supports their investment thesis (low d). Maximalist ideologues are also beneficiaries, as their identity and influence are tied to this narrative, and they actively enforce it (low d, but identity_locked exit). Developers and new users are targets, bearing the costs of limited scalability and suppressed innovation (high d). Alternative layer innovators are also targets, as their work is often devalued by the maximalist narrative (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (sound, decentralized money) is still live, but the maximalist interpretation has led to a situation where the means (absolute immutability) have become an end in themselves, potentially hindering the original goal of accessible digital cash. This prevents mislabeling it as a simple 'rope' by highlighting the active extraction and suppression involved in maintaining this specific interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_utility,
    'Is the absolute immutability of Bitcoin''s monetary policy a foundational principle necessary for its value proposition, or is it a contingent design choice that can be optimized for broader utility?',
    'Long-term empirical observation of other decentralized monetary systems that allow for protocol evolution, and their comparative performance in terms of security, decentralization, and adoption.',
    'If immutability is found to be a contingent choice, the constraint''s extractiveness and suppression would be re-evaluated as unnecessary, potentially reclassifying it closer to a ''tangled_rope'' or ''scaffold'' that has overstayed its utility. If foundational, the ''snare'' classification would be reinforced as a necessary cost of the system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_vs_utility, conceptual, 'Ambiguity between immutability as a core principle versus a design choice.').

omega_variable(
    social_vs_technical_enforcement,
    'What proportion of the observed suppression against protocol changes is due to technical limitations of distributed consensus versus social and ideological pressure from the maximalist community?',
    'Analysis of ''forking'' events and their outcomes: if technical forks are consistently rejected despite community support, technical limitations dominate. If social pressure alone prevents technically feasible changes, social enforcement dominates.',
    'If social enforcement is the primary driver, the ''snare'' classification is strengthened, as it highlights the human-driven coercive element. If technical limitations are dominant, the constraint might lean more towards a ''mountain'' or ''rope'' that is genuinely hard to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_technical_enforcement, empirical, 'Distinguishing technical vs. social enforcement of immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, lightning_network_scalability_constraint).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, altcoin_innovation_suppression).

% DUAL FORMULATION NOTE:
% This constraint is the 'maximalist_reading' of the 'bitcoin_consensus_kernel'. It is linked to other readings of the same kernel, as well as downstream constraints affected by its strict interpretation of immutability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
