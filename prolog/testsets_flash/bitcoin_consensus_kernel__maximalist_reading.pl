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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Reading: Immutable Monetary Policy
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'maximalist reading' of the Bitcoin
 *   consensus kernel, which asserts that the whitepaper establishes an
 *   immutable monetary policy and any deviation constitutes a violation of
 *   the founding covenant. This interpretation is actively enforced by a
 *   powerful ideological faction within the Bitcoin community, leading to
 *   high extraction from those seeking protocol innovation and high
 *   suppression of alternative development paths. The claimed type is 'snare'
 *   because the coordination story (trust-minimized money) serves as cover
 *   for the extraction of value from innovation and the suppression of
 *   dissent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.9).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, snare).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Reading: Immutable Monetary Policy").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, 'af959172-1967-4e15-9b6a-71a46c02b73a').
narrative_ontology:cs_kernel_codification('af959172-1967-4e15-9b6a-71a46c02b73a', fixed_text).
narrative_ontology:cs_authority_grounding('af959172-1967-4e15-9b6a-71a46c02b73a', lineage).
narrative_ontology:cs_interpretation_layer_present('af959172-1967-4e15-9b6a-71a46c02b73a').
narrative_ontology:cs_reading_relation('af959172-1967-4e15-9b6a-71a46c02b73a', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('af959172-1967-4e15-9b6a-71a46c02b73a', bitcoin_consensus_kernel__pragmatic_synthesis, forecloses).
narrative_ontology:cs_axiom('af959172-1967-4e15-9b6a-71a46c02b73a', foundational, monetary_policy_is_immutable).
narrative_ontology:cs_axiom_status(monetary_policy_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('af959172-1967-4e15-9b6a-71a46c02b73a', monetary_policy_is_immutable, deontological).
narrative_ontology:cs_axiom('af959172-1967-4e15-9b6a-71a46c02b73a', foundational, whitepaper_is_founding_covenant).
narrative_ontology:cs_axiom_status(whitepaper_is_founding_covenant, holdable).
narrative_ontology:cs_axiom_grounding('af959172-1967-4e15-9b6a-71a46c02b73a', whitepaper_is_founding_covenant, conventional).
narrative_ontology:cs_reference_frame('af959172-1967-4e15-9b6a-71a46c02b73a', satoshi_vision_immutable_money).
narrative_ontology:cs_drift_state('af959172-1967-4e15-9b6a-71a46c02b73a', contemporary_innovation_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('af959172-1967-4e15-9b6a-71a46c02b73a', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perceived scarcity and immutability of Bitcoin's monetary policy, which underpins their investment thesis. They actively resist any changes to the core protocol that might dilute this perceived value.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Similar to long-term holders, they benefit from the established narrative of Bitcoin's fixed supply and resistance to change, which has historically driven its value. They often hold significant influence in the community.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Actively promote and enforce the interpretation that the Bitcoin whitepaper establishes an immutable monetary policy, and any deviation is a violation of the 'founding covenant'. They exert social and technical pressure against protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, maximalist_ideologues, agenda_setter,
    organized, civilizational, identity_locked, global).

% Bear the cost of resistance and ideological purity tests when proposing or implementing changes aimed at improving scalability, privacy, or other features. Their work is often framed as 'violating the covenant' by maximalists.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_developers, payer,
    moderate, biographical, constrained, global).

% Develop and deploy solutions (e.g., Lightning Network, sidechains) to address Bitcoin's limitations. They face ideological opposition and technical hurdles imposed by the maximalist interpretation, which often views such layers as 'not real Bitcoin'.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers, payer,
    moderate, biographical, constrained, global).

% Are attracted to the promise of decentralized money but may be deterred by the lack of innovation, high transaction fees, or slow confirmation times resulting from the immutable policy. They often seek alternative cryptocurrencies with more flexible development paths.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_innovation, payer,
    powerless, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unchangeable monetary policy for Bitcoin, providing a predictable and trust-minimized store of value for its adherents.
% TRANSFER_FUNCTION: Transfers the power to influence monetary policy from any centralized or even decentralized human governance to a fixed, algorithmically enforced rule, benefiting those who hold the asset under this assumption.
% ABSENT_VOICES: Those who believe in iterative improvement, adaptive governance, or a more utility-focused vision for Bitcoin are often marginalized or driven to other protocols. Their voices are suppressed by the ideological enforcement of maximalism.
% DISAPPEARANCE_RATIONALE: If the maximalist interpretation of immutable monetary policy vanished overnight, the Bitcoin community would immediately fracture, leading to intense debates and likely hard forks over protocol changes. The asset's value proposition would fundamentally shift, and its role in the broader crypto ecosystem would be redefined.
% FOUNDING_PROBLEM: The problem of centralized control over money, inflationary monetary policies, and the erosion of purchasing power by traditional financial institutions.
% FOUNDING_PROBLEM_CORROBORATION: The problem of centralized monetary control is widely attested by economists, political scientists, and a broad public concerned about inflation and government overreach. This corroboration comes from outside the immediate maximalist community, validating the initial problem Bitcoin aimed to solve.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the maximalist interpretation effectively 'taxes' any attempt at protocol evolution or scalability, forcing developers and users to either conform or exit. Suppression is also very high (0.90) due to the intense ideological pressure, social ostracization, and technical resistance (e.g., refusal to adopt soft forks) against any perceived deviation from the 'original vision'. Theater ratio is low (0.10) because the enforcement is genuinely ideological and functional in maintaining the maximalist stance, rather than merely performative. The metrics show a clear trend of increasing extractiveness and suppression as the maximalist ideology solidified over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of long-term holders and maximalists, this constraint is a 'mountain' or 'rope' – a natural, unchangeable law or a pure coordination mechanism for sound money. From the perspective of developers and innovators, it operates as a 'snare', extracting value and suppressing progress under the guise of foundational principles. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and early adopters are clear beneficiaries, as their investment thesis relies on the immutability narrative. Maximalist ideologues act as agenda-setters, actively enforcing this interpretation. Protocol developers, scalability solution providers, and new users seeking innovation are victims, bearing the costs of stagnation and ideological purity tests. Their exit options are constrained by the network effects and liquidity of Bitcoin, making full exit costly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutable_vs_evolvable_protocol,
    'Is the Bitcoin protocol, as defined by the whitepaper, fundamentally immutable in its monetary policy, or is it an evolvable system designed for iterative improvement?',
    'Historical analysis of Satoshi Nakamoto''s later communications, community consensus shifts over time, and the long-term success/failure of protocols that embrace evolvability vs. immutability.',
    'If evolvable, the maximalist reading''s high extractiveness and suppression would be reclassified as illegitimate, potentially shifting the constraint towards a ''tangled_rope'' or ''piton'' as its foundational justification erodes. If truly immutable, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutable_vs_evolvable_protocol, conceptual, 'Ambiguity regarding the core nature of the Bitcoin protocol''s design intent.').

omega_variable(
    ideological_vs_technical_suppression,
    'To what extent is the observed suppression of innovation purely ideological, versus being a necessary technical consequence of maintaining a secure, decentralized base layer?',
    'Technical audits comparing the security and decentralization trade-offs of proposed innovations against the maximalist-approved status quo, and analysis of whether ideological arguments consistently align with demonstrable technical necessity.',
    'If primarily ideological, the suppression metric''s legitimacy would be further undermined, reinforcing the ''snare'' classification. If technically necessary, a portion of the suppression might be re-attributed to ''mountain'' or ''rope'' characteristics of the underlying technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_vs_technical_suppression, empirical, 'Distinguishing between ideological and technical drivers of suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2012, 0.07).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2018, 0.8).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2021, 0.83).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2012, 0.65).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2018, 0.82).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2021, 0.87).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.08).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, lightning_network_scalability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. The maximalist reading emphasizes immutability and fixed monetary policy, leading to high extraction from innovation. The utility reading (bitcoin_consensus_kernel__utility_reading) views the whitepaper as a minimum viable consensus mechanism for iterative improvement, and the pragmatic synthesis reading (bitcoin_consensus_kernel__pragmatic_synthesis) attempts to reconcile base layer immutability with upper layer innovation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
