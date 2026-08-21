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
 *   consensus kernel, where the whitepaper is interpreted as establishing an
 *   immutable monetary policy and any deviation is seen as a violation of a
 *   founding covenant. This reading prioritizes absolute scarcity and
 *   resistance to change, often at the expense of scalability and innovation.
 *   The high extractiveness reflects the costs imposed on those seeking to
 *   evolve the protocol, while high suppression indicates the strong
 *   ideological and social pressure against such changes. The claimed type is
 *   'snare' because the coordination story (immutable money) serves as cover
 *   for the extraction of value from those who would innovate or scale the
 *   protocol, with identifiable victims.
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
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '4b8dfc8e-b08a-44a9-9956-7fe97e194948').
narrative_ontology:cs_kernel_codification('4b8dfc8e-b08a-44a9-9956-7fe97e194948', fixed_text).
narrative_ontology:cs_authority_grounding('4b8dfc8e-b08a-44a9-9956-7fe97e194948', lineage).
narrative_ontology:cs_interpretation_layer_present('4b8dfc8e-b08a-44a9-9956-7fe97e194948').
narrative_ontology:cs_reading_relation('4b8dfc8e-b08a-44a9-9956-7fe97e194948', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('4b8dfc8e-b08a-44a9-9956-7fe97e194948', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('4b8dfc8e-b08a-44a9-9956-7fe97e194948', foundational, whitepaper_as_immutable_covenant).
narrative_ontology:cs_axiom_status(whitepaper_as_immutable_covenant, holdable).
narrative_ontology:cs_axiom_grounding('4b8dfc8e-b08a-44a9-9956-7fe97e194948', whitepaper_as_immutable_covenant, deontological).
narrative_ontology:cs_axiom('4b8dfc8e-b08a-44a9-9956-7fe97e194948', foundational, fixed_supply_as_sacred_principle).
narrative_ontology:cs_axiom_status(fixed_supply_as_sacred_principle, holdable).
narrative_ontology:cs_axiom_grounding('4b8dfc8e-b08a-44a9-9956-7fe97e194948', fixed_supply_as_sacred_principle, deontological).
narrative_ontology:cs_reference_frame('4b8dfc8e-b08a-44a9-9956-7fe97e194948', satoshi_vision_immutable_protocol).
narrative_ontology:cs_drift_state('4b8dfc8e-b08a-44a9-9956-7fe97e194948', contemporary_scalability_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b8dfc8e-b08a-44a9-9956-7fe97e194948', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users_with_high_fees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perceived scarcity and immutability of Bitcoin's monetary policy, which they believe drives its value. They resist any changes that could dilute this narrative or introduce inflationary pressures, as their wealth is tied to this specific interpretation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Similar to long-term holders, they benefit from the established narrative of Bitcoin's fixed supply and resistance to change. Their early entry gives them significant influence in the community, which they often leverage to oppose protocol modifications.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Actively promote and defend the maximalist interpretation of Bitcoin's founding principles, viewing any deviation as a betrayal of its core value proposition. They exert influence through social media, forums, and developer communities, often framing dissent as an attack on Bitcoin itself.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, maximalist_ideologues, agenda_setter,
    organized, civilizational, identity_locked, global).

% Bear the cost of extreme resistance to protocol changes, even those aimed at improving scalability or functionality. Their proposals are often met with ideological opposition, making it difficult to implement necessary updates without risking community schism or accusations of violating the 'covenant'.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_developers, payer,
    moderate, biographical, constrained, global).

% Attempt to build solutions on top of Bitcoin's base layer to address its limitations, but face constant scrutiny and accusations of 'violating' the core principles if their solutions require any base-layer adjustments or are perceived as centralizing. This limits their ability to innovate and gain widespread adoption.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_providers, payer,
    moderate, biographical, constrained, global).

% Experience high transaction fees and slow confirmation times due to the base layer's limited capacity, a direct consequence of the resistance to scalability improvements. They have little power to influence protocol development and are effectively trapped by the existing constraints if they wish to use Bitcoin.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, new_users_with_high_fees, payer,
    powerless, immediate, constrained, global).

% Advocate for a more flexible approach where the base layer remains stable but upper layers are allowed to innovate. They are often marginalized or dismissed by maximalists, despite offering solutions to real-world problems faced by users and developers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unchanging monetary policy that coordinates expectations around Bitcoin's scarcity and resistance to inflation, fostering long-term trust and investment.
% TRANSFER_FUNCTION: Transfers the power to influence monetary policy and protocol evolution from a broad base of stakeholders to a specific ideological faction and early adopters, in exchange for a narrative of absolute scarcity and immutability.
% ABSENT_VOICES: Advocates for more flexible protocol development, those prioritizing scalability and utility over absolute immutability, and new users who bear the costs of limited capacity are often excluded from the core decision-making processes, their concerns dismissed as not understanding 'the Bitcoin way'.
% DISAPPEARANCE_RATIONALE: If the maximalist reading of immutable monetary policy vanished overnight, the Bitcoin protocol would likely undergo significant changes to address scalability and functionality. This would fundamentally alter its value proposition, potentially leading to a re-evaluation of its role in the global financial system and a shift in power dynamics within the community.
% FOUNDING_PROBLEM: The problem of centralized control over monetary policy, inflation, and censorship inherent in traditional financial systems.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist ideologues and long-term holders attest that the problem of centralized control is still live, and the immutable monetary policy is the only solution. Pragmatic synthesis advocates and protocol developers acknowledge the original problem but argue that the maximalist reading has created new problems (scalability, innovation stagnation) that undermine the original goal of a usable, decentralized currency.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the maximalist interpretation effectively 'taxes' any attempt at protocol evolution or scalability, forcing developers and users to either conform or exit. Suppression is very high due to the intense social and ideological pressure exerted by maximalists, often leading to 'cancel culture' or ostracization for those proposing changes. The low theater ratio indicates that the enforcement of this immutability is largely genuine, driven by strong belief, rather than mere performance. Accessibility collapse is near total for fundamental changes, as the ideological barriers are immense. Resistance is high from developers and users who experience the limitations, but this resistance is often met with overwhelming counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the maximalist perspective, this is a 'mountain' or 'rope' – a natural law of sound money or a pure coordination mechanism for trust. From the perspective of developers and new users, it operates as a 'snare', extracting value and suppressing innovation under the guise of immutable principles. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and early adopters are clear beneficiaries, as their investment thesis relies on this immutability. Maximalist ideologues act as agenda-setters, actively enforcing this interpretation. Protocol developers, scalability solution providers, and new users facing high fees are victims, bearing the costs of stagnation. Pragmatic synthesis advocates are excluded, their alternative readings marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by showing that while the original problem (centralized monetary control) is still live, the maximalist solution has created new problems (scalability, innovation stagnation) that are themselves extractive. The persistence of the constraint is not due to its continued optimal function for all parties, but due to the concentrated benefits for early adopters and the strong ideological enforcement by maximalists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_ambiguity,
    'Who holds legitimate interpretive authority over the Bitcoin whitepaper and protocol rules?',
    'Emergence of a widely recognized, neutral arbitration body or a clear, community-wide voting mechanism for protocol changes.',
    'If interpretive authority is centralized or clearly defined, the constraint might shift from a ''snare'' (enforced by diffuse ideological pressure) to a ''tangled_rope'' (formalized governance with extraction). If it remains diffuse, the ''snare'' classification holds due to the power of ideological factions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_ambiguity, conceptual, 'Ambiguity over who has the final say on protocol interpretation.').

omega_variable(
    scalability_necessity_empirical,
    'To what extent are base-layer protocol changes empirically necessary to achieve widespread, low-cost Bitcoin usage, versus being solvable by off-chain solutions?',
    'Long-term empirical data on transaction fees, network congestion, and adoption rates in various economic contexts, alongside the performance of off-chain solutions.',
    'If base-layer changes are empirically proven necessary for widespread utility, the maximalist reading''s suppression of such changes becomes a clearer case of extraction. If off-chain solutions prove sufficient, the maximalist stance appears more aligned with a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_necessity_empirical, empirical, 'Empirical necessity of base-layer changes for scalability.').

omega_variable(
    identity_lock_strength,
    'How deeply is the ''maximalist'' identity fused with the Bitcoin protocol, and how would a shift in interpretation affect individual and group identity?',
    'Sociological studies of Bitcoin communities, analysis of exit patterns from maximalist groups, and psychological profiling of core ideologues.',
    'If identity-lock is extremely strong, the suppression mechanism is highly internalized, making exit or dissent extremely costly at a personal level, amplifying effective extraction. If identity is more fluid, structural suppression is the dominant factor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Strength of identity fusion for maximalist ideologues.').


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
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, lightning_network_scalability).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, altcoin_innovation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. It represents the maximalist interpretation, which emphasizes immutability and fixed monetary policy. It influences and is influenced by other readings and related protocols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
