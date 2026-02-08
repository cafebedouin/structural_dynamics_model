% ============================================================================
% CONSTRAINT STORY: quantum_decryption_risk_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_quantum_decryption_risk_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: quantum_decryption_risk_2026
 * human_readable: Quantum Decryption Risk to Public Key Cryptography
 * domain: technological
 * * SUMMARY:
 * The security of most modern digital communication (e.g., RSA, ECC) relies on the computational difficulty of certain mathematical problems like integer factorization. The advent of fault-tolerant quantum computers threatens to solve these problems efficiently (via Shor's algorithm), rendering current encryption standards obsolete. This creates a "harvest now, decrypt later" scenario, where encrypted data intercepted today could be decrypted retroactively once the technology matures.
 * * KEY AGENTS:
 * - Private Citizen: Subject (Powerless) whose current and past communications are at risk.
 * - State Intelligence Agency: Beneficiary (Institutional) who can leverage this future capability for mass surveillance.
 * - Quantum Physicist: Auditor (Analytical) who views the capability as an inevitable outcome of physical laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(quantum_decryption_risk_2026, 0.80). % Snare extraction >= 0.46. The ability to retroactively decrypt global communications is profoundly extractive.
domain_priors:suppression_score(quantum_decryption_risk_2026, 0.30).   % Structural property (raw, unscaled). The risk is known, but alternatives (PQC) are slow to deploy.
domain_priors:theater_ratio(quantum_decryption_risk_2026, 0.10).       % Piton detection (>= 0.70). The threat is real, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(quantum_decryption_risk_2026, extractiveness, 0.80).
narrative_ontology:constraint_metric(quantum_decryption_risk_2026, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(quantum_decryption_risk_2026, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% From the physicist's perspective, this is a feature of the natural world.
narrative_ontology:constraint_claim(quantum_decryption_risk_2026, mountain).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(quantum_decryption_risk_2026, state_intelligence_agencies).
narrative_ontology:constraint_victim(quantum_decryption_risk_2026, private_citizens).
narrative_ontology:constraint_victim(quantum_decryption_risk_2026, human_rights_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a private citizen, their past and present encrypted data is held in a
% temporal trap. The security they rely on today becomes the source of
% future vulnerability. Effective extraction is amplified by powerlessness.
% χ = 0.80 * 1.5 (powerless) * 1.2 (global) = 1.44 (Extreme Snare)
constraint_indexing:constraint_classification(quantum_decryption_risk_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For a state agency, this is a powerful coordination tool for achieving
% total information awareness. The negative power modifier reflects their
% ability to absorb the costs and treat it as infrastructure.
% χ = 0.80 * -0.2 (institutional) * 1.2 (global) = -0.192 (Pure Rope)
constraint_indexing:constraint_classification(quantum_decryption_risk_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% To the physicist or computer scientist, the vulnerability is not a social
% construct but a consequence of the laws of physics (quantum mechanics)
% meeting the structure of mathematics (Shor's algorithm). It is an
% unchangeable fact about what is computable in our universe.
constraint_indexing:constraint_classification(quantum_decryption_risk_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_decryption_risk_2026_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope).
    constraint_indexing:constraint_classification(quantum_decryption_risk_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_decryption_risk_2026, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_mountain) :-
    % Verify the analytical observer correctly identifies the physical limit.
    constraint_indexing:constraint_classification(quantum_decryption_risk_2026, mountain, context(agent_power(analytical), _, _, _)).

test(high_extraction_threshold) :-
    % Verify the base extractiveness meets the Snare threshold.
    domain_priors:base_extractiveness(quantum_decryption_risk_2026, E),
    E >= 0.46.

:- end_tests(quantum_decryption_risk_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a constraint rooted in a physical reality (Mountain) that manifests as a social Snare.
 * The base extractiveness is very high (0.80) because the potential for retroactive, mass decryption represents a near-total loss of privacy for targeted groups.
 * The Perspectival Gap is extreme: what is a physical law to a scientist (Mountain) is a tool for total information awareness to a state (Rope) and an inescapable trap for a citizen (Snare). This is a classic example of how a change in physical paradigms can create new, highly asymmetric power structures.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a Mandatrophy risk because the "Rope" (total information awareness for the state) is constructed directly from the broken privacy of the "Snare" (for the citizen). The resolution lies not in changing the Mountain (the physics), but in building a new constraint: the widespread adoption of Post-Quantum Cryptography (PQC). PQC acts as a countermeasure, creating a new Rope for coordination around privacy that is resistant to this specific Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_q_day_arrival,
    "When will a fault-tolerant quantum computer capable of breaking RSA-2048 be built?",
    "Physical demonstration of Shor's algorithm on a relevant cryptographic target.",
    "If soon: Immediate Snare for all legacy data. If distant: Ample time for PQC transition, mitigating the Snare.",
    confidence_without_resolution(low)
).

omega_variable(
    omega_extraction_intent,
    "Is quantum computer development primarily a scientific pursuit (byproduct) or a strategic state-level goal for surveillance (deliberate)?",
    "Declassification of state funding directives for quantum research programs.",
    "If byproduct: Mountain. If deliberate: Reinforces the Snare classification from a policy perspective.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(quantum_decryption_risk_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as quantum computing moved from theory to reality.
% The base extractiveness (the potential for harm) grew as the timeline to "Q-Day" shortened.
% Theater ratio remains low as the threat is physical, not performative.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(qdr_tr_t0, quantum_decryption_risk_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qdr_tr_t5, quantum_decryption_risk_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(qdr_tr_t10, quantum_decryption_risk_2026, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(qdr_ex_t0, quantum_decryption_risk_2026, base_extractiveness, 0, 0.10). % T=0: Purely theoretical risk
narrative_ontology:measurement(qdr_ex_t5, quantum_decryption_risk_2026, base_extractiveness, 5, 0.50).  % T=5: Experimental progress, "harvest now" begins
narrative_ontology:measurement(qdr_ex_t10, quantum_decryption_risk_2026, base_extractiveness, 10, 0.80). % T=10: Imminent threat, PQC transition urgent

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Network relationships (structural influence edges)
% The risk of quantum decryption directly drives the need for and adoption of
% post-quantum cryptography standards.
narrative_ontology:affects_constraint(quantum_decryption_risk_2026, post_quantum_cryptography_adoption).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */