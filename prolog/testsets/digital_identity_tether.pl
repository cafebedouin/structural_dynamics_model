% ============================================================================
% CONSTRAINT STORY: digital_identity_tether
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_digital_identity_tether, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: digital_identity_tether
 * human_readable: The Centralized Identity Anchor
 * domain: technological
 * * SUMMARY:
 * This constraint defines the inability of a digital subject to decouple
 * their reputation, social graph, and authentication from a primary
 * provider (the "Identity Anchor"). Initially a convenience (Rope),
 * it becomes a tool of extraction as the cost of exit approaches total
 * social/professional erasure.
 * * KEY AGENTS:
 * - The Freelancer: Subject (Powerless). Reputation is locked to the platform.
 * - The Platform Sovereign: Beneficiary (Institutional). Owns the namespace.
 * - The Protocol Architect: Auditor (Analytical). Evaluates interoperability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.58) is driven by the "Tax on Exit."
% The platform extracts behavioral data and loyalty by threatening identity-death.
domain_priors:base_extractiveness(digital_identity_tether, 0.58).
domain_priors:suppression_score(digital_identity_tether, 0.92).   % Alternatives exist but lack network effects.
domain_priors:theater_ratio(digital_identity_tether, 0.35).      % Low: The constraint is functional and active, not yet a Piton.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(digital_identity_tether, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_identity_tether, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(digital_identity_tether, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(digital_identity_tether, tangled_rope).
narrative_ontology:human_readable(digital_identity_tether, "The Centralized Identity Anchor").
narrative_ontology:topic_domain(digital_identity_tether, "technological").

% Binary flags
domain_priors:requires_active_enforcement(digital_identity_tether).
% The platform may claim a sunset clause (e.g., "data portability"), but its
% high extraction (0.58) and suppression (0.92) make a Scaffold classification
% invalid. The legitimacy of this claim is handled by the Omega variable.
% narrative_ontology:has_sunset_clause(digital_identity_tether).

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(digital_identity_tether, platform_sovereigns).
narrative_ontology:constraint_victim(digital_identity_tether, platform_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE FREELANCER (SNARE)
% To the user, the identity is a trap; leaving means losing 10 years of
% verified work history and social connections.
% χ = 0.58 (ε) * 1.5 (π(powerless)) * 1.2 (σ(global)) = 1.044. This is a very high
% effective extraction, clearly a Snare.
constraint_indexing:constraint_classification(digital_identity_tether, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM SOVEREIGN (ROPE)
% Viewed as a vital security coordination service (Anti-Sybil/KYC).
% χ = 0.58 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.139.
% The negative extraction reflects a net benefit, classifying as a Rope.
constraint_indexing:constraint_classification(digital_identity_tether, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature where the "Single Sign-On" convenience (Rope)
% is the mechanism that delivers the user into the data-harvesting silo (Snare).
% This is the correct structural classification.
constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_identity_tether_tests).

test(perspectival_gap, [nondet]) :-
    constraint_indexing:constraint_classification(digital_identity_tether, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_identity_tether, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_conditions_met) :-
    narrative_ontology:constraint_beneficiary(digital_identity_tether, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(digital_identity_tether, _),       % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(digital_identity_tether).

:- end_tests(digital_identity_tether_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The identity tether is highly "sticky" due to the Suppression Score (0.92).
 * While technical alternatives (DIDs/Self-Sovereign Identity) exist, the
 * "Social Exit Cost" remains the primary enforcement mechanism.
 * The Perspectival Gap exists because the Platform views the tether as
 * "Safety/Coordination," while the User (once established) views it as
 * "Captivity/Extraction." The high base extraction (0.58) makes a Scaffold
 * classification impossible, even if a sunset clause is claimed.
 *
 * MANDATROPHY ANALYSIS:
 * This case is a canonical "Tangled Rope" because the coordination benefit
 * (trust, reputation, anti-sybil) is the source of the extraction potential.
 * You cannot have the reputation (Rope) without the silo (Snare). The system
 * correctly identifies this hybrid nature from the analytical perspective,
 * preventing a misclassification as a pure Snare, which would ignore the
 * genuine (if weaponized) coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_portability_legitimacy,
    'Is "Data Portability" a genuine Sunset Clause or a Theatrical facade?',
    'Analysis of successful user migrations vs. technical friction logs and dark patterns.',
    'If genuine (and extraction were reduced), it could become a Scaffold; if theatrical, it risks becoming a Piton over time.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(digital_identity_tether, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint evolved from a convenience to a lock-in mechanism.
% The data models extraction increasing as network effects solidified.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(dit_tr_t0, digital_identity_tether, theater_ratio, 0, 0.10).
narrative_ontology:measurement(dit_tr_t5, digital_identity_tether, theater_ratio, 5, 0.20).
narrative_ontology:measurement(dit_tr_t10, digital_identity_tether, theater_ratio, 10, 0.35).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(dit_ex_t0, digital_identity_tether, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(dit_ex_t5, digital_identity_tether, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(dit_ex_t10, digital_identity_tether, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(digital_identity_tether, information_standard).

% Network relationships (structural influence edges)
% This constraint is coupled with labor market precarity.
narrative_ontology:affects_constraint(digital_identity_tether, gig_economy_precarity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */