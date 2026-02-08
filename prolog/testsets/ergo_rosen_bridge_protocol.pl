% ============================================================================
% CONSTRAINT STORY: rosen_bridge_protocol
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_rosen_bridge_protocol, []).

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
 * * constraint_id: rosen_bridge_protocol
 * human_readable: Rosen Bridge Cross-Chain Mechanism
 * domain: technological/economic
 * * SUMMARY:
 * Rosen Bridge is an Ergo-centric cross-chain protocol that enables asset
 * transfers without deploying smart contracts on external chains.
 * It uses a two-layer security model: Watchers monitor events, and a federated
 * set of Guards verifies and executes transactions. All consensus logic resides
 * on Ergo, making the bridge auditable from a single chain.
 * * KEY AGENTS:
 * - Small Value Transactor: Subject (Powerless)
 * - Liquidity Provider / Guard: Beneficiary (Institutional)
 * - Protocol Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rosen_bridge_protocol, 0.5). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(rosen_bridge_protocol, 0.2).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(rosen_bridge_protocol, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rosen_bridge_protocol, extractiveness, 0.5).
narrative_ontology:constraint_metric(rosen_bridge_protocol, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(rosen_bridge_protocol, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The protocol presents itself as a pure coordination utility.
narrative_ontology:constraint_claim(rosen_bridge_protocol, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(rosen_bridge_protocol). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(rosen_bridge_protocol, watchers_and_guards).
narrative_ontology:constraint_victim(rosen_bridge_protocol, small_value_transactors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For users moving small amounts (e.g., <$100), the $10 minimum fee is a Snare.
% The high cost per transaction "strangles" their mobility across chains,
% effectively trapping their capital on one network.
constraint_indexing:constraint_classification(rosen_bridge_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For high-net-worth users, liquidity providers, or Guards, the bridge is a
% robust Rope. The 0.5% fee is a fair trade for security. They use the bridge
% as a coordination tool to move assets where yield is higher.
constraint_indexing:constraint_classification(rosen_bridge_protocol, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the protocol is a Tangled Rope. It provides a genuine
% coordination function (Rope for institutional users) but also has high,
% asymmetric extraction from a specific victim class (Snare for small users).
% It requires active enforcement (Guards) to function.
constraint_indexing:constraint_classification(rosen_bridge_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rosen_bridge_protocol_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rosen_bridge_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rosen_bridge_protocol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Verify that the base extractiveness meets the criteria for a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rosen_bridge_protocol, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_properties) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(rosen_bridge_protocol, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(rosen_bridge_protocol, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(rosen_bridge_protocol).

:- end_tests(rosen_bridge_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.5 is chosen because the fee structure (0.5% or
 * $10 minimum) is significant and primarily benefits the security providers
 * (Watchers and Guards). This creates a sharp perspectival gap. For institutional
 * actors, it's a low-cost coordination tool (Rope). For small retail users,
 * the flat minimum fee is highly extractive, acting as a Snare that makes
 * cross-chain movement prohibitively expensive.
 *
 * * MANDATROPHY ANALYSIS:
 * The analytical classification is Tangled Rope, not Snare. This is critical.
 * A pure Snare classification would ignore the genuine, high-security
 * coordination function the bridge provides for large-value transfers.
 * The Tangled Rope classification correctly captures this duality: it is
 * simultaneously a valuable coordination tool and a mechanism of asymmetric
 * extraction, depending on the user's scale. This prevents the system from
 * mislabeling a complex hybrid system as pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rosen_bridge_protocol,
    'Will the federated Guard set remain independent or consolidate over time, increasing censorship risk?',
    'Monitoring the on-chain rotation, diversity, and ownership of the Guard set entities.',
    'If consolidated, the Rope aspect weakens and the Snare aspect strengthens, shifting the constraint towards pure extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rosen_bridge_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (E=0.5 > 0.46).
% Models a slight increase in extraction and performative activity as the
% protocol matures and its fee structure becomes more entrenched.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rosen_bridge_protocol_tr_t0, rosen_bridge_protocol, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rosen_bridge_protocol_tr_t5, rosen_bridge_protocol, theater_ratio, 5, 0.08).
narrative_ontology:measurement(rosen_bridge_protocol_tr_t10, rosen_bridge_protocol, theater_ratio, 10, 0.1).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rosen_bridge_protocol_ex_t0, rosen_bridge_protocol, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(rosen_bridge_protocol_ex_t5, rosen_bridge_protocol, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(rosen_bridge_protocol_ex_t10, rosen_bridge_protocol, base_extractiveness, 10, 0.5).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% As a cross-chain bridge, it functions as a piece of global infrastructure.
narrative_ontology:coordination_type(rosen_bridge_protocol, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */