% ============================================================================
% CONSTRAINT STORY: swift_piton_snap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_swift_piton_snap, []).

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
 * * constraint_id: swift_piton_snap
 * human_readable: The Great Decoupling
 * domain: technological/economic
 * * SUMMARY:
 * This constraint models the new global financial settlement layer that emerges
 * after the sudden failure of a legacy protocol (like SWIFT). The old system,
 * a "Piton" held in place by institutional inertia, snaps under the tension of
 * maintenance debt and the rise of superior alternatives. The new system is a
 * transitional scaffold, intended to become a fixed, mountain-like piece of
 * global infrastructure.
 * * KEY AGENTS:
 * - Global Citizens: Beneficiary (Powerless) - Experience the new system as a fixed, low-cost utility.
 * - Legacy Financial Institutions: Victim (Institutional) - Their old business model is trapped and made obsolete by the new system.
 * - Network Architects: Architect (Organized) - Manage the transition and view the system as a temporary scaffold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(swift_piton_snap, 0.04). % Very low; the new system is a utility, not a rent-seeking vehicle.
domain_priors:suppression_score(swift_piton_snap, 0.10).   % Low; many alternatives exist, and participation is voluntary.
domain_priors:theater_ratio(swift_piton_snap, 0.05).       % Minimal; the system is purely functional code. The "Piton" it replaced had high theater.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(swift_piton_snap, extractiveness, 0.04).
narrative_ontology:constraint_metric(swift_piton_snap, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(swift_piton_snap, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(swift_piton_snap, scaffold).

% Binary flags
narrative_ontology:has_sunset_clause(swift_piton_snap).      % Mandatory for Scaffold: the transition phase is temporary.
domain_priors:requires_active_enforcement(swift_piton_snap). % Enforcement is automated via protocol rules (smart contracts).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(swift_piton_snap, global_citizens).
narrative_ontology:constraint_victim(swift_piton_snap, legacy_financial_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LEGACY BANKER (SNARE)
% For the institution that can no longer extract rent, the new network is a Snare—it traps their old business model.
% Even with low base extraction, the *effect* is total suppression of their prior mode of operation.
constraint_indexing:constraint_classification(swift_piton_snap, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE GLOBAL CITIZEN (MOUNTAIN)
% For the user, the protocol is as fundamental and unchangeable as a Mountain (a fixed logical limit).
% Its rules are immutable and its function is a given.
constraint_indexing:constraint_classification(swift_piton_snap, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE TRANSITION ARCHITECT (SCAFFOLD)
% The system is a temporary support structure until the new global standard hardens.
constraint_indexing:constraint_classification(swift_piton_snap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(swift_piton_snap).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SCAFFOLD)
% The analytical view recognizes the coordination function, the temporary nature (sunset clause),
% and the low extraction, classifying it as a Scaffold.
constraint_indexing:constraint_classification(swift_piton_snap, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_piton_snap_tests).

test(perspectival_gap_victim_vs_user) :-
    % Verify the gap between the trapped legacy institution and the mobile user.
    constraint_indexing:constraint_classification(swift_piton_snap, TypeInstitutional, context(agent_power(institutional), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(swift_piton_snap, TypePowerless, context(agent_power(powerless), _, exit_options(mobile), _)),
    TypeInstitutional == snare,
    TypePowerless == mountain,
    TypeInstitutional \= TypePowerless.

test(scaffold_properties_present) :-
    % Verify that the necessary properties for a Scaffold classification are declared.
    narrative_ontology:has_sunset_clause(swift_piton_snap),
    narrative_ontology:constraint_beneficiary(swift_piton_snap, _).

test(low_extraction_and_theater) :-
    % The new system should have metrics consistent with a utility, not a Piton or Snare.
    domain_priors:base_extractiveness(swift_piton_snap, E), E < 0.15,
    domain_priors:theater_ratio(swift_piton_snap, TR), TR < 0.70.

:- end_tests(swift_piton_snap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This story models the *result* of a "Piton Snap." The old SWIFT-like system was a Piton with high theater and inertia.
 * The new system that replaces it has very low base extraction (0.04) and suppression (0.10), making it function like a
 * public utility. The key insight is the dramatic perspectival gap this creates.
 *
 * For legacy institutions whose rent-seeking models depended on the old system, the new one is a 'Snare' because it
 * completely traps and invalidates their way of operating, even though its own extraction is low. For end-users, the
 * new protocol is a 'Mountain'—a fixed, immutable piece of infrastructure. For its architects, it is a 'Scaffold,'
 * a temporary measure designed to be replaced by an even more permanent standard.
 *
 * The `has_sunset_clause` and `constraint_beneficiary` facts are included to correctly enable the Scaffold classification,
 * resolving the original file's lint error.
 *
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy of the old system (a Piton masquerading as a Rope) is resolved by its replacement with a new system
 * whose nature as a coordination utility is transparent. The coordination is achieved through code and mathematics,
 * not institutional coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_swift_piton_snap_recentralization,
    'Will the new "decentralized" scaffold eventually be captured by new powerful actors, re-creating a Tangled Rope?',
    'Longitudinal analysis of network validator/node concentration and protocol governance changes over a generational time horizon.',
    'If concentration occurs, the cycle repeats (Piton -> Scaffold -> Tangled Rope). If it remains distributed, it hardens into a true civilizational Rope/Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(swift_piton_snap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is 0.04 (< 0.46), so temporal measurements are not required
% for this constraint.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The new settlement layer is a form of global infrastructure.
narrative_ontology:coordination_type(swift_piton_snap, global_infrastructure).

% The emergence of a new settlement layer directly impacts the effectiveness
% and mechanisms of traditional monetary policy.
narrative_ontology:affects_constraint(swift_piton_snap, central_bank_monetary_policy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */