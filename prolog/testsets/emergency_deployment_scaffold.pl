% ============================================================================
% CONSTRAINT STORY: emergency_bridge_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_emergency_bridge_scaffold, []).

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
 * * constraint_id: emergency_bridge_scaffold
 * human_readable: The Tactical Crossing
 * domain: technological/political
 * * SUMMARY:
 * A temporary military-grade bridge deployed following the collapse of a legacy
 * Piton. It restores vital coordination (transport links) but extracts high
 * compliance and privacy costs from users via surveillance and checkpoints.
 * It is explicitly designed with a sunset clause for its removal upon completion
 * of a permanent replacement.
 * * KEY AGENTS:
 * - The Commuter: Subject (Powerless) - Grateful for access, but under surveillance.
 * - Regional Government: Beneficiary (Institutional) - Sees restored economic activity.
 * - The Engineer Corps: Architect (Organized) - Maintaining the temporary Scaffold.
 * - The Civil Liberties Auditor: Auditor (Analytical) - Ensuring the sunset clause is honored.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(emergency_bridge_scaffold, 0.49). % High: Extracts data/privacy for security.
domain_priors:suppression_score(emergency_bridge_scaffold, 0.95).   % Extreme: Strict military control over a vital bottleneck.
domain_priors:theater_ratio(emergency_bridge_scaffold, 0.05).       % Minimal: The bridge is purely functional; no theater.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(emergency_bridge_scaffold, extractiveness, 0.49).
narrative_ontology:constraint_metric(emergency_bridge_scaffold, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(emergency_bridge_scaffold, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(emergency_bridge_scaffold, snare).

% Binary flags
narrative_ontology:has_sunset_clause(emergency_bridge_scaffold).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(emergency_bridge_scaffold). % Armed oversight at checkpoints.

% Structural property derivation hooks:
% has_coordination_function/1 is derived from constraint_beneficiary/2 (required for Scaffold)
% has_asymmetric_extraction/1 is derived from constraint_victim/2
narrative_ontology:constraint_beneficiary(emergency_bridge_scaffold, regional_commuters).
narrative_ontology:constraint_victim(emergency_bridge_scaffold, regional_commuters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the commuter with no other way to cross, the constant surveillance and
% checkpoints feel like a trap, despite its utility. The high suppression and
% extraction are unavoidable.
% χ = 0.49 * π(powerless, 1.5) * σ(regional, 0.9) = 0.66
constraint_indexing:constraint_classification(emergency_bridge_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The regional government, focused on economic recovery, sees only the restored
% flow of commerce and labor. The extraction is an acceptable operational cost.
% χ = 0.49 * π(institutional, -0.2) * σ(national, 1.0) = -0.098 (felt as a benefit)
constraint_indexing:constraint_classification(emergency_bridge_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% The engineer corps views this as a high-tension support structure that must
% not harden into a permanent fixture. Its temporary nature is its defining feature.
constraint_indexing:constraint_classification(emergency_bridge_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(emergency_bridge_scaffold).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% The civil liberties auditor, focusing on the raw metrics of high suppression (0.95)
% and extraction (0.49), classifies it as a Snare, whose only justification is its
% temporary status defined by the sunset clause.
% χ = 0.49 * π(analytical, 1.15) * σ(global, 1.2) = 0.67
constraint_indexing:constraint_classification(emergency_bridge_scaffold, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_scaffold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless (snare) and institutional (rope).
    constraint_indexing:constraint_classification(emergency_bridge_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_bridge_scaffold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(scaffold_identity) :-
    % Verify the presence of sunset_clause triggers Scaffold for organized agents.
    constraint_indexing:constraint_classification(emergency_bridge_scaffold, scaffold, context(agent_power(organized), _, _, _)).

:- end_tests(emergency_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a "necessary evil." The extraction (0.49) is high due to
 * the "Security Tax"—users trade privacy and freedom of movement for the restoration
 * of a vital transport link. The suppression (0.95) reflects military control.
 * The key is the perspectival gap: commuters feel it as a Snare, the government
 * sees it as a Rope, and its architects correctly identify it as a Scaffold.
 * The classification as Scaffold depends entirely on the has_sunset_clause/1 fact.
 * Without it, it would be a Tangled Rope or Snare from all non-beneficiary perspectives.
 *
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the fact that the constraint is highly
 * functional (Theater Ratio 0.05) and explicitly temporary. By labeling it a
 * Scaffold, we identify it as a "Necessary Snare" that must decay as the
 * permanent Rope (the new bridge) is constructed. The system avoids mislabeling
 * it as a pure Snare by recognizing its coordination function (beneficiary exists)
 * and its sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scaffold_persistence,
    'Will the "temporary" surveillance infrastructure and checkpoints be dismantled as promised, or will they persist after the new bridge is built?',
    'Legislative audit of the decommissioning schedule vs. budget amendments that extend funding for "security operations" at the site.',
    'If sunset honored: Successful Scaffold lifecycle. If persisted: Scaffold degrades into a Snare or Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(emergency_bridge_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (0.49 > 0.46).
% Models the constraint from initial chaotic deployment to a more routine,
% but still highly controlled, state before decommissioning.

% Theater ratio over time (starts minimal and stays minimal):
narrative_ontology:measurement(ebs_tr_t0, emergency_bridge_scaffold, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ebs_tr_t5, emergency_bridge_scaffold, theater_ratio, 5, 0.04).
narrative_ontology:measurement(ebs_tr_t10, emergency_bridge_scaffold, theater_ratio, 10, 0.05).

% Extraction over time (starts very high during initial crisis, then stabilizes):
narrative_ontology:measurement(ebs_ex_t0, emergency_bridge_scaffold, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ebs_ex_t5, emergency_bridge_scaffold, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(ebs_ex_t10, emergency_bridge_scaffold, base_extractiveness, 10, 0.49).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The bridge is physical infrastructure that allocates resources (transit capacity).
narrative_ontology:coordination_type(emergency_bridge_scaffold, resource_allocation).

% Network relationships (structural influence edges)
% The scaffold was deployed because the previous constraint failed.
narrative_ontology:affects_constraint(legacy_bridge_piton, emergency_bridge_scaffold).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */