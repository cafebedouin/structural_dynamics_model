% ============================================================================
% CONSTRAINT STORY: mandatrophy_systemic_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_mandatrophy_systemic_collapse, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   [RESOLVED MANDATROPHY]
   MANDATROPHY RESOLUTION: This constraint's extreme extraction (0.9) is resolved
   by classifying it as a Tangled Rope from an analytical perspective. The
   mandate provides a genuine, albeit perverse, coordination function (aligning
   the organization towards a single metric), which is why beneficiaries
   perceive it as a Rope. However, this coordination is achieved by extracting
   the system's resilience and safety margins, creating a Snare for operators.
   The Tangled Rope classification correctly captures this duality, preventing
   the system from being misread as either pure coordination (Rope) or
   pure coercion (Snare). The catastrophic failure endpoint is correctly
   indexed as a Mountain for the ultimate victims, representing the point where
   administrative fiction collides with physical reality.
   ========================================================================== */

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: mandatrophy_systemic_collapse
 * human_readable: Mandatrophy (Systemic Resilience Wasting)
 * domain: institutional/technological
 * * SUMMARY:
 * Mandatrophy is the "invisible" extraction of a system's resilience (its "margin")
 * to satisfy a high-priority administrative or political goal (its "mandate").
 * By treating safety buffers, technical expertise, and physical maintenance as
 * "wasteful slack," the institution gains short-term performance while
 * creating a state of "brittle stability" that ensures catastrophic failure
 * when the first unanticipated stressor arrives.
 * * KEY AGENTS:
 * - The Mandator (KPI Architect): Institutional; views all non-active resources as "excess" to be optimized.
 * - The Operator (Engineer/Technician): Powerless; perceives the tightening Snare as operational "slack" disappears.
 * - The End User (Passenger/Citizen): Powerless; experiences the sudden transition from perceived stability to catastrophic failure (Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(mandatrophy_systemic_collapse, 0.90). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(mandatrophy_systemic_collapse, 0.75).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(mandatrophy_systemic_collapse, 0.40).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(mandatrophy_systemic_collapse, extractiveness, 0.90).
narrative_ontology:constraint_metric(mandatrophy_systemic_collapse, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mandatrophy_systemic_collapse, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(mandatrophy_systemic_collapse, tangled_rope).
narrative_ontology:human_readable(mandatrophy_systemic_collapse, "Mandatrophy (Systemic Resilience Wasting)").

% Binary flags
domain_priors:requires_active_enforcement(mandatrophy_systemic_collapse). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(mandatrophy_systemic_collapse, administrative_centralization).
narrative_ontology:constraint_beneficiary(mandatrophy_systemic_collapse, short_term_fiscal_metrics).
narrative_ontology:constraint_victim(mandatrophy_systemic_collapse, long_term_system_resilience).
narrative_ontology:constraint_victim(mandatrophy_systemic_collapse, operational_safety_margins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE OPERATOR (SNARE)
% The engineer or technician who feels the system "tightening" as redundancies
% and safety margins are removed. The mandate is a coercive force that chokes
% their ability to maintain safety.
constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MANDATOR (ROPE)
% For HQ, the mandate is a Rope. It is the tool they use to "pull" a lazy or
% bloated system toward modern efficiency. They view the "margin" as
% parasitic drag that must be eliminated to succeed.
constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (aligning the organization)
% and the asymmetric extraction (destroying resilience). This duality is the
% signature of a Tangled Rope.
constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE END USER AT FAILURE (MOUNTAIN)
% For the victim of the eventual collapse (e.g., a passenger), the event is a
% Mountain. It is a sudden, unyielding physical limit that appears as a "freak
% accident," where negotiation with the system is impossible.
constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, mountain,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophy_systemic_collapse_tests).

test(perspectival_gap_manager_vs_operator) :-
    % Verify the manager (institutional) sees a Rope while the operator (powerless) sees a Snare.
    constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, snare, context(agent_power(powerless), time_horizon(immediate), _, _)).

test(tangled_rope_analytical_view) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, tangled_rope, context(agent_power(analytical), _, _, _)),
    domain_priors:requires_active_enforcement(mandatrophy_systemic_collapse),
    narrative_ontology:constraint_beneficiary(mandatrophy_systemic_collapse, _),
    narrative_ontology:constraint_victim(mandatrophy_systemic_collapse, _).

test(high_extraction_and_suppression) :-
    % The system must have high base extraction and suppress alternatives (like maintaining safety margins).
    domain_priors:base_extractiveness(mandatrophy_systemic_collapse, E), E > 0.8,
    domain_priors:suppression_score(mandatrophy_systemic_collapse, S), S > 0.7.

:- end_tests(mandatrophy_systemic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that is perversely self-optimizing. The base
 * extractiveness of 0.90 is extreme because the asset being extracted is the
 * system's future survival capacity, which is then converted into short-term
 * performance metrics. The high suppression score (0.75) represents the
 * administrative logic that actively reframes resilience and redundancy as
 * "waste" or "inefficiency."
 *
 * The Perspectival Gap is profound:
 * - The `institutional` agent sees a pure Rope, a tool for coordination and efficiency.
 * - The `powerless` operator on the front line feels the Snare tightening, removing their agency.
 * - The `analytical` observer sees the whole picture: a Tangled Rope where a coordination
 *   function is inextricably linked to a destructive, asymmetric extraction.
 * - The `powerless` end-user, at the moment of failure, experiences a Mountain—the
 *   abrupt, non-negotiable consequence of depleted margins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mandatrophy_audit_paradox,
    "Do 'Safety Audits' in a mandatrophic system identify the loss of margin (acting as a counter-force) or merely validate the mandate's fulfillment (acting as a Snare component)?",
    "Comparative analysis of internal audit logs vs. post-mortem failure reports from independent investigators (e.g., NTSB).",
    "If audits validate the mandate, they are part of the Snare's enforcement. If they identify margin loss, they represent a potential, but suppressed, Scaffold.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mandatrophy_systemic_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the system's degradation over time. Initially, the
% mandate is a reasonable efficiency drive (moderate extraction, low theater).
% Over time, it intensifies, extracting core resilience and requiring more
% performative safety checks to maintain an illusion of control.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(mandatrophy_systemic_collapse_tr_t0, mandatrophy_systemic_collapse, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mandatrophy_systemic_collapse_tr_t5, mandatrophy_systemic_collapse, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mandatrophy_systemic_collapse_tr_t10, mandatrophy_systemic_collapse, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mandatrophy_systemic_collapse_ex_t0, mandatrophy_systemic_collapse, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(mandatrophy_systemic_collapse_ex_t5, mandatrophy_systemic_collapse, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(mandatrophy_systemic_collapse_ex_t10, mandatrophy_systemic_collapse, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The mandate acts as a resource allocation mechanism, diverting funds and
% attention from maintenance/resilience toward KPI-driven goals.
narrative_ontology:coordination_type(mandatrophy_systemic_collapse, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */