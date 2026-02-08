% ============================================================================
% CONSTRAINT STORY: infrastructure_interoperability_decay
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_infrastructure_interoperability_decay, []).

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
 * * constraint_id: infrastructure_interoperability_decay
 * human_readable: The Protocol Silo Trap
 * domain: technological/economic
 * * SUMMARY:
 * A scenario where a universal communication or transport standard (Rope)
 * fragmentates into proprietary, incompatible sub-layers as providers
 * seek to "lock in" users. This coordination substrate for global trade
 * becomes a "Snare" for the subject, as the cost of bridging these
 * artificial silos liquidates their primary operational agency, trapping
 * them in a territory of high-friction integration and terminal vendor
 * dependency.
 * * KEY AGENTS:
 * - Independent Logistics Operator: Subject (Powerless)
 * - Platform Ecosystem Provider: Beneficiary (Institutional)
 * - Interoperability Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's
% operational surplus into "compatibility tolls" and custom integration labor.
domain_priors:base_extractiveness(infrastructure_interoperability_decay, 0.86).
domain_priors:suppression_score(infrastructure_interoperability_decay, 0.78). % Open, cross-compatible alternatives are suppressed by proprietary API locks.
domain_priors:theater_ratio(infrastructure_interoperability_decay, 0.91).    % Extreme theater: "Integration Partners" programs that performatively signal openness while 0.86 extraction occurs.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, extractiveness, 0.86).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
% The platform claims to be providing essential coordination for a superior experience.
narrative_ontology:constraint_claim(infrastructure_interoperability_decay, piton).

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(infrastructure_interoperability_decay).
narrative_ontology:constraint_beneficiary(infrastructure_interoperability_decay, platform_ecosystem_provider).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, independent_logistics_operator).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The operator is trapped: they must use the platform to access the market,
% but the decay of standards liquidates their ability to exit the ecosystem.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the proprietary silo as a Rope—the essential coordination
% substrate for providing a "superior, secure, and integrated" user experience.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Open Standards Initiative"
% is an inertial spike; it performatively charts a path to unity while extraction continues.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_interoperability_decay_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless operator vs Rope for the institutional provider.
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, rope,
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, piton,
        context(agent_power(analytical), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(infrastructure_interoperability_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the decay of a public good (interoperability) into a
 * private, extractive tollgate. Extraction (0.86) is the value siphoned
 * through compatibility fees and lock-in. Suppression (0.78) is the active
 * technical and legal effort to prevent open alternatives. The theater ratio
 * (0.91) is extremely high, representing the vast corporate marketing and
 * "developer relations" efforts that frame this lock-in as a feature.
 *
 * * PERSPECTIVAL GAP:
 * The Independent Logistics Operator feels a Snare because they pay a tax
 * on every transaction that crosses an artificial boundary. The Platform
 * Provider sees a Rope because the silos coordinate a highly profitable
 * and "curated" marketplace for their specific users.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.86) creates a Mandatrophy state where a system's
 * claimed coordination function is achieved by liquidating the agency of its
 * users. The system resolves this by classifying the constraint as a Tangled Rope
 * from the analytical perspective. This prevents a misclassification as a pure
 * Snare, which would ignore the genuine (but now extractive) coordination
 * function. The Piton classification further refines this, identifying the
 * performative "openness" initiatives as theatrical maintenance of a degraded system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_standard_reconvergence,
    'Can "Public Good" protocols restore the Rope, or is silo-formation a physical law of platform economics (Snare vs Mountain)?',
    'Tracking the market share of un-walled logistics protocols versus proprietary giants through 2030.',
    'If un-walled protocols scale: Snare of current design. If they fail: Mountain of Economic Gravity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(infrastructure_interoperability_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a decay from a healthy Rope (open standard) to a
% Tangled Rope/Piton (proprietary silo). The measurements show extraction
% and theater accumulating over the interval.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(iid_tr_t0, infrastructure_interoperability_decay, theater_ratio, 0, 0.15).
narrative_ontology:measurement(iid_tr_t5, infrastructure_interoperability_decay, theater_ratio, 5, 0.60).
narrative_ontology:measurement(iid_tr_t10, infrastructure_interoperability_decay, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(iid_ex_t0, infrastructure_interoperability_decay, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(iid_ex_t5, infrastructure_interoperability_decay, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(iid_ex_t10, infrastructure_interoperability_decay, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The original constraint was a coordination mechanism for global infrastructure.
narrative_ontology:coordination_type(infrastructure_interoperability_decay, global_infrastructure).

% The decay of interoperability directly increases the fragility of supply chains.
narrative_ontology:affects_constraint(infrastructure_interoperability_decay, global_supply_chain_fragility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */