% ============================================================================
% CONSTRAINT STORY: institutional_mutation_without_selection
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_institutional_mutation_without_selection, []).

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
 * * constraint_id: institutional_mutation_without_selection
 * human_readable: The Zombie Bureaucracy Drift
 * domain: organizational/political
 * * SUMMARY:
 * A scenario where an institution's internal rules and goals mutate over time
 * but are shielded from any external "selection" or accountability mechanism
 * (e.g., market failure, democratic removal). The coordination substrate
 * becomes a "Snare" for the subject, as the institution's primary function is
 * liquidated and replaced by self-serving internal logic, trapping the user in
 * a territory of "institutional hallucination" where the organization
 * performatively claims to serve a goal it has long since abandoned.
 * * KEY AGENTS:
 * - Public Service User: Subject (Powerless Victim)
 * - Institutional Administrator: Beneficiary (Institutional)
 * - Organizational Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(institutional_mutation_without_selection, 0.84). % High extraction reflects liquidation of public mandate.
domain_priors:suppression_score(institutional_mutation_without_selection, 0.76).   % Alternatives suppressed by legal monopoly or regulatory capture.
domain_priors:theater_ratio(institutional_mutation_without_selection, 0.94).       % Extreme theater: "Strategic Vision" documents masking lack of output.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(institutional_mutation_without_selection, extractiveness, 0.84).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, theater_ratio, 0.94).

% Constraint self-claim (what does the constraint claim to be?)
% The institution claims to be a coordination mechanism for public good.
narrative_ontology:constraint_claim(institutional_mutation_without_selection, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(institutional_mutation_without_selection). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(institutional_mutation_without_selection, institutional_administrator).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, public_service_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: they must use the institution for essential services,
% but the mutation liquidates their agency to receive the actual service.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the mutation as a Rope—the essential coordination
% substrate for maintaining "institutional stability" in a complex world.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and suppression (0.76) masking as essential
% coordination. The presence of beneficiaries, victims, and active enforcement
% confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Official Mission Statement"
% is an inertial spike; it performatively signals a purpose that no longer exists.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_without_selection_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional administrator.
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(institutional_mutation_without_selection, _),
    narrative_ontology:constraint_victim(institutional_mutation_without_selection, _),
    domain_priors:requires_active_enforcement(institutional_mutation_without_selection).

:- end_tests(institutional_mutation_without_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models institutional decay where internal goals replace the
 * original public mandate. The high base_extractiveness (0.84) represents the
 * value extracted from the public (in the form of a non-delivered service) to
 * maintain the internal stability and salaries of the administrators. The high
 * suppression_score (0.76) comes from the institution's legal monopoly,
 * preventing users from seeking alternatives. The extremely high theater_ratio
 * (0.94) reflects the vast resources spent on performative compliance (reports,
 * mission statements, strategic plans) that have no connection to functional
 * outcomes, triggering the Piton classification.
 *
 * The temporal data models this decay: it begins as a functional public body
 * with low extraction and theater, and drifts into a self-serving state.
 *
 * * PERSPECTIVAL GAP:
 * The Public Service User experiences a Snare: they are trapped by a legal
 * requirement to use a service that no longer functions, extracting their time
 * and taxes for no benefit. The Institutional Administrator, however, perceives
 * a Rope: the complex internal rules are a necessary coordination mechanism to
 * manage the organization and ensure its stability (and their employment).
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The system avoids misclassifying this as a simple
 * Snare by using the Tangled Rope classification from an analytical perspective.
 * This acknowledges that the institution *does* perform a coordination function
 * (for its internal members), but this function is coupled with severe,
 * asymmetric extraction from its intended public beneficiaries. The Piton
 * classification further resolves this by identifying the public-facing mandate
 * as an inert, purely theatrical artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_imws_reintroduction,
    'Can "Sunset Clauses" restore the Rope, or is mutation a physical law of hierarchies (Snare vs Mountain)?',
    'Tracking the performance delta of institutions with vs without mandatory 10-year re-chartering audits.',
    'If re-chartering works: Snare of current design. If it fails: Mountain of Organizational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_mutation_without_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation). The model shows an institution that began with a
% legitimate public service function (low extraction, low theater) and degraded
% over the interval into a self-serving bureaucracy.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(imws_tr_t0, institutional_mutation_without_selection, theater_ratio, 0, 0.20).
narrative_ontology:measurement(imws_tr_t5, institutional_mutation_without_selection, theater_ratio, 5, 0.65).
narrative_ontology:measurement(imws_tr_t10, institutional_mutation_without_selection, theater_ratio, 10, 0.94).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(imws_ex_t0, institutional_mutation_without_selection, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(imws_ex_t5, institutional_mutation_without_selection, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(imws_ex_t10, institutional_mutation_without_selection, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The mutated institution's primary function is now allocating its own budget
% and resources to maintain its internal state, making it a form of
% resource_allocation mechanism that has turned inward.
narrative_ontology:coordination_type(institutional_mutation_without_selection, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */