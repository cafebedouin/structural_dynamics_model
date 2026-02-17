% ============================================================================
% CONSTRAINT STORY: fragile_middle_layer_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-15
% ============================================================================

:- module(constraint_fragile_middle_layer_collapse, []).

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
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: fragile_middle_layer_collapse
 * human_readable: The Intermediary Decay
 * domain: economic/technological/logistical
 * * SUMMARY:
 * A scenario where the "middle layer" of a system (e.g., regional distributors,
 * human moderators, local maintenance contractors) is hollowed out by
 * automation and direct-to-end-user scaling. This "Rope" for achieving
 * global cost-efficiency becomes a "Snare" during a local failure, as
 * the subject is trapped in a territory where the specialized human
 * agency required to resolve the failure has been liquidated. The "Efficient"
 * system collapses into a binary state of "Working" or "Terminal Failure"
 * with no middle-tier buffer to provide repair or nuance.
 *
 * * KEY AGENTS:
 * - Local End-User: Subject (Powerless)
 * - Global Platform: Beneficiary (Institutional)
 * - Resilience Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the middle layer's
% maintenance surplus into the global platform's margin.
domain_priors:base_extractiveness(fragile_middle_layer_collapse, 0.86).
domain_priors:suppression_score(fragile_middle_layer_collapse, 0.73). % Local repair alternatives are suppressed by proprietary software/legal locks.
domain_priors:theater_ratio(fragile_middle_layer_collapse, 0.84).    % High theater: "Self-service help centers" that mask the lack of actual human support.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, extractiveness, 0.86).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, theater_ratio, 0.84).

% Constraint self-claim (what does the constraint claim to be?)
% The platform claims this is a necessary coordination mechanism for efficiency.
narrative_ontology:constraint_claim(fragile_middle_layer_collapse, tangled_rope).
narrative_ontology:human_readable(fragile_middle_layer_collapse, "The Intermediary Decay").

% Binary flags
% Proprietary locks and restrictive terms of service require active enforcement.
domain_priors:requires_active_enforcement(fragile_middle_layer_collapse).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, global_platform).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, local_end_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The end-user is trapped: when the automated system fails, there is no
% "middle" human to call, liquidating their agency to restore service.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the collapse as a Rope—the essential coordination
% substrate for removing "human friction" and achieving perfect price scaling.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) and suppression (0.73) masking as essential
% coordination (Rope), with both beneficiaries and victims. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.84) > 0.70 triggers Piton: the "Community Support"
% dashboard is an inertial spike; it performatively signals care while the
% middle layer has been systematically liquidated.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(fragile_middle_layer_collapse, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fragile_middle_layer_collapse_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.84) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, _),
    narrative_ontology:constraint_victim(fragile_middle_layer_collapse, _),
    domain_priors:requires_active_enforcement(fragile_middle_layer_collapse).

:- end_tests(fragile_middle_layer_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the
 * "coordination" benefit of direct scaling is achieved by liquidating the
 * repair agency of the system's human middle layer. The suppression score (0.73)
 * is high due to proprietary locks preventing third-party repair. The high
 * theater ratio (0.84) captures the performative nature of "self-service"
 * support systems that replace functional human assistance. The temporal data
 * models this "hollowing out" process, showing extraction and theater rising
 * as the functional middle layer is dismantled.
 *
 * * PERSPECTIVAL GAP:
 * The Local End-User feels a Snare because they have lost the human
 * interface that once mediated between them and the machine. The Global
 * Platform sees a Rope because the removal of the middle layer coordinates
 * a massive reduction in operational overhead. The analytical observer sees
 * a Tangled Rope, recognizing both the coordination function and the severe
 * asymmetric extraction.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the "Automated Trouble-Ticket" system is no longer functional
 * for resilience (Theater 0.84); it is an inert spike (Piton) that is part of
 * a larger Tangled Rope structure siphoning 0.86 of the system's adaptive capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_middle_layer_reconstruction,
    'Can AI "Agents" simulate the middle layer Rope, or is repair a biological "Snare" (Snare vs Mountain)?',
    'Tracking the success rate of autonomous agents in resolving high-context "out-of-distribution" local failures.',
    'If AI succeeds: Snare of current software design. If AI fails: Mountain of Human Tacit Knowledge.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fragile_middle_layer_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The "hollowing out" process is modeled as a rise in both extraction (value
% captured from the former middle layer) and theater (performative support
% replacing real support) over the interval.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fmlc_tr_t0, fragile_middle_layer_collapse, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fmlc_tr_t5, fragile_middle_layer_collapse, theater_ratio, 5, 0.50).
narrative_ontology:measurement(fmlc_tr_t10, fragile_middle_layer_collapse, theater_ratio, 10, 0.84).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fmlc_ex_t0, fragile_middle_layer_collapse, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(fmlc_ex_t5, fragile_middle_layer_collapse, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(fmlc_ex_t10, fragile_middle_layer_collapse, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The platform acts as a global infrastructure for service delivery.
narrative_ontology:coordination_type(fragile_middle_layer_collapse, global_infrastructure).

% Network relationships (structural influence edges)
% The collapse of the middle layer directly impacts the economic resilience of local communities.
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, local_economic_resilience).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */