% ============================================================================
% CONSTRAINT STORY: individual_revolution_autonomy
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_individual_revolution_autonomy, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: individual_revolution_autonomy
 * human_readable: The One-Man Revolution
 * domain: political/social
 * * SUMMARY:
 * This constraint contrasts the "futile" pursuit of mass socio-political revolution with the "fertile" 
 * pursuit of individual autonomy. It posits that mass institutional change is often co-opted, 
 * making the "only revolution that is coming" an internal, individual shift in values.
 * * KEY AGENTS:
 * - The Subject (Citizen): Powerless agent trapped in mass-culture narratives.
 * - The Beneficiary (Elite): Institutional agent maintaining status-quo coordination.
 * - The Auditor (Historian): Analytical observer tracking cycles of control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(individual_revolution_autonomy, 0.75). % High extraction (Snare/Tangled territory)
domain_priors:suppression_score(individual_revolution_autonomy, 0.80).   % High suppression of alternatives
domain_priors:theater_ratio(individual_revolution_autonomy, 0.45).       % Significant but not yet Piton-level inertia

% Primary keys for the classification engine
narrative_ontology:constraint_metric(individual_revolution_autonomy, extractiveness, 0.75).
narrative_ontology:constraint_metric(individual_revolution_autonomy, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(individual_revolution_autonomy, theater_ratio, 0.45).

% Structural property derivation hooks
% High extraction (> 0.46) requires beneficiary/victim declaration for proper modeling.
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, elite_institutional_class).
narrative_ontology:constraint_victim(individual_revolution_autonomy, disengaged_citizenry).
domain_priors:requires_active_enforcement(individual_revolution_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISENGAGED CITIZEN (SNARE)
% χ = 0.75 * 1.5 (powerless) * 1.0 (national) = 1.125
% Perceived as a predatory trap with no viable exit.
constraint_indexing:constraint_classification(individual_revolution_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL MANAGER (ROPE)
% χ = 0.75 * -0.2 (institutional) * 1.2 (global) = -0.18
% Viewed as essential (if coercive) infrastructure for global stability.
constraint_indexing:constraint_classification(individual_revolution_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ONE-MAN REVOLUTIONARY (ROPE)
% χ = 0.75 * 0.4 (organized/individual_moderate) * 0.8 (local) = 0.24
% By narrowing scope and organizing internal power, extraction becomes manageable coordination.
constraint_indexing:constraint_classification(individual_revolution_autonomy, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035
% High extraction + High suppression + Presence of Coordination (via beneficiary) = Tangled Rope.
constraint_indexing:constraint_classification(individual_revolution_autonomy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_revolution_autonomy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_revolution_autonomy, rope, context(agent_power(institutional), _, _, _)).

test(extraction_scaling) :-
    % Verify that the powerless feel significantly more extraction than the analytical observer
    % (Due to π(powerless)=1.5 vs π(analytical)=1.15)
    constraint_indexing:effective_extraction(individual_revolution_autonomy, context(agent_power(powerless), _, _, national), ChiPowerless),
    constraint_indexing:effective_extraction(individual_revolution_autonomy, context(agent_power(analytical), _, _, national), ChiAnalytical),
    ChiPowerless > ChiAnalytical.

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.75 base extraction reflects the source's claim that mass movements are "elite projects." 
 * The perspectival gap is extreme: the system is a 'Snare' to the trapped citizen but a 'Rope' 
 * (essential coordination) to the elite. The "One-Man Revolution" works by reducing S (Scope) 
 * to 'local' and increasing P (Power) to 'organized' autonomy, which drops effective 
 * extraction (χ) into the Rope category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_autonomy_viability,
    'Can individual autonomy reach a critical mass to trigger systemic decoupling?',
    'Trace whether individual identity shifts (Amish-style) trigger measurable institutional resource-diversion.',
    'If True: Transition to Scaffold. If False: Remains a Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_revolution_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness 0.75 > 0.46)
% Tracks the intensification of extraction and the rise of "theatrical" management.

% Theater ratio: Rising over time indicates metric substitution (Goodhart drift).
narrative_ontology:measurement(ira_tr_t0, individual_revolution_autonomy, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ira_tr_t5, individual_revolution_autonomy, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ira_tr_t10, individual_revolution_autonomy, theater_ratio, 10, 0.45).

% Extraction: Tracking extraction accumulation (rent-seeking layering).
narrative_ontology:measurement(ira_ex_t0, individual_revolution_autonomy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ira_ex_t5, individual_revolution_autonomy, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ira_ex_t10, individual_revolution_autonomy, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
