% ============================================================================
% CONSTRAINT STORY: infrastructure_interoperability_decay
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(infrastructure_interoperability_decay, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: infrastructure_interoperability_decay
 * human_readable: The Protocol Silo Trap
 * domain: technological/logistical/economic
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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, extractiveness, 0.86).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
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
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(infrastructure_interoperability_decay, E), E >= 0.50,
    domain_priors:suppression_score(infrastructure_interoperability_decay, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interoperability_decay_tests).

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

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(infrastructure_interoperability_decay, E),

    E > 0.70.

:- end_tests(interoperability_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a digital ecosystem is achieved by liquidating 
 * the subject's primary capacity for cross-platform agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Independent Logistics Operator feels a Snare because they pay a tax 
 * on every transaction that crosses an artificial boundary. The Platform 
 * Provider sees a Rope because the silos coordinate a highly profitable 
 * and "curated" marketplace for their specific users.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Open API" pledge is no longer functional (Theater 0.91); 
 * it is an inert spike siphoning 0.86 of the species' logistical surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_standard_reconvergence,
    'Can "Public Good" protocols restore the Rope, or is silo-formation a physical law of platform economics (Snare vs Mountain)?',
    'Tracking the market share of un-walled logistics protocols versus proprietary giants through 2027.',
    'If un-walled protocols scale: Snare of current design. If they fail: Mountain of Economic Gravity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(infrastructure_interoperability_decay, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
