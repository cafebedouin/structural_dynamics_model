% ============================================================================
% CONSTRAINT STORY: responsibility_without_power
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(responsibility_without_power, []).

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
 * * constraint_id: responsibility_without_power
 * human_readable: The Scapegoat Architecture
 * domain: organizational/legal/socio-economic
 * * SUMMARY:
 * A scenario where a "Rope" for maintaining system safety or ethical standards 
 * (e.g., liability for automated errors, compliance for supply chains, or 
 * middle-management performance) assigns legal or moral responsibility to a 
 * subject who lacks the actual power or tools to control the outcome. 
 * This coordination substrate becomes a "Snare" as the subject's personal 
 * assets and career agency are liquidated to absorb systemic shocks, 
 * trapping them in a territory of terminal liability where they are 
 * punished for failures inherent to a design they cannot modify. 
 *
 * * KEY AGENTS:
 * - Compliance Officer: Subject (Powerless)
 * - Strategic Architect: Beneficiary (Institutional)
 * - Liability Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the total liquidation of the subject's 
% individual security to subsidize the institution's risk profile.
domain_priors:base_extractiveness(responsibility_without_power, 0.91). 
domain_priors:suppression_score(responsibility_without_power, 0.82). % "Protected" roles or whistleblowing are suppressed by the structural hierarchy.
domain_priors:theater_ratio(responsibility_without_power, 0.93).    % Extreme theater: "Empowerment Seminars" that performatively signal agency while siphoning power.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(responsibility_without_power, extractiveness, 0.91).
narrative_ontology:constraint_metric(responsibility_without_power, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(responsibility_without_power, theater_ratio, 0.93).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The officer is trapped: they are legally responsible for the system's 
% failures, but the lack of power liquidates their primary defensive agency.
constraint_indexing:constraint_classification(responsibility_without_power, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the assignment as a Rope—the essential coordination 
% substrate for ensuring that *someone* is always "on the hook" to satisfy regulators.
constraint_indexing:constraint_classification(responsibility_without_power, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.93) > 0.70 triggers Piton: the "Official Job Description" 
% is an inertial spike; it performatively lists "Authority" while extraction occurs.
constraint_indexing:constraint_classification(responsibility_without_power, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.91) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(responsibility_without_power, E), E >= 0.50,
    domain_priors:suppression_score(responsibility_without_power, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_power_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless officer vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(responsibility_without_power, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_without_power, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.93) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(responsibility_without_power, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (0.91) > 0.70 requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(responsibility_without_power, E),

    E > 0.70.

:- end_tests(responsibility_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of legal legibility is achieved by liquidating the 
 * subject's primary capacity for personal safety.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Compliance Officer feels a Snare because they are an insurance policy 
 * with no control over the risk. The Strategic Architect sees a Rope 
 * because the transfer coordinates the external legibility needed to 
 * maintain the institution's license to operate.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Leadership Training" is no longer functional (Theater 0.93); 
 * it is an inert spike siphoning 0.91 of the subject's agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_liability_decoupling,
    'Can "No-Fault Insurance" restore the Rope, or is scapegoating a physical law of hierarchies (Snare vs Mountain)?',
    'Tracking the delta between "Assigned Responsibility" and "Control Access" in 2026-style gig platforms.',
    'If insurance restores agency: Snare of current design. If it fails: Mountain of Organizational Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(responsibility_without_power, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
