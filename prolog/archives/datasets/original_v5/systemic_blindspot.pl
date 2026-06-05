% ============================================================================
% CONSTRAINT STORY: systemic_blindspot
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(systemic_blindspot, []).

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
 * * constraint_id: systemic_blindspot
 * human_readable: The Incalculable Tail-Risk
 * domain: cognitive/organizational/economic
 * * SUMMARY:
 * A scenario where an institutional framework's metrics and sensors are 
 * fundamentally incapable of detecting a rising existential risk because 
 * the risk falls outside the system's "legible" taxonomy. It functions as a 
 * Rope for internal coordination and career stability within the institution, 
 * but represents a massive Snare for the broader population whose survival 
 * depends on the system's (missing) peripheral vision.
 * * KEY AGENTS:
 * - Marginalized Citizen: Subject (Powerless)
 * - Risk Assessment Bureau: Beneficiary (Institutional)
 * - Epistemological Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) because the blindspot allows for the unchecked 
% accumulation of catastrophic risk while siphoning resources into "safety" 
% metrics that measure the wrong variables.
domain_priors:base_extractiveness(systemic_blindspot, 0.86). 
domain_priors:suppression_score(systemic_blindspot, 0.70). 
domain_priors:theater_ratio(systemic_blindspot, 0.82). % High theater: focus on precise reporting of irrelevant data.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(systemic_blindspot, extractiveness, 0.86).
narrative_ontology:constraint_metric(systemic_blindspot, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(systemic_blindspot, theater_ratio, 0.82).

% This is an inherent property of bounded rationality in institutions.
% narrative_ontology:has_sunset_clause(systemic_blindspot). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the blindspot is a snare: they are aware of the 
% danger but are ignored by the system because their data is "anecdotal."
constraint_indexing:constraint_classification(systemic_blindspot, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the restricted taxonomy as a Rope—a way to ensure 
% consistent, high-fidelity coordination and "objective" reporting.
constraint_indexing:constraint_classification(systemic_blindspot, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "risk management" process 
% is a non-functional, performative spike maintained by institutional inertia.
constraint_indexing:constraint_classification(systemic_blindspot, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(systemic_blindspot, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of high coordination efficiency (Ropes) 
% masking predatory, unpriced risk accumulation (Snare).
constraint_indexing:constraint_classification(systemic_blindspot, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(systemic_blindspot, E), E >= 0.50,
    domain_priors:suppression_score(systemic_blindspot, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(systemic_blindspot_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(systemic_blindspot, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(systemic_blindspot, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(systemic_blindspot, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.86) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(systemic_blindspot, E),

    E > 0.70.

:- end_tests(systemic_blindspot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * system's "coordination" has atrophied into a performative loop that 
 * liquidates species-level safety for institutional peace of mind.
 * 
 * * PERSPECTIVAL GAP:
 * The Marginalized Citizen feels a Snare because they are trapped in a 
 * reality that the system refuses to "see." The Risk Bureau sees a Rope 
 * because their metrics provide the illusion of total control and 
 * organizational predictability.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. This identifies 
 * that the "coordination" is no longer functional relative to reality 
 * (Theater 0.82), making it an inert spike of logic that extracts 
 * resources from the subjects it was designed to protect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_ontological_expansion,
    'Can the system expand its taxonomy to include the "invisible" risk (Snare or Mountain)?',
    'Auditing the speed of institutional adoption of "out-of-framework" signals.',
    'If taxonomy adapts: Snare of policy. If taxonomy is rigid: Mountain of Cognitive Closure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(systemic_blindspot, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
