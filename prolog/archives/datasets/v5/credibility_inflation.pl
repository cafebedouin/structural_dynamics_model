% ============================================================================
% CONSTRAINT STORY: credibility_inflation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(credibility_inflation, []).

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
 * * constraint_id: credibility_inflation
 * human_readable: The Meritocratic Dilution
 * domain: social/academic/economic
 * * SUMMARY:
 * A scenario where the metrics used to signal competence or trust (e.g., 
 * academic degrees, certifications, social media verification) proliferate 
 * so rapidly that their marginal signal value approaches zero. This "Rope" 
 * for coordinating talent and trust becomes a "Snare" for the individual, 
 * who must constantly acquire more expensive and redundant credentials to 
 * maintain the same relative social position, liquidating their time and capital.
 * * KEY AGENTS:
 * - Entry-Level Applicant: Subject (Powerless)
 * - Credentialing Body: Beneficiary (Institutional)
 * - Labor Market Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.82) reflects the siphoning of subject resources into 
% non-productive "signaling" to maintain institutional status-quo.
domain_priors:base_extractiveness(credibility_inflation, 0.82). 
domain_priors:suppression_score(credibility_inflation, 0.71). % Alternatives (e.g., portfolio-based hiring) are suppressed by legacy norms.
domain_priors:theater_ratio(credibility_inflation, 0.89).    % High theater: ceremonies and "prestige" rituals masking metric decay.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(credibility_inflation, extractiveness, 0.82).
narrative_ontology:constraint_metric(credibility_inflation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(credibility_inflation, theater_ratio, 0.89).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The applicant is trapped: failing to participate results in exclusion, 
% while participating offers no actual gain in capability, only survival.
constraint_indexing:constraint_classification(credibility_inflation, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the credentialing system as a Rope—the only way to 
% coordinate "standards" and filter massive pools of human capital.
constraint_indexing:constraint_classification(credibility_inflation, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Degree" is an inertial 
% spike; it remains a requirement only because it has always been one.
constraint_indexing:constraint_classification(credibility_inflation, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.82) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(credibility_inflation, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(credibility_inflation, E), E >= 0.50,
    domain_priors:suppression_score(credibility_inflation, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credibility_inflation_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless applicant vs Rope for the institution.
    constraint_indexing:constraint_classification(credibility_inflation, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credibility_inflation, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(credibility_inflation, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (>0.70) triggers mandatory resolution logic.
    domain_priors:base_extractiveness(credibility_inflation, E),

    E > 0.70.

:- end_tests(credibility_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of credentials has been consumed by the 
 * inflationary cycle of competitive signaling.
 * 
 * * PERSPECTIVAL GAP:
 * The Applicant feels a Snare because they are running a race with 
 * no finish line. The Institution sees a Rope because the signals 
 * coordinate stable hierarchies and revenue streams.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Meritocracy" is no longer functional (Theater 0.89); 
 * it is an inert spike siphoning 0.82 of the subject's future surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_signal_collapse,
    'Does the signal system collapse when the extraction reaches 1.0, or does it become a Mountain of law (Snare vs Mountain)?',
    'Tracking the adoption rate of "uncredentialed" skill validation in high-stakes sectors.',
    'If adoption rises: Snare of current norms. If adoption fails: Mountain of social hierarchy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(credibility_inflation, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
