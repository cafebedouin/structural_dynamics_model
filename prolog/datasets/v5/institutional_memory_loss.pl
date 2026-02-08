% ============================================================================
% CONSTRAINT STORY: institutional_memory_loss
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(institutional_memory_loss, []).

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
 * * constraint_id: institutional_memory_loss
 * human_readable: The Amnesiac Organization
 * domain: organizational/political/technological
 * * SUMMARY:
 * A scenario where rapid personnel turnover, over-reliance on ephemeral digital 
 * communications, and the retirement of "tacit knowledge" holders cause an 
 * institution to lose the "why" behind its own internal constraints. This 
 * "Rope" for maintaining administrative consistency becomes a "Snare" for 
 * new members, who are forced to follow obsolete or dangerous protocols 
 * they can no longer explain, liquidating the organization's adaptive agency.
 * * KEY AGENTS:
 * - New Employee: Subject (Powerless)
 * - Legacy Administrator: Beneficiary (Institutional)
 * - Knowledge Management Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) reflects the siphoning of subject agency into the 
% maintenance of "ghost protocols" that no longer serve a functional purpose.
domain_priors:base_extractiveness(institutional_memory_loss, 0.84). 
domain_priors:suppression_score(institutional_memory_loss, 0.73). % High suppression: questioning the "why" is seen as a breach of protocol.
domain_priors:theater_ratio(institutional_memory_loss, 0.91).    % Extreme theater: meticulous "onboarding" rituals masking a lack of substance.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, 0.84).
narrative_ontology:constraint_metric(institutional_memory_loss, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(institutional_memory_loss, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The new hire is trapped: they must obey rules they don't understand to 
% survive the bureaucracy, while the actual rationale for those rules is lost.
constraint_indexing:constraint_classification(institutional_memory_loss, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the memory loss as a Rope—it ensures absolute 
% procedural compliance and coordination without the "friction" of debate.
constraint_indexing:constraint_classification(institutional_memory_loss, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Standard Operating Procedure" 
% is an inert spike; it remains in place solely through institutional inertia.
constraint_indexing:constraint_classification(institutional_memory_loss, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and high theater (0.91) as a hybrid signature.
constraint_indexing:constraint_classification(institutional_memory_loss, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(institutional_memory_loss, E), E >= 0.50,
    domain_priors:suppression_score(institutional_memory_loss, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_memory_loss_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless employee vs Rope for the institutional administrator.
    constraint_indexing:constraint_classification(institutional_memory_loss, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_memory_loss, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(institutional_memory_loss, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(institutional_memory_loss, E),

    E > 0.70.

:- end_tests(institutional_memory_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of institutional consistency is achieved by 
 * liquidating the subject's primary reasoning agency.
 
 * * PERSPECTIVAL GAP:
 * The New Employee feels a Snare because they are running a machine they 
 * don't have the manual for. The Administrator sees a Rope because the 
 * preservation of form coordinates the stability of the hierarchy.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Knowledge Base" is no longer functional (Theater 0.91); 
 * it is an inert spike siphoning 0.84 of the subject's cognitive surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_tacit_recovery,
    'Can tacit knowledge be digitized, or is its loss a Mountain of biology (Snare vs Mountain)?',
    'Tracking the failure rate of "AI-reconstructed" protocols in high-risk environments.',
    'If AI succeeds: Snare of current storage tech. If AI fails: Mountain of Human Experience.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_memory_loss, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
