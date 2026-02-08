% ============================================================================
% CONSTRAINT STORY: sleep_debt_externality
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(sleep_debt_externality, []).

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
 * * constraint_id: sleep_debt_externality
 * human_readable: The Circadian Siphon
 * domain: biological/economic/social
 * * SUMMARY:
 * This constraint represents the systemic extraction of biological rest to 
 * fuel 24/7 economic and digital activity. Institutions externalize the cost 
 * of this "sleep debt" onto the individual’s long-term health and cognitive 
 * function. It functions as a Snare for the subject, whose health is 
 * liquidated, while serving as a Rope for a globalized economy that 
 * coordinates across all time zones simultaneously.
 * * KEY AGENTS:
 * - Shift Worker / Heavy User: Subject (Powerless)
 * - 24/7 Service Infrastructure: Beneficiary (Institutional)
 * - Circadian Biologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================= */

% High extraction (0.82) because the system siphons irreducible biological 
% recovery time to generate short-term transactional surplus.
domain_priors:base_extractiveness(sleep_debt_externality, 0.82). 
domain_priors:suppression_score(sleep_debt_externality, 0.65). 
domain_priors:theater_ratio(sleep_debt_externality, 0.38). % Low theater; the exhaustion is physiologically inescapable.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(sleep_debt_externality, extractiveness, 0.82).
narrative_ontology:constraint_metric(sleep_debt_externality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sleep_debt_externality, theater_ratio, 0.38).

% Sleep is a biological invariant; this is not a temporary scaffold.
% narrative_ontology:has_sunset_clause(sleep_debt_externality). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the economic necessity of the "always-on" 
% environment makes adequate rest a luxury they cannot afford.
constraint_indexing:constraint_classification(sleep_debt_externality, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The infrastructure views the elimination of "downtime" as a Rope— 
% the only way to coordinate global logistics and digital markets.
constraint_indexing:constraint_classification(sleep_debt_externality, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a biological standpoint, the need for glymphatic clearance during 
% sleep is an irreducible Mountain of mammalian physics.
constraint_indexing:constraint_classification(sleep_debt_externality, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.82) triggers the hybrid Tangled Rope signature at the 
% civilizational scale.
constraint_indexing:constraint_classification(sleep_debt_externality, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(sleep_debt_externality, E), E >= 0.50,
    domain_priors:suppression_score(sleep_debt_externality, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sleep_debt_externality_tests).

test(perspectival_gap) :-
    % Verify Snare for the individual vs Rope for the global institution.
    constraint_indexing:constraint_classification(sleep_debt_externality, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sleep_debt_externality, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.82) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(sleep_debt_externality, E),

    E > 0.70.

:- end_tests(sleep_debt_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects a "Mandatrophy" state where the 
 * biological maintenance of the human agent is sacrificed for system uptime.
 * 
 * * PERSPECTIVAL GAP:
 * The Shift Worker feels a Snare because their biology is punished for 
 * participating in the modern economy. The 24/7 Infrastructure sees a Rope 
 * because time-zone-agnostic coordination is essential for global market 
 * dominance.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that 
 * global economic coordination is currently "tangled" with the predatory 
 * extraction of the subject's biological recovery period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_sleep_simulation,
    'Can pharmaceutical or digital "compressed sleep" simulate biological rest (Snare or Mountain)?',
    'Clinical trials of glymphatic-enhancing agents vs. traditional REM cycles.',
    'If successful: Snare of access. If impossible: Mountain of Biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(sleep_debt_externality, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
