% ============================================================================
% CONSTRAINT STORY: legal_formalism_overhang
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(legal_formalism_overhang, []).

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
 * * constraint_id: legal_formalism_overhang
 * human_readable: The Ghost of Statutes Past
 * domain: political/legal/social
 * * SUMMARY:
 * A scenario where rigid, literalist adherence to the text of historical laws 
 * persists even after the social or technological context that gave them meaning 
 * has vanished. This "Rope" for judicial predictability acts as a "Snare" for 
 * modern subjects, whose productive agency is liquidated by "zombie laws" 
 * that no longer map to the territory of current reality.
 * * KEY AGENTS:
 * - Modern Litigant: Subject (Powerless)
 * - Judicial Formalist: Beneficiary (Institutional)
 * - Forensic Legal Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the overhang siphons the subject's resources into 
% compliance with non-functional or counter-productive mandates.
domain_priors:base_extractiveness(legal_formalism_overhang, 0.83). 
domain_priors:suppression_score(legal_formalism_overhang, 0.74). 
domain_priors:theater_ratio(legal_formalism_overhang, 0.92). % Extreme theater: meticulous legal ritual masking zero functional utility.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(legal_formalism_overhang, extractiveness, 0.83).
narrative_ontology:constraint_metric(legal_formalism_overhang, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(legal_formalism_overhang, theater_ratio, 0.92).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the litigant, the overhang is a snare: they are legally bound by 
% definitions that have no bearing on the modern dispute.
constraint_indexing:constraint_classification(legal_formalism_overhang, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional judiciary views formalism as a Rope—the only way to 
% coordinate stable expectations and prevent "judicial activism" or chaos.
constraint_indexing:constraint_classification(legal_formalism_overhang, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "rule of law" has become 
% a performative artifact maintained by institutional inertia.
constraint_indexing:constraint_classification(legal_formalism_overhang, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83) and high theater (0.92) as a hybrid signature.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(legal_formalism_overhang, E), E >= 0.50,
    domain_priors:suppression_score(legal_formalism_overhang, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_formalism_overhang_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(legal_formalism_overhang, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_formalism_overhang, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(legal_formalism_overhang, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.83) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(legal_formalism_overhang, E),

    E > 0.70.

:- end_tests(legal_formalism_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of legal stability is eclipsed by the parasitic 
 * nature of obsolete rules.
 * 
 * * PERSPECTIVAL GAP:
 * The Modern Litigant feels a Snare because they are penalized by an 
 * 18th-century logic applied to 21st-century technology. The Judicial 
 * Formalist sees a Rope because the literal text is the only coordination 
 * signal they are permitted to see without "breaking" the institutional model.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "law" is no longer functional relative to justice (Theater 0.92); 
 * it is an inert spike siphoning 0.83 of the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_linguistic_drift,
    'Can the law be "translated" to modern context without losing the Rope (Snare vs Mountain)?',
    'Tracking the success rate of purposive vs formalist interpretation in economic outcomes.',
    'If purpose prevails: Snare of current policy. If literalism persists: Mountain of Semantic Inertia.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(legal_formalism_overhang, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
