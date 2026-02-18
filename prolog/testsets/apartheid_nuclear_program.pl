% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(apartheid_nuclear_program, []).

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
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: apartheid_nuclear_program
 * human_readable: Apartheid South Africa's Clandestine Nuclear Program
 * domain: political/military/technological
 * * SUMMARY:
 * A clandestine weapons program aimed at securing the survival of the minority 
 * regime through nuclear deterrence. Fuelled by an extractive energy economy 
 * and forced labor under the guise of civil electrification.
 * * KEY AGENTS:
 * - The Apartheid State: Beneficiary (Institutional) - Seeking sovereign security.
 * - Renfrew Christie (Scholar-Spy): Auditor (Analytical) - Mapped energy requirements.
 * - Forced Laborers: Subject (Powerless) - Experienced high labor extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.85). %
domain_priors:suppression_score(apartheid_nuclear_program, 0.90).   %
domain_priors:theater_ratio(apartheid_nuclear_program, 0.20).       % Clandestine focus; low performative signaling.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(apartheid_nuclear_program, extractiveness, 0.85).
narrative_ontology:constraint_metric(apartheid_nuclear_program, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(apartheid_nuclear_program, theater_ratio, 0.2).

% Constraint classification claim
narrative_ontology:constraint_claim(apartheid_nuclear_program, tangled_rope).
narrative_ontology:human_readable(apartheid_nuclear_program, "Apartheid South Africa's Clandestine Nuclear Program").
narrative_ontology:topic_domain(apartheid_nuclear_program, "political/military/technological").

% Constraint metric facts used by the classification engine
domain_priors:requires_active_enforcement(apartheid_nuclear_program).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(apartheid_nuclear_program, apartheid_regime).
narrative_ontology:constraint_victim(apartheid_nuclear_program, forced_laborers).
narrative_ontology:constraint_victim(apartheid_nuclear_program, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE APARTHEID REGIME (ROPE)
constraint_indexing:constraint_classification(apartheid_nuclear_program, rope, 
    context(agent_power(institutional), time_horizon(historical), exit_options(arbitrage), spatial_scope(national))).

% PERSPECTIVE 2: THE POLITICAL PRISONER (SNARE)
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare, 
    context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))).

% PERSPECTIVE 4: THE NUCLEAR PHYSICIST (MOUNTAIN)
constraint_indexing:constraint_classification(apartheid_nuclear_program, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(apartheid_nuclear_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypeRegime, context(institutional, historical, arbitrage, national)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypePrisoner, context(powerless, biographical, trapped, local)),
    TypeRegime \= TypePrisoner.

test(extraction_signature) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E >= 0.46. % Confirms high-extraction signature for drift detection requirement.

:- end_tests(apartheid_nuclear_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) is high because the program was funded by 
 * extreme labor exploitation in coal mines. 
 * * MANDATROPHY ANALYSIS:
 * Resolved via Tangled Rope for the Auditor, acknowledging that the 
 * program solved a coordination problem (energy/deterrence) through 
 * predatory extraction of rights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apartheid_extraction_intent,
    'Was the 0.85 extraction a functional necessity for survival or predatory racial ideology?',
    'Audit of state energy expenditures vs mining profits during clandestine phase',
    'If necessity: Mountain. If predatory: Snare/Mandatrophy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(apartheid_nuclear_program, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time: Rising as the program shifts from secret research (0.05)
% to using the facility for performative strategic deterrence (0.20).
narrative_ontology:measurement(anp_tr_t0, apartheid_nuclear_program, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anp_tr_t5, apartheid_nuclear_program, theater_ratio, 5, 0.12).
narrative_ontology:measurement(anp_tr_t10, apartheid_nuclear_program, theater_ratio, 10, 0.20).

% Extraction over time: Hardening of the labor-extraction snare as the 
% project requirements for "the cheapest electricity" intensified.
narrative_ontology:measurement(anp_ex_t0, apartheid_nuclear_program, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(anp_ex_t5, apartheid_nuclear_program, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(anp_ex_t10, apartheid_nuclear_program, base_extractiveness, 10, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
