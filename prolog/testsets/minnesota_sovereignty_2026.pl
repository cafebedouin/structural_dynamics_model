% ============================================================================
% CONSTRAINT STORY: minnesota_sovereignty_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_minnesota_sovereignty_2026, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: minnesota_sovereignty_2026
 * human_readable: Operation Metro Surge (Minnesota Crisis)
 * domain: political/social
 * * SUMMARY:
 * Operation Metro Surge represents a high-friction federal enforcement action in 
 * Minnesota. Despite a drawdown of 700 agents, the presence of 2,300 
 * personnel and fatal shootings (e.g., Alex Pretti) have converted a 
 * "Rope" of rule-of-law into a "Snare" of civil disruption.
 * * KEY AGENTS:
 * - Local Residents/Students: Subject (Powerless)
 * - Federal Enforcement (ICE/Border Czar): Beneficiary (Institutional)
 * - Legislative Leaders (Schumer/Jeffries): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.68) due to the "steep drop in attendance" at schools 
% and the loss of civilian life.
domain_priors:base_extractiveness(minnesota_sovereignty_2026, 0.68). 

% Suppression is extremely high (0.92) as federal agents use face coverings and 
% operate near "sensitive locations" like the School of St. Philip.
domain_priors:suppression_score(minnesota_sovereignty_2026, 0.92).   

% Theater ratio is high (0.75) as the 700-agent drawdown is framed as a return 
% to "pre-operation levels" while 2,300 agents remain.
domain_priors:theater_ratio(minnesota_sovereignty_2026, 0.75).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(minnesota_sovereignty_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(minnesota_sovereignty_2026, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(minnesota_sovereignty_2026, theater_ratio, 0.75).

% Constraint classification claim
narrative_ontology:constraint_claim(minnesota_sovereignty_2026, piton).

% Primary keys for the classification engine
% Active enforcement is the core of this constraint.
domain_priors:requires_active_enforcement(minnesota_sovereignty_2026).

% Beneficiaries and Victims for high-extraction (E > 0.46)
narrative_ontology:constraint_beneficiary(minnesota_sovereignty_2026, federal_executive_branch).
narrative_ontology:constraint_victim(minnesota_sovereignty_2026, minnesota_civic_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Local residents and school districts view the operation as a Snare, 
% citing disrupted learning environments and fatal force.
constraint_indexing:constraint_classification(minnesota_sovereignty_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Federal leadership (Homan) views the operation as a Rope—essential 
% infrastructure for national border security.
constraint_indexing:constraint_classification(minnesota_sovereignty_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% With 65% of Americans believing ICE has "gone too far," the observer 
% sees a Piton: inertial enforcement that has lost its functional mandate.
constraint_indexing:constraint_classification(minnesota_sovereignty_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% The "legislative guardrails" proposed by Schumer/Jeffries attempt to 
% reclassify the action as a Scaffold with mandatory limits.
constraint_indexing:constraint_classification(minnesota_sovereignty_2026, scaffold,
    context(agent_power(organized),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(minnesota_sovereignty_2026).

/* ==========================================================================
   4. VALIDATION TESTS (v3.4 Suite)
   ========================================================================== */

:- begin_tests(minnesota_sovereignty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(minnesota_sovereignty_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(minnesota_sovereignty_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_accumulation) :-
    domain_priors:base_extractiveness(minnesota_sovereignty_2026, E),
    E > 0.46.

test(piton_threshold) :-
    domain_priors:theater_ratio(minnesota_sovereignty_2026, TR),
    TR >= 0.70.

:- end_tests(minnesota_sovereignty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is driven by the lethal outcomes and school 
 * disruptions. The 'Theater Ratio' is high (0.75) because the 
 * announced "drawdown" of 700 agents is performative relative to the 
 * 2,300 agents who remain active in the state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% [RESOLVED MANDATROPHY]
% High extraction (0.68) triggers the requirement for an omega variable.

omega_variable(
    omega_mn_2026,
    'Will the "Legislative Guardrails" be adopted or remains a Snare?',
    'Congressional vote on the Schumer-Jeffries formal proposal.',
    'Adoption converts the Snare to a Scaffold; Rejection maintains the Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(minnesota_sovereignty_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Tracking the escalation of "Operation Metro Surge" over its second month.
% Theater ratio rises as "drawdown" rhetoric replaces actual departure.
narrative_ontology:measurement(mn_tr_t0, minnesota_sovereignty_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mn_tr_t5, minnesota_sovereignty_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(mn_tr_t10, minnesota_sovereignty_2026, theater_ratio, 10, 0.75).

% Extraction rises sharply following the fatal shootings and school lawsuits.
narrative_ontology:measurement(mn_ex_t0, minnesota_sovereignty_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(mn_ex_t5, minnesota_sovereignty_2026, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(mn_ex_t10, minnesota_sovereignty_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
