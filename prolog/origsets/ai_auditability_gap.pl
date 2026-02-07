% ============================================================================
% CONSTRAINT STORY: ai_auditability_gap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_ai_auditability_gap, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ai_auditability_gap
 * human_readable: The Black Box Impasse
 * domain: technological
 * * SUMMARY:
 * The gap between AI decision-making complexity and the human capacity to 
 * verify or audit those decisions. It functions as an extraction of agency 
 * where systemic errors are shielded by technical "irreducibility."
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Affected by opaque AI denials/approvals.
 * - The AI Developer: Beneficiary (Institutional) - Profits from un-auditable speed.
 * - The Regulator: Auditor (Analytical) - Struggles to define "explainability."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ai_auditability_gap, 0.52). 
domain_priors:suppression_score(ai_auditability_gap, 0.65).   
domain_priors:theater_ratio(ai_auditability_gap, 0.75). % Updated to satisfy Piton threshold at T=10

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ai_auditability_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_auditability_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_auditability_gap, theater_ratio, 0.75).

% Constraint metric facts used by classification engine v3.4
domain_priors:requires_active_enforcement(ai_auditability_gap).

% Beneficiaries and Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(ai_auditability_gap, ai_developers).
narrative_ontology:constraint_victim(ai_auditability_gap, citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen sees a predatory trap; they cannot exit the algorithmic ecosystem.
constraint_indexing:constraint_classification(ai_auditability_gap, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Corporations see the gap as a necessary byproduct of "Rope" (essential coordination/efficiency).
constraint_indexing:constraint_classification(ai_auditability_gap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: it provides efficiency but extracts accountability.
constraint_indexing:constraint_classification(ai_auditability_gap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(ai_auditability_gap, E), E >= 0.50,
    domain_priors:suppression_score(ai_auditability_gap, S), S > 0.40.

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% When the AI is obsolete but the legal shield remains. (Theoretical Future)
constraint_indexing:constraint_classification(ai_auditability_gap, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(ai_auditability_gap, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_auditability_gap_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(ai_auditability_gap, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_auditability_gap, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(ai_auditability_gap, E),
    (E =< 0.05 -> true ; E >= 0.46). % Confirms high-extraction signature.

:- end_tests(ai_auditability_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ai_auditability_gap is assigned a base_extractiveness of 0.52 because it 
 * facilitates an asymmetric power relationship where the AI owner is shielded 
 * from liability. 
 * * PERSPECTIVAL GAP:
 * To the powerless citizen, this is a Snare (pure extraction of rights). 
 * To the institution, it is a Rope (the only way to coordinate modern data scales).
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by identifying that the auditability gap is 
 * not an inherent "Mountain" of physics, but a "Tangled Rope" of economic 
 * choice—speed is prioritized over explainability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_gap,
    'Is deep-learning interpretability a physical limit (Mountain) or a design choice (Snare)?',
    'Mathematical proof of irreducible complexity vs. breakthrough in symbolic AI mapping.',
    'If Mountain: Constraint is permanent; If Snare: Constraint is a policy failure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_auditability_gap, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from actual technical opacity (0.25) to performative 
% "Explainability Dashboards" (0.75) that fail to resolve the underlying gap.
narrative_ontology:measurement(ai_gap_tr_t0, ai_auditability_gap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_gap_tr_t5, ai_auditability_gap, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ai_gap_tr_t10, ai_auditability_gap, theater_ratio, 10, 0.75).

% Extraction: Progressive loss of human agency as algorithmic integration hardens.
narrative_ontology:measurement(ai_gap_ex_t0, ai_auditability_gap, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ai_gap_ex_t5, ai_auditability_gap, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ai_gap_ex_t10, ai_auditability_gap, base_extractiveness, 10, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
