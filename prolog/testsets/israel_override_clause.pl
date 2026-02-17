% ============================================================================
% CONSTRAINT STORY: israel_override_clause
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_israel_override_clause, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: israel_override_clause
 * human_readable: The 61-Member Judicial Override Clause
 * domain: political
 * * SUMMARY:
 * A proposed mechanism to allow the Knesset, by a majority of 61 out of 120 MKs,
 * to bypass Supreme Court rulings that invalidate legislation for violating
 * Basic Laws. This effectively shifts the final word on constitutionality from
 * the judiciary to the ruling coalition.
 * * KEY AGENTS:
 * - Minority Groups: Subject (Powerless) - Vulnerable to majoritarian legislation without judicial recourse.
 * - Ruling Coalition: Beneficiary (Institutional) - Gains the ability to implement policies (e.g., draft exemptions, land regularization) previously blocked by the court.
 * - Supreme Court: Auditor (Analytical) - Historically the sole balancing mechanism in the Israeli system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% [RESOLVED MANDATROPHY]
% Extraction is high (0.75) because it extracts the protected status of
% constitutional rights (equality, dignity) and reclaims them as political
% assets for the majority.
domain_priors:base_extractiveness(israel_override_clause, 0.75).

% Suppression is very high (0.85). The clause is designed specifically to
% suppress the "alternative" of judicial review for specific laws.
domain_priors:suppression_score(israel_override_clause, 0.85).

% Theater ratio is significant (0.35) as the proposal is frequently framed
% in the theatrical language of "pure democracy" and "sovereignty".
domain_priors:theater_ratio(israel_override_clause, 0.35).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(israel_override_clause, extractiveness, 0.75).
narrative_ontology:constraint_metric(israel_override_clause, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(israel_override_clause, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
% Proponents frame it as a tool for enacting the democratic will of the majority.
narrative_ontology:constraint_claim(israel_override_clause, tangled_rope).
narrative_ontology:human_readable(israel_override_clause, "The 61-Member Judicial Override Clause").

% Binary flags
domain_priors:requires_active_enforcement(israel_override_clause). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(israel_override_clause, ruling_coalition).
narrative_ontology:constraint_victim(israel_override_clause, minority_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE VULNERABLE MINORITY (SNARE)
% For groups relying on judicial protection (e.g., women in public spaces,
% Arab citizens, secular draft advocates), the clause is a Snare that
% traps them under the absolute will of a 61-seat majority.
% χ = 0.75 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 1.125.
constraint_indexing:constraint_classification(israel_override_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE COALITION GOVERNMENT (ROPE)
% Viewed by proponents as a Rope (coordination) that allows the government
% to fulfill its democratic mandate and solve governance paralysis caused
% by "judicial overreach".
% χ = 0.75 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.15.
constraint_indexing:constraint_classification(israel_override_clause, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE HISTORICAL ANALYST (TANGLED ROPE)
% Viewed as a Tangled Rope: it provides coordination for the ruling bloc
% but does so by extracting the fundamental "checks and balances" required
% for long-term democratic stability.
% χ = 0.75 (ε) * 1.15 (π(analytical)) * 1.0 (σ(national)) = 0.8625.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_override_clause_tests).

test(perspectival_gap) :-
    % Verify it is a Rope for institutions but a Snare for the powerless.
    constraint_indexing:constraint_classification(israel_override_clause, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the high extraction value is consistent with a Snare/Tangled Rope classification.
    domain_priors:base_extractiveness(israel_override_clause, E),
    E >= 0.46,
    % And that the structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(israel_override_clause, _),
    narrative_ontology:constraint_victim(israel_override_clause, _),
    domain_priors:requires_active_enforcement(israel_override_clause).

:- end_tests(israel_override_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Override Clause represents the ultimate "Perspectival Gap." The
 * institutional logic (Rope) argues that a 61-seat majority *is* the
 * democratic coordinate. However, because Israel lacks a formal written
 * constitution, a second house, or regional representation, the analytical
 * view (Tangled Rope) sees this coordination as inextricably linked to
 * the extraction of minority protections. The scores reflect this: high
 * base extraction (0.75) and suppression (0.85) capture the removal of
 * judicial review, a core check on power.
 *
 * * MANDATROPHY ANALYSIS:
 * The high extractiveness (0.75) is resolved by the "Tangled Rope"
 * classification. This prevents the system from misclassifying the constraint
 * as a pure Snare, which would ignore its genuine (though contentious)
 * coordination function for the ruling coalition. The classification is
 * structurally justified by the presence of clear beneficiaries (ruling_coalition),
 * victims (minority_groups), and the requirement for active enforcement
 * (passing override legislation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_override_majority,
    'Would a higher majority (e.g., 70 or 80 MKs) convert the clause from a Snare into a genuine Rope of broad consensus?',
    'Comparative analysis of legislative consensus levels in countries with similar override mechanisms (e.g., Canada).',
    'High majority requirements might preserve the "Rope" (coordination) without the "Snare" (predatory extraction).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% The interval represents the period of intense political debate and legislative
% push for this clause, peaking in 2022-2023.
narrative_ontology:interval(israel_override_clause, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's intensification from a theoretical
% political idea to a concrete, high-stakes legislative proposal.
% This is required as base_extractiveness > 0.46.

% Theater ratio over time (framing it as "restoring democracy"):
narrative_ontology:measurement(israel_override_clause_tr_t0, israel_override_clause, theater_ratio, 0, 0.10).
narrative_ontology:measurement(israel_override_clause_tr_t5, israel_override_clause, theater_ratio, 5, 0.30).
narrative_ontology:measurement(israel_override_clause_tr_t10, israel_override_clause, theater_ratio, 10, 0.35).

% Extraction over time (from idea to actionable policy):
narrative_ontology:measurement(israel_override_clause_ex_t0, israel_override_clause, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(israel_override_clause_ex_t5, israel_override_clause, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(israel_override_clause_ex_t10, israel_override_clause, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The clause functions as a mechanism to enforce the will of the legislative
% majority over the judiciary.
narrative_ontology:coordination_type(israel_override_clause, enforcement_mechanism).

% narrative_ontology:boltzmann_floor_override(israel_override_clause, 0.0).
% narrative_ontology:affects_constraint(israel_override_clause, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */