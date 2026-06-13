% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes a reading of a constitutional text where the
 *   legislature holds ultimate authority over constitutional meaning, often
 *   through mechanisms like 'notwithstanding clauses' or simple legislative
 *   override of judicial decisions. Courts may offer advice or initial
 *   interpretations, but the final say rests with the elected
 *   representatives. This reading prioritizes majoritarian democracy and
 *   legislative flexibility over judicial checks on power.
 *
 * KEY AGENTS:
 *   - legislature: Agenda setter (institutional/generational) — holds final interpretive authority.
 *   - majoritarian_will: Beneficiary (organized/generational) — its preferences are directly enacted.
 *   - judicial_branch: Payer (institutional/generational) — its interpretive authority is advisory, not final.
 *   - minority_rights_advocates: Victim (organized/generational) — their protections are subject to legislative override.
 *   - constitutional_scholars: Observer (analytical/generational) — analyze the structural implications of this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.4).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.3).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'fb126ecc-b917-458c-8af6-fcce15468693').
narrative_ontology:cs_kernel_codification('fb126ecc-b917-458c-8af6-fcce15468693', fixed_text).
narrative_ontology:cs_authority_grounding('fb126ecc-b917-458c-8af6-fcce15468693', lineage).
narrative_ontology:cs_interpretation_layer_present('fb126ecc-b917-458c-8af6-fcce15468693').
narrative_ontology:cs_reading_relation('fb126ecc-b917-458c-8af6-fcce15468693', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb126ecc-b917-458c-8af6-fcce15468693', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fb126ecc-b917-458c-8af6-fcce15468693', foundational, legislative_will_is_supreme).
narrative_ontology:cs_axiom_status(legislative_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('fb126ecc-b917-458c-8af6-fcce15468693', legislative_will_is_supreme, conventional).
narrative_ontology:cs_axiom('fb126ecc-b917-458c-8af6-fcce15468693', foundational, judicial_review_is_advisory).
narrative_ontology:cs_axiom_status(judicial_review_is_advisory, holdable).
narrative_ontology:cs_axiom_grounding('fb126ecc-b917-458c-8af6-fcce15468693', judicial_review_is_advisory, conventional).
narrative_ontology:cs_reference_frame('fb126ecc-b917-458c-8af6-fcce15468693', parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('fb126ecc-b917-458c-8af6-fcce15468693', contemporary_rights_charter_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('fb126ecc-b917-458c-8af6-fcce15468693', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_will).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judicial_branch).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) as the legislature can override judicial decisions, potentially at the expense of minority rights or long-term constitutional principles. Suppression is low (0.3) because the mechanisms for legislative supremacy are typically explicit in the constitutional text or established convention, not covert. Theater ratio is low (0.1) as the legislative override is a direct and functional exercise of power, not merely performative. Accessibility collapse is moderate (0.6) as judicial avenues for challenging legislation are constrained, but not entirely eliminated. Resistance is moderate (0.45) from those advocating for stronger judicial review or minority protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and majoritarian will, this constraint is a Rope, ensuring democratic accountability and responsiveness. From the perspective of the judicial branch and minority rights advocates, it can feel more extractive, as their claims can be overridden. The engine will compute these divergences based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and majoritarian will are clear beneficiaries (d near 0.0) as they gain final interpretive authority. The judicial branch and minority rights advocates are targets (d near 1.0) as their power or protections are subject to legislative override. Constitutional scholars are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its function (ensuring legislative supremacy) is actively maintained. The question is whether this function is genuinely coordinative or becomes extractive when applied to minority rights. The classification as Rope reflects the coordination of majoritarian governance, while the moderate extractiveness and suppression metrics capture the costs borne by other seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_vs_judicial_supremacy,
    'Is the constitutional text truly establishing legislative supremacy, or is this reading a political interpretation that downplays judicial review?',
    'Analysis of constitutional amendment procedures, historical practice of notwithstanding clauses, and the actual legal effect of judicial rulings on legislation.',
    'If legislative supremacy is structurally embedded, the constraint is a Rope coordinating majoritarian will. If it''s a political interpretation, the constraint might be a Tangled Rope where the legislature extracts power from the judiciary and minorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_vs_judicial_supremacy, conceptual, 'Ambiguity between legislative and judicial final authority on constitutional meaning.').

omega_variable(
    reading_of_constitutional_text,
    'This constraint is the ''legislative_sovereignty_reading'' of the ''constitutional_text'' kernel. What would change if a ''judicial_supremacy_reading'' or ''popular_sovereignty_reading'' were adopted?',
    'Empirical observation of legal outcomes and political practice under different constitutional interpretations.',
    'A ''judicial_supremacy_reading'' would shift authority to the courts, potentially increasing protection for minority rights but reducing legislative flexibility. A ''popular_sovereignty_reading'' would emphasize direct citizen involvement, potentially destabilizing both legislative and judicial authority. This reading emphasizes majoritarian will and legislative flexibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_constitutional_text, conceptual, 'Impact of alternative readings of the constitutional text kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text' kernel, alongside 'judicial_supremacy_reading' and 'popular_sovereignty_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
