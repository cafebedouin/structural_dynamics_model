% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'constitutional nullity' reading of
 *   the statutory debt ceiling, which posits that the 14th Amendment Section
 *   4 (Public Debt Clause) renders any statutory limit on the government's
 *   ability to pay its debts unconstitutional and therefore legally void.
 *   Under this reading, the debt ceiling is a political artifact with no
 *   legal force, and the Treasury is obligated to issue debt as necessary to
 *   cover appropriations. The constraint is classified as a Mountain because
 *   its legal inoperability is seen as a fixed constitutional reality, not a
 *   human-enforced choice. Its persistence is purely theatrical, as its
 *   actual legal effect is zero.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.02).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '570150a4-3f53-409c-918c-3577ae70f8e1').
narrative_ontology:cs_kernel_codification('570150a4-3f53-409c-918c-3577ae70f8e1', fixed_text).
narrative_ontology:cs_authority_grounding('570150a4-3f53-409c-918c-3577ae70f8e1', lineage).
narrative_ontology:cs_interpretation_layer_present('570150a4-3f53-409c-918c-3577ae70f8e1').
narrative_ontology:cs_reading_relation('570150a4-3f53-409c-918c-3577ae70f8e1', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('570150a4-3f53-409c-918c-3577ae70f8e1', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('570150a4-3f53-409c-918c-3577ae70f8e1', foundational, public_debt_validity_unquestionable).
narrative_ontology:cs_axiom_status(public_debt_validity_unquestionable, holdable).
narrative_ontology:cs_axiom_grounding('570150a4-3f53-409c-918c-3577ae70f8e1', public_debt_validity_unquestionable, deontological).
narrative_ontology:cs_axiom('570150a4-3f53-409c-918c-3577ae70f8e1', foundational, congressional_appropriations_mandate_payment).
narrative_ontology:cs_axiom_status(congressional_appropriations_mandate_payment, holdable).
narrative_ontology:cs_axiom_grounding('570150a4-3f53-409c-918c-3577ae70f8e1', congressional_appropriations_mandate_payment, conventional).
narrative_ontology:cs_reference_frame('570150a4-3f53-409c-918c-3577ae70f8e1', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('570150a4-3f53-409c-918c-3577ae70f8e1', contemporary_political_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('570150a4-3f53-409c-918c-3577ae70f8e1', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, federal_creditors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, us_congress).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_four).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the Treasury is constitutionally obligated to pay all debts incurred by lawful appropriations, rendering the debt ceiling statutorily void. It would continue to issue debt as needed, treating the ceiling as a legal nullity.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_treasury, agenda_setter,
    institutional, immediate, analytical, national).

% Under this reading, Congress's votes on the debt ceiling are ceremonial or ignored, as the 14th Amendment Section 4 supersedes any statutory limit. Its power to appropriate funds remains, but its power to block payment of those funds is nullified.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_congress, payer,
    institutional, biographical, constrained, national).

% Under this reading, federal creditors are assured that the 'validity of the public debt... shall not be questioned,' guaranteeing payment regardless of political maneuvering around the debt ceiling. This maintains confidence in US sovereign debt.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_creditors, beneficiary,
    powerful, generational, mobile, global).

% Analyze the legal arguments for and against the debt ceiling's constitutionality, particularly in light of the 14th Amendment. This reading aligns with a specific interpretation of constitutional supremacy.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the continuous payment of legally incurred federal obligations, preventing default and maintaining the full faith and credit of the United States, by nullifying any statutory impediment.
% TRANSFER_FUNCTION: No direct transfer function, as the constraint itself is read as inoperative. It prevents the transfer of sovereign risk from the government to creditors and the public.
% ABSENT_VOICES: Political factions that use the debt ceiling as leverage for policy concessions are effectively silenced by this reading, as their threat of default is rendered constitutionally void.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling 'disappeared' (i.e., was formally declared unconstitutional), the actual fiscal operations of the US government would remain unchanged under this reading, as the 14th Amendment already renders it inoperative. The Treasury would continue to borrow as needed to pay appropriations.
% FOUNDING_PROBLEM: The 14th Amendment Section 4 was enacted to prevent questioning the validity of the public debt, particularly debt incurred during the Civil War, ensuring the stability of the Union's finances.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal experts widely corroborate the historical context and intent of the 14th Amendment Section 4. While its application to the modern debt ceiling is debated, the original problem of ensuring debt validity remains relevant to sovereign credit.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05) because, under this reading, the debt ceiling cannot legally extract anything; it is a nullity. Suppression is also near zero (0.02) as there is no legitimate legal mechanism to suppress borrowing. Theater ratio is extremely high (0.95) because the entire exercise of debating and voting on the debt ceiling is considered a political performance, devoid of actual legal consequence. Accessibility collapse is high (0.9) because the constitutional argument collapses any statutory alternative to paying the debt. Resistance is low (0.01) because the constitutional principle is seen as settled, even if politically contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those who hold this reading, the debt ceiling is a Mountain (a fixed constitutional reality). From the perspective of those who see it as a coordination mechanism or an extraction tool, it would be classified differently. This story captures only the constitutional nullity reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury is the agenda-setter, as it would continue to execute borrowing regardless of the ceiling. Federal creditors are beneficiaries, as their claims are constitutionally protected. The US Congress, particularly those seeking to use the ceiling for leverage, are payers in the sense that their political tool is rendered ineffective. Constitutional scholars are observers, analyzing the legal landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading, the debt ceiling's original mandate (to control borrowing) is superseded by a higher constitutional mandate (to pay debts). The constraint is effectively mandatrophied from a legal perspective, persisting only as a political ritual. The high theater ratio reflects this. The classification as a Mountain prevents mislabeling this as a functional coordination or extraction mechanism, instead highlighting its legal inertness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_enforcement_ambiguity,
    'Would a court actually enforce the 14th Amendment Section 4 to nullify the debt ceiling, or would it defer to Congress on a ''political question''?',
    'A direct legal challenge to the debt ceiling''s constitutionality, resulting in a Supreme Court ruling on the matter.',
    'If a court enforced it, this reading''s ''Mountain'' classification would be judicially affirmed. If a court deferred, the legal nullity would remain an academic argument, and the constraint''s practical effect might lean more towards a ''Tangled Rope'' or ''Snare'' in practice, depending on political enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_enforcement_ambiguity, empirical, 'Uncertainty regarding judicial willingness to intervene in a political dispute over the debt ceiling''s constitutionality.').

omega_variable(
    alternative_readings_validity,
    'Is this ''constitutional nullity'' reading the only valid interpretation of the 14th Amendment Section 4 in relation to the debt ceiling, or do other readings (e.g., coordination, extraction) hold equal or greater legal/political weight?',
    'Continued legal and political debate, shifts in judicial philosophy, or a constitutional amendment clarifying the issue.',
    'If other readings gain prominence, this constraint''s classification as a Mountain would be challenged, potentially reclassifying it as a ''Tangled Rope'' (coordination) or ''Snare'' (extraction) depending on the dominant interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_validity, conceptual, 'The conceptual contestability of the debt ceiling''s legal status among different constitutional interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.95).
narrative_ontology:measurement(stat_tr_t1945, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1945, 0.95).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1980, 0.95).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2000, 0.95).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2010, 0.95).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2024, 0.95).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1945, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1917, 0.02).
narrative_ontology:measurement(stat_su_t1945, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1945, 0.02).
narrative_ontology:measurement(stat_su_t1980, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2010, 0.02).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the statutory debt ceiling kernel. This 'constitutional nullity' reading asserts the debt ceiling is legally inoperative due to the 14th Amendment Section 4. The 'coordination scaffold' reading views it as a procedural mechanism, and the 'extraction snare' reading sees it as a tool for political leverage. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
