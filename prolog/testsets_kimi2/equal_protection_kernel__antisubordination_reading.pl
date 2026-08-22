% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause â Antisubordination Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the antisubordination reading of the
 *   Equal Protection Clause: the constitutional principle that the clause
 *   targets caste-like subordination of historically oppressed groups rather
 *   than racial classification per se. Under this reading, state action that
 *   entrenches hierarchy is forbidden and race-conscious action that
 *   dismantles it is permitted. The reading is one of three structurally
 *   distinct interpretations of the same constitutional kernel, decomposed
 *   per the Îµ-invariance principle because the colorblind, remedial, and
 *   antisubordination readings carry different beneficiary/victim structures,
 *   different enforcement requirements, and different empirical claims about
 *   constitutional meaning.
 *
 * KEY AGENTS:
 *   - Subordinated castes: Primary beneficiary (organized/constrained) â receive protective and remedial constitutional coverage.
 *   - State governments: Primary payer (institutional/constrained) â lose policy autonomy to entrench hierarchy, may be compelled to remediate.
 *   - Dominant groups: Secondary payer (powerful/constrained) â lose legal standing to challenge race-conscious remedial measures.
 *   - Federal judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the antisubordination principle.
 *   - Civil rights organizations: Observer (organized/analytical) â litigate and theorize the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.62).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.48).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause â Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '5e72515c-3c8f-479a-a4a1-583cc80b0d60').
narrative_ontology:cs_kernel_codification('5e72515c-3c8f-479a-a4a1-583cc80b0d60', fixed_text).
narrative_ontology:cs_authority_grounding('5e72515c-3c8f-479a-a4a1-583cc80b0d60', lineage).
narrative_ontology:cs_interpretation_layer_present('5e72515c-3c8f-479a-a4a1-583cc80b0d60').
narrative_ontology:cs_reading_relation('5e72515c-3c8f-479a-a4a1-583cc80b0d60', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('5e72515c-3c8f-479a-a4a1-583cc80b0d60', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('5e72515c-3c8f-479a-a4a1-583cc80b0d60', foundational, state_may_dismantle_subordination_via_race_conscious_action).
narrative_ontology:cs_axiom_status(state_may_dismantle_subordination_via_race_conscious_action, holdable).
narrative_ontology:cs_axiom_grounding('5e72515c-3c8f-479a-a4a1-583cc80b0d60', state_may_dismantle_subordination_via_race_conscious_action, conventional).
narrative_ontology:cs_axiom('5e72515c-3c8f-479a-a4a1-583cc80b0d60', foundational, dominant_groups_lack_equal_protection_claim_against_remediation).
narrative_ontology:cs_axiom_status(dominant_groups_lack_equal_protection_claim_against_remediation, holdable).
narrative_ontology:cs_axiom_grounding('5e72515c-3c8f-479a-a4a1-583cc80b0d60', dominant_groups_lack_equal_protection_claim_against_remediation, conventional).
narrative_ontology:cs_reference_frame('5e72515c-3c8f-479a-a4a1-583cc80b0d60', reconstruction_antisubordination_mandate).
narrative_ontology:cs_drift_state('5e72515c-3c8f-479a-a4a1-583cc80b0d60', contemporary_colorblind_retrenchment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5e72515c-3c8f-479a-a4a1-583cc80b0d60', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, subordinated_castes).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_governments).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically oppressed racial and ethnic groups who are the intended beneficiaries of the antisubordination principle. They receive constitutional protection against state-enforced caste-like hierarchies and may be the direct beneficiaries of race-conscious remedial measures in education and public policy. They cannot exit the constitutional order but rely on judicial enforcement and political mobilization to vindicate claims.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, subordinated_castes, beneficiary,
    organized, generational, constrained, national).

% State and local governmental actors bound by federal constitutional interpretation. They lose policy autonomy to enact or maintain laws and practices that entrench racial hierarchy, and may be compelled to undertake affirmative remedial measures. Compliance is mandatory; exit requires constitutional amendment or defiance risking sanctions and loss of federal funding.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Socially dominant racial groups who, under this reading, cannot successfully invoke the Equal Protection Clause to block race-conscious remedial measures designed to dismantle subordination. They bear the cost of lost legal standing and policy preferences in education, employment, and contracting.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups, payer,
    powerful, biographical, constrained, national).

% Federal courts that articulate and enforce the antisubordination reading through constitutional interpretation. They determine what constitutes caste-like subordination, authorize race-conscious remedies, and reject colorblind challenges. They administer the constraint without being its direct material beneficiaries.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocacy organizations and legal clinics that litigate antisubordination claims, develop doctrinal theory, and monitor state compliance. They operate as analytical observers and organized advocates, shaping how the constraint is understood and enforced.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_organizations, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state entrenchment of caste-like racial hierarchies and authorizes race-conscious state action to dismantle existing subordination, coordinating a constitutional order toward substantive equality rather than formal neutrality.
% TRANSFER_FUNCTION: Transfers policy autonomy away from state actors who would entrench hierarchy, and transfers legal standing to challenge remedial measures away from dominant groups, toward subordinated castes who gain protective and remedial constitutional coverage.
% ABSENT_VOICES: Conservative legal scholars and dominant-group litigants who advocate for the colorblind reading are formally present in litigation but their substantive claims are structurally excluded from prevailing under this reading; their objections are heard yet the doctrinal framework is not designed to accommodate them.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished overnight, constitutional doctrine would revert toward colorblind or purely remedial frameworks. State actors would regain broader latitude to enact formally neutral policies that entrench hierarchy; subordinated castes would lose both the protective shield against caste-like subordination and the affirmative authorization for race-conscious remedial measures. The landscape of civil rights litigation and education policy would reorganize around different constitutional premises.
% FOUNDING_PROBLEM: The post-Civil War problem of state-sponsored caste systems and Black Codes that maintained racial subordination through formal and informal state action after the abolition of slavery.
% FOUNDING_PROBLEM_CORROBORATION: Historical Reconstruction-era congressional records attest the founding problem. Contemporary critical race theorists and civil rights historians attest it persists in altered forms. However, dominant-group political actors and originalist jurists attest the founding problem is historically bounded and the antisubordination reading now operates beyond its mandate. Corroboration from outside the direct beneficiary set includes constitutional historians and some moderate jurists who acknowledge persistent structural inequality but dispute the antisubordination framework as the appropriate remedy.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the reading asymmetrically strips state policy autonomy and dominant-group legal standing, transferring protective and remedial capacity to subordinated castes. Suppression (0.48) is moderate: the reading actively suppresses hierarchical alternatives and colorblind challenges through judicial review, but these alternatives persist as live political and doctrinal positions. Theater_ratio (0.42) reflects growing performative maintenance as the reading has shifted from controlling doctrine to prominent dissent and academic argument while retaining institutional forms. Accessibility_collapse (0.60) captures the legal closure of explicitly hierarchical policies, though covert alternatives persist. Resistance (0.75) is high due to sustained political and juridical pushback from colorblind advocates and state actors. The measurement series share a single time grid (0â70) to prevent misaligned substitution errors.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (state governments, dominant groups) and the beneficiary seat (subordinated castes) should compute divergent constraint types. From the state-government seat, the reading appears as an actively enforced extraction of traditional police powers and policy discretion. From the subordinated-caste seat, it appears as protective coordination against caste-like hierarchy. The federal judiciary may experience it as coordinative given its role in structuring constitutional order. The engine computes this divergence from the structural beneficiary/victim declarations and differentiated exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated castes sit at the beneficiary end: the constraint subsidizes their constitutional claims and remedial prospects. State governments and dominant groups sit at the target end: the constraint extracts policy autonomy and legal standing from them. The federal judiciary sits near symmetric as agenda-setter; it administers the constraint without being a direct material beneficiary, though it gains institutional authority from interpretive control. Civil rights organizations sit near the beneficiary end analytically but do not directly collect the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The antisubordination reading avoids mandatrophy mislabeling because its founding problem (post-Civil War state-sponsored caste systems) is contested rather than dead. The reading is not a piton: it still carries genuine coordination function (preventing hierarchy) and identifiable extraction (asymmetric limits on state and dominant-group power). It is not a snare because the coordination is not cover: the protection of subordinated groups is structurally central, not decorative. It is not a scaffold because it lacks a sunset clause; the reading is conceived as a permanent constitutional principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_classification_boundary,
    'Where is the operational line between permissible race-conscious classification and impermissible caste-like subordination in contemporary institutions?',
    'Sociological institutional analysis of disparate outcomes combined with longitudinal judicial doctrine tracking to identify where courts actually draw the line.',
    'If the boundary cannot be stabilized independently of the reading itself, the constraint collapses into either the remedial reading (narrower) or the colorblind reading (broader), dissolving its distinct structural identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_classification_boundary, conceptual, 'Indeterminacy of the subordination threshold').

omega_variable(
    remedial_measures_extraction_or_restoration,
    'Do race-conscious remedial measures authorized by this reading constitute extraction from dominant groups and state actors, or restoration of a baseline eroded by historical subordination?',
    'Comparative analysis of policy outcomes and resource allocations across jurisdictions with and without antisubordination frameworks, measured against historical baselines.',
    'If restoration, effective extraction is lower than the structural measure suggests and the constraint trends toward rope for remedial phases; if extraction, the asymmetric cost confirms tangled_rope or snare classification for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_measures_extraction_or_restoration, empirical, 'Whether remedial authorization is restorative or extractive').

omega_variable(
    colorblind_retrenchment_piton_risk,
    'Given the current Supreme Court majority''s embrace of the colorblind reading, does the antisubordination reading persist now primarily as theatrical performance in dissent and academia rather than as an operative constraint?',
    'Track controlling judicial outcomes over the next decade; if the reading produces no binding decisions and survives only in non-majority opinions, reclassify as piton.',
    'If piton, base_extractiveness should be revised downward, theater_ratio upward, and the constraint''s active enforcement flag re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_retrenchment_piton_risk, empirical, 'Whether retrenchment has reduced the reading to inertial performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__antisubordination_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__antisubordination_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__antisubordination_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(equa_tr_t60, equal_protection_kernel__antisubordination_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(equa_tr_t70, equal_protection_kernel__antisubordination_reading, theater_ratio, 70, 0.62).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__antisubordination_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__antisubordination_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__antisubordination_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(equa_be_t60, equal_protection_kernel__antisubordination_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(equa_be_t70, equal_protection_kernel__antisubordination_reading, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__antisubordination_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__antisubordination_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__antisubordination_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(equa_su_t60, equal_protection_kernel__antisubordination_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(equa_su_t70, equal_protection_kernel__antisubordination_reading, suppression_requirement, 70, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_kernel, decomposed per the Îµ-invariance principle because the colorblind, remedial, and antisubordination readings have different beneficiary/victim structures, different Îµ values, and different enforcement requirements. The readings are mutually exclusive at the level of controlling doctrine though they coexist in legal discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
