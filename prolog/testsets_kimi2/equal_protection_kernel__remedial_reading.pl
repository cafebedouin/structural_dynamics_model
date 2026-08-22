% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Remedial Reading â Race-Conscious Admissions Doctrine
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint story models the remedial reading of the Equal Protection
 *   Clause as applied to higher education admissions: a judicial doctrine
 *   that permits race-conscious state action when narrowly tailored to remedy
 *   documented historical exclusion or to achieve a compelling student-body
 *   diversity interest. The constraint is one of three sibling readings of
 *   the equal_protection_kernel; it treats the Constitution as permitting
 *   (under strict scrutiny) exactly what the colorblind reading forbids
 *   categorically. The story authors a tangled_rope classification because
 *   the doctrine simultaneously coordinates remedial access for historically
 *   excluded groups and extracts admission opportunity from displaced
 *   applicants who would have been admitted under a race-blind regime.
 *
 * KEY AGENTS:
 *   - supreme_court: Analytical observer â sets and revises the doctrinal framework through precedent.
 *   - universities: Agenda-setter â designs and defends race-conscious admissions policies within the legal framework.
 *   - historically_excluded_applicants: Beneficiary â gains admission opportunities under the race-as-plus-factor regime.
 *   - displaced_applicants: Primary target â bears the cost of reduced admission prospects due to race-conscious preferences.
 *   - colorblind_advocates: Excluded voice â structurally marginalized by the permission structure of the remedial reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Remedial Reading â Race-Conscious Admissions Doctrine").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '3949ec6c-c89b-48bd-8172-bff752f290c6').
narrative_ontology:cs_kernel_codification('3949ec6c-c89b-48bd-8172-bff752f290c6', fixed_text).
narrative_ontology:cs_authority_grounding('3949ec6c-c89b-48bd-8172-bff752f290c6', lineage).
narrative_ontology:cs_interpretation_layer_present('3949ec6c-c89b-48bd-8172-bff752f290c6').
narrative_ontology:cs_reading_relation('3949ec6c-c89b-48bd-8172-bff752f290c6', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('3949ec6c-c89b-48bd-8172-bff752f290c6', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('3949ec6c-c89b-48bd-8172-bff752f290c6', foundational, remedial_race_consciousness_permissible).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_permissible, holdable).
narrative_ontology:cs_axiom_grounding('3949ec6c-c89b-48bd-8172-bff752f290c6', remedial_race_consciousness_permissible, deontological).
narrative_ontology:cs_axiom('3949ec6c-c89b-48bd-8172-bff752f290c6', foundational, student_body_diversity_compelling).
narrative_ontology:cs_axiom_status(student_body_diversity_compelling, holdable).
narrative_ontology:cs_axiom_grounding('3949ec6c-c89b-48bd-8172-bff752f290c6', student_body_diversity_compelling, empirically_contingent).
narrative_ontology:cs_reference_frame('3949ec6c-c89b-48bd-8172-bff752f290c6', remedial_equality_framework).
narrative_ontology:cs_drift_state('3949ec6c-c89b-48bd-8172-bff752f290c6', post_sffa_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3949ec6c-c89b-48bd-8172-bff752f290c6', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, displaced_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the doctrinal framework through majority opinions, concurrences, and dissents; applies strict scrutiny to race-conscious policies while permitting narrowly tailored remedies; its interpretive moves determine what state action survives.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Design, implement, and defend race-conscious admissions policies; bear the burden of proving narrow tailoring and compelling interest in litigation; their operational autonomy is structured by the doctrinal framework and threatened by withdrawal of federal funding or accreditation if they deviate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities, agenda_setter,
    institutional, generational, constrained, national).

% Receive consideration as applicants from underrepresented groups; gain admission opportunities that would be reduced or absent under a race-blind regime; their educational trajectory is partially shaped by the constraint's permission structure.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Applicants with stronger objective academic credentials who are denied admission because race-conscious preferences shift the composition of the admitted class; they bear the cost of reduced institutional access and attendant lifetime socioeconomic opportunity.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, displaced_applicants, payer,
    moderate, immediate, constrained, national).

% Advocate for a constitutional rule that categorically forbids state use of racial classifications; their preferred policy regime is structurally excluded by the remedial reading's permission structure, though they mount legal and political challenges to it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Remedies the lingering effects of historical racial exclusion in selective higher education and assembles diverse learning environments that are claimed to improve pedagogical and civic outcomes for all students.
% TRANSFER_FUNCTION: Moves admission offers, institutional prestige, and lifetime socioeconomic opportunity from academically strong applicants who would have been admitted under a race-blind process to historically excluded racial groups, mediated by university admissions committees.
% ABSENT_VOICES: Displaced applicants are rarely represented in the doctrinal construction; the litigation is brought by specific rejected applicants or reverse-discrimination plaintiffs, but the broader class of applicants who silently lose preferred admission status under the plus-factor regime is not structurally present in the deliberative forum.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, universities would immediately revert to race-blind metrics, the demographic composition of selective institutions would shift, historically excluded groups would face reduced access, and the admissions landscape would reorganize around standardized testing and class-based proxies.
% FOUNDING_PROBLEM: Persistent exclusion of historically disadvantaged racial minorities from selective public and private institutions due to legacies of segregation, unequal primary schooling, and biased admissions criteria.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and social scientists outside the immediate beneficiary set attest to the historical reality of segregation and exclusion; conservative legal scholars and colorblind advocates contest that the founding problem persists at a level justifying racial classification. Corroboration is thus split along methodological and ideological lines, with no uncontested outside seat.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the doctrine authorizes a direct transfer of admission slots based on racial classification. Suppression (0.65) reflects the binding force of Supreme Court precedent foreclosing the colorblind alternative for institutions wishing to use race. Theater_ratio (0.55) captures the growing performative quality of strict scrutiny rhetoric, where narrow tailoring is claimed but rarely disqualifies a plan. Accessibility_collapse (0.60) registers that race-blind alternatives become legally disfavored once the remedial reading is accepted. Resistance (0.80) is very high due to persistent litigation, state ballot initiatives, and ideological polarization. The measurement series show extraction rising through the middle of the interval and then dropping sharply at the end as the doctrine faces overriding judicial rejection.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (historically excluded applicants) experiences the constraint as opening access and remedying historical injustice. The payer seat (displaced applicants) experiences the same doctrine as an extraction of their admission opportunity based on racial classification. The Supreme Court observer seat sees a balancing test, while the university seat sees a compliance framework that structures their admissions design. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded applicants are beneficiaries (the constraint subsidizes their access, pushing d toward the beneficiary end). Displaced applicants are payers (the constraint extracts from their admission prospects, pushing d toward the target end). Universities sit near symmetric: they gain operational flexibility but bear litigation and compliance costs. The Supreme Court is an analytical seat; its directionality reverts to the canonical fallback. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine was founded to remedy segregation and exclusion. Over the interval, the diversity rationale partially displaced the remedial rationale, and the constraint persisted beyond the original problem's most acute phase. However, the constraint does not read as pure piton because the coordination function (diverse educational environments) is still actively claimed and litigated, and the extraction is not merely theatrical. The sharp doctrinal challenge at the interval's end represents an axiom-overriding event that may push the constraint toward extinction or transformation, but the remedial reading itself has not become purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_diversity_ambiguity,
    'Does the remedial reading conflate two structurally distinct constraintsâbackward-looking remedial action and forward-looking diversity interestâeach with different beneficiary structures and evidentiary requirements?',
    'Decompose into separate constraint stories if empirical analysis shows different epsilon values; judicial treatment already applies slightly different evidentiary standards to each rationale.',
    'If they are distinct, the current story''s epsilon is an average of two different constraints, blurring classification and potentially masking a higher extraction rate in one sub-constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_diversity_ambiguity, conceptual, 'Whether remedial and diversity rationales are structurally separable constraints.').

omega_variable(
    kernel_reading_position,
    'This constraint is the remedial reading of the equal_protection_kernel. How would the structural classification change if the colorblind reading were adopted instead?',
    'Compare the two as separate constraint stories; the colorblind reading would flip the beneficiary and victim sets and eliminate the race-conscious coordination function entirely.',
    'Would likely reclassify as rope (if colorblind neutrality is treated as genuine coordination) or mountain (if treated as an inherent constitutional principle), removing the asymmetric extraction that defines this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural delta between remedial and colorblind readings of the same kernel.').

omega_variable(
    narrow_tailoring_efficacy,
    'Is the narrow tailoring requirement a genuine structural limit on extraction, or has it become performative theater where nearly any admissions design survives review?',
    'Empirical study of strict scrutiny outcomes in federal courts: measure the percentage of race-conscious plans struck down versus upheld after applying the narrow tailoring test.',
    'If performative, theater_ratio is higher and the constraint''s extraction is less constrained than the doctrinal rhetoric claims, pushing the computed type closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_efficacy, empirical, 'Whether strict scrutiny is a genuine brake or rhetorical cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epr_remedial_tr_t0, equal_protection_kernel__remedial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(epr_remedial_tr_t8, equal_protection_kernel__remedial_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(epr_remedial_tr_t16, equal_protection_kernel__remedial_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(epr_remedial_tr_t24, equal_protection_kernel__remedial_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(epr_remedial_tr_t32, equal_protection_kernel__remedial_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(epr_remedial_tr_t40, equal_protection_kernel__remedial_reading, theater_ratio, 40, 0.65).
narrative_ontology:measurement(epr_remedial_tr_t45, equal_protection_kernel__remedial_reading, theater_ratio, 45, 0.75).

% Extraction over time
narrative_ontology:measurement(epr_remedial_be_t0, equal_protection_kernel__remedial_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(epr_remedial_be_t8, equal_protection_kernel__remedial_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(epr_remedial_be_t16, equal_protection_kernel__remedial_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(epr_remedial_be_t24, equal_protection_kernel__remedial_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(epr_remedial_be_t32, equal_protection_kernel__remedial_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(epr_remedial_be_t40, equal_protection_kernel__remedial_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(epr_remedial_be_t45, equal_protection_kernel__remedial_reading, base_extractiveness, 45, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(equal_protection_kernel__remedial_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, antisubordination_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three readingsâremedial, colorblind, and antisubordinationâeach instantiating a distinct constraint with its own epsilon, beneficiary structure, and classification. This story is the remedial reading; the siblings are separate constraints linked in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
