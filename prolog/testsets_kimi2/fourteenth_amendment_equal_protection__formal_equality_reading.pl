% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection Formal Equality Reading (Anti-Classification)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the formal equality reading of the
 *   Fourteenth Amendment's Equal Protection Clause: the state may not
 *   explicitly classify individuals by race or status absent a compelling
 *   justification. The reading treats racial neutrality as the constitutional
 *   default and places race-conscious remedial programs in the victim set. It
 *   is one reading of a contested kernel; the sibling anti-caste reading
 *   holds that the Clause requires active state dismantling of hierarchy. The
 *   constraint carries both a genuine coordination function (preventing
 *   arbitrary state discrimination) and asymmetric extraction (blocking
 *   redress for structural inequality while protecting existing
 *   distributions).
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/analytical) â administers and enforces the formal equality doctrine through constitutional interpretation
 *   - classification_challengers: beneficiary (organized/constrained) â litigants who invoke the doctrine to invalidate race-conscious state action
 *   - state_governments: payer (institutional/constrained) â state actors whose remedial and redistributive policy tools are constitutionally limited
 *   - race_conscious_beneficiaries: payer (powerless/trapped) â communities denied access to blocked remedial programs
 *   - anti_caste_jurists: excluded (organized/constrained) â scholars and advocates structurally marginalized by the formal equality interpretive framework
 *   - constitutional_historians: observer (analytical/analytical) â external empirical and historical analysts of the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.52).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection Formal Equality Reading (Anti-Classification)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '9dbc014f-dc87-417c-80d7-fbbe15be5e54').
narrative_ontology:cs_kernel_codification('9dbc014f-dc87-417c-80d7-fbbe15be5e54', fixed_text).
narrative_ontology:cs_authority_grounding('9dbc014f-dc87-417c-80d7-fbbe15be5e54', lineage).
narrative_ontology:cs_interpretation_layer_present('9dbc014f-dc87-417c-80d7-fbbe15be5e54').
narrative_ontology:cs_reading_relation('9dbc014f-dc87-417c-80d7-fbbe15be5e54', fourteenth_amendment_equal_protection__anti_caste_reading, forecloses).
narrative_ontology:cs_axiom('9dbc014f-dc87-417c-80d7-fbbe15be5e54', foundational, explicit_racial_classification_presumptively_invalid).
narrative_ontology:cs_axiom_status(explicit_racial_classification_presumptively_invalid, holdable).
narrative_ontology:cs_axiom_grounding('9dbc014f-dc87-417c-80d7-fbbe15be5e54', explicit_racial_classification_presumptively_invalid, conventional).
narrative_ontology:cs_axiom('9dbc014f-dc87-417c-80d7-fbbe15be5e54', foundational, state_neutrality_constitutional_default).
narrative_ontology:cs_axiom_status(state_neutrality_constitutional_default, holdable).
narrative_ontology:cs_axiom_grounding('9dbc014f-dc87-417c-80d7-fbbe15be5e54', state_neutrality_constitutional_default, deontological).
narrative_ontology:cs_reference_frame('9dbc014f-dc87-417c-80d7-fbbe15be5e54', formal_neutrality_constitutional_order).
narrative_ontology:cs_drift_state('9dbc014f-dc87-417c-80d7-fbbe15be5e54', contemporary_anti_caste_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9dbc014f-dc87-417c-80d7-fbbe15be5e54', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, classification_challengers).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_governments).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, race_conscious_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Equal Protection Clause through judicial review, applying a presumption against explicit racial or status-based state classification. Maintains that neutrality is the constitutional default and that deviations require compelling justification.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Litigants and advocacy organizations that invoke formal equality to challenge state racial classifications, including affirmative action and race-conscious remedial programs. They benefit from judicial precedents that treat explicit race-consciousness as presumptively unconstitutional.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, classification_challengers, beneficiary,
    organized, biographical, constrained, national).

% State and local legislative and administrative bodies that are constitutionally barred from using explicit racial or status classifications in policymaking, including in remedial or redistributive programs. Must design law and administration around the prohibition, even when seeking to correct structural inequality.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Individuals and communities who would benefit from race-conscious remedial programs, set-asides, or representation mechanisms but are denied access because such programs are struck down under the formal equality framework. Cannot readily substitute the blocked state remedy through private action.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, race_conscious_beneficiaries, payer,
    powerless, generational, trapped, national).

% Legal scholars and advocates who argue that the Fourteenth Amendment mandates active dismantling of caste hierarchy and that formal neutrality perpetuates substantive inequality. Their framework is structurally marginalized in the dominant equal protection jurisprudence, treated as outside the acceptable interpretive bounds.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_jurists, excluded,
    organized, generational, constrained, national).

% Academic historians who study the original purposes of the Fourteenth Amendment and the subsequent evolution of equal protection doctrine, providing empirical and historical evidence about whether the formal equality reading captures the provision's founding intent.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary state discrimination by establishing a uniform rule against explicit racial or status classification, removing the coordination problem of which groups the state may rank and under what criteria.
% TRANSFER_FUNCTION: Transfers constitutional protection from potential beneficiaries of race-conscious state remediation to parties challenging explicit classification, and transfers policy discretion away from state legislatures toward judicial enforcement of neutrality.
% ABSENT_VOICES: Anti-caste theorists and advocates of structural remediation are structurally excluded from the formal equality interpretive framework; they would argue that colorblindness locks in pre-existing hierarchy but are treated as offering legally irrelevant arguments in the dominant doctrine.
% DISAPPEARANCE_RATIONALE: If the formal equality prohibition vanished overnight, state actors would resume explicit racial classification for both discriminatory and remedial purposes; affirmative action programs would expand; anti-discrimination doctrine would reorganize around anti-caste or disparate-impact frameworks rather than classification prohibition.
% FOUNDING_PROBLEM: Post-Civil War state-sanctioned racial discrimination embodied in the Black Codes and Jim Crow, which used explicit racial classifications to subordinate newly freed slaves and maintain white supremacy.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction historians attest to the founding problem of state-mandated racial discrimination. However, the claim that a formal equality readingâas opposed to an anti-caste readingâwas the intended or adequate solution is contested by critical legal historians and anti-caste jurists outside the formal equality beneficiary set.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 (low-moderate) because the constraint is primarily a negative prohibition on state action rather than an active resource-transfer mechanism; the 'low Îµ for race-conscious remedies' guidance is reflected in the moderate base score. Suppression (0.52) reflects the active judicial enforcement that strikes down democratically enacted remedial legislation. Accessibility collapse (0.68) is high because once the formal equality framework is accepted, race-conscious alternatives are constitutionally foreclosed. Resistance (0.60) captures sustained opposition from civil rights advocates and critical legal scholars. The temporal series show a mid-interval peak in extraction as the doctrine shifted from attacking invidious discrimination to attacking affirmative action, with a slight recent plateau.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal judiciary) and the beneficiary seat (classification challengers) experience the constraint as the enforcement of neutral constitutional principle. The payer seats (state governments, race-conscious beneficiaries) experience the same doctrine as the constitutionalization of colorblindness that blocks redress. The excluded seat (anti-caste jurists) experiences the constraint as a silencing mechanism that treats their central normative commitment as legally illegitimate. The engine computes this divergence from structural data rather than authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits near the beneficiary end of the directionality axis: it defines and administers the constraint without bearing its costs. Classification challengers are explicit beneficiaries (low d). State governments are constrained payers (moderate-high d): they lose policy flexibility. Race-conscious beneficiaries are trapped payers (high d): they cannot exit the loss of remedial programs because the barrier is constitutional. Anti-caste jurists are excluded rather than coordinated; their exclusion is the enforcement mechanism by which the interpretive frame is maintained.
 *
 * MANDATROPHY ANALYSIS:
 *   The formal equality reading prevents mandatrophy mislabeling by preserving the genuine coordination function of the original anti-discrimination principle (preventing arbitrary state classification) while naming the asymmetric extraction that occurs when the same principle blocks remedial action. Without the Tangled Rope classification, the doctrine would appear as either pure coordination (Rope) if only the anti-Jim Crow function were observed, or pure extraction (Snare) if only the anti-affirmative-action function were observed. The structural data force both functions into the same account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_anti_caste_framing,
    'Does the Equal Protection Clause prohibit all explicit racial classification, or does it require active state dismantling of caste hierarchy?',
    'Historical-legal analysis of Framers'' intent combined with sociological evidence on whether formal neutrality produces substantive equality in contemporary conditions.',
    'Determines whether the constraint''s victim set is limited to arbitrary discriminators or includes state redress agents; resolves the kernel in favor of one reading or the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_anti_caste_framing, conceptual, 'Core interpretive contest between formal equality and anti-caste readings of the Equal Protection Clause').

omega_variable(
    structural_inequality_background_assumption,
    'Is structural racial inequality a pre-constitutional background condition that the state may ignore, or is it a constitutionally cognizable harm that justifies race-conscious state action?',
    'Empirical social science on the persistence and causes of racial inequality; constitutional history of the Fourteenth Amendment''s Reconstruction purposes.',
    'If structural inequality is constitutionally cognizable, the formal equality reading''s treatment of it as background is a false summit or tangled rope benefiting the status quo; if it is genuinely background, the formal equality constraint''s neutrality claim is structurally vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_inequality_background_assumption, empirical, 'Whether structural inequality is pre-constitutional background or active constitutional harm').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of race-conscious alternatives structural (constitutional text and precedent) or internalized (broad social acceptance of colorblindness as a normative ideal)?',
    'Cross-jurisdictional comparison: if jurisdictions without formal equality constitutional doctrines still resist race-conscious remediation, suppression is partially internalized; if resistance tracks the doctrine''s presence, suppression is structural.',
    'If internalized, effective suppression persists even if the constitutional doctrine were revised, complicating reclassification; if purely structural, changing the doctrine would substantially alter the constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of race-conscious alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(four_tr_t60, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(four_be_t20, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(four_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(four_be_t60, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(four_su_t60, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
