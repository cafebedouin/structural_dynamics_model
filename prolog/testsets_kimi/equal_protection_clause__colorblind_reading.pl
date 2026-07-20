% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause â Colorblind Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint instantiates the colorblind reading of the Equal
 *   Protection Clause: the constitutional rule that government may never
 *   classify individuals by race. Under this reading, the Clause operates as
 *   a permanent, formal prohibition on all governmental racial categories,
 *   treating individuals as rights-bearers wholly independent of group
 *   membership. It is one reading of a contested kernel; the diversity and
 *   remedial readings are structurally foreclosed by its core premise. The
 *   constraint coordinates a multiracial polity by supplying a bright-line
 *   rule, while extracting policy discretion from governmental institutions
 *   and foreclosing race-conscious remedial tools.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers: Beneficiaries (organized/mobile) â protected from governmental racial classification
 *   - governmental_institutions: Payers (institutional/constrained) â lose race-conscious policy tools and must redesign compliance
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â enforces the prohibition through judicial review
 *   - diversity_equity_advocates: Excluded (moderate/constrained) â argue for race-conscious approaches barred by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.2).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.55).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause â Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'd3e7d8ad-bc83-4c28-af6f-621d50cbc2ef').
narrative_ontology:cs_kernel_codification('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', fixed_text).
narrative_ontology:cs_authority_grounding('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', lineage).
narrative_ontology:cs_interpretation_layer_present('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef').
narrative_ontology:cs_reading_relation('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', foundational, fourteenth_amendment_text_mandates_colorblindness).
narrative_ontology:cs_axiom_status(fourteenth_amendment_text_mandates_colorblindness, holdable).
narrative_ontology:cs_axiom_grounding('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', fourteenth_amendment_text_mandates_colorblindness, conventional).
narrative_ontology:cs_axiom('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', foundational, racial_classifications_violate_intrinsic_human_equality).
narrative_ontology:cs_axiom_status(racial_classifications_violate_intrinsic_human_equality, holdable).
narrative_ontology:cs_axiom_grounding('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', racial_classifications_violate_intrinsic_human_equality, deontological).
narrative_ontology:cs_reference_frame('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', race_neutral_government).
narrative_ontology:cs_drift_state('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', affirmative_action_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3e7d8ad-bc83-4c28-af6f-621d50cbc2ef', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_rights_bearers).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, governmental_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals subject to government action who are protected from racial classification; their rights are assessed individually without regard to racial group membership, and they may invoke judicial review when classified by race.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_rights_bearers, beneficiary,
    organized, generational, mobile, national).

% Public universities, agencies, and legislatures forbidden from using racial classifications in admissions, employment, contracting, and other decisions; they must redesign policies to comply and forfeit race-conscious policy tools that had been used to pursue diversity or remediation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, governmental_institutions, payer,
    institutional, generational, constrained, national).

% Courts that interpret and enforce the Equal Protection Clause under the colorblind reading, striking down race-conscious policies and setting the constitutional standard that governmental racial classifications are per se invalid.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Actors who argue race-conscious policies serve compelling interests in diversity or remediation; their preferred approaches are constitutionally barred under this reading and excluded from the enforceable policy set, though they remain active in public discourse and dissent.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, diversity_equity_advocates, excluded,
    moderate, generational, constrained, national).

narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents governmental actors from using racial classifications in decision-making, establishing a uniform rule of individual treatment independent of group membership across a multiracial polity.
% TRANSFER_FUNCTION: Transfers policy discretion away from governmental institutions by forbidding race-conscious tools, conferring on individual rights-bearers a guarantee of assessment without racial classification.
% ABSENT_VOICES: Advocates for race-conscious remediation and diversity interests are structurally excluded from the enforceable constitutional framework under this reading; they would argue that group-based remedies are necessary for substantive equality but are treated as constitutionally impermissible.
% DISAPPEARANCE_RATIONALE: If the colorblind prohibition vanished, governmental institutions would immediately resume race-conscious policymaking in admissions, contracting, and employment; the legal architecture of individual-rights formalism would collapse, and constitutional doctrine would reorganize around group-based or diversity-based frameworks.
% FOUNDING_PROBLEM: The post-Civil War problem of state-mandated racial caste systems (slavery, Black Codes, Jim Crow) in which government explicitly classified and subordinated citizens by race.
% FOUNDING_PROBLEM_CORROBORATION: Historical record and post-Civil War congressional debates attest the founding problem. Contemporary civil rights historians and constitutional scholars outside the colorblind-reading beneficiary set corroborate that de jure racial subordination has ended, though they dispute whether the colorblind reading remains the appropriate tool for current conditions.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.20) because the constraint operates primarily as a negative prohibition on state action rather than a resource transfer. Suppression is moderate (0.55) because race-conscious policy alternatives are legally foreclosed and judicially suppressed. Theater ratio is low (0.15) as enforcement consists mainly of genuine constitutional adjudication rather than performative compliance. Accessibility collapse is high (0.75) because once the colorblind rule is announced, governmental actors understand race-conscious alternatives are legally unavailable. Resistance is moderate-to-high (0.60) because institutions and affected groups actively litigate and lobby against the constraint. The temporal series shows suppression_requirement oscillating with the rise and fall of competing readings, reaching its nadir during the diversity-reading era and recovering sharply after the SFFA decision.
 *
 * PERSPECTIVAL GAP:
 *   From the individual rights-bearer's seat, the constraint appears as protective coordination (a shield against state racial classification). From the governmental institution's seat, it appears as an active limitation on policy autonomy that forecloses tools the institution views as serving compelling interests. The engine computes this divergence from structural data: same constraint, opposite directionalities depending on whether the agent is a beneficiary of the prohibition or a payer in policy discretion.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual_rights_bearers are declared beneficiaries; their directionality sits near the beneficiary pole (low d), yielding negligible effective extraction because the constraint subsidizes their right to race-neutral treatment. Governmental_institutions are declared victims/payers; their directionality sits near the target pole (high d), yielding moderate effective extraction measured as lost policy discretion. The federal_judiciary, as agenda-setter with analytical exit, sits near neutral. Diversity_equity_advocates, though excluded, are trapped by the legal framework and derive a target-like directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling by distinguishing the colorblind reading's genuine coordination function (preventing state racial discrimination) from the remedial reading's extraction function (redistributing opportunities by race). The colorblind reading claims its mandate is permanent because racial classification is always dangerous; the metrics confirm low extraction but non-zero enforcement, consistent with a formal rule rather than an atrophied or purely extractive mechanism. A claimed rope would be falsified by the non-zero victim structure and the suppressed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_equality,
    'Does formal colorblindness prevent racial subordination in practice, or does it legitimate existing structural inequality by forbidding race-conscious remediation?',
    'Longitudinal empirical comparison of socioeconomic outcomes across jurisdictions with strict colorblind regimes versus regimes permitting race-conscious remediation, controlling for confounding variables.',
    'If formal colorblindness correlates with persistent or widening racial disparities, the constraint''s classification as low-extraction coordination is undermined and it may read as a false-summit mountain or a snare preserving advantage. If outcomes converge, the coordination framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_formal_equality, empirical, 'Whether formal equality produces substantive equality or masks extraction').

omega_variable(
    enforcement_dependency,
    'Would the colorblind norm persist without ongoing judicial enforcement, or would legislative majorities and institutions revert to race-conscious policies?',
    'Natural experiment from jurisdictions where judicial enforcement lags or is withdrawn; observation of institutional policy drift toward or away from race-conscious tools.',
    'If institutions revert quickly when judicial pressure eases, the constraint is enforcement-dependent and structurally a tangled rope or snare. If institutions maintain colorblindness voluntarily, the constraint is closer to a self-enforcing rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependency, empirical, 'Whether colorblindness is self-sustaining or enforcement-dependent').

omega_variable(
    sibling_reading_foreclosure,
    'Does the colorblind reading logically foreclose its siblings, or can a single constitutional framework accommodate context-sensitive exceptions?',
    'Jurisprudential analysis of whether any intermediate doctrinal position (e.g., strict scrutiny that never satisfies) can coherently occupy the logical space between per-se invalidity and permissive use.',
    'If foreclosure is genuine, the kernel generates irreconcilable constraints. If intermediate positions are coherent, the colorblind reading''s foreclose relation weakens to influences or coexists_with, altering the network topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether colorblind and race-conscious readings are logically reconcilable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_colorblind_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(epc_colorblind_tr_t12, equal_protection_clause__colorblind_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(epc_colorblind_tr_t24, equal_protection_clause__colorblind_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(epc_colorblind_tr_t36, equal_protection_clause__colorblind_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(epc_colorblind_tr_t48, equal_protection_clause__colorblind_reading, theater_ratio, 48, 0.17).
narrative_ontology:measurement(epc_colorblind_tr_t60, equal_protection_clause__colorblind_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(epc_colorblind_tr_t70, equal_protection_clause__colorblind_reading, theater_ratio, 70, 0.13).

% Extraction over time
narrative_ontology:measurement(epc_colorblind_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(epc_colorblind_be_t12, equal_protection_clause__colorblind_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement(epc_colorblind_be_t24, equal_protection_clause__colorblind_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement(epc_colorblind_be_t36, equal_protection_clause__colorblind_reading, base_extractiveness, 36, 0.17).
narrative_ontology:measurement(epc_colorblind_be_t48, equal_protection_clause__colorblind_reading, base_extractiveness, 48, 0.18).
narrative_ontology:measurement(epc_colorblind_be_t60, equal_protection_clause__colorblind_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(epc_colorblind_be_t70, equal_protection_clause__colorblind_reading, base_extractiveness, 70, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(epc_colorblind_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(epc_colorblind_su_t12, equal_protection_clause__colorblind_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(epc_colorblind_su_t24, equal_protection_clause__colorblind_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement(epc_colorblind_su_t36, equal_protection_clause__colorblind_reading, suppression_requirement, 36, 0.25).
narrative_ontology:measurement(epc_colorblind_su_t48, equal_protection_clause__colorblind_reading, suppression_requirement, 48, 0.28).
narrative_ontology:measurement(epc_colorblind_su_t60, equal_protection_clause__colorblind_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(epc_colorblind_su_t70, equal_protection_clause__colorblind_reading, suppression_requirement, 70, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, affirmative_action_admissions_policies).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, minority_business_set_asides).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_clause kernel. It is structurally paired with diversity_reading and remedial_reading as sibling constraints derived from the same constitutional text. The epsilon values diverge sharply: this reading has very low extractiveness as a formal prohibition, while the sibling readings involve higher extractiveness through race-conscious allocation mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
