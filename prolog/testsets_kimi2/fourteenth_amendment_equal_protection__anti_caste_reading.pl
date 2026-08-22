% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection Anti-Caste Reading
 *   domain: constitutional law/political philosophy/civil rights
 *
 * SUMMARY:
 *   The anti-caste reading of the Fourteenth Amendment Equal Protection
 *   Clause holds that the constitutional guarantee requires active state
 *   dismantling of racial, gender, and status hierarchies, not merely facial
 *   neutrality. This reading legitimates race-conscious remedial programs,
 *   affirmative action, and structural interventions that redistribute power
 *   and resources from dominant to subordinated groups. It is contested by
 *   the formal equality reading, which treats explicit state classification
 *   as presumptively invalid. This constraint story authors the anti-caste
 *   reading as a structurally extractive coordination mechanism: it solves
 *   the collective action problem of dismantling entrenched discrimination
 *   but extracts heavily from those who benefit from the status quo
 *   hierarchy. The reading is one side of a contested kernel; the sibling
 *   formal equality reading is addressed in a separate constraint story.
 *
 * KEY AGENTS:
 *   - subordinated_groups: Primary beneficiary (moderate/constrained) â receive state corrective action and remedial programs
 *   - dominant_status_groups: Primary target (powerful/mobile) â bear the costs of dismantling through redistribution, restructuring, and displacement
 *   - federal_judiciary: Agenda-setter (institutional/constrained) â interprets Equal Protection as mandating anti-caste state action
 *   - state_legislatures: Implementing agenda-setter (institutional/constrained) â design and fund dismantling programs under constitutional mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.7).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection Anti-Caste Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional law/political philosophy/civil rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '314b5e66-1a29-48e5-b2d1-72a98595b112').
narrative_ontology:cs_kernel_codification('314b5e66-1a29-48e5-b2d1-72a98595b112', formalized).
narrative_ontology:cs_authority_grounding('314b5e66-1a29-48e5-b2d1-72a98595b112', lineage).
narrative_ontology:cs_interpretation_layer_present('314b5e66-1a29-48e5-b2d1-72a98595b112').
narrative_ontology:cs_reading_relation('314b5e66-1a29-48e5-b2d1-72a98595b112', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('314b5e66-1a29-48e5-b2d1-72a98595b112', foundational, fourteenth_amendment_anti_caste_mandate).
narrative_ontology:cs_axiom_status(fourteenth_amendment_anti_caste_mandate, holdable).
narrative_ontology:cs_axiom_grounding('314b5e66-1a29-48e5-b2d1-72a98595b112', fourteenth_amendment_anti_caste_mandate, conventional).
narrative_ontology:cs_axiom('314b5e66-1a29-48e5-b2d1-72a98595b112', foundational, substantive_equality_imperative).
narrative_ontology:cs_axiom_status(substantive_equality_imperative, holdable).
narrative_ontology:cs_axiom_grounding('314b5e66-1a29-48e5-b2d1-72a98595b112', substantive_equality_imperative, deontological).
narrative_ontology:cs_reference_frame('314b5e66-1a29-48e5-b2d1-72a98595b112', reconstruction_anti_caste_mandate).
narrative_ontology:cs_drift_state('314b5e66-1a29-48e5-b2d1-72a98595b112', contemporary_colorblind_resurgence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('314b5e66-1a29-48e5-b2d1-72a98595b112', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_status_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Black Americans, women, and other historically subordinated groups who face persistent racial, gender, and status hierarchies. Under this constitutional reading, they are the intended recipients of state corrective action, affirmative programs, and structural reform. They cannot readily exit their group-based subordinated status or the national jurisdiction.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups, beneficiary,
    moderate, generational, constrained, national).

% White Americans, men, and other groups who hold advantages within existing hierarchies. They bear the costs of dismantling programs through taxation, loss of preferential access to institutions, and displacement from historical privileges. They possess resources to relocate or restructure but remain subject to national constitutional mandates.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_status_groups, payer,
    powerful, generational, mobile, national).

% Federal courts that interpret the Fourteenth Amendment. Under this reading, they must strike down facially neutral laws that preserve hierarchy and uphold race-conscious remedial state action. They are bound by constitutional text, precedent, and the adversarial presentation of cases.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% State and local legislative bodies that design and fund dismantling programs such as school desegregation plans, affirmative action, and economic redistribution. They face political backlash and fiscal costs while implementing constitutional mandates under judicial supervision.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the dismantling of entrenched racial, gender, and status hierarchies by legitimating active state remedial action across branches and levels of government, converting constitutional equality norms into operative structural reform.
% TRANSFER_FUNCTION: Transfers resources, opportunities, and institutional power from dominant status groups to subordinated groups through state-mandated corrective programs, restructuring, and affirmative intervention.
% ABSENT_VOICES: Formal equality advocates and colorblind constitutionalists who reject race-conscious state action; they are present in broader legal discourse but structurally disempowered under this reading's doctrinal framework.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading vanished, state and federal remedial programs would lose constitutional footing, affirmative action and structural reform litigation would collapse, and constitutional equality doctrine would revert toward formal neutrality â subordinated groups would lose a primary legal avenue for demanding corrective state action.
% FOUNDING_PROBLEM: The post-Civil War persistence of caste-like racial and status hierarchies that formal legal neutrality failed to dismantle, leaving subordinated groups structurally subordinate despite facially neutral laws.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era legislators and civil rights historians attest the founding problem. Contemporary critical race theorists and some civil rights practitioners corroborate that structural hierarchy persists. Formal equality advocates and conservative jurists contest both the diagnosis and the remedy, attesting from outside the beneficiary set that the problem is either resolved or mischaracterized.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the reading mandates continuous, active state intervention to dismantle hierarchy, transferring resources and opportunities from dominant to subordinated groups. Suppression is high (0.70) because the constraint's persistence requires active judicial and legislative enforcement against colorblind resistance and institutional inertia. Theater is moderate (0.35): much anti-discrimination activity is substantive, but a growing share consists of performative compliance that does not alter structural hierarchy. Accessibility_collapse is moderate (0.45) because the formal equality alternative remains intellectually and institutionally viable. Resistance is high (0.75) because dominant groups and conservative institutions actively oppose race-conscious state action.
 *
 * PERSPECTIVAL GAP:
 *   The subordinated_groups seat experiences the constraint as liberating coordination that vindicates constitutional promise; the dominant_status_groups seat experiences the same constraint as coercive extraction that strips away status-quo advantages. The federal_judiciary and state_legislatures occupy mixed positions: they are the enforcers but also bear political and institutional costs. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated_groups are declared beneficiaries with constrained exit (they cannot exit their subordinated status or the jurisdiction easily), yielding low directionality and damped effective extraction. Dominant_status_groups are declared payers with mobile exit options (they can relocate or restructure assets, though the constitutional mandate is national), yielding high directionality and amplified extraction. Federal_judiciary and state_legislatures are agenda_setters with constrained exit (bound by constitutional oath and democratic mandate), sitting near the symmetric middle.
 *
 * MANDATROPHY ANALYSIS:
 *   The anti-caste reading risks mandatrophy if the founding problem (structural caste hierarchy) is declared solved while the remedial apparatus persists. Current measurements show theater_ratio rising but not yet dominant; the founding_problem_status is contested rather than dead, which prevents piton misclassification. The constraint is not a snare because it delivers genuine coordination benefits to subordinated groups; it is not a rope because the extraction from dominant groups is asymmetric and involuntary. Tangled rope captures the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_historical_pedigree,
    'Does the Fourteenth Amendment''s original public meaning support the anti-caste reading, or is the reading a later progressive construction?',
    'Historical-linguistic analysis of Reconstruction-era sources; corpus linguistics of ''equal protection'' usage 1866-1868.',
    'If the anti-caste reading lacks historical pedigree, its conventional grounding weakens and it may be reclassified toward snare; if supported, the extraction is textually authorized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_caste_historical_pedigree, empirical, 'Historical foundation of anti-caste interpretation').

omega_variable(
    remedial_program_structural_vs_theater,
    'Do state corrective programs under the anti-caste reading produce structural dismantling of hierarchy, or primarily theatrical compliance?',
    'Longitudinal outcome studies of affirmative action, school finance equalization, and voting rights restoration.',
    'If outcomes are theatrical, theater_ratio rises and the coordination function degrades, pushing the computed type toward piton; if structural, the coordination function is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_program_structural_vs_theater, empirical, 'Efficacy of remedial dismantling programs').

omega_variable(
    kernel_reading_foreclosure,
    'Is the anti-caste reading genuinely foreclosed by the formal equality reading''s current judicial dominance, or does it remain a holdable alternative framework?',
    'Judicial behavior analysis and doctrinal logic: can a single constitutional framework simultaneously require and prohibit explicit racial classification?',
    'If genuinely foreclosed, this constraint becomes a dead-letter reading with no institutional traction; if coexistent in doctrinal tension, it remains a live tangled rope whose enforcement waxes and wanes with political coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural foreclosure of anti-caste reading by dominant sibling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
