% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Equal Protection Formal Equality Reading (Colorblindness Doctrine)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the formal_equality_reading of the contested
 *   fourteenth_amendment_equal_protection kernel. Under this reading, the
 *   Equal Protection Clause prohibits explicit state racial or status
 *   classification absent a compelling justification, establishing a
 *   presumption of colorblind state neutrality. It is presented and often
 *   experienced by its defenders as a protective coordination mechanism
 *   against invidious discrimination. Simultaneously, it operates as an
 *   active constraint on state corrective action, entering race-conscious
 *   remedial programs and affirmative enforcement into the victim set. The
 *   structural inequality produced by centuries of state action is treated as
 *   pre-constitutional background that the Clause does not authorize the
 *   state to remedy through explicit classification. This is one of two
 *   sibling readings of the same kernel; the anti_caste_reading inverts the
 *   beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - federal_judiciary (institutional/constrained): agenda-setter that interprets and enforces the formal equality rule through judicial review
 *   - anti_discrimination_litigants (moderate/constrained): beneficiaries who invoke the doctrine to challenge explicit state discrimination
 *   - disadvantaged_communities (powerless/trapped): primary targets who lose access to state remedial programs
 *   - state_corrective_agencies (institutional/constrained): secondary targets whose affirmative programs are struck down
 *   - anti_caste_advocates (organized/constrained): excluded voices arguing for structural remediation
 *   - constitutional_scholars (analytical/analytical): observers tracking doctrinal evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.54).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.48).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection Formal Equality Reading (Colorblindness Doctrine)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '28feeee1-541e-4c9b-8de6-8b908c872896').
narrative_ontology:cs_kernel_codification('28feeee1-541e-4c9b-8de6-8b908c872896', formalized).
narrative_ontology:cs_authority_grounding('28feeee1-541e-4c9b-8de6-8b908c872896', lineage).
narrative_ontology:cs_interpretation_layer_present('28feeee1-541e-4c9b-8de6-8b908c872896').
narrative_ontology:cs_reading_relation('28feeee1-541e-4c9b-8de6-8b908c872896', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('28feeee1-541e-4c9b-8de6-8b908c872896', foundational, racial_classification_presumptively_invalid).
narrative_ontology:cs_axiom_status(racial_classification_presumptively_invalid, holdable).
narrative_ontology:cs_axiom_grounding('28feeee1-541e-4c9b-8de6-8b908c872896', racial_classification_presumptively_invalid, conventional).
narrative_ontology:cs_axiom('28feeee1-541e-4c9b-8de6-8b908c872896', foundational, state_neutrality_mandate).
narrative_ontology:cs_axiom_status(state_neutrality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('28feeee1-541e-4c9b-8de6-8b908c872896', state_neutrality_mandate, deontological).
narrative_ontology:cs_reference_frame('28feeee1-541e-4c9b-8de6-8b908c872896', formal_neutrality_baseline).
narrative_ontology:cs_drift_state('28feeee1-541e-4c9b-8de6-8b908c872896', post_sffa_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('28feeee1-541e-4c9b-8de6-8b908c872896', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, anti_discrimination_litigants).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, disadvantaged_communities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause to prohibit explicit state racial classification absent a compelling justification; actively strikes down race-conscious remedial programs and affirmative action policies; derives institutional authority from constitutional text and precedent but is constrained by the same textual and precedential commitments from adopting competing readings.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Invoke the formal equality doctrine to challenge explicit state racial classifications that disadvantage them; obtain judicial invalidation of discriminatory statutes and policies under the colorblindness principle; their access to remedy depends on judicial adoption of this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_discrimination_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of judicial invalidation of race-conscious remedial programs and affirmative action; excluded from state channels for addressing accumulated structural inequality because the formal equality reading treats such inequality as pre-constitutional background not reachable by state remedy; private alternatives are structurally inadequate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, disadvantaged_communities, payer,
    powerless, generational, trapped, national).

% State and local agencies tasked with correcting racial disparity in education, employment, and contracting; their race-conscious remedial programs are struck down or chilled by judicial review under the formal equality framework, forcing them into less effective facially neutral or private-sector alternatives.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_agencies, payer,
    institutional, biographical, constrained, national).

% Argue that Equal Protection requires active dismantling of structural hierarchy through race-conscious state action; their interpretive framework is systematically excluded from majority doctrinal outcomes and treated as legally illegitimate under the formal equality reading, though it remains live in dissenting opinions and academic discourse.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_advocates, excluded,
    organized, generational, constrained, national).

% Analyze and document the doctrinal evolution of Equal Protection; some support the formal equality reading while others critique its mismatch with structural inequality, but their role is analytical rather than directly governed by the constraint's operation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary state discrimination by establishing a rule of formal neutrality: the state may not explicitly classify individuals by race or status without meeting the highest burden of justification, thereby providing a predictable, judicially enforceable check on majoritarian racial prejudice.
% TRANSFER_FUNCTION: Moves the power to enact race-conscious remedial legislation and affirmative action programs away from state legislatures and agencies to the judiciary, which vets all explicit classifications; transfers the practical burden of structural inequality onto private actors and pre-constitutional social forces by removing state remedial tools from the policy space.
% ABSENT_VOICES: Anti-caste theorists and advocates of structural remediation are present in academia and dissenting opinions but largely excluded from majority doctrinal outcomes; disadvantaged communities whose remedy depends on state affirmative action are heard as petitioners but structurally lose under this reading.
% DISAPPEARANCE_RATIONALE: If the formal equality prohibition vanished overnight, state and local governments would immediately regain latitude to enact race-conscious remedial programs; existing anti-discrimination doctrine would destabilize as the baseline rule against explicit classification dissolved; constitutional jurisprudence would reorganize around either anti-caste mandates or majoritarian preference.
% FOUNDING_PROBLEM: Post-Civil War state regimes maintained explicit racial caste systems through Black Codes and Jim Crow laws; the Fourteenth Amendment was adopted to invalidate these overt racial classifications and secure equal legal treatment.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the formal equality tradition attest that de jure segregation is formally abolished; critical race scholars corroborate that the problem has shifted to structural and de facto inequality, arguing the formal equality framework fails to address the current problem and that no party outside the benefiting seats treats the founding problem as live in its original form.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.54, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.54) is moderate: the constraint genuinely blocks invidious discrimination but also blocks corrective state action, extracting remedial capacity from disadvantaged communities and state agencies. Suppression (0.48) reflects active judicial enforcement striking down programs, tempered by ongoing doctrinal contestation. Theater ratio rises over the interval (0.32 at end) because the neutrality claim becomes increasingly performative as structural inequality persists despite formal colorblindness. Accessibility collapse (0.62) is substantial: the anti-caste reading exists but is marginalized in federal precedent. Resistance (0.58) is significant, coming from dissenting justices, critical race theorists, and affected communities. The temporal series use one shared grid; base_extractiveness and suppression_requirement rise as the Court hardens the formal equality rule against race-conscious remedies, while theater_ratio climbs as the gap between formal neutrality and substantive outcomes widens.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and anti-discrimination litigants experience the constraint as protective coordination that limits majoritarian arbitrariness. Disadvantaged communities and state corrective agencies experience the same constraint as extraction of the very policy tools intended to redress accumulated inequality. The perspectival gap is structural, not rhetorical: the same judicial opinion that invalidates an invidious classification also invalidates a remedial program, and the seat determines which face of the constraint is visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-discrimination litigants are structural beneficiaries: the constraint subsidizes their legal claims and yields low directionality. Disadvantaged communities and state corrective agencies are structural victims: the constraint extracts remedial opportunities and yields high directionality. The federal judiciary sits near the middle as agenda-setter; it does not collect material rents but exercises concentrated interpretive authority, giving it a mildly beneficiary-skewed directionality that the structural derivation captures. No override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâabolishing de jure racial casteâis dead, yet the constraint has expanded to block de facto remedial action. This prevents mislabeling the constraint as pure rope (it now actively extracts remedial capacity) or pure snare (it still genuinely prevents invidious discrimination). The Tangled Rope classification captures the hybrid: a coordination function that has accumulated asymmetric extraction as its mandate outlived its original target and was repurposed to police state corrective action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the Equal Protection Clause command state neutrality toward race, or does it mandate active state dismantling of racial hierarchy?',
    'Historical-linguistic analysis of the Clause''s original public meaning, comparative structural analysis of which reading better stabilizes constitutional practice, and empirical study of which reading produces measurable equalization of life chances.',
    'If the anti-caste reading is structurally superior, the formal equality reading inverts from coordination-plus-extraction to misclassification; its victims become beneficiaries and its extraction collapses into failed coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'This constraint is one reading of a contested kernel; the anti-caste reading would invert beneficiary and victim structures.').

omega_variable(
    mandate_obsolescence,
    'Has the founding problem of de jure racial caste been sufficiently resolved that the formal equality doctrine now operates as inertial constraint rather than active remedy?',
    'Empirical measurement of the prevalence and severity of explicit state racial classification today, paired with analysis of whether the doctrine''s current enforcement profile tracks the original problem or has migrated to new targets.',
    'If the founding problem is dead and the doctrine persists only by inertia, the classification shifts toward piton; if the doctrine has adaptively extended to new genuine coordination problems, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the constraint''s persistence is justified by live coordination needs or maintained by institutional inertia.').

omega_variable(
    remedy_permissibility_boundary,
    'Are race-conscious remedial programs constitutionally permissible means to achieve equal protection, or do they necessarily violate the neutrality principle?',
    'Comparative constitutional analysis of jurisdictions that permit race-conscious remediation; natural experiment from sub-national units that have banned versus retained such programs; longitudinal outcome data on disparities.',
    'If race-conscious remedies are structurally permissible, the formal equality reading''s victim set shrinks and its extractiveness falls; if they are structurally impermissible, the reading''s classification as tangled_rope hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_permissibility_boundary, conceptual, 'Whether the coordination and extraction components of the constraint are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(four_be_t20, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(four_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Equal Protection' conflates two structurally distinct constraints: the formal equality reading (prohibits explicit classification) and the anti-caste reading (requires corrective action). They share a kernel but have opposite beneficiary/victim structures and different Îµ values. Decomposition per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
