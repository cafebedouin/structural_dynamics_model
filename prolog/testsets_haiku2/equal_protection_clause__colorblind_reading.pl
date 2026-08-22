% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause – Colorblind Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   The Equal Protection Clause of the Fourteenth Amendment is a contested
 *   kernel. This constraint story instantiates the colorblind reading: equal
 *   protection forbids all governmental racial classifications, treating
 *   individuals as rights-bearers independent of group membership. Under this
 *   reading, the rule is a near-natural constraint – a formal principle that
 *   applies uniformly without regard to context or consequence. The
 *   colorblind reading treats race as categorically irrelevant to government
 *   action; any policy that sorts by race violates the principle. This
 *   contrasts sharply with the remedial reading (which requires
 *   race-conscious remediation of historical subordination) and the diversity
 *   reading (which permits race-conscious policies serving educational
 *   diversity). The three readings share the same constitutional text but
 *   instantiate three structurally distinct constraints with different ε
 *   values, different victim/beneficiary structures, and different temporal
 *   horizons.
 *
 * KEY AGENTS:
 *   - All individuals as rights-bearers (analytical beneficiary under the colorblind frame)
 *   - Supreme Court originalist faction (agenda-setter; enforces the colorblind interpretation)
 *   - Racial minorities and white applicants excluded by diversity policies (identified as victims of racial classification under colorblindness)
 *   - Remedial and diversity reading coalitions (excluded from the constraint's enforcement; would endorse alternative readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.08).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause – Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'a072b35f-3c69-4072-8b93-49a7ccaa6b4c').
narrative_ontology:cs_kernel_codification('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', formalized).
narrative_ontology:cs_authority_grounding('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', lineage).
narrative_ontology:cs_interpretation_layer_present('a072b35f-3c69-4072-8b93-49a7ccaa6b4c').
narrative_ontology:cs_reading_relation('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', foundational, race_categorically_irrelevant_to_government_action).
narrative_ontology:cs_axiom_status(race_categorically_irrelevant_to_government_action, holdable).
narrative_ontology:cs_axiom_grounding('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', race_categorically_irrelevant_to_government_action, deontological).
narrative_ontology:cs_axiom('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', foundational, individuals_not_groups_as_rights_bearers).
narrative_ontology:cs_axiom_status(individuals_not_groups_as_rights_bearers, holdable).
narrative_ontology:cs_axiom_grounding('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', individuals_not_groups_as_rights_bearers, deontological).
narrative_ontology:cs_reference_frame('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', colorblind_equal_protection).
narrative_ontology:cs_drift_state('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', contemporary_post_students_fair_admissions, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a072b35f-3c69-4072-8b93-49a7ccaa6b4c', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, racial_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, white_applicants_excluded_by_diversity_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The colorblind reading posits individuals as the beneficiaries of non-discrimination law regardless of race. Under this reading, every person benefits from a guarantee that no governmental actor will classify them by race. The beneficiary here is not a concrete constituency but a normative category – the universal subject who encounters government policy without racial categorization.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers).

% In the colorblind reading's frame, racial minorities harmed by race-conscious policies (e.g., affirmative action admissions) are the policy's victims – they are classified by race and subjected to differentiated treatment when they compete for university slots. Their exit from the constraint is participation in a colorblind admissions process; they cannot exit from the governmental system itself.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, racial_minority_applicants, payer,
    powerless, biographical, constrained, national).

% The colorblind reading identifies white applicants denied admission partly because of race-conscious diversity preferences as victims of the same harm – racial classification. They argue they are classified by race and treated less favorably as a result, violating the colorblind principle. Private universities (outside direct state action) are outside the constraint; public university admissions are the contested domain.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, white_applicants_excluded_by_diversity_policy, payer,
    moderate, biographical, constrained, national).

% Interprets and enforces the colorblind reading by striking down race-conscious policies (Students for Fair Admissions, 2023; prior precedent Curtis, Fisher). Justifies the reading as fidelity to the text and original public meaning of the Fourteenth Amendment. The faction's power is the authority to invalidate statutes and policies that violate this interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court_originalist_faction, agenda_setter,
    institutional, generational, analytical, national).

% A coalition of jurists, legislators, educators, and civil rights organizations that would endorse the remedial reading – arguing that colorblindness perpetuates historical subordination and that equal protection requires race-conscious remediation. They are excluded from the colorblind constraint's adoption decision; their objections are doctrinal and legislative, not part of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, remedial_reading_coalition, excluded,
    institutional, generational, analytical, national).

% A coalition endorsing the diversity reading – that equal protection permits race-conscious policies serving compelling educational interests. They argue the colorblind reading ignores the legitimate coordination function of diversity in higher education. Like the remedial coalition, they are institutionally excluded from the doctrine's adoption following recent Supreme Court shifts.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, diversity_reading_coalition, excluded,
    institutional, generational, analytical, national).

% Universities that must comply with the colorblind constraint via admissions policy. Under the current reading, they can consider race neither as a plus nor as a remedy. Their observational role reflects that they adapt their processes to whatever the legal doctrine requires; they are not the principled adopters of the reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, educational_institutions, observer,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, judicially enforceable standard: government must not classify persons by race. Solves the coordination problem of what counts as lawful government action across jurisdictions and officials – a uniform rule prevents race-conscious policymaking fragmentation.
% TRANSFER_FUNCTION: Transfers authority over race-relevant policy decisions from elected legislatures and administrators to courts charged with enforcing colorblindness. The constraint moves legitimacy from democratic pluralism (legislatures choosing race-conscious policies) to constitutional formalism (courts striking them down as race-classifications).
% ABSENT_VOICES: Racial minorities and communities historically subordinated by law are present in litigation but absent from the institutional seats that adopted the colorblind reading (the originalist Supreme Court faction). They are excluded from the doctrine's framing authority; their position appears in dissent and non-judicial discourse, not in the institutional decision that enforces colorblindness.
% DISAPPEARANCE_RATIONALE: If the colorblind constraint disappeared – if courts permitted race-conscious policy again – the world would substantially rearrange: universities would re-adopt race-conscious admissions; legislators would consider race in allocating resources; civil rights remedies tied to racial categories would become available again. The contest is over whether that rearrangement would represent progress or regression. The colorblind reading asserts disappearance would enable discrimination; the remedial reading asserts colorblindness enables it.
% FOUNDING_PROBLEM: Race-conscious classifications by government became a mechanism for subordination and exclusion (slavery, Jim Crow, segregation). The founding problem solved was: how to prevent government from using race as a sorting mechanism for oppression.
% FOUNDING_PROBLEM_CORROBORATION: The colorblind reading's own tradition (originalist jurisprudence) attests the problem remains live – that race-consciousness in policy is a persistent risk. The remedial reading and diversity coalitions attest the founding problem was structural subordination, not racial classification itself; they argue colorblindness now enables new subordination by denying remedies for historical wrongs. Civil rights historians outside the benefiting parties offer competing corroborations: some support colorblindness as the achieved norm preventing government oppression; others argue it obscures ongoing structural inequality and forecloses legitimate remedies.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_clause__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The colorblind reading is authored as a mountain because it claims to be a formal principle – race should never be relevant to government action, independent of consequences or historical context. Extractiveness is very low (0.08) because the constraint operates as a transparent rule application: does the policy sort by race? If yes, it violates the constraint. There is no asymmetric gain captured by any concrete party; the rule applies uniformly. Suppression is low (0.12) because the constraint persists through doctrinal authority and judicial enforcement, not through coercive suppression of alternatives. Theater is minimal (0.05) because there is little performative maintenance – the rule is stated clearly and applied straightforwardly. Accessibility collapse is very high (0.92) because once the colorblind principle is understood, the alternative (that government can classify by race) appears categorically foreclosed by the principle itself. Resistance is high (0.78) because the constraint meets sustained resistance from those who argue colorblindness perpetuates historical subordination – the remedial and diversity coalitions actively contest it. The measurement series show stability over the interval: extractiveness and suppression remain flat and low, consistent with a stable formal rule. Theater rises slightly due to the burden of enforcing colorblindness against persistent pressure to adopt race-conscious policies, but remains minimal. The constraint's temporal trajectory is one of institutional entrenchment (as of Students for Fair Admissions, 2023) rather than degradation.
 *
 * PERSPECTIVAL GAP:
 *   The originalist Supreme Court faction (agenda-setter) experiences this constraint as a formal principle discovered in constitutional text – a rule that applies equally to all. Racial minorities and civil rights organizations (excluded but present in litigation) experience the same constraint as a principle that obscures ongoing structural inequality and forecloses remedies for historical wrongs. White applicants denied admission under diversity policies experience it as a protection against discrimination they face. The engine computes per-seat classifications; all seats derive from the same structural facts (the colorblind principle, racial classification, government action) but experience them through different readings. The perspectival gap is not a defect – it is the kernel contest itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The colorblind reading declares all individuals (regardless of race) as beneficiaries of a constraint that forbids racial classification. This is an unusual beneficiary structure for a constraint story – the beneficiary is not a concrete constituency but a normative category, 'individuals as rights-bearers.' Under colorblindness, victims are those subjected to race-conscious policies (racial minorities harmed by colorblindness; white applicants excluded by diversity policies that the colorblind reading forbids). The directionality structure is symmetric across all individuals – no one should be classified by race, and everyone benefits from that principle. This symmetric framing is exactly what the colorblind reading asserts. The derivative omega captures the contest: is the colorblind principle truly neutral, or does it asymmetrically benefit those not historically subordinated by race-consciousness?
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading does not face mandatrophy in the classical sense – its founding problem (preventing government use of race for subordination) remains live in the originalist view. However, an omega variable captures the remedial reading's claim that colorblindness's founding problem (preventing racial oppression via government sorting) has been solved, and the constraint now obscures new subordination. This is a kernel-level contest about whether the constraint's mandate persists, not a degradation of the constraint's own function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_vs_colorblind_enforcement,
    'Is the colorblind reading a discovered principle in the Constitution, or a constructed interpretation that benefits those whose position does not require race-consciousness to remedy historical harm?',
    'Originalist analysis of historical constitutional text and ratification-era understanding versus historical reconstruction showing no consensus on colorblindness at ratification; comparison with constitutional interpretation traditions across democracies.',
    'If discovered principle: the constraint is a genuine mountain and should resist FSM reclassification. If constructed interpretation: the constraint may be a false summit – beneficiaries (originalist jurists, those not disadvantaged by historical subordination) exist, and the beneficiary structure contradicts the ''universal principle'' framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblindness_vs_colorblind_enforcement, conceptual, 'Whether colorblindness is a discovered constitutional principle or a constructed interpretation favoring certain constituencies.').

omega_variable(
    racial_classification_symmetry,
    'Does the colorblind principle apply symmetrically to all racial classifications, or do race-conscious remedies addressing historical subordination stand on different structural ground?',
    'Jurisprudential analysis of whether remedial race-consciousness serves a fundamentally different function than subordinating race-consciousness; empirical study of whether colorblindness and remedial policies produce equivalent harms.',
    'If race-conscious remedies are structurally distinct: the victims of colorblindness (those harmed by foreclosed remedies) would be reclassified, and the constraint''s beneficiary structure would shift. If all racial classifications are equivalent harms: the colorblind principle holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_classification_symmetry, empirical, 'Whether colorblindness treats all racial classifications symmetrically or whether remedial race-consciousness is structurally distinct.').

omega_variable(
    kernel_sibling_foreclosure,
    'Does the colorblind reading logically foreclose the remedial and diversity readings within a single constitutional framework, or do all three remain live interpretive options?',
    'Constitutional doctrine and jurisprudential analysis: do originalist canons, living-constitutionalism, or purposivist approaches permit or rule out the competing readings? Can a single constitutional framework hold both colorblindness and remedial race-consciousness?',
    'If colorblindness forecloses remedial/diversity: the reading relations are ''forecloses.'' If all three remain live: the reading relations are ''coexists_with.'' The classification determines the kernel''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, conceptual, 'Whether the colorblind reading logically forecloses or coexists with the remedial and diversity readings of equal protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t8, equal_protection_clause__colorblind_reading, theater_ratio, 8, 0.03).
narrative_ontology:measurement_basis(equa_tr_t8, observed).
narrative_ontology:measurement(equa_tr_t16, equal_protection_clause__colorblind_reading, theater_ratio, 16, 0.04).
narrative_ontology:measurement_basis(equa_tr_t16, observed).
narrative_ontology:measurement(equa_tr_t24, equal_protection_clause__colorblind_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement_basis(equa_tr_t24, observed).
narrative_ontology:measurement(equa_tr_t32, equal_protection_clause__colorblind_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement_basis(equa_tr_t32, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__colorblind_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(equa_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t8, equal_protection_clause__colorblind_reading, base_extractiveness, 8, 0.07).
narrative_ontology:measurement_basis(equa_be_t8, observed).
narrative_ontology:measurement(equa_be_t16, equal_protection_clause__colorblind_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement_basis(equa_be_t16, observed).
narrative_ontology:measurement(equa_be_t24, equal_protection_clause__colorblind_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement_basis(equa_be_t24, observed).
narrative_ontology:measurement(equa_be_t32, equal_protection_clause__colorblind_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement_basis(equa_be_t32, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__colorblind_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(equa_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t8, equal_protection_clause__colorblind_reading, suppression_requirement, 8, 0.09).
narrative_ontology:measurement_basis(equa_su_t8, observed).
narrative_ontology:measurement(equa_su_t16, equal_protection_clause__colorblind_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement_basis(equa_su_t16, observed).
narrative_ontology:measurement(equa_su_t24, equal_protection_clause__colorblind_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement_basis(equa_su_t24, observed).
narrative_ontology:measurement(equa_su_t32, equal_protection_clause__colorblind_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement_basis(equa_su_t32, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__colorblind_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(equa_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.06).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause is a contested kernel with three structurally distinct readings. The colorblind reading (this story) treats race as categorically irrelevant to government; the remedial reading treats race-consciousness as required to remedy historical subordination; the diversity reading permits race-consciousness to serve compelling educational interests. Each reading instantiates a different constraint with different ε, different victim/beneficiary structures, and different classifications. All three are linked via this network field and form a constraint family. The upstream story (colorblind reading, currently dominant in Supreme Court doctrine) influences the downstream stories (remedial and diversity readings, which must work within or against the colorblind precedent established by the upstream reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
