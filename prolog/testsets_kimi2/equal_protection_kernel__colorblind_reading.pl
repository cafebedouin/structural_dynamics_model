% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading (Per Se Racial Classification Ban)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause holds that any
 *   state use of racial classifications is per se unconstitutional,
 *   regardless of whether the purpose is remedial or benign. This reading was
 *   instantiated in the 2023 Students for Fair Admissions (SFFA) decisions
 *   striking down race-conscious admissions at Harvard and UNC. It operates
 *   as a constitutional constraint on educational institutions and state
 *   actors, foreclosing race-conscious remedies while asserting formal
 *   neutrality. As a kernel reading, it is contested by the remedial reading
 *   (race-conscious action permitted for compelling diversity or remediation)
 *   and the antisubordination reading (the clause targets subordination, not
 *   classification). This story treats the colorblind reading as the standing
 *   arrangement under contest in the post-SFFA regime.
 *
 * KEY AGENTS:
 *   - Federal judiciary (agenda_setter, institutional/analytical): enforces the per se rule through judicial review.
 *   - Non-remedy-eligible applicants (beneficiary, moderate/mobile): gain competitive position in formally neutral admissions.
 *   - Historically excluded groups (payer, organized/identity_locked): lose remedial pathways and bear the cost of foreclosed remediation.
 *   - Selective universities (payer, institutional/constrained): lose admissions autonomy to judicial oversight.
 *   - Colorblind advocacy groups (beneficiary, organized/mobile): organizational mission and legitimacy vindicated.
 *   - Civil rights organizations (observer, organized/mobile): oppose the reading through litigation and advocacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.72).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.8).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Colorblind Reading (Per Se Racial Classification Ban)").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'a4d1ec8c-52d0-401b-a59c-1a4495d57747').
narrative_ontology:cs_kernel_codification('a4d1ec8c-52d0-401b-a59c-1a4495d57747', fixed_text).
narrative_ontology:cs_authority_grounding('a4d1ec8c-52d0-401b-a59c-1a4495d57747', lineage).
narrative_ontology:cs_interpretation_layer_present('a4d1ec8c-52d0-401b-a59c-1a4495d57747').
narrative_ontology:cs_reading_relation('a4d1ec8c-52d0-401b-a59c-1a4495d57747', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a4d1ec8c-52d0-401b-a59c-1a4495d57747', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('a4d1ec8c-52d0-401b-a59c-1a4495d57747', foundational, racial_classification_per_se_invalid).
narrative_ontology:cs_axiom_status(racial_classification_per_se_invalid, holdable).
narrative_ontology:cs_axiom_grounding('a4d1ec8c-52d0-401b-a59c-1a4495d57747', racial_classification_per_se_invalid, empirically_contingent).
narrative_ontology:cs_reference_frame('a4d1ec8c-52d0-401b-a59c-1a4495d57747', colorblind_constitutional_order).
narrative_ontology:cs_drift_state('a4d1ec8c-52d0-401b-a59c-1a4495d57747', post_sffa_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('a4d1ec8c-52d0-401b-a59c-1a4495d57747', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, non_remedy_eligible_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_advocacy_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, selective_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, per_se_race_neutrality).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, colorblind_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Equal Protection Clause as a categorical ban on state racial classifications. Through judicial review, it strikes down race-conscious admissions policies and preempts legislative or institutional attempts to adopt remedial race-conscious measures. Its authority derives from the constitutional text and the power of judicial precedent.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Compete for selective admissions slots without race-conscious preferences operating in favor of other groups. They benefit from a formally neutral process where academic metrics are the stated criterion, though the constraint's removal of holistic race consideration may obscure other structural advantages.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, non_remedy_eligible_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of foreclosed remedial pathways in selective education. State and institutional race-conscious outreach, admissions, and scholarship programs designed to counteract historical discrimination are judicially barred. Exit is identity-locked because the constraint operates precisely on their racial classification; they cannot exit the category the constraint targets.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_groups, payer,
    organized, generational, identity_locked, national).

% Lose autonomy to design admissions policies that consider race as one factor among many to achieve student-body diversity or remediate historical exclusion. Must comply with judicial mandates or face injunctive relief, loss of federal funding, or reputational damage in litigation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, selective_universities, payer,
    institutional, generational, constrained, national).

% Litigate and lobby for the categorical colorblind reading. Their organizational missions, funding, and public legitimacy are vindicated when courts adopt per se rules against racial classification. They participate as amici and strategists in the enforcement architecture.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Challenge the colorblind reading in litigation and public advocacy, arguing for remedial and antisubordination interpretations. They file amicus briefs, support defendant institutions, and mount doctrinal counter-arguments, but do not control the judicial agenda.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_organizations, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, administrable rule for state action: no racial classifications, reducing judicial line-drawing in equal protection cases and preventing what proponents see as racial balkanization of public life.
% TRANSFER_FUNCTION: Moves access to selective educational opportunities and institutional policy autonomy away from historically excluded groups and selective universities, toward applicants competing on formally neutral metrics and toward judicial oversight of admissions.
% ABSENT_VOICES: Applicants who would have benefited from narrowly tailored race-conscious admissions but are excluded from the legal frame because the reading treats all racial classification as per se invalid; also state legislators who might craft nuanced remedial policies but are preempted by constitutional fiat.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished overnight, selective universities would immediately reinstate race-conscious holistic admissions, targeted outreach, and race-conscious scholarship programs. The demographic composition of selective institutions would shift, state institutions would regain remedial autonomy, and the judicial framework would collapse into the remedial or antisubordination readings.
% FOUNDING_PROBLEM: Reconstruction-era state regimes used racial classifications to subordinate newly freed slaves and maintain caste systems; the Fourteenth Amendment was adopted to prohibit state-mandated racial discrimination and subordination.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction historians and legal scholars outside the colorblind advocacy ecosystem attest that the Fourteenth Amendment was designed to dismantle caste subordination rather than to establish formal classification neutrality. Dissenting justices in SFFA and proponents of the antisubordination reading corroborate the subordination-framing from oppositional seats.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint structurally blocks remediation for historical exclusion, transferring relative access opportunities to the non-remedied pool. Suppression (0.80) is higher still: the constraint's persistence requires active judicial striking of race-conscious policies and preemption of legislative alternatives. Theater ratio (0.55) reflects the growth of formalist originalist rhetoric that presents the constraint as historically mandated while the historical evidence is heavily contested. Accessibility collapse (0.75) is high because once the colorblind rule is announced, race-conscious alternatives legally collapse for state actors. Resistance (0.68) reflects sustained legal and political opposition from civil rights organizations, dissenting justices, and institutions that valued holistic diversity. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and colorblind advocacy groups experience this constraint as the enforcement of neutral principle and historical constitutional meaning. Historically excluded groups and selective universities experience it as the active foreclosure of remedial tools and institutional autonomy. The engine computes this divergence from structural data: agenda-setter with analytical exit versus identity-locked payers with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits at low directionality as the structural beneficiary and administrator of the constraint. Colorblind advocacy groups also sit at low directionality (mission vindication). Non-remedy-eligible applicants are diffuse beneficiaries with mobile exit, sitting near the beneficiary end. Historically excluded groups sit at high directionality (target side): they bear the cost of foreclosed remediation and are identity-locked. Selective universities are institutional payers with constrained exit, sitting at moderate-high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading risks mandatrophy if its founding problem (state-mandated racial subordination) is misidentified as classification per se rather than subordination. If the problem is subordination, the colorblind reading's categorical rule prevents the very remediation the Amendment was designed to enableâthe mandate has outlived its proper function and now operates as inertial formalism. The reading's proponents argue the problem IS classification itself, keeping the mandate live. The divergence between these genealogies is the measurement the framework captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_ambiguity,
    'Does the original public meaning of the Fourteenth Amendment support a categorical ban on all state racial classifications, or only on those subordinating historically oppressed groups?',
    'Historical archival research and corpus linguistics of the 1866-1868 congressional debates and state ratification debates; comparative analysis of Reconstruction-era legislation that used race-conscious classifications.',
    'If the original meaning permits remedial or non-subordinating race-conscious action, the colorblind reading''s authority grounding shifts from lineage to extraction, raising extractiveness and reclassifying the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_ambiguity, empirical, 'Whether the constitutional text originally foreclosed all racial classifications.').

omega_variable(
    formal_substantive_equality_separability,
    'Is formal equality (no racial classifications) structurally separable from substantive equality (no subordination), or does the former necessarily produce the latter without remediation?',
    'Comparative longitudinal analysis of jurisdictions with and without race-conscious remediation, measuring stratification in selective education and labor markets.',
    'If formal equality produces persistent subordination without remediation, the coordination story collapses and extraction dominates; if formal equality achieves substantive equality, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formal_substantive_equality_separability, conceptual, 'Whether formal equality achieves substantive equality without remediation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__colorblind_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__colorblind_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__colorblind_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__colorblind_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(equa_tr_t50, equal_protection_kernel__colorblind_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__colorblind_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__colorblind_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__colorblind_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__colorblind_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(equa_be_t50, equal_protection_kernel__colorblind_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__colorblind_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__colorblind_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__colorblind_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__colorblind_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(equa_su_t50, equal_protection_kernel__colorblind_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the colorblind reading of the equal_protection_kernel. It is structurally distinct from the remedial_reading (which permits race-conscious remediation) and the antisubordination_reading (which targets hierarchy rather than classification). Decomposed per the Îµ-invariance principle because the kernel's referent differs across readings: colorblind reads the kernel as a formal rule against classification; remedial reads it as a flexible standard permitting diversity; antisubordination reads it as a substantive anti-caste principle. Each reading has distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
