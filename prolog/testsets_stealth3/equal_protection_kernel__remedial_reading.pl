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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Action Permitted When Narrowly Tailored)
 *   domain: constitutional law/education policy/civil rights
 *
 * SUMMARY:
 *   Under the remedial reading of the equal protection clause, race-conscious
 *   state action is constitutionally permissible when narrowly tailored to a
 *   compelling interest — remediation of documented historical exclusion or
 *   the educational benefits of diversity. Instantiated in selective higher
 *   education, the arrangement lets universities weigh race as one plus
 *   factor in individualized holistic review, conditioned on documentation of
 *   the compelling interest and subject to strict scrutiny. The epsilon
 *   referent is the standing arrangement under contest — race-conscious
 *   admissions as this reading licenses it — assessed by the reading's own
 *   lights; the reading's endorsed ideal (perfectly tailored,
 *   self-terminating remediation) is not the referent. Claim and metrics are
 *   authored independently: the constraint is CLAIMED as tangled_rope because
 *   it pairs a genuine coordination function (a disciplined channel for
 *   repairing documented exclusion) with asymmetric extraction (marginal
 *   admission probability moved from race-blind-admissible rejected
 *   applicants to beneficiaries) held in place by active judicial
 *   enforcement. Family membership: this is one of three readings of the
 *   equal_protection_kernel; the siblings are separate stories with their own
 *   epsilon values, beneficiary sets, and types, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda-setting administrator (institutional/identity_locked) — runs race-conscious holistic review, documents the compelling interest, bears litigation and compliance costs
 *   - federal_courts: enforcement authority (institutional/analytical) — polices narrow tailoring; its stepwise hardening drove the enforcement ratchet across the interval
 *   - underrepresented_minority_applicants: primary beneficiary (moderate/constrained) — receives marginal admission consideration
 *   - historically_excluded_communities: grounding beneficiary (organized/generational) — supplies the documented-exclusion record the remedial predicate draws on
 *   - rejected_race_blind_admissible_applicants: primary target (moderate/constrained) — bears the margin loss a race-blind process would not have produced
 *   - general_student_body: diffuse beneficiary (moderate/constrained) — receives the claimed diversity benefits
 *   - civil_rights_advocacy_organizations: organizational beneficiary (organized/mobile) — defends the arrangement and draws standing and mission from its legal contests
 *   - colorblind_principle_advocates: excluded voice (organized/mobile) — holds the categorical objection this reading's frame defines out rather than answers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Action Permitted When Narrowly Tailored)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional law/education policy/civil rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, 'cf3830ff-cc5d-462f-8ae2-595fef79abf4').
narrative_ontology:cs_kernel_codification('cf3830ff-cc5d-462f-8ae2-595fef79abf4', fixed_text).
narrative_ontology:cs_authority_grounding('cf3830ff-cc5d-462f-8ae2-595fef79abf4', lineage).
narrative_ontology:cs_interpretation_layer_present('cf3830ff-cc5d-462f-8ae2-595fef79abf4').
narrative_ontology:cs_reading_relation('cf3830ff-cc5d-462f-8ae2-595fef79abf4', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('cf3830ff-cc5d-462f-8ae2-595fef79abf4', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('cf3830ff-cc5d-462f-8ae2-595fef79abf4', foundational, narrow_tailoring_legitimates_race_conscious_remedy).
narrative_ontology:cs_axiom_status(narrow_tailoring_legitimates_race_conscious_remedy, holdable).
narrative_ontology:cs_axiom_grounding('cf3830ff-cc5d-462f-8ae2-595fef79abf4', narrow_tailoring_legitimates_race_conscious_remedy, instrumental).
narrative_ontology:cs_axiom('cf3830ff-cc5d-462f-8ae2-595fef79abf4', secondary, compelling_interest_requires_documented_predicate).
narrative_ontology:cs_axiom_status(compelling_interest_requires_documented_predicate, holdable).
narrative_ontology:cs_axiom_grounding('cf3830ff-cc5d-462f-8ae2-595fef79abf4', compelling_interest_requires_documented_predicate, empirically_contingent).
narrative_ontology:cs_reference_frame('cf3830ff-cc5d-462f-8ae2-595fef79abf4', equal_protection_as_remedial_authorization).
narrative_ontology:cs_drift_state('cf3830ff-cc5d-462f-8ae2-595fef79abf4', contemporary_post_sffa, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('cf3830ff-cc5d-462f-8ae2-595fef79abf4', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_communities).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_race_blind_admissible_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, general_student_body).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, selective_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, strict_scrutiny_narrow_tailoring_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_interest_documentation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer holistic admissions at selective institutions and, under this reading, may weigh race as one plus factor among many. Keeping that authority requires documenting a compelling interest — remediation of documented exclusion or the educational benefits of diversity — and showing the program is narrowly tailored: individualized review, no quotas, durational limits. They carry the compliance burden: mission statements, committee records, litigation defense. Leaving voluntarily would mean abandoning a diversity commitment fused with institutional identity and mission, which they treated for decades as costlier than defending the program through successive lawsuits.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, selective_universities, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, selective_universities, payer).

% Adjudicate whether each program survives strict scrutiny. Across the interval they moved from deference to demanding review, requiring ever more rigorous documentation and ultimately withdrawing the deference that had sustained race-conscious admissions. They neither collect nor pay under the arrangement; they set and revise the terms under which every other seat operates.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Apply to selective institutions and receive additional consideration in holistic review; at the margin this converts into admission offers a race-blind process would not have extended. Their access depends on universities maintaining the program and on courts sustaining its legality. Applying elsewhere or reapplying is possible but does not reach the specific institutions where the consideration operates.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, underrepresented_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).

% The groups whose documented exclusion — segregation statutes, discriminatory admissions, exclusionary practices — supplies the remedial predicate. Members and descendants gain representation at selective institutions when the program operates, and community organizations, churches, and civic groups compile the historical record and discrimination findings that the documentation requirement draws on.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_communities, beneficiary,
    organized, generational, mobile, national).

% Applicants who would have received an offer under a race-blind process but are denied at the margin where race-conscious consideration tips the decision. They lose a specific seat at a specific institution and are evaluated partly by race. Recourse is limited: reapplication, other institutions, or litigation; no available choice restores a denied admission.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_race_blind_admissible_applicants, payer,
    moderate, biographical, constrained, national).

% Enrolled students who receive the claimed educational benefits of diverse classrooms — the diversity half of the compelling-interest rationale. They pay no distinct burden beyond tuition already owed, and their benefit is diffuse and difficult to verify individually.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, general_student_body, beneficiary,
    moderate, biographical, constrained, national).

% Organizations whose constituencies and missions are advanced by the arrangement's continued operation. They defend the program in litigation, mobilize support, and draw standing, funding, and relevance from the legal contests the arrangement generates. They can redirect effort to other campaigns if the arrangement ends.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Advocates, litigants, and legislators who hold that the state may not use racial classifications for any purpose. Within this reading's frame their position functions as a definitional exclusion rather than a competing answer — the frame presumes narrowly tailored use can be legitimate. They operate through ballot initiatives, state legislation, and lawsuits aimed at ending the arrangement altogether.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_principle_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, underrepresented_minority_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a disciplined channel through which state actors and universities may address documented historical exclusion: individualized, holistic evaluation in which race may count as one factor among many, bounded by strict scrutiny's requirements — documented compelling interest, no quotas, durational expectation — so that repair proceeds without categorical preferences or quota regression.
% TRANSFER_FUNCTION: Moves marginal admission probability at selective institutions from applicants who would have been admitted under a race-blind process to applicants from historically excluded groups, and moves documentation and litigation-risk burdens onto the universities administering the process.
% ABSENT_VOICES: Colorblind-principle advocates hold that any state racial classification is constitutionally illegitimate regardless of purpose; within this reading's frame their objection is defined out by construction rather than answered on its merits. Rejected applicants similarly enter the frame mainly as litigation plaintiffs rather than as seated parties with standing interests of their own.
% DISAPPEARANCE_RATIONALE: If the permission vanished overnight, selective-university admissions processes, campus composition, K-12 pipeline and outreach programs, and the litigation agenda surrounding race and education would all reorganize — as they demonstrably began doing after 2023 when the permission was withdrawn.
% FOUNDING_PROBLEM: Documented historical exclusion — de jure segregation and discriminatory admissions — left historically excluded racial groups severely underrepresented at selective institutions; the reading was built to let state actors remedy that exclusion without violating the equal protection guarantee.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: federal desegregation litigation and consent decrees documenting state-imposed exclusion, civil-rights enforcement findings, and historical scholarship on de jure and de facto exclusion. Notably, the opposing litigation record itself concedes the history of discrimination while disputing whether present-day gaps remain traceable to it — no party disputes that the exclusion occurred; the dispute is whether the remedial predicate remains satisfied now.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.58: the arrangement moves real admission probability at the margin from race-blind-admissible rejected applicants to beneficiaries, and litigation-disclosed institutional data indicate the margin effects were material, but the reading's own design disciplines the transfer — individualized review, no quotas, durational expectations — so extraction is bounded rather than open-ended. Suppression is authored at 0.65 as a raw structural property, unscaled by power or scope: holding the arrangement in place required escalating legal machinery, and subjects' alternatives were partial (race-blind regimes operated lawfully throughout; applicants could apply elsewhere, but no alternative restored a denied seat). Theater is 0.50 at interval end: individualized review was real, but a growing share of activity was litigation-proofing — mission statements, committee reports, and post-hoc rationalizations assembled to survive strict scrutiny rather than to operate the program. Accessibility collapse is low (0.35) because alternatives never collapsed: race-blind admissions remained lawful and widespread, and several large states ran ban regimes concurrently. Resistance is high (0.80): ballot initiatives, state bans, and three generations of litigation made this one of the most actively contested arrangements in the domain. The measurement series share one grid (1978, 1989, 1996, 2003, 2014, 2023); all three tracked metrics rise monotonically — extraction accumulates as programs mature, theater grows as legal risk grows, and the suppression_requirement series traces the enforcement ratchet from Bakke-era deference through Croson-era hardening to the withdrawal of deference in 2023. The shape is a stepwise ratchet, not a cycle: each doctrinal episode permanently raised the enforcement intensity required to hold the arrangement, with no relaxation phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the rejected-applicant seat the arrangement is enforced extraction: a specific, personal loss produced by rules the applicant never agreed to and cannot exit. From the beneficiary seat the same rules are access: the difference between an offer and a denial. From the university seat the arrangement is governance: an authority it exercises, documents, and defends, whose costs (litigation, compliance, reputational whiplash) it pays while its identity is fused with the mission the authority serves. From the bench the arrangement is doctrine to be administered — near-symmetric and analytical. The engine computes these divergent per-seat classifications from the structural data; the divergence between the payer seat and the agenda-setter seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (underrepresented_minority_applicants, historically_excluded_communities, plus the diffuse student-body and advocacy-organization beneficiaries) place those seats near the beneficiary end: the arrangement subsidizes them, and their exit options do not convert them into net payers. The victim declaration (rejected_race_blind_admissible_applicants) places that seat near the target end: concentrated, personal, non-restorable loss with constrained exit. Selective_universities sit mid-range and mixed — they exercise the authority and advance their mission through it (pull toward the beneficiary end) while paying litigation and compliance costs under identity-locked exit (pull toward the target end); the secondary payer role encodes this so the derivation does not flatten them into pure administrators. Federal_courts derive near-symmetric: they set and enforce the terms without collecting from the operation. No directionality_overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships, and the universities' mixed position is carried structurally by the secondary role and identity-locked exit rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — documented historical exclusion from selective higher education — is corroborated as historical fact from outside the benefiting parties, but its present-day status is contested: the opposing litigation record concedes the history while disputing whether current gaps remain traceable to remediable exclusion. The reading carries an implicit durational expectation (narrow tailoring conventionally demands time limits; the quarter-century horizon articulated at the doctrine's high-water mark) that was never codified — an unfired sunset. That is the mandatrophy hazard: if remediation is judged complete, or the evidentiary predicate fails, while the permission persists by inertia and routine documentation, the arrangement degrades toward an inertial shell — administered by universities who could change it, borne diffusely by rejected applicants, and deliberately maintained by no one. Classifying this as tangled_rope rather than pure extraction preserves the distinction the corpus needs: the coordination function (a disciplined repair channel plus the diversity-integration rationale) is genuine under this reading's own lights, and the margin transfer rides on it rather than constituting its cover. mandatrophy_resolved is deliberately not declared: the mandate's death is contested, not settled, and the dual_rationale_separability omega tracks the specific pathway by which the mandate could die while the arrangement lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equal_protection_kernel_reading_position,
    'This constraint is one reading (remedial_reading) of the equal_protection_kernel — what changes structurally if a sibling reading governs instead, and where exactly is the disagreement located?',
    'Doctrinal resolution of the trigger-condition dispute: the readings disagree on the clause''s trigger — classification per se (colorblind_reading), subordination-direction (antisubordination_reading), or adequacy of narrow tailoring to a documented compelling interest (this reading). Adoption of colorblind_reading removes the beneficiary set entirely and renders the arrangement itself impermissible; adoption of antisubordination_reading widens permitted action beyond narrow tailoring and shrinks the victim set to hierarchy-entrenching uses.',
    'Under colorblind_reading this constraint''s epsilon redistributes toward zero-for-this-arrangement (the practice becomes unconstitutional per se) and rejected applicants cease to be victims of this constraint specifically; under antisubordination_reading the documentation obligation loosens and the coordination function broadens from repair-channel to hierarchy-dismantling license.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equal_protection_kernel_reading_position, conceptual, 'Committer-frame position: this story is the remedial reading of a three-reading kernel; sibling readings are separate constraints, not measurement parameters of this one.').

omega_variable(
    remedial_evidentiary_threshold,
    'Can present-day institutions satisfy the documented-historical-exclusion predicate with contemporary evidence of representational gaps, or does the predicate require tracing current disadvantage to specific documented discriminatory acts?',
    'Admissions litigation and civil-rights enforcement findings testing whether current gaps are causally traceable to documented exclusion versus generalized societal discrimination that the doctrine has long held insufficient.',
    'If the threshold tightens to specific causal tracing, the beneficiary set contracts sharply and the constraint''s coordination function collapses toward documentation theater; if generalized evidence suffices, the constraint operates as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_evidentiary_threshold, empirical, 'Whether the remedial predicate remains satisfiable under tightening evidentiary standards.').

omega_variable(
    durational_limit_never_fires,
    'Narrow tailoring conventionally demands durational limitation (the Powell/Grutter quarter-century expectation) — does the constraint carry an operative sunset, and what happens when the expected endpoint passes without formal lapse?',
    'Track whether any jurisdiction terminates race-conscious programs upon declaring remediation complete, versus persisting by inertia and routine renewal after the expected window closes.',
    'If the durational expectation never fires, the arrangement drifts from transitional repair toward permanent preference maintenance — the mandate-outlived-function pathway that degrades coordination into inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durational_limit_never_fires, conceptual, 'Unfired implicit sunset: the gap between the reading''s built-in transitory expectation and its actual open-ended operation.').

omega_variable(
    plus_factor_magnitude_opacity,
    'How large is the actual race effect at the admission margin relative to the ''one plus factor among many'' scale the reading''s rhetoric assumes?',
    'Institutional data disclosed in litigation (internal odds-ratio and counterfactual analyses) and natural experiments from race-blind transitions in ban jurisdictions.',
    'Large measured margin effects raise effective extraction on rejected applicants above the authored value and push the arrangement toward the pure-extraction boundary; small effects support the narrow-tailoring defense and the authored value stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plus_factor_magnitude_opacity, empirical, 'Magnitude-versus-rhetoric gap in the plus-factor operation, the key input to accurate epsilon.').

omega_variable(
    dual_rationale_separability,
    'The reading licenses action for BOTH documented remediation AND compelling diversity — are these one interest or two, and which sustains the arrangement if the other fails?',
    'Observe program behavior when one rationale weakens: if programs persist on diversity alone after remediation evidence erodes (or on remediation alone after diversity skepticism spreads), the rationales are separable supports.',
    'If separable, the coordination function can migrate from repair to preference maintenance without textual change — extraction continues under a swapped justification; if fused, failure of either rationale collapses the whole permission structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_rationale_separability, conceptual, 'Whether the twin compelling interests are structurally independent load-bearing walls or one wall with two doors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1989, equal_protection_kernel__remedial_reading, theater_ratio, 1989, 0.28).
narrative_ontology:measurement_basis(equa_tr_t1989, observed).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_kernel__remedial_reading, theater_ratio, 1996, 0.33).
narrative_ontology:measurement_basis(equa_tr_t1996, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2014, equal_protection_kernel__remedial_reading, theater_ratio, 2014, 0.45).
narrative_ontology:measurement_basis(equa_tr_t2014, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.5).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1989, equal_protection_kernel__remedial_reading, base_extractiveness, 1989, 0.44).
narrative_ontology:measurement_basis(equa_be_t1989, observed).
narrative_ontology:measurement(equa_be_t1996, equal_protection_kernel__remedial_reading, base_extractiveness, 1996, 0.49).
narrative_ontology:measurement_basis(equa_be_t1996, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.53).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2014, equal_protection_kernel__remedial_reading, base_extractiveness, 2014, 0.56).
narrative_ontology:measurement_basis(equa_be_t2014, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement_basis(equa_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.22).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1989, equal_protection_kernel__remedial_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement_basis(equa_su_t1989, observed).
narrative_ontology:measurement(equa_su_t1996, equal_protection_kernel__remedial_reading, suppression_requirement, 1996, 0.38).
narrative_ontology:measurement_basis(equa_su_t1996, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.46).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2014, equal_protection_kernel__remedial_reading, suppression_requirement, 2014, 0.56).
narrative_ontology:measurement_basis(equa_su_t2014, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.65).
narrative_ontology:measurement_basis(equa_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% One fixed text (the Fourteenth Amendment's equal protection clause) decomposes into three structurally distinct constraints. The readings differ on the clause's trigger: classification per se (colorblind_reading), subordination-direction (antisubordination_reading), or inadequately-tailored action (remedial_reading, this file). Because the trigger differs, each reading yields a different beneficiary set, victim set, and epsilon over the same institutional surface: colorblind_reading empties the beneficiary set and renders the arrangement itself impermissible; antisubordination_reading widens permitted action and narrows the victim set to hierarchy-entrenching uses; this reading yields the plus-factor arrangement with rejected race-blind-admissible applicants as victims. Lineage: the antisubordination tradition supplied this reading's historical warrant, while colorblind_reading exerts continuous repudiation pressure on it. All three files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
