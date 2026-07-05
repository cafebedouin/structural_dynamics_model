% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint is the narrow-warning reading of the beta designation
 *   kernel: a software firm's 'beta' label is legitimate testing-phase
 *   disclosure only when the testing window is genuine, the disclosed risk is
 *   specific, and base product liability resumes fully once the window closes
 *   or once harm falls outside the disclosed scope. Under this reading the
 *   coordination function is real and modest — developers get a bounded,
 *   honest mechanism to gather live-use data, and testers get informed,
 *   revocable participation, not a permanent waiver. This story deliberately
 *   does NOT model the expansive_shield_reading (where the same label is read
 *   as comprehensive, indefinite waiver) or the severity_carve_out_reading
 *   (where beta status is categorically unavailable for critical systems
 *   regardless of disclosure) — those are separate constraints with different
 *   beneficiary/victim structures and different epsilon values, linked here
 *   only through the shared kernel_id.
 *
 * KEY AGENTS:
 *   - software_developers_conducting_genuine_testing: agenda_setter/beneficiary (organized/constrained) — sets and administers the testing-phase disclosure
 *   - early_adopter_testers: beneficiary/payer (moderate/mobile) — accepts bounded, disclosed risk in exchange for early access and retained recourse
 *   - consumer_protection_regulators: observer (institutional/analytical) — polices genuineness of the testing window
 *   - courts_adjudicating_beta_disputes: observer (institutional/analytical) — resolves boundary disputes about scope and duration
 *   - expansive_shield_proponents: excluded (organized/constrained) — prefers a broader reading this constraint forecloses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.28).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '722107df-74da-4539-8286-4ee1068cb73b').
narrative_ontology:cs_kernel_codification('722107df-74da-4539-8286-4ee1068cb73b', distributed).
narrative_ontology:cs_authority_grounding('722107df-74da-4539-8286-4ee1068cb73b', distributed).
narrative_ontology:cs_reading_relation('722107df-74da-4539-8286-4ee1068cb73b', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('722107df-74da-4539-8286-4ee1068cb73b', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('722107df-74da-4539-8286-4ee1068cb73b', foundational, shield_duration_must_track_actual_testing_activity).
narrative_ontology:cs_axiom_status(shield_duration_must_track_actual_testing_activity, holdable).
narrative_ontology:cs_axiom_grounding('722107df-74da-4539-8286-4ee1068cb73b', shield_duration_must_track_actual_testing_activity, empirically_contingent).
narrative_ontology:cs_axiom('722107df-74da-4539-8286-4ee1068cb73b', foundational, base_liability_resumes_outside_disclosed_scope).
narrative_ontology:cs_axiom_status(base_liability_resumes_outside_disclosed_scope, holdable).
narrative_ontology:cs_axiom_grounding('722107df-74da-4539-8286-4ee1068cb73b', base_liability_resumes_outside_disclosed_scope, deontological).
narrative_ontology:cs_reference_frame('722107df-74da-4539-8286-4ee1068cb73b', informed_consent_bounded_disclosure_standard).
narrative_ontology:cs_drift_state('722107df-74da-4539-8286-4ee1068cb73b', post_platform_scale_software_deployment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('722107df-74da-4539-8286-4ee1068cb73b', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, genuine_testing_phase_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the beta label to a product actually undergoing pre-release testing, discloses known instability, and collects real-world performance and bug data from users. The disclosure buys a temporary reduction in liability exposure for defects that are genuinely unresolved because testing is incomplete — but the shield only covers the testing window and does not touch liability for the underlying product once it ships or for harms unrelated to the disclosed instability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing, beneficiary).

% Chooses to use beta software knowing it is unfinished, in exchange for early access and a voice in shaping the final product. Bears the ordinary risk of bugs and instability that come with genuinely unfinished software, but retains full recourse for harms outside the disclosed testing scope — the disclosure narrows what they assumed the risk of, not whether they have a remedy at all. Can simply decline to test or can stop using the beta at will.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers, payer).

% Monitors whether beta labeling is being used as advertised — a genuine, time-limited testing phase with disclosed risk — or is being stretched into something else. Under this reading, regulators have a clear, administrable rule to enforce: a real test period, real disclosure, and unaffected base liability once the test ends or the disclosed defect is unrelated to the harm.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% Applies the doctrine at the boundary case: was this actually a testing phase, was the duration genuine, and did the harm fall inside or outside the disclosed risk? Under the narrow reading, courts have a tractable, fact-bound inquiry rather than a binary shield/no-shield question.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts_adjudicating_beta_disputes, observer,
    institutional, generational, analytical, national).

% Firms and counsel who would prefer beta labeling function as a durable, indefinite liability shield are not accommodated by this reading — the narrow reading forecloses treating a permanent 'beta' tag as comprehensive waiver. They are not victims of this constraint (no harm accrues to them), but their preferred framing is structurally unavailable here.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, expansive_shield_proponents, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets developers ship genuinely unfinished software for real-world testing while giving users clear notice of the specific, time-bounded risk they are accepting — solving the problem that no software can be perfected without deployment-scale testing, without requiring blanket liability exposure for known, disclosed, temporary defects.
% TRANSFER_FUNCTION: Moves a narrow, disclosed slice of defect risk from developer to informed tester for the duration of a genuine testing phase; does not move liability for undisclosed defects, for harms outside the tested scope, or for any period after the testing phase ends.
% ABSENT_VOICES: Firms that would prefer an expansive or indefinite reading of the shield are not accommodated here, but they are not silenced victims — they simply operate under a different, less favorable-to-them doctrine than the one this reading establishes. Testers harmed by defects outside the disclosed scope are NOT absent under this reading — they retain full recourse, which is the reading's central design feature.
% DISAPPEARANCE_RATIONALE: If this narrow reading disappeared and were replaced by no beta doctrine at all, developers would either forgo real-world testing (reducing product quality feedback loops) or ship untested software without any disclosure framework, increasing undisclosed risk to users. If it were replaced by the expansive reading instead, users would lose ongoing recourse for genuinely undisclosed or out-of-scope harms — a different and worse rearrangement.
% FOUNDING_PROBLEM: Software cannot be fully validated before real-world deployment at scale; some class of defects only surfaces under live use. The doctrine was built to let developers gather that data honestly, with users' informed participation, without either forcing indefinite pre-release limbo or permitting silent risk-shifting.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection regulators and courts adjudicating beta-labeling disputes independently attest that genuine pre-release testing remains a real technical necessity and that disclosure-based risk allocation for a bounded period serves users' interests when duration and scope are policed — this corroboration comes from adjudicating and regulatory bodies outside the developer beneficiary class, not merely from developers asserting their own good faith.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) and stable across the interval because the narrow reading structurally caps what can be extracted: liability only shifts for disclosed, in-scope, time-bounded defects, and reverts automatically once the test ends. Suppression is low (0.22) because testers retain mobility and full recourse outside the tested scope — there is no coercive lock-in. Theater ratio is low-modest (0.20) reflecting that some firms may over-use the label decoratively even under a well-policed regime, but the doctrine's core function (real testing, real disclosure) remains substantively operative rather than performative. Accessibility collapse is moderate-low (0.30): once informed, testers understand exactly what alternative they gave up (waiting for the finished release) and can exit at will.
 *
 * PERSPECTIVAL GAP:
 *   Developers experience this as a workable, bounded coordination tool; regulators and courts experience it as an enforceable, fact-bound doctrine with a genuine administrability test (was this really testing? was the duration real?). Expansive-shield proponents would experience the identical structural facts as an unwelcome constraint on their preferred use of the label — but that is a preference gap about which reading should govern, not a divergence within this reading's own operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers sit near the beneficiary end: they collect the liability-narrowing benefit during the disclosed window and administer the designation. Testers are near-symmetric-to-beneficiary: they gain early access and product influence, bear only the specific disclosed risk, and keep an exit at all times, so directionality is not pushed toward victimhood. No group is declared a victim under this reading precisely because the reading's defining structural feature is that the shield is narrow enough that no one absorbs undisclosed or out-of-scope risk without recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (validating software under real-world conditions before general release) remains live, and this reading prevents mandatrophy by keeping the shield's scope pinned to that live problem: the moment the testing phase ends or extends indefinitely without genuine testing activity, the shield doctrine no longer applies and base liability resumes. This is precisely what distinguishes the narrow reading from the expansive reading, which would let a stale 'beta' label persist as permanent liability insulation long after the founding problem (uncertain real-world performance) has been resolved by accumulated testing data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_phase_determination,
    'What objective criteria distinguish a genuine testing phase (justifying the narrow shield) from a nominal or extended ''beta'' label used to obtain shield benefits without corresponding testing activity?',
    'Courts and regulators would need to examine actual telemetry collection, iteration cadence, disclosed-defect resolution rates, and duration relative to comparable product categories to determine whether a given beta period was substantively a test or was functionally indefinite.',
    'If no administrable line exists between genuine and nominal testing, the narrow reading collapses in practice toward either the expansive reading (courts defer to the label) or a de facto ban (courts refuse the shield whenever contested) — undermining the reading''s claim to be a distinct, stable middle position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_determination, empirical, 'Whether ''genuine testing phase'' is administrable as a bright-line or fact-intensive standard.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three sibling readings (expansive_shield, narrow_warning, severity_carve_out) actually governs beta designation in a given jurisdiction or contract, absent controlling precedent?',
    'Track appellate outcomes and legislative activity across jurisdictions to see which reading courts and regulators converge on, and whether convergence differs by product category (consumer software vs. safety-critical systems).',
    'If the severity_carve_out_reading is adopted for a given product category, the narrow_warning_reading modeled here would not apply to that category at all regardless of how genuine the testing phase was — the category boundary would override the disclosure-adequacy analysis this story assumes governs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which sibling reading of the beta designation kernel actually controls in unsettled jurisdictions.').

omega_variable(
    disclosure_adequacy_vs_comprehension,
    'Does formal disclosure of beta status satisfy the doctrine''s informed-consent premise even when ordinary consumers do not meaningfully understand the technical risk being disclosed?',
    'Behavioral studies of consumer comprehension of beta-risk disclosures, compared against the legal standard of what constitutes adequate notice.',
    'If comprehension is systematically low, the narrow reading''s core distinguishing claim (users are informed, not victimized) weakens, pushing the constraint''s real-world operation closer to the expansive reading''s risk-shifting effect despite the formal doctrine remaining narrow.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disclosure_adequacy_vs_comprehension, empirical, 'Whether formal disclosure achieves substantive informed consent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'beta designation liability doctrine' per the epsilon-invariance principle. The three readings have materially different epsilon values: this narrow reading has low, stable extractiveness (~0.28) because the shield is bounded and reversible; the expansive_shield_reading would carry substantially higher extractiveness and suppression because it treats the label as durable, near-unlimited waiver; the severity_carve_out_reading would show near-zero extractiveness for the specific product categories it covers because the shield is categorically unavailable there, making it closer to a mountain/rope hybrid within its scope. All three are linked via affects_constraints because a court or legislature resolving the kernel in favor of one reading directly forecloses or narrows the practical space available to the others within the same jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
