% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The narrow-warning reading of the beta-designation kernel holds that a
 *   beta label legitimately creates a time-bounded, disclosed testing regime:
 *   developers receive temporary relief from strict liability for known
 *   defects, users who opt in receive clear disclosure and exit options, and
 *   the shield expires when the testing phase ends or a predetermined time
 *   limit is reached. This reading treats the beta window as coordination—a
 *   genuine transition mechanism—and assumes good-faith bounds. The
 *   constraint's extraction is low and its suppressiveness minimal because
 *   the reading does NOT permit indefinite liability evasion, does NOT hide
 *   defects from users who receive them, and DOES preserve baseline product
 *   liability once testing ends. The measurement series captures modest
 *   extraction that reflects the limited-duration relief itself: developers
 *   gain a window of reduced liability, but that window is bounded and
 *   transparent.
 *
 * KEY AGENTS:
 *   - software_developers_during_testing: beneficiary of temporary liability relief during bounded testing window; rely on clear labeling and good-faith transition to stable release
 *   - testing_participants: beneficiary and secondary payer; receive early access and product influence in exchange for bearing risk of disclosed defects
 *   - post_beta_release_users: payer; expect standard liability after testing window ends
 *   - product_liability_system: observer; evaluates whether the beta designation preserves or erodes baseline accountability
 *   - liability_claimants: excluded; those harmed during or post-beta have limited or full recourse depending on timing, but do not participate in the beta decision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.38).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.21).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.21).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, rope).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "legal/technological").

narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'ddd51539-37b4-45ed-8edd-01f09d6f3caa').
narrative_ontology:cs_kernel_codification('ddd51539-37b4-45ed-8edd-01f09d6f3caa', fixed_text).
narrative_ontology:cs_authority_grounding('ddd51539-37b4-45ed-8edd-01f09d6f3caa', extraction).
narrative_ontology:cs_interpretation_layer_present('ddd51539-37b4-45ed-8edd-01f09d6f3caa').
narrative_ontology:cs_reading_relation('ddd51539-37b4-45ed-8edd-01f09d6f3caa', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddd51539-37b4-45ed-8edd-01f09d6f3caa', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('ddd51539-37b4-45ed-8edd-01f09d6f3caa', foundational, testing_phase_is_time_bounded).
narrative_ontology:cs_axiom_status(testing_phase_is_time_bounded, holdable).
narrative_ontology:cs_axiom_grounding('ddd51539-37b4-45ed-8edd-01f09d6f3caa', testing_phase_is_time_bounded, empirically_contingent).
narrative_ontology:cs_axiom('ddd51539-37b4-45ed-8edd-01f09d6f3caa', foundational, liability_shield_expires_with_testing_window).
narrative_ontology:cs_axiom_status(liability_shield_expires_with_testing_window, holdable).
narrative_ontology:cs_axiom_grounding('ddd51539-37b4-45ed-8edd-01f09d6f3caa', liability_shield_expires_with_testing_window, deontological).
narrative_ontology:cs_axiom('ddd51539-37b4-45ed-8edd-01f09d6f3caa', secondary, user_consent_requires_clear_disclosure).
narrative_ontology:cs_axiom_status(user_consent_requires_clear_disclosure, holdable).
narrative_ontology:cs_axiom_grounding('ddd51539-37b4-45ed-8edd-01f09d6f3caa', user_consent_requires_clear_disclosure, conventional).
narrative_ontology:cs_reference_frame('ddd51539-37b4-45ed-8edd-01f09d6f3caa', bounded_testing_window_with_preserved_liability).
narrative_ontology:cs_drift_state('ddd51539-37b4-45ed-8edd-01f09d6f3caa', contemporary_regulatory_escalation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ddd51539-37b4-45ed-8edd-01f09d6f3caa', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers_during_testing).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, testing_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, testing_participants).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, post_beta_release_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers explicitly label software as beta and disclose known limitations. They receive temporary relief from strict liability during a genuine, bounded testing phase—liability shield applies only to disclosed defects and expires when testing ends or a time limit is reached. They retain the duty to test in good faith and to transition to stable release within the declared testing window.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers_during_testing, beneficiary,
    moderate, biographical, mobile, global).

% Users who opt into beta testing receive clear disclosure of the beta status and known defects. They get early access to features and the ability to shape the product through feedback. They also bear the risk of encountering undisclosed bugs during the testing window. Their exit is straightforward: wait for the stable release or use an alternative product.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, testing_participants, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, testing_participants, payer).

% Users who encounter the software only after the beta period ends. They expect standard product liability because the beta designation is no longer valid. If a defect persists from the testing phase, they cannot be told it was a 'known issue' from beta—the liability shield has expired and standard negligence and strict liability rules apply.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, post_beta_release_users, payer,
    powerless, biographical, constrained, global).

% Evaluates whether the beta designation creates a legitimate time-bounded exception to product liability or an indefinite shield against accountability. This reading holds that a genuinely bounded testing phase (measured in months, not years) with good-faith disclosure preserves the base liability regime while permitting temporary relief for disclosed defects.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, product_liability_system, observer,
    institutional, generational, analytical, national).

% Users harmed by defects during or immediately after the testing phase. Under this reading, those harmed during the testing window by disclosed defects have limited recourse; those harmed post-beta have full recourse. Their voice is excluded from the beta designation itself—they are not consulted on what constitutes a 'genuine' testing phase.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, liability_claimants, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, software_developers_during_testing).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from development to market-ready software by creating a defined, time-bounded window where developers test under controlled conditions with informed participants, reducing the risk of catastrophic failures on release.
% TRANSFER_FUNCTION: Transfers liability risk from developers to testing participants during the testing window; the participant accepts the risk of undisclosed defects in exchange for early access and influence on the product. The transfer expires when the testing window ends.
% ABSENT_VOICES: Post-beta users and liability claimants who do not opt into beta testing are excluded from the beta designation decision. They would argue that the liability shield should not persist beyond the genuine testing phase and that 'beta' is routinely abused as indefinite cover. Alternative software developers and competing products are also absent—they would challenge whether a single vendor's unilateral beta declaration should receive legal deference.
% DISAPPEARANCE_RATIONALE: If the narrow beta reading vanished—i.e., if all software releases received standard product liability from day one—developers would be forced to conduct internal testing longer before release, or would conduct testing in secret. The testing-disclosure window would compress or disappear. Users would gain stronger liability protections at the cost of slower feature release and fewer opportunities to shape products during development.
% FOUNDING_PROBLEM: Early software development required a testing phase to identify defects before release to general users. Developers needed a way to signal that software was experimental and solicit feedback from willing testers without facing liability for every undisclosed bug. The founding problem is the gap between development and market readiness.
% FOUNDING_PROBLEM_CORROBORATION: Software industry practice attests that testing is necessary; independent security researchers and usability engineers attest that external testing improves product quality. However, consumer protection advocates and liability researchers attest that the 'beta' label is routinely applied indefinitely (some software remains in 'beta' for years), rendering the testing-phase framing unmoored from reality. Regulatory agencies and tort reformers offer divergent readings: some accept the narrow testing window, others argue it is systematically abused.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).

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
 *   Extractiveness is moderate-low (0.38 at interval end) because the liability relief is genuinely bounded: developers gain temporary shelter only for disclosed defects, and the shelter expires when the testing phase concludes. There is no indefinite rent collection or status-quo protection. Suppression is low (0.21) because the constraint's enforcement depends on good-faith disclosure and clear labeling, not on hiding information or trapping users—testing participants have mobile exit (they can wait for the stable release) and post-beta users have standard recourse. Theater is very low (0.12) because the beta label is meant to signal real testing activity, not to perform a function while doing something else. The measurement trajectory is essentially flat, reflecting that the constraint's operation does not substantially change as time passes within the testing window—the benefit and cost are stable until the window expires. This measurement pattern (flat extractiveness, stable suppression, minimal theater) is consistent with a genuine coordination mechanism operating normally, without drift toward capture or performance.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat and the testing-participant seat should compute similarly under this reading: both benefit from the bounded window, both understand the shield is temporary. The post-beta user seat diverges: they expect standard liability and have no shield at all. The engine computes this from the structural data—developers and participants are coordinated around the testing function; post-beta users are outside the window and subject to full liability. The key divergence is TEMPORAL: the developer's beneficiary position expires when the testing window ends, whereas the post-beta user's payer position activates at that same moment. This temporal flip is the reading's core structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers receive low directionality (d near 0.2–0.3): they are coordinated around the testing function and the relief is temporary and disclosed. Testing participants receive symmetric directionality (d near 0.4–0.5): they benefit from early access and product influence but bear the risk of disclosed defects; their exit is mobile. Post-beta users receive high directionality (d near 0.7–0.8): they receive standard product liability and have no shelter—they are full targets of the liability regime, though the regime itself is presumed fair. Liability claimants are excluded, not seated—their interests are represented through the post-beta user seat and through the product liability system as observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading addresses mandatrophy explicitly through the sunset-clause requirement and the good-faith testing bound. If a constraint claiming to be beta-testing relief persists for years without transitioning to stable release, the reading's core premise is violated: the mandate was testing, not indefinite protection. Under this reading, a software product in 'beta' for 5+ years without declared transition is no longer beta—it is a snare hiding behind beta language. The measurement trajectory would shift (theater_ratio would rise, extractiveness would climb) if the testing window were repeatedly extended beyond declared bounds. The current measurements (flat, stable) reflect a constraint operating as the narrow reading describes it: genuine bounded testing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_phase_bounds,
    'What duration and development milestones constitute a ''genuine testing phase'' under this reading? Is 6 months sufficient? 18 months? Can a product be in beta indefinitely if development continues?',
    'Regulatory or judicial precedent establishing explicit time limits or transition criteria (e.g., product must transition to stable release or full liability within X months, or testing-phase activity must meet documented goals). Industry-standard development timelines for comparable products provide empirical baseline.',
    'If the boundary is vague, developers can exploit ambiguity by perpetually extending the ''testing'' window, converting the reading into a shield that behaves like the expansive reading. If the boundary is clear, the narrow reading''s core mandate-expiration mechanism functions as designed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_testing_phase_bounds, empirical, 'The boundary between legitimate testing window and indefinite beta evasion.').

omega_variable(
    good_faith_disclosure_scope,
    'What constitutes ''good faith'' disclosure of known defects? Must developers list every bug, or only ''major'' ones? Who decides what is major? Can a defect be withheld if the developer expects it will be fixed by release?',
    'Case law or regulatory guidance on disclosure standards; empirical study of what information typical testers actually receive vs. what defects later emerge.',
    'Narrow disclosure standards favor developers (shield covers more defects) and drift toward the expansive reading. Broad standards favor testers and approach strict liability. The reading''s coherence depends on disclosure norms being sufficiently clear that testers can make informed choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_disclosure_scope, empirical, 'The definition of adequate disclosure within the testing window.').

omega_variable(
    liability_preservation_mechanics,
    'How is base product liability actually preserved after the testing window expires? If a defect persists from beta into stable release, can a post-beta user sue under strict liability, or must they prove the defect was negligently retained?',
    'Statutory or case law clarifying the relationship between pre-release testing and post-release liability; empirical outcome tracking of product-liability cases involving formerly-beta software.',
    'Strong preservation (strict liability applies to all defects, even those from beta) reinforces the reading''s liability shield as truly temporary. Weak preservation (post-release users must prove negligence about retained defects) undermines the reading and slides toward indefinite protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_preservation_mechanics, empirical, 'Whether base product liability actually governs after the testing window ends.').

omega_variable(
    kernel_reading_contestation,
    'Which reading of the beta-designation kernel (narrow_warning, expansive_shield, or severity_carve_out) will prevail in regulatory and judicial interpretation?',
    'Emerging statutory law, judicial precedent, regulatory guidance from consumer-protection agencies, and international harmonization (EU, China, other jurisdictions have begun regulating beta designation explicitly). The narrow reading is presently ascendant in newer regulations (e.g., EU Digital Products Act discourages indefinite beta), but the expansive reading remains dominant in US software licensing practice.',
    'If the narrow reading prevails, this constraint remains stable at moderate extraction and low suppression. If the expansive reading prevails, the narrow reading''s scope shrinks and this constraint may become obsolete (replaced by the expansive-shield constraint). If the severity reading prevails, critical systems are carved out and the narrow reading''s domain is further restricted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'The broader contest among the three readings for legal and regulatory authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(beta_tr_t0, observed).
narrative_ontology:measurement(beta_tr_t6, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement_basis(beta_tr_t6, observed).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement_basis(beta_tr_t12, observed).
narrative_ontology:measurement(beta_tr_t18, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement_basis(beta_tr_t18, observed).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(beta_tr_t24, observed).
narrative_ontology:measurement(beta_tr_t30, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(beta_tr_t30, observed).
narrative_ontology:measurement(beta_tr_t36, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 36, 0.12).
narrative_ontology:measurement_basis(beta_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(beta_be_t0, observed).
narrative_ontology:measurement(beta_be_t6, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(beta_be_t6, observed).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(beta_be_t12, observed).
narrative_ontology:measurement(beta_be_t18, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement_basis(beta_be_t18, observed).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(beta_be_t24, observed).
narrative_ontology:measurement(beta_be_t30, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(beta_be_t30, observed).
narrative_ontology:measurement(beta_be_t36, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 36, 0.38).
narrative_ontology:measurement_basis(beta_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(beta_su_t0, observed).
narrative_ontology:measurement(beta_su_t6, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 6, 0.19).
narrative_ontology:measurement_basis(beta_su_t6, observed).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement_basis(beta_su_t12, observed).
narrative_ontology:measurement(beta_su_t18, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 18, 0.22).
narrative_ontology:measurement_basis(beta_su_t18, observed).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement_basis(beta_su_t24, observed).
narrative_ontology:measurement(beta_su_t30, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(beta_su_t30, observed).
narrative_ontology:measurement(beta_su_t36, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 36, 0.21).
narrative_ontology:measurement_basis(beta_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, product_liability_general_regime).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, software_warranty_disclaimers).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the beta_designation_doctrine kernel. The three readings—narrow_warning, expansive_shield, and severity_carve_out—are structurally distinct constraints with different beneficiary sets, victim sets, and ε values. They are linked via network.affects_constraints to establish the constraint family. The narrow reading (this file) instantiates a bounded-testing-window regime with preserved base liability; the expansive reading instantiates indefinite liability waiver; the severity reading excises critical systems from beta eligibility entirely. See kernel_context in commentary and cs_structure for the reading relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__narrow_warning_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
