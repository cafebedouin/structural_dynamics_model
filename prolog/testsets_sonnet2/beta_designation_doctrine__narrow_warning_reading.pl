% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
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
 *   Software publishers commonly designate a product 'beta' to disclose that
 *   it remains under active testing, informing users of elevated instability
 *   risk in exchange for early access. This story instantiates the narrow
 *   warning reading of the contested 'beta designation' kernel: the
 *   designation functions as time-bounded, good-faith testing disclosure,
 *   base product liability for undisclosed defects is preserved throughout,
 *   and the shield is void ab initio if the testing period is not genuine
 *   (indefinite duration, no real defect-fixing activity, or disclosure that
 *   does not match actual risk). This is a low-extraction,
 *   low-coordination-overhead scaffold — the coordination function (honest
 *   testing disclosure) is real and modest, and the shield sunsets by design
 *   when the testing phase ends or is exposed as pretextual.
 *
 * KEY AGENTS:
 *   - software_publishers_conducting_genuine_testing: agenda_setter/beneficiary (organized/constrained) — sets and administers the designation, bears the sunset condition
 *   - beta_testing_participants: beneficiary/payer (moderate/mobile) — accepts disclosed risk, retains recourse for undisclosed harm
 *   - courts_and_regulators: observer (institutional/analytical) — adjudicates genuineness of the testing phase
 *   - downstream_consumers_of_released_product: excluded (powerless/constrained) — outside the designation's scope under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.28).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.15).

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
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '72e89a90-a976-4ba8-a909-904611b84c8c').
narrative_ontology:cs_kernel_codification('72e89a90-a976-4ba8-a909-904611b84c8c', distributed).
narrative_ontology:cs_authority_grounding('72e89a90-a976-4ba8-a909-904611b84c8c', distributed).
narrative_ontology:cs_reading_relation('72e89a90-a976-4ba8-a909-904611b84c8c', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('72e89a90-a976-4ba8-a909-904611b84c8c', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('72e89a90-a976-4ba8-a909-904611b84c8c', foundational, shield_expires_with_genuine_testing_phase).
narrative_ontology:cs_axiom_status(shield_expires_with_genuine_testing_phase, holdable).
narrative_ontology:cs_axiom_grounding('72e89a90-a976-4ba8-a909-904611b84c8c', shield_expires_with_genuine_testing_phase, conventional).
narrative_ontology:cs_axiom('72e89a90-a976-4ba8-a909-904611b84c8c', foundational, base_liability_for_undisclosed_defects_survives_beta_label).
narrative_ontology:cs_axiom_status(base_liability_for_undisclosed_defects_survives_beta_label, holdable).
narrative_ontology:cs_axiom_grounding('72e89a90-a976-4ba8-a909-904611b84c8c', base_liability_for_undisclosed_defects_survives_beta_label, deontological).
narrative_ontology:cs_reference_frame('72e89a90-a976-4ba8-a909-904611b84c8c', consent_based_bounded_disclosure_regime).
narrative_ontology:cs_drift_state('72e89a90-a976-4ba8-a909-904611b84c8c', contemporary_saas_continuous_release_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72e89a90-a976-4ba8-a909-904611b84c8c', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_publishers_conducting_genuine_testing).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_testing_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_testing_participants).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_to_known_defect_risk).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, testing_phase_disclosure_adequacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Labels a product 'beta' to disclose that it is undergoing genuine pre-release testing, gaining a bounded, temporary reduction in exposure to claims arising from disclosed, testing-phase defects. Must actually ship a final, fully-liable release within a genuine testing window or the designation lapses and full liability re-attaches retroactively to the testing period under this reading.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_publishers_conducting_genuine_testing, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, software_publishers_conducting_genuine_testing, beneficiary).

% Opt into beta programs knowingly, receiving early access and a role in improving the product in exchange for accepting disclosed instability risk. Retain full recourse for undisclosed defects, and for any harm outside the scope of what the beta disclosure actually warned about; can exit the beta program at will.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_testing_participants, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, beta_testing_participants, payer).

% Adjudicate whether a given 'beta' designation was a genuine, time-bounded testing phase or a pretextual label used to indefinitely dodge liability. Under this reading, courts scrutinize duration, disclosure content, and whether base product liability for undisclosed harms was actually preserved.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% Purchase or use the product after it exits beta and is marketed as a finished release. They were never part of the beta program and did not consent to any testing-phase risk; under this reading they are outside the beta designation's scope entirely and retain ordinary full liability protection.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, downstream_consumers_of_released_product, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows publishers to disclose genuine pre-release instability to a bounded, opted-in group of users, enabling real-world testing that improves the eventual released product while giving testers a clear, honest signal about the risk they are accepting.
% TRANSFER_FUNCTION: Moves a narrow, disclosed slice of testing-phase risk from publisher to informed, consenting beta participants for the duration of a genuine testing window; moves nothing from parties who never consented to beta status, and moves nothing at all once the designation expires or is found pretextual.
% ABSENT_VOICES: Downstream consumers who buy the finished, post-beta release have no voice in how the beta period was conducted, but under this reading they are also outside its scope and are not exposed to any transferred risk — their absence is not a suppressed grievance because the shield does not reach them.
% DISAPPEARANCE_RATIONALE: If this narrow reading of beta designation disappeared, publishers would either forgo real-world pre-release testing (reducing product quality and defect discovery) or would test covertly without disclosure, converting a transparent, consent-based arrangement into a hidden one — the informed-testing coordination function would have to be reconstructed some other way.
% FOUNDING_PROBLEM: Software could not be adequately tested against real-world use conditions without external testers, but testers exposed to genuine early-stage instability needed a clear signal of that risk and publishers needed some bounded assurance they would not face full-release liability for defects they were actively still hunting during a disclosed testing window.
% FOUNDING_PROBLEM_CORROBORATION: Independent software-quality researchers and consumer-protection litigators (outside the publisher beneficiary class) corroborate that disclosed beta testing genuinely surfaces defects undetectable in internal QA, and that courts applying this narrow reading have in practice voided beta shields where duration was indefinite or disclosure was thin — evidence the doctrine is being tested against its own genealogy rather than simply asserted by publishers.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.28) and stable because this reading limits the shield strictly to disclosed, testing-phase-specific risk accepted by consenting participants; the slight upward drift reflects ordinary scope-creep pressure toward calling ever-longer periods 'beta,' but the reading's genuineness requirement caps how far that can go before a court reclassifies. Suppression is low (0.22) because participants are informed and can exit the beta program; there is no coercion to remain a tester. Theater ratio is low and only mildly rising (0.08 to 0.15) — most beta programs under this reading do real defect-fixing work, though some drift toward performative 'beta' labeling for marketing purposes is observable over time. Accessibility collapse and resistance are both moderate-low, consistent with a genuine, functioning scaffold rather than an entrenched extraction structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers are near-beneficiary but not purely so — the sunset condition and the retroactive liability reattachment for pretextual use constrain their position; they bear real risk if the testing window is later found non-genuine. Beta testers sit close to symmetric: real benefit (early access, product influence) balanced against disclosed, bounded risk they accepted knowingly, with full exit available. Downstream consumers are entirely outside this reading's scope — the shield does not reach them, so they are neither beneficiaries nor victims here, only excluded from the beta-phase conversation while remaining structurally unaffected by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for disclosed, consent-based real-world testing) remains live and is corroborated by parties outside the publisher class, which is why this reading resists mandatrophy: the shield expires with the testing phase by construction (has_sunset_clause), and courts under this reading actively police genuineness rather than rubber-stamping indefinite 'beta' labeling. This prevents the coordination function from calcifying into a permanent extraction vehicle — the moment duration or disclosure content stops tracking genuine testing, this reading's own logic voids the shield.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_testing_phase_ambiguity,
    'How does a court or regulator reliably distinguish a genuine, good-faith testing phase from a pretextual ''beta'' label used to extend liability protection indefinitely?',
    'Establish objective proxies: rate of defect-fix commits, existence of a published or internally-tracked release criterion, actual duration relative to comparable products'' testing phases, and whether marketing materials describe the product as feature-complete despite the beta label.',
    'If no reliable proxy exists, the narrow reading collapses in practice into the expansive_shield_reading, because publishers can indefinitely claim genuineness without effective external check — this would raise this constraint''s effective extraction toward the expansive reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_testing_phase_ambiguity, empirical, 'Whether genuineness of a testing phase can be reliably verified, which is load-bearing for keeping this reading distinct from the expansive_shield_reading.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings (narrow_warning, expansive_shield, severity_carve_out) does controlling law in a given jurisdiction actually adopt, and is that choice stable across contexts (consumer software vs. embedded/critical systems)?',
    'Survey case law and statutory consumer-protection provisions across jurisdictions to determine which reading (or hybrid) is operative; track whether courts apply severity_carve_out as an override on top of narrow_warning_reading in critical-systems cases.',
    'If most jurisdictions in practice apply something closer to expansive_shield_reading, this narrow reading is aspirational/minority doctrine rather than the operative legal rule, which would not change this story''s own authored ε but would change how much real-world weight this reading should be given in downstream analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Documents that this story is one of three live readings of a single contested kernel, and that the operative legal reading is itself unsettled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(beta_designation_doctrine__narrow_warning_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the beta_designation_doctrine kernel, each authored as a separate constraint per the epsilon-invariance principle. narrow_warning_reading (this story) authors a low, stable epsilon (~0.28) reflecting a bounded, genuinely time-limited testing disclosure with preserved base liability. expansive_shield_reading would author a substantially higher epsilon reflecting indefinite-duration, all-context liability waiver. severity_carve_out_reading operates orthogonally as a categorical domain restriction rather than a duration/disclosure test, and would likely author its own distinct epsilon around the cost of misapplying any beta shield to critical systems. The three are linked via affects_constraints because a court adopting one reading structurally forecloses or pressures the others within that jurisdiction's controlling doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
