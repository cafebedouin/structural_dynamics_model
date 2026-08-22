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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Reading)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Beta designation under the narrow_warning_reading is a temporary,
 *   good-faith testing framework that suspends certain product liability
 *   categories while an authorized developer tests software on consenting
 *   users. The liability suspension is not a blanket waiver: it covers only
 *   defects arising from incomplete development (strict liability,
 *   merchantability breach) and expires automatically when the testing phase
 *   ends or the product reaches market release. Developers retain liability
 *   for gross negligence, willful misconduct, undisclosed safety hazards, and
 *   violations of explicit safety commitments. Users are informed and
 *   participate voluntarily. Regulatory authorities police the bounds: they
 *   investigate whether declared testing phases are genuine, scrutinize
 *   indefinite beta designations as liability escape attempts, and can revoke
 *   the suspension if abused. The constraint is CLAIMED as scaffold
 *   (temporary, sunset built in) and the metrics describe low extraction with
 *   moderate enforcement overhead—the claim and metrics align because the
 *   reading's structural design is genuinely transitory.
 *
 * KEY AGENTS:
 *   - software_developers_testing_phase: Exercise the beta designation; benefit from liability suspension; must end testing or release the product (cannot use beta indefinitely)
 *   - beta_users_informed_participants: Volunteer for testing with explicit notice; bear defect risk; can exit by discontinuing use
 *   - downstream_dependents_non_consenting: May be affected by beta software without consent; retain full liability claims; motivate the reading's policing mechanism
 *   - regulatory_authorities_consumer_protection: Enforce good-faith bounds; investigate abuse; can reclassify a false-testing regime as snare
 *   - product_safety_advocates: Argue for the severity_carve_out reading; monitor scope creep; provide external corroboration of the founding problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.32).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.18).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '61b39a4f-f707-487a-a9a9-5c16946f796b').
narrative_ontology:cs_kernel_codification('61b39a4f-f707-487a-a9a9-5c16946f796b', fixed_text).
narrative_ontology:cs_authority_grounding('61b39a4f-f707-487a-a9a9-5c16946f796b', practice).
narrative_ontology:cs_interpretation_layer_present('61b39a4f-f707-487a-a9a9-5c16946f796b').
narrative_ontology:cs_reading_relation('61b39a4f-f707-487a-a9a9-5c16946f796b', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('61b39a4f-f707-487a-a9a9-5c16946f796b', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('61b39a4f-f707-487a-a9a9-5c16946f796b', foundational, testing_phase_temporally_bounded).
narrative_ontology:cs_axiom_status(testing_phase_temporally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('61b39a4f-f707-487a-a9a9-5c16946f796b', testing_phase_temporally_bounded, deontological).
narrative_ontology:cs_axiom('61b39a4f-f707-487a-a9a9-5c16946f796b', foundational, base_product_liability_preserved).
narrative_ontology:cs_axiom_status(base_product_liability_preserved, holdable).
narrative_ontology:cs_axiom_grounding('61b39a4f-f707-487a-a9a9-5c16946f796b', base_product_liability_preserved, deontological).
narrative_ontology:cs_axiom('61b39a4f-f707-487a-a9a9-5c16946f796b', secondary, informed_consent_required_for_suspension).
narrative_ontology:cs_axiom_status(informed_consent_required_for_suspension, holdable).
narrative_ontology:cs_axiom_grounding('61b39a4f-f707-487a-a9a9-5c16946f796b', informed_consent_required_for_suspension, conventional).
narrative_ontology:cs_reference_frame('61b39a4f-f707-487a-a9a9-5c16946f796b', testing_disclosure_with_bounded_liability_suspension).
narrative_ontology:cs_drift_state('61b39a4f-f707-487a-a9a9-5c16946f796b', contemporary_software_market_evolution, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61b39a4f-f707-487a-a9a9-5c16946f796b', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers_testing_phase).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_users_informed_participants).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_users_informed_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% During a genuine testing phase, may distribute software under explicit beta designation. The designation suspends certain product liability categories (strict liability for defects, breach of merchantability) while preserving liability for willful misconduct, gross negligence, and violations of explicit safety commitments. The developer sets the testing scope and duration, but the benefit (liability reduction) automatically expires when the testing phase ends or when the product reaches market release—whichever is sooner. Exit from the regime means releasing a finished product and accepting full liability, or continuing beta testing indefinitely (prohibited under this reading).
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers_testing_phase, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, software_developers_testing_phase, agenda_setter).

% Participate in testing with explicit, clear notice that the software is in testing, that defects are expected, and that liability for defect-caused harm is suspended. They receive the benefit of early access and the ability to shape development; they bear the cost of expected defects. Under this reading, the suspension is real but bounded: it applies only to defects, not to injuries from undisclosed security flaws or willfully unsafe features. They can exit by simply not installing or by discontinuing use; the suspension does not trap them.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_users_informed_participants, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, beta_users_informed_participants, beneficiary).

% May be affected by beta software used by another party without their knowledge or consent (e.g., medical device that uses a beta-licensed library; financial system relying on beta components). Under this reading, they are excluded from the testing consent frame and retain full product liability claims against the developer—the liability suspension does not extend to non-participants. Their structural position is the constraint's vulnerability: the reading requires developers to ensure beta designation does not invisibly propagate downstream.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, downstream_dependents_non_consenting, excluded,
    powerless, immediate, trapped, global).

% Enforce the bounds of the testing regime: scrutinize whether declared testing phases are genuine, investigate whether beta designation is being used indefinitely as a liability escape, and revoke the suspension if a developer abuses the mechanism (e.g., selling obviously finished software as 'beta' to avoid recalls). They can impose remedies that reclassify a constraint from scaffold to snare if the testing frame is fraudulent.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, regulatory_authorities_consumer_protection, agenda_setter,
    institutional, generational, analytical, national).

% Monitor and contest the scope of beta designations, especially in safety-critical domains. They argue for the severity_carve_out_reading (beta never valid for life-safety systems) and provide testimony and analysis to regulators and courts. They see the narrow reading as a necessary compromise between innovation and safety, but one that must be policed strictly.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, product_safety_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, software_developers_testing_phase).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables software developers to test products on real users in real environments while explicitly limiting liability exposure for defects, so that iteration happens without catastrophic legal risk. Simultaneously enables users to participate in development, shape features, and gain early access. The coordination solves a genuine problem: a finished-product liability regime freezes innovation because defects trigger massive liability; a testing regime with bounds allows iteration.
% TRANSFER_FUNCTION: Moves the risk of defect-caused harm from developers to participating users (and only during the testing phase). Developers retain liability for gross negligence, willful misconduct, and safety violations. The transfer is temporary (expires with the testing phase) and conditional (requires explicit disclosure to participants).
% ABSENT_VOICES: Downstream dependents who use beta software without consent are structurally excluded—they did not volunteer for the testing regime. Vendors who sell beta software as finished product under a false testing designation are also absent from the conversation, by definition; the reading's bounds depend on detecting and excluding them. Product safety advocates argue for the severity_carve_out reading and are present in regulatory debate but not seated in development decisions.
% DISAPPEARANCE_RATIONALE: If the beta designation regime disappeared—if developers could never suspend liability for testing—the software industry would restructure around closed alpha testing (inviting small groups under private contracts), reduced public testing feedback, longer development cycles, and higher insurance costs. Alternatively, more software would be released as finished product with known defects rather than labeled as testing. The regime's absence would redistribute risk and change development economics substantially.
% FOUNDING_PROBLEM: Early software liability law treated all releases as finished products; defects triggered strict liability. This created a legal cliff: once software left a developer's control, liability became catastrophic, so developers minimized testing on real hardware/environments. Testing had to happen in closed labs, which produced inferior products. The founding problem was: how do you allow developers to test on real systems with real users while limiting legal exposure for expected defects during testing?
% FOUNDING_PROBLEM_CORROBORATION: Software development teams, venture capital investors, and major software vendors attest the problem remains live: closed testing is expensive and inferior, and liability exposure for testing releases remains high. Regulatory bodies and product safety organizations acknowledge the problem exists but contest the reading's solution—they argue the severity_carve_out reading (no beta for critical systems) is necessary because the testing frame cannot be policed reliably in practice. Independent software liability scholars document the testing/liability tension as a persistent structural problem.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.32 at interval end) because the liability suspension is genuine but bounded—it does not extend to gross negligence or willful misconduct, and it expires automatically. Theater rises early (0.08 → 0.22 by t=15) as the regime matures and developers become more skillful at maintaining plausible testing frames while releasing near-finished products; plateau at t=15+ reflects stabilization of enforcement (regulators have learned to police the bounds). Suppression is minimal (0.18) because users are informed and can exit, and the regime does not coerce non-participants into the testing frame—non-consenting downstream dependents retain full recourse. The measurement series show a regime that starts genuinely low-impact, gradually accumulates extractiveness as developers learn to work the boundaries, then stabilizes as regulatory enforcement tightens. Accessibility of alternatives is moderate (0.41): closed alpha testing is available but expensive; releasing as finished product is available but triggers full liability; the beta regime creates a middle path that is genuinely attractive to developers, so the alternatives are accessible but the beta path is preferred.
 *
 * PERSPECTIVAL GAP:
 *   From a developer's seat, the regime is a necessary temporary escape from impossible liability exposure—testing requires real users and real environments, and liability exposure prevents it. From a beta user's seat (informed, voluntary), the regime is a fair trade: early access and influence for accepting defect risk. From a non-consenting downstream dependent's seat (who did not volunteer), the regime is dangerous—they bear the defect risk without the benefit or the choice. From a regulatory seat, the regime is a constant tension: it solves a real problem but is persistently subject to abuse (indefinite testing, false testing frames, scope creep into safety-critical domains). The narrow_reading's entire structure is designed to manage this gap: explicit time bounds, regulatory oversight, exclusion of non-participants from the suspension, and automatic expiration. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers (beneficiary) have moderate power and constrained exit—they benefit from the liability reduction but cannot use it indefinitely (the sunset clause forces a choice: release or abandon). Beta users (payer, but secondary beneficiary) have low power and mobile exit—they bear the defect risk but voluntarily, with full knowledge, and can discontinue at any time. Non-consenting downstream dependents (excluded) have low power and trapped exit—they bear the risk without consent and without the ability to avoid it, which is why they are excluded from the liability suspension entirely. The reading's directionality structure is: developers move from target (full liability under finished-product law) to near-symmetric (reduced liability for testing, but bounded and time-limited); users move from non-existent (no role in closed testing) to payer-with-exit (voluntary, informed participation); non-participants are protected by their exclusion from the suspension. This is why the constraint is claimed as scaffold, not snare: the exit architecture is real and the expiration is automatic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is designed to resist mandatrophy: its founding problem (testing/liability tension) is live, the regime explicitly bounds its own duration (sunset clause), and regulatory authorities monitor for indefinite use as a liability escape. However, the constraint is vulnerable to gradual erosion of its time-bound character: as developers become more skilled at maintaining plausible testing frames, the boundary between 'genuine testing phase' and 'released product marketed as beta' blurs. Theater_ratio rising from 0.08 to 0.22 suggests this erosion is already occurring—the regime is performing its function (testing) but increasingly also performing the theater of testing (maintaining the legal fiction). If theater_ratio were to continue rising toward 0.5+, the constraint would be approaching piton status (a formal scaffold whose substance has atrophied into performance). The mandatrophy would be resolved by either (a) regulatory enforcement tightening (shortening allowed testing windows, requiring objective milestones to declare testing complete), or (b) adoption of the severity_carve_out reading (banning beta designation in safety-critical contexts, shifting those domains out of the scaffold into a different constraint structure entirely).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    good_faith_testing_frame_policeability,
    'Can regulators reliably distinguish between a genuine testing phase and a false testing frame (developer maintains beta status indefinitely to avoid full liability)?',
    'Empirical audit of enforcement patterns: what percentage of beta designations are eventually released as finished products vs. abandoned vs. maintained indefinitely? What fraction of enforcement actions involve developers misrepresenting testing phase duration?',
    'If policeability is low (regulators cannot distinguish reliably), the constraint converges toward snare (a false testing frame that persists indefinitely under regulatory capture). If high (enforcement successfully distinguishes and revokes suspension for false frames), the scaffold structure holds. This determines whether the narrow reading is viable or whether the severity_carve_out reading is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_testing_frame_policeability, empirical, 'Whether the good-faith testing bound is enforceably policed or becomes a stable cover story').

omega_variable(
    downstream_dependency_invisibility,
    'How often is beta software embedded in non-consenting downstream systems (e.g., beta library used by production financial system, beta component in medical device) such that the non-consenting-dependent exclusion fails in practice?',
    'Supply-chain audit: trace incidents where beta software caused harm to non-participants; measure frequency of such incidents relative to intentional beta testing participation.',
    'If downstream dependency is frequent and hidden, the constraint fails to protect non-participants even under the narrow reading—the reading requires developers to ensure beta designation does not propagate invisibly, but if this requirement is unenforceable, the reading converges toward snare (beta suspension applies to victims who never consented). This would motivate adoption of the severity_carve_out reading for critical supply chains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_dependency_invisibility, empirical, 'Whether the exclusion of non-consenting dependents is enforced in supply-chain practice').

omega_variable(
    testing_phase_objective_criteria,
    'Is there an objective definition of ''genuine testing phase'' that can ground the good-faith requirement, or is the distinction inherently contested?',
    'Jurisprudential analysis: do courts/regulators use consistent criteria (e.g., number of crashes per user-hour, version numbering, feature freeze deadline) or is testing-phase status determined ad-hoc by regulatory discretion?',
    'If objective criteria exist and are deployed, the scaffold''s time bound is enforceable and the narrow reading is stable. If testing-phase status is inherently discretionary, the reading depends on regulatory good faith, which makes the constraint vulnerable to regulatory capture (a regime that becomes snare for well-connected developers).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(testing_phase_objective_criteria, conceptual, 'Whether ''genuine testing phase'' has objective or discretionary boundaries').

omega_variable(
    expansive_vs_narrow_kernel_interpretation,
    'Does the kernel (beta designation''s role in software liability) admit of a genuine expansive reading, or is expansive_shield reading a false reading that misinterprets the kernel?',
    'Textual and historical analysis of the origins of beta designation in software practice; determination of whether the practice ever intended to operate as a blanket waiver or whether it was always intended as a bounded testing frame.',
    'If the kernel''s history supports expansive interpretation, the narrow and expansive readings genuinely coexist and the forecast is constraint convergence or regulatory settlement. If the kernel''s history supports only narrow interpretation, the expansive reading is a recent mis-reading (gaming the doctrine), and the forecast is the engine computing expansive as a false reading (not a live alternate reading but a fabrication).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansive_vs_narrow_kernel_interpretation, conceptual, 'Whether expansive_shield is an alternate reading or a false reading of the beta doctrine kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 25, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 25, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, product_liability_strict_liability_doctrine).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, regulatory_enforcement_consumer_protection_regimes).

% DUAL FORMULATION NOTE:
% The beta_designation_doctrine kernel has three structurally distinct constraint readings: narrow_warning_reading (this file, time-bounded testing with preserved base liability), expansive_shield_reading (comprehensive liability waiver, indefinite duration), and severity_carve_out_reading (beta categorically prohibited in critical systems). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and types. The narrow reading is claimed as scaffold; the expansive reading would be claimed as snare; the carve-out reading would be claimed as mountain (absolute prohibition). All three files link via network.affects_constraints to show their family relationship and kernel kinship. The contest is not about measuring the same constraint differently—it is about three different constraint instantiations of the same underlying kernel (the role of beta designation in managing software liability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
