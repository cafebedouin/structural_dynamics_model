% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)
 *   domain: legal/technology/consumer_protection
 *
 * SUMMARY:
 *   This constraint is the narrow-warning reading of the beta designation
 *   doctrine kernel. It frames beta designation as a time-bounded testing
 *   disclosure mechanism under which developers receive temporary reduced
 *   liability exposure in exchange for transparent disclosure that the
 *   software is in active testing. The reading preserves base product
 *   liability once testing concludes and enforces genuine testing-phase
 *   temporality: indefinite beta status is disallowed. This reading
 *   instantiates a temporary coordination scaffold that dissolves when the
 *   founding problem (need for real-world testing data) is resolved by
 *   product transition to release.
 *
 * KEY AGENTS:
 *   - Software developers: receive temporary liability shield during testing phase, lose shield at transition to release
 *   - Early-adopter users: gain early access to features and participate in testing, enter with full disclosure and costless exit
 *   - Courts and regulators: enforce temporal boundaries and adjudicate whether claimed testing phases are genuine
 *   - Injured users post-transition: retain full product liability claims after beta phase ends
 *   - Liability insurers: price coverage around defined testing-phase duration
 *   - Competing developers: excluded from the narrow reading's legal reasoning, advocate for broader shields
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.38).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "legal/technology/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'db3be875-ff5a-4baa-bd4c-cdfe0c90e51c').
narrative_ontology:cs_kernel_codification('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', formalized).
narrative_ontology:cs_authority_grounding('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', lineage).
narrative_ontology:cs_interpretation_layer_present('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c').
narrative_ontology:cs_reading_relation('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', foundational, testing_phase_temporality_required).
narrative_ontology:cs_axiom_status(testing_phase_temporality_required, holdable).
narrative_ontology:cs_axiom_grounding('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', testing_phase_temporality_required, instrumental).
narrative_ontology:cs_axiom('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', foundational, liability_restoration_at_release_transition).
narrative_ontology:cs_axiom_status(liability_restoration_at_release_transition, holdable).
narrative_ontology:cs_axiom_grounding('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', liability_restoration_at_release_transition, deontological).
narrative_ontology:cs_reference_frame('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', time_bounded_testing_disclosure).
narrative_ontology:cs_drift_state('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', contemporary_indefinite_beta_evasion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('db3be875-ff5a-4baa-bd4c-cdfe0c90e51c', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers_during_testing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, injured_users_post_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Release software marked as beta, which under this reading permits reduced liability exposure during a genuine testing phase. Developers receive a temporary shield that expires when the testing phase concludes and the product transitions to release. The shield requires good-faith disclosure that the software is in testing, setting clear user expectations.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Gain access to new software features before general release and participate in shaping the product's development. They receive transparent disclosure of testing status and understand that known issues may exist. They are not victimized because they enter with open eyes and can exit costlessly.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_users, beneficiary,
    moderate, immediate, mobile, global).

% Enforce the temporal boundary: beta designation is valid only for genuine testing phases with defined endpoints. They adjudicate whether a product released as beta for years without progress toward completion is abusing the designation. The courts preserve base product liability once testing concludes or the duration claim becomes unreasonable.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% If harmed by defects after a product has transitioned from beta to release, they retain full product liability claims against the developer. Under this reading, the beta shield does not extend past the testing phase; once released, base liability is restored. They bear the injury cost if the developer defaults or the harm exceeds damages available.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, injured_users_post_transition, payer,
    powerless, immediate, constrained, global).

% Price product liability coverage for developers, accounting for the time-bounded nature of the beta shield. Under this reading, liability resumes at a known transition point, making risk assessment tractable. They can distinguish genuine testing phases from indefinite evasion of liability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, liability_insurers, observer,
    organized, biographical, constrained, global).

% Would benefit from expanded liability shields that extend past testing, but are excluded from the conversation when courts enforce the narrow temporal bound. Their interest in broader evasion mechanisms is not represented in the legal reasoning that grounds the narrow reading.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, competing_developers, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a time-bounded mechanism for software developers to conduct testing in production, where real-world use reveals defects faster than closed-lab testing. Users gain early access to features in exchange for accepting known risks during an explicit testing phase. The coordination solves the real problem: how to gather diverse use-case data without exposing the developer to full liability for every edge case not yet discovered.
% TRANSFER_FUNCTION: Temporarily transfers some liability exposure from developers to early-adopter users during the testing phase only. Once testing concludes (a defined transition), base product liability reverts to the developer. The transfer is bounded both in scope (testing-phase defects only) and duration (the testing phase itself).
% ABSENT_VOICES: Developers who would advocate for indefinite liability shields are not present in the narrow reading's legal reasoning — their interest in perpetual beta status is structurally excluded. Users harmed by products released as beta years without progress toward completion are not present to testify about the harm of indefinite evasion.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, developers would lose the ability to conduct time-bounded, transparent testing phases in production. They would either conduct testing only in closed labs (slowing feature discovery) or face full liability from the first user (pricing testing conduct as uninsurable risk). Users would lose early-access opportunities to shape product development. The market for beta testing as a coordination mechanism would collapse.
% FOUNDING_PROBLEM: Software development requires real-world testing to identify defects across diverse hardware, use cases, and configurations that lab testing cannot fully capture. Early-stage testing with users generates better data than isolated development. However, exposing untested software to full liability under product liability law makes testing conduct economically irrational for developers.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering practice, published studies on defect discovery rates in alpha/beta testing, and testimony from developers during product liability legislative hearings corroborate that real-world testing is necessary and that indefinite full liability exposure would suppress it. Courts and regulatory bodies (FTC guidance, state consumer protection statutes) have acknowledged the testing-phase problem independently of developer claims.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low (0.25 at t=0) because the scaffold's primary function is genuine coordination — enabling real-world testing that benefits users and developers. It rises modestly to 0.32-0.38 as disputes emerge over transition timing and some developers attempt to extend beta status beyond reasonable testing durations (the core contestation of the narrow reading). Theater rises early (0.08 to 0.17 by t=12) as courts develop tests for genuine-testing-phase determination; some developers perform testing bureaucracy without substantive testing progress, triggering increased judicial scrutiny. Theater plateaus at 0.18 by t=24 as the constraint matures and developers learn which transition claims courts will accept. Suppression requirement remains low (0.15 to 0.22) because the constraint is enforced through judicial review of transition claims, not by prior restraint on beta designation itself; developers retain choice to claim beta status, but courts audit the claim. The temporal measurements share one grid: every metric is authored at every time point [0, 4, 8, 12, 16, 20, 24].
 *
 * PERSPECTIVAL GAP:
 *   The developer seat and the injured-user-post-transition seat should compute differently under the engine. From the developer's position, beta is a legitimate temporary shield justified by testing-phase coordination — they benefit from the reduced liability exposure and exit cleanly by transitioning to release. From the injured user's position (harm occurring after release), the same constraint operates as a transition trigger that restores full liability, which is proper under the narrow reading's logic but inadequate if the developer conceals the transition or keeps the product perpetually beta. The engine computes this divergence from the stakeholder power (powerful developer vs. powerless user) and exit options (mobile developer vs. constrained injured user); the authored claim stays independent of these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are structural beneficiaries (d near 0.0-0.3): they receive temporary liability reduction during the testing phase in exchange for transparent disclosure. The reduction is temporary and conditioned on genuine testing progress, so it is not a full beneficiary position, but the benefit is real and measurable. Early-adopter users are symmetric (d near 0.5): they receive early access to features and participate in shaping the product, but they also accept testing-phase risks. Once the product transitions to release, injured users become targets (d near 1.0): they bear the injury cost if the developer defaults or the harm exceeds damages available. Courts are agenda-setters (d near 0.5): they enforce the temporal boundary and preserve liability restoration, benefiting users post-transition while constraining developers from indefinite evasion. Competing developers and liability insurers have different structural relationships (organized and constrained, respectively) that differentiate them from the agenda-setter role.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because it is explicitly scaffolded with a sunset clause: the temporary liability shield expires when the testing phase concludes. The founding problem (need for real-world testing) remains live, but the constraint is designed to dissolve once the product transitions to release. Unlike a piton (where function has atrophied but performance continues for institutional inertia), the narrow-warning reading treats the sunset as a feature, not a bug. Courts actively enforce the temporal boundary, so the constraint does not persist as theater after its coordination function ends. The measurement series show theater increasing to 0.17-0.20 during the interval, which reflects growing disputes over when the testing phase has ended and the sunset is triggered—this is enforcement activity, not theatrical performance of a vestigial function. The constraint avoids mandatrophy precisely by making the transition dispute explicit and justiciable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_phase_boundaries,
    'What constitutes a genuine testing phase versus indefinite beta evasion? How should courts define ''reasonable testing duration''?',
    'Accumulated case law establishing benchmarks: version-increment frequency, bug-count reduction rate, publicly documented roadmap progress, independent audits of testing-phase claims. Regulatory guidance (FTC, state AGs) establishing safe harbors for testing duration by software category.',
    'If courts develop tight boundaries (6-month max, 50% bug-count reduction required), the constraint becomes more protective of users and reduces developer extractiveness. If boundaries are loose, the constraint becomes a vehicle for indefinite evasion and extractiveness rises. The narrow reading''s enforceability depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_boundaries, conceptual, 'Whether genuine testing-phase temporality is a judicially enforceable standard or a soft disclosure requirement.').

omega_variable(
    liability_preservation_scope,
    'Does base product liability fully restore at product transition to release, or are there residual carve-outs for known-at-transition defects?',
    'Legislative clarification or appeals-court ruling establishing whether defects identified during beta but unfixed at release trigger liability under the narrow reading, or whether developers can claim the narrow reading''s intent was time-bounded partial immunity.',
    'If liability fully restores, users post-transition have strong claims. If carve-outs exist, the shield extends past formal transition and becomes more extractive. The narrow reading''s theoretical commitment to liability preservation is tested by this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_preservation_scope, empirical, 'Scope of product liability restoration at product transition.').

omega_variable(
    narrow_vs_expansive_kernel_foreclosure,
    'Does the narrow reading''s core premise (time-bounded testing disclosure) logically foreclose the expansive reading''s core premise (indefinite liability waiver), or do the readings coexist as live policy positions?',
    'Jurisprudential analysis: a court that adopts the narrow reading''s reasoning cannot simultaneously uphold indefinite beta evasion within the same case, so the relation is locally foreclosing. But different jurisdictions can hold the readings simultaneously, so the relation is globally coexisting. Determine whether to classify the relation as forecloses or coexists_with based on whether foreclosure is decided within a single decision-maker''s framework.',
    'If forecloses: the narrow reading structurally eliminates the expansive reading''s claim to legitimacy within principled legal reasoning. If coexists_with: both readings remain live policy options in a pluralistic system, and the contest is empirical (which reading delivers better outcomes) not logical. This omega determines the cs_structure.reading_relations field value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_vs_expansive_kernel_foreclosure, conceptual, 'Logical relationship between narrow and expansive readings of the beta designation kernel.').

omega_variable(
    users_informed_not_victimized,
    'Does transparent disclosure of testing status + costless exit eliminate victim status for early-adopter users, or is there residual victimization from information asymmetry about latent defects?',
    'Behavioral research on user comprehension of beta disclosures; empirical study of actual user switching costs when beta products harm data or workflow; analysis of defects identified post-hoc that developers concealed during testing phase.',
    'If disclosure eliminates victim status, early-adopter users are genuine beneficiaries and the constraint is low-extraction coordination. If information asymmetry creates latent victimization, the constraint''s extraction is higher and users are partly victimized despite nominal disclosure. This affects the base_properties.victims array and the directionality computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(users_informed_not_victimized, empirical, 'Whether informed consent via beta disclosure fully protects user interests or conceals residual victimization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 4, 0.17).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, product_liability_doctrine__strict_liability_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, software_warranty_exclusion__implied_merchantability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested beta_designation_doctrine kernel. The expansive_shield_reading permits indefinite liability waiver; the severity_carve_out_reading prohibits beta designation for critical systems. The narrow_warning_reading enforces time-bounded testing phases with liability restoration at product transition. Each reading instantiates a structurally distinct constraint with different ε values and stakeholder victim sets. The network edge indicates the reading influences (not forecloses) the sibling readings by establishing temporal boundaries that constrain their scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
