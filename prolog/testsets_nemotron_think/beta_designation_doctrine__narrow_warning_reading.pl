% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The beta_designation_doctrine kernel governs how 'beta' labeling affects
 *   software liability. This constraint story instantiates the
 *   narrow_warning_reading: beta designation is a time-bounded testing
 *   disclosure regime that preserves base product liability and requires the
 *   beta period to be a genuine testing phase. Under this reading, the
 *   constraint operates as a scaffold — a temporary coordination mechanism
 *   allowing developers to test with informed users while preserving users'
 *   full legal rights. The liability limitation (if any) expires
 *   automatically when the testing phase ends. This reading stands in
 *   structural opposition to the expansive_shield_reading (which treats beta
 *   as comprehensive liability waiver of indefinite duration) and coexists
 *   with the severity_carve_out_reading (which categorically bars beta for
 *   critical systems).
 *
 * KEY AGENTS:
 *   - software_developers: Primary beneficiary (moderate/organized power, constrained exit) — gains testing period with disclosure obligations
 *   - beta_users: Secondary beneficiary (organized/moderate power, constrained exit) — receives early access with full liability rights preserved
 *   - courts_regulators: Agenda setter (institutional power, analytical exit) — enforces time-bounds and good-faith requirements
 *   - expansive_shield_proponents: Excluded (powerful/institutional power, trapped exit) — would extend beta shield indefinitely
 *   - critical_system_operators: Observer (institutional power, analytical exit) — subject to severity carve-out regardless
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.18).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.15).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '2da77ae5-f79f-4dbb-b908-8380e56b2de9').
narrative_ontology:cs_kernel_codification('2da77ae5-f79f-4dbb-b908-8380e56b2de9', distributed).
narrative_ontology:cs_authority_grounding('2da77ae5-f79f-4dbb-b908-8380e56b2de9', practice).
narrative_ontology:cs_interpretation_layer_present('2da77ae5-f79f-4dbb-b908-8380e56b2de9').
narrative_ontology:cs_reading_relation('2da77ae5-f79f-4dbb-b908-8380e56b2de9', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('2da77ae5-f79f-4dbb-b908-8380e56b2de9', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('2da77ae5-f79f-4dbb-b908-8380e56b2de9', foundational, beta_designation_preserves_base_liability).
narrative_ontology:cs_axiom_status(beta_designation_preserves_base_liability, holdable).
narrative_ontology:cs_axiom_grounding('2da77ae5-f79f-4dbb-b908-8380e56b2de9', beta_designation_preserves_base_liability, deontological).
narrative_ontology:cs_axiom('2da77ae5-f79f-4dbb-b908-8380e56b2de9', foundational, beta_phase_must_be_genuine_testing).
narrative_ontology:cs_axiom_status(beta_phase_must_be_genuine_testing, holdable).
narrative_ontology:cs_axiom_grounding('2da77ae5-f79f-4dbb-b908-8380e56b2de9', beta_phase_must_be_genuine_testing, empirically_contingent).
narrative_ontology:cs_reference_frame('2da77ae5-f79f-4dbb-b908-8380e56b2de9', time_bounded_testing_disclosure_framework).
narrative_ontology:cs_drift_state('2da77ae5-f79f-4dbb-b908-8380e56b2de9', contemporary_continuous_deployment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2da77ae5-f79f-4dbb-b908-8380e56b2de9', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, consumer_product_liability_preservation).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, good_faith_testing_disclosure_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and release software in beta to gather real-world usage data, feedback, and bug reports. Must provide clear disclosure that the product is in testing, not feature-complete, and that full liability rights apply. Gains structured testing period but cannot use beta label indefinitely or for monetized production use. Exit means moving to general release (losing beta protections) or abandoning the product.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, beneficiary,
    organized, biographical, constrained, global).

% Opt into using pre-release software with full knowledge it is in testing. Retain all standard product liability rights — can sue for defects, data loss, or harm under existing consumer protection law. Benefit from early access and ability to shape the product. Exit means stopping use and potentially migrating data; constrained by switching costs and lack of alternatives for novel software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_users, beneficiary,
    moderate, biographical, constrained, global).

% Adjudicate whether a given 'beta' designation meets the narrow_warning_reading criteria: genuine testing phase, time-bounded, good-faith disclosure, preserved liability. Enforce sunset by rejecting beta defenses for products that have exited genuine testing. Set precedent on what constitutes 'genuine testing' vs. indefinite beta. Their enforcement capacity determines whether the scaffold's sunset holds.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Large platform companies and industry groups advocating that beta designation should confer broad liability immunity regardless of duration or testing genuineness. Their preferred reading (expansive_shield_reading) is structurally foreclosed by this constraint's premises. They are excluded from the narrow_warning_reading's coordination function because their model requires suppressing user liability rights — the very thing this reading preserves.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, expansive_shield_proponents, excluded,
    powerful, biographical, trapped, global).

% Operators of life-safety, financial, medical, and infrastructure systems. Subject to the severity_carve_out_reading which categorically bars beta designation regardless of testing status. They observe this constraint but are governed by a different reading of the same kernel. Their situation is analytically relevant for boundary disputes: when does a system become 'critical' enough for the carve-out?
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, critical_system_operators, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables structured, time-bounded software testing with real users by providing a recognized 'beta' designation that signals testing status while preserving all existing liability rights — solving the coordination problem of how developers can safely test in production without users losing legal protections.
% TRANSFER_FUNCTION: Moves no value from users to developers. The arrangement transfers information (testing status disclosure) from developers to users, and transfers temporary forbearance from strict release standards from regulators to developers — conditioned on genuine testing and automatic sunset.
% ABSENT_VOICES: End-users who lack technical literacy to understand 'beta' implications; small developers without legal resources to navigate disclosure requirements; jurisdictions without developed product liability frameworks where beta designation may be the only consumer protection signal.
% DISAPPEARANCE_RATIONALE: If the narrow_warning_reading vanished, developers would either lose the structured testing safe harbor (reverting to full liability during testing, chilling innovation) or courts would default to the expansive_shield_reading (stripping user liability rights). The mobile app ecosystem, SaaS development practices, and consumer protection expectations would reorganize around whichever reading fills the vacuum.
% FOUNDING_PROBLEM: Early personal computing and internet software had no standard way to distinguish testing releases from production releases, leading to either chilled innovation (developers afraid to release early) or user harm (users treating beta as production with no recourse). The beta designation doctrine emerged to create a middle ground: structured testing with informed consent and preserved rights.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering literature on continuous deployment and beta testing practices (independent of vendor advocacy) corroborates that structured testing phases remain necessary. Consumer protection agencies (FTC, EU Commission) have affirmed that beta labeling does not waive liability. Courts in multiple jurisdictions have rejected indefinite beta shields. No corroboration comes solely from developer beneficiaries.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint primarily coordinates a testing period rather than extracting value; the preserved liability means no transfer from users to developers. Suppression is low (0.15) because alternatives exist (wait for general release, use competing products) and the constraint does not block exits. Theater ratio is low (0.12) because the testing disclosure function is genuine and the sunset is enforced by courts. Accessibility collapse is low (0.25) because users and developers retain full contractual and tort alternatives outside the beta frame. Resistance is low (0.15) because this reading aligns with established product liability principles. The scaffold classification fits: temporary support for software testing with explicit sunset (end of genuine testing phase), coordination function (structured testing with disclosure), and no asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the developer seat, the constraint looks like a rope (pure coordination: testing disclosure for mutual benefit). From the expansive_shield_proponent seat, it looks like a mountain (immutable liability rule they cannot overcome). From the beta_user seat, it looks like a mountain (preserved liability rights that feel like natural law). The engine computes these per-seat divergences from the structural data; this reading's claim of scaffold reflects the authoring seat's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are beneficiaries (d ~ 0.2) — they gain a structured testing period but bear disclosure costs and lose the shield when testing ends. Beta users are beneficiaries (d ~ 0.1) — they get early access with full liability rights intact; the constraint subsidizes their position. Courts/regulators are agenda_setters (d ~ 0.5 symmetric) — they administer the time-bounds. Expansive shield proponents are excluded — their preferred reading is foreclosed by this constraint's premises. The directionality derivation from beneficiary declarations + exit options yields low d for both developer and user seats, consistent with scaffold coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (safe structured testing for evolving software) remains live — software complexity and release velocity have increased, making testing phases more necessary, not less. The arrangement has not outlived its function. Mandatrophy is not resolved; the scaffold remains functional. The sunset clause (end of genuine testing) prevents drift into piton by forcing re-evaluation when testing concludes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the narrow_warning_reading instantiate a distinct constraint from the expansive_shield_reading and severity_carve_out_reading of the beta_designation_doctrine kernel?',
    'Structural decomposition: if the three readings produce different ε values, different beneficiary/victim structures, and different constraint types, they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own constraint story with independent classification; the kernel is a family, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame commitment: this story is one reading of a contested kernel; the other readings are separate constraints.').

omega_variable(
    genuine_testing_phase_boundary,
    'What constitutes a ''genuine testing phase'' versus indefinite beta used as liability shield?',
    'Case law analysis of duration, feature completeness, user base size, and monetization during beta; regulatory guidance on testing phase termination criteria.',
    'If boundary is unenforceable, the scaffold''s sunset clause becomes nominal and the constraint drifts toward piton or snare; if enforceable, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_boundary, empirical, 'Operationalizing the sunset condition that makes this a scaffold rather than an indefinite waiver.').

omega_variable(
    user_informed_consent_effectiveness,
    'Does ''users informed'' in practice mean meaningful consent or click-through theater?',
    'Empirical study of beta user comprehension, disclosure prominence, and ability to decline participation without losing access to essential services.',
    'If consent is theater, beta_users shift from beneficiary to payer (victim), raising extractiveness and potentially reclassifying to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_informed_consent_effectiveness, empirical, 'Whether the coordination function''s informed-consent premise holds structurally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_narrow_warning_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_narrow_warning_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(beta_narrow_warning_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(beta_narrow_warning_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(beta_narrow_warning_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(beta_narrow_warning_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(beta_narrow_warning_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(beta_narrow_warning_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(beta_narrow_warning_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(beta_narrow_warning_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(beta_narrow_warning_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(beta_narrow_warning_su_t5, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(beta_narrow_warning_su_t10, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(beta_narrow_warning_su_t15, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(beta_narrow_warning_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.02).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the beta_designation_doctrine kernel into three structurally distinct readings with different ε values, beneficiary/victim structures, and constraint types. The narrow_warning_reading (this story) has low ε (0.18), scaffold type, preserved liability. The expansive_shield_reading has high ε (~0.7), snare/tangled_rope type, liability waiver. The severity_carve_out_reading has near-zero ε for critical systems (mountain/rope), categorical bar. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
