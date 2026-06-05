% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Beta Designation Doctrine (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The beta designation doctrine is a contested kernel in technology law:
 *   how should product liability doctrine treat software products explicitly
 *   labeled as pre-release? This constraint models ONE reading of that kernel
 *   — the narrow warning reading — which holds that beta designation may
 *   provide a temporary liability shield only during a genuine, time-bounded
 *   testing phase with clear disclosure, and that shield must expire when the
 *   testing phase ends and the product reaches actual maturity. This reading
 *   coordinates legitimate development practices (getting structured feedback
 *   from early users) while extracting from beta testers (who bear testing
 *   costs) and from developers (who must actually use the testing phase, not
 *   claim it indefinitely). The kernel itself remains contested across
 *   different legal jurisdictions and institutional actors: the expansive
 *   shield reading (used by some technology firms) treats beta as a perpetual
 *   liability exemption; the severity carve-out reading (used by regulators)
 *   permits beta designation only for non-critical systems. Each reading
 *   instantiates a different constraint with different ε values,
 *   beneficiary/victim structures, and terminal classifications. This story
 *   instantiates the narrow reading alone.
 *
 * KEY AGENTS:
 *   - Software Developer: Primary beneficiary (institutional/arbitrage) — benefits from structured feedback mechanism and temporary liability deferral; constrained by requirement to actually conduct testing
 *   - Beta User (Informed): Secondary beneficiary and victim (moderate/constrained) — benefits from early access and influence over roadmap; bears testing costs (crashes, data loss, workflow disruption)
 *   - Beta User (Uninformed): Primary victim (powerless/trapped) — encounters beta-labeled software through download platforms or search results; may not recognize risk implications; trapped by network effects or workflow dependency
 *   - Regulators & Consumer Protection Bodies: Organized enforcer (organized/constrained) — responsible for enforcing the sunset clause and preventing expansion of shield; extract compliance costs from developers
 *   - Open Source Communities: Alternative coordination model (powerful/mobile) — see beta designation as temporary structure that becomes unnecessary with mature CI/CD practices
 *   - Enterprise Licensees: Institutional customer (institutional/arbitrage) — may maintain beta designation in legacy contracts long after testing phase ends; extract ongoing negotiating leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.38).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.42).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'beta-narrow-warning-reading-2026-02-26').
narrative_ontology:cs_kernel_codification('beta-narrow-warning-reading-2026-02-26', formalized).
narrative_ontology:cs_authority_grounding('beta-narrow-warning-reading-2026-02-26', lineage).
narrative_ontology:cs_interpretation_layer_present('beta-narrow-warning-reading-2026-02-26').
narrative_ontology:cs_reading_relation('beta-narrow-warning-reading-2026-02-26', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('beta-narrow-warning-reading-2026-02-26', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('beta-narrow-warning-reading-2026-02-26', foundational, liability_shield_duration_tracks_testing_phase).
narrative_ontology:cs_axiom_status(liability_shield_duration_tracks_testing_phase, holdable).
narrative_ontology:cs_axiom_grounding('beta-narrow-warning-reading-2026-02-26', liability_shield_duration_tracks_testing_phase, deontological).
narrative_ontology:cs_axiom('beta-narrow-warning-reading-2026-02-26', foundational, genuine_testing_activity_required_to_claim_beta).
narrative_ontology:cs_axiom_status(genuine_testing_activity_required_to_claim_beta, holdable).
narrative_ontology:cs_axiom_grounding('beta-narrow-warning-reading-2026-02-26', genuine_testing_activity_required_to_claim_beta, empirically_contingent).
narrative_ontology:cs_reference_frame('beta-narrow-warning-reading-2026-02-26', good_faith_testing_regime_bounded_in_time).
narrative_ontology:cs_drift_state('beta-narrow-warning-reading-2026-02-26', post_mobile_app_era_legacy_enterprise_software, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('beta-narrow-warning-reading-2026-02-26', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developer).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, testing_regime).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_user_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BETA USER — UNINFORMED (SNARE) — Users who download software carrying a minimal beta warning face full product liability risk without genuine consent or capacity to assess that risk. They cannot exit the product once encountered in their workflow; cannot negotiate terms; cannot obtain alternative stable versions easily. Maximum extraction against a powerless agent with no alternatives.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMED BETA TESTER (TANGLED ROPE) — Users who explicitly enroll in beta testing programs with clear disclosure of risks and testing timeline benefit from early access and influence over development roadmap, but bear genuine testing costs (data loss, crashes, workflow disruption). The constraint coordinates testing feedback collection while extracting from the tester through unpaid labor and risk absorption. This is the narrow reading's intended perspective.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOFTWARE DEVELOPER (ROPE) — From the developer's perspective, genuine beta designation serves as a coordination mechanism: it signals the product's maturity level, solicits structured feedback, and defers perfection requirements during development. The narrow reading requires developers to actually use the testing phase for development, not merely as liability shield. Net beneficiary but not primarily through extraction.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORS (TANGLED ROPE) — Organized agents enforcing consumer protection standards see beta designation as requiring active oversight to prevent abuse. The constraint coordinates genuine testing phase boundaries (sunset clause) while extracting from developers through compliance labor. Enforcement is necessary to prevent the narrow reading from degrading into the expansive shield reading.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN SOURCE COMMUNITIES (SCAFFOLD) — Communities using transparent development cycles (rolling releases with clear version stability tiers) see the narrow beta reading as a temporary coordination structure that becomes unnecessary as development practices mature. Sunset logic applies: as CI/CD and semantic versioning mature, explicit beta phases degrade toward ritual.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ENTERPRISE LICENSING (PITON) — Large organizations with SLAs and commercial support contracts maintain beta designation as a ritualized hedge against liability even when products are functionally mature. The beta label persists through institutional inertia and contractual copy-paste, not actual testing needs. Theater ratio is high because the constraint persists after its functional purpose has expired.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — A purely analytical view might naturalize beta designation as an immutable feature of how software development works: the gap between conception and stable release is inherent to complex systems. However, this naturalizes what is actually a contingent legal construct designed to manage that gap. Engine false summit detection applies: the constraint is not naturally inevitable but institutionally constructed.
constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beta_designation_doctrine__narrow_warning_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, TR),
    TR >= 0.70.

:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The narrow reading coordinates legitimate testing while imposing asymmetric costs on beta users (unpaid labor, risk absorption) and compliance labor on developers (enforcement of sunset clause, granular disclosure requirements). The value reflects genuine testing phase coordination (lower extraction) combined with asymmetric risk allocation (moderate extraction). If the reading successfully constrains the developer's ability to claim indefinite beta status, extraction remains moderate. If enforcement fails and developers exploit the ambiguity, the constraint degrades toward higher extractiveness. Suppression (0.42): Moderate. Barriers to beta users exiting the constraint include network effects (the software is already embedded in their workflow), information asymmetries (they may not understand beta label implications), and limited alternatives (competing products may not exist or may require switching costs). But suppression is not total — users can technically switch to stable versions or alternative products; the barrier is cost, not impossibility. Theater ratio (0.55): Moderate. Beta designation includes genuine functional elements (testing-phase coordination, feedback collection) but also performative elements (generic warnings that do not specify actual risks, sustained beta labels on functionally mature products). The narrow reading requires theater to remain low by enforcing meaningful distinction between beta and stable status.
 *
 * PERSPECTIVAL GAP:
 *   The null gap between narrow and expansive readings is the crux of the kernel contestation. Both readings use the same label ('beta designation') but instantiate fundamentally different constraints. The narrow reading's core premise is that liability shield duration must track genuine testing phase duration. The expansive shield reading's core premise is that any ongoing development activity justifies indefinite shield. These readings do not describe the same constraint — they describe different structural realities with different ε values, beneficiary/victim sets, and terminal types. The narrow reading is the one modeled here; the others are separate constraint stories with their own ε values and perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from their structural position relative to the constraint: informed beta testers have constrained exit (switching costs) and mixed beneficiary/victim status (benefits from early access, bears testing costs) — moderate d. Uninformed users have trapped exit and pure victim status — high d toward full target. Developers have arbitrage exit and beneficiary status (coordinate testing, benefit from feedback) — low d toward full beneficiary. Regulators have constrained exit and mixed status (organize enforcement, extract compliance costs) — moderate d. The engine derives d automatically from these structural facts; directionality overrides are not required for this story because the structural relationships are clear.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_phase_threshold,
    'What objective criteria distinguish a genuine testing phase from a liability shield masquerading as beta?',
    'Comparison of beta-labeled products: historical frequency of breaking changes, severity of discovered bugs, frequency of security patches, correlation between beta duration and feature-completeness metrics. Empirical classification of actual vs nominal testing phases.',
    'If threshold is clear and enforceable: narrow reading is structurally stable and the tangled_rope classification holds. If threshold is ambiguous or subjective: developers can claim beta indefinitely, reading degrades toward expansive_shield_reading and classification shifts to Snare for users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_threshold, empirical, 'Objective boundary between genuine testing phase and prolonged liability shield').

omega_variable(
    user_consent_granularity,
    'Does a generic ''beta software'' warning constitute genuine informed consent for the range of failures users might encounter, or is more granular disclosure of known limitations required?',
    'Correlation between warning specificity and user understanding; surveys of beta users about their comprehension of actual risks; analysis of dispute resolution data (complaints, support tickets, litigation) showing whether users encountered failures they claimed to not have foreseen.',
    'If generic warning suffices: current liability structure holds and constraint remains Tangled Rope. If granular disclosure required: developers face higher enforcement costs and the constraint shifts toward Snare for users who lack that granular disclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_consent_granularity, empirical, 'Whether generic beta warning constitutes adequate informed consent').

omega_variable(
    reading_boundary_contestation,
    'Does the narrow reading (time-bounded testing phase with genuine development activity) foreclose the expansive shield reading (liability exemption valid indefinitely if any development activity is claimed)?',
    'Case law and regulatory guidance evolution. Examine whether courts/regulators treat the readings as logically contradictory or as coexisting positions held by different institutional actors (developers vs consumer advocates).',
    'If foreclosure holds: only one reading can be law within a given jurisdiction. If coexistence holds: readings remain in active dispute and the constraint''s classification varies by jurisdiction and institutional context. This resolution determines reading_relations: forecloses vs coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_contestation, conceptual, 'Whether narrow reading logically forecloses expansive shield reading').

omega_variable(
    sunset_clause_enforceability,
    'Can regulatory bodies effectively enforce an actual testing-phase endpoint, or does developer discretion over ''maturity status'' render the sunset clause performative?',
    'Analysis of products that transitioned from beta: frequency of explicit graduation statements, delay patterns, comparison of beta-labeled legacy products vs newly released products. Examination of enforcement actions (consent decrees, settlements) requiring developers to actually end beta designation.',
    'If sunset enforcement is strong: scaffold classification is real and the narrow reading''s Tangled Rope is sustainable. If enforcement is weak: sunset clause is theater and the constraint degrades toward Piton (degraded regulation) or toward unconstrained Snare (if warning is minimal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Actual enforceability of sunset clause on testing-phase endpoint').

omega_variable(
    narrow_vs_expansive_core_premise_contradiction,
    'Is the core normative premise of the narrow reading (liability shield may apply only during genuine, time-bounded testing phase) logically incompatible with the core premise of the expansive shield reading (liability shield remains valid whenever any development activity continues)?',
    'Logical analysis of commitment structures. Can a single legal framework hold both: ''shield expires when testing ends'' AND ''shield remains active as long as development continues''? These appear contradictory if ''development'' can be defined to encompass indefinite minor updates. Examine whether they coexist as different interpretations of ambiguous statutory language or whether one forecloses the other.',
    'If contradiction is genuine: readings foreclose each other and only one can govern a given jurisdiction (forecloses relation). If statements can coexist by parsing ''genuine testing'' narrowly and ''continued development'' broadly: readings coexist despite tension (coexists_with relation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_vs_expansive_core_premise_contradiction, conceptual, 'Logical compatibility of narrow vs expansive reading axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_narrow_theater_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(beta_narrow_theater_t2, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(beta_narrow_theater_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(beta_narrow_extract_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(beta_narrow_extract_t2, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(beta_narrow_extract_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(beta_narrow_suppress_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(beta_narrow_suppress_t2, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(beta_narrow_suppress_t5, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, attachment_coordination).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% The beta designation kernel decomposes into three distinct constraints, each with different ε values and beneficiary/victim structures. The narrow_warning_reading (this story, ε=0.38, Tangled Rope) coordinates testing while extracting from users. The expansive_shield_reading would have higher ε (≥0.60) and classify as Snare from the user perspective because developers use the label indefinitely without genuine testing. The severity_carve_out_reading would have lower ε (≤0.25) and classify as Rope because it restricts shield to non-critical systems, reducing extraction against users. All three stories share kernel_id but instantiate different readings with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__narrow_warning_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
