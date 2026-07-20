% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Control Regime â Freedom Imperative Reading
 *   domain: software engineering/political economy/intellectual property
 *
 * SUMMARY:
 *   This constraint story models the proprietary software control
 *   regimeâlicensing, copyright enforcement, DRM, and distribution
 *   restrictionsâthrough the freedom imperative reading of the
 *   software_control_legitimacy kernel. From this reading, all proprietary
 *   software is ethically illegitimate because it denies users control over
 *   their computing. The reading treats user freedom as a fundamental right
 *   and proprietary control as a snare that extracts autonomy from users to
 *   benefit vendors. This is one reading of a contested kernel; siblings
 *   include the pragmatic openness reading, the property rights reading, and
 *   the commons reading. The authored metrics are descriptively honest for
 *   the regime's operation, while the claimed_type reflects this reading's
 *   structural assessment.
 *
 * KEY AGENTS:
 *   - Proprietary vendors (institutional/agenda_setter): set licensing terms, enforce restrictions, and collect revenue globally.
 *   - End users (powerless/payer): bear the loss of the four freedoms through opaque binaries and restrictive terms.
 *   - Software developers (moderate/payer): constrained by proprietary APIs and licensing that prevent inspection and redistribution.
 *   - Free software advocates (organized/observer): resist the regime and provide alternative infrastructure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.8).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Control Regime â Freedom Imperative Reading").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software engineering/political economy/intellectual property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '43fd8658-8a89-48bd-aaf2-47735cf0b97e').
narrative_ontology:cs_kernel_codification('43fd8658-8a89-48bd-aaf2-47735cf0b97e', fixed_text).
narrative_ontology:cs_authority_grounding('43fd8658-8a89-48bd-aaf2-47735cf0b97e', lineage).
narrative_ontology:cs_interpretation_layer_present('43fd8658-8a89-48bd-aaf2-47735cf0b97e').
narrative_ontology:cs_reading_relation('43fd8658-8a89-48bd-aaf2-47735cf0b97e', software_control_legitimacy__pragmatic_openness_reading, forecloses).
narrative_ontology:cs_reading_relation('43fd8658-8a89-48bd-aaf2-47735cf0b97e', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('43fd8658-8a89-48bd-aaf2-47735cf0b97e', software_control_legitimacy__commons_reading, forecloses).
narrative_ontology:cs_axiom('43fd8658-8a89-48bd-aaf2-47735cf0b97e', foundational, user_control_as_moral_imperative).
narrative_ontology:cs_axiom_status(user_control_as_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('43fd8658-8a89-48bd-aaf2-47735cf0b97e', user_control_as_moral_imperative, deontological).
narrative_ontology:cs_axiom('43fd8658-8a89-48bd-aaf2-47735cf0b97e', foundational, proprietary_restriction_always_harmful).
narrative_ontology:cs_axiom_status(proprietary_restriction_always_harmful, holdable).
narrative_ontology:cs_axiom_grounding('43fd8658-8a89-48bd-aaf2-47735cf0b97e', proprietary_restriction_always_harmful, deontological).
narrative_ontology:cs_reference_frame('43fd8658-8a89-48bd-aaf2-47735cf0b97e', user_sovereign_control).
narrative_ontology:cs_drift_state('43fd8658-8a89-48bd-aaf2-47735cf0b97e', contemporary_cloud_computing_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('43fd8658-8a89-48bd-aaf2-47735cf0b97e', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of software licensing, control distribution channels, deploy technical restrictions such as DRM and remote attestation, and enforce copyright through legal instruments. Collect revenue from licenses, subscriptions, and platform fees. They determine what users may run, inspect, modify, and share on computing devices they administer.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive software only as opaque binary executables subject to end-user license agreements that prohibit reverse engineering, redistribution, and modification. Subject to remote revocation, telemetry, and format lock-in. Migration to free alternatives requires abandoning data, relearning interfaces, and replacing hardware or peripherals with proprietary drivers.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, end_users, payer,
    powerless, biographical, constrained, global).

% Build applications atop proprietary operating systems, libraries, and cloud APIs whose source code is withheld. License terms restrict how derivative works may be distributed. Unable to inspect, patch, or port the underlying stack, even when bugs or security vulnerabilities are discovered.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_developers, payer,
    moderate, biographical, constrained, global).

% Maintain and promote free software licenses, distributions, and documentation that preserve the four freedoms. Document the harms of proprietary control, provide technical alternatives, and represent a persistent source of resistance to the regime's legitimacy claims.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates large-scale software production by centralizing investment risk, quality assurance, marketing, and technical support under firms that recoup costs through unit sales and subscriptions.
% TRANSFER_FUNCTION: Transfers control over computing from end users and developers to proprietary vendors through legal restrictions on use, modification, and distribution, supplemented by technical prevention of inspection and interoperability; transfers money from users to vendors as the price of access.
% ABSENT_VOICES: Users in low-income regions who forfeit autonomy because proprietary licenses dominate educational and government procurement; hobbyist maintainers who would adapt software for local languages and accessibility needs if source were available; free software advocates are routinely excluded from standards-setting bodies dominated by vendor consortia.
% DISAPPEARANCE_RATIONALE: Without proprietary licensing and technical control, software financing would shift to service contracts, commons-based peer production, and public funding; vendor lock-in, planned obsolescence, and format dependency would lose their structural support; the entire ecology of software distribution would reorganize around user-modifiable code.
% FOUNDING_PROBLEM: How to finance complex software development and coordinate reliable distribution when digital goods can be copied at near-zero marginal cost, creating a free-rider threat to concentrated investment.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors and industry trade groups attest the problem remains live, citing development costs and security liability. Free software advocates and institutional economists (e.g., Benkler, Weber) attest that commons-based peer production and service models already solve it at scale; no independent arbiter has resolved the dispute.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the regime systematically withholds source code, prohibits modification, and deploys technical and legal barriers that strip users of control over their own computing. Suppression is high (0.80) because the regime depends on active enforcement: copyright litigation, DRM, anti-circumvention laws, and network-effects lock-in that suppresses viable alternatives. Theater ratio is moderate (0.35): there is growing performative openness (source-available licenses, corporate open-washing) that does not restore substantive freedom, masking the underlying extraction. Accessibility collapse (0.75) reflects how file formats, hardware compatibility, and network effects collapse user alternatives once they enter a proprietary ecosystem. Resistance (0.60) captures the sustained but structurally disadvantaged opposition from the free software movement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (proprietary vendors) experiences the constraint as legitimate coordination that funds innovation and provides accountable support. The payer seats (users and developers) experience the same structure as denial of autonomy and extraction of control. The freedom imperative reading authored here adopts the payer-seat structural assessment as its primary frame, producing a claimed_type of snare that the engine may or may not replicate when processing the beneficiary seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors are the declared beneficiaries: they collect revenue and control from the constraint and have arbitrage-grade exit (they can pivot to service models or open-core strategies). End users and developers are the declared victims: they are denied the four freedoms, face high switching costs, and have constrained or trapped exit. The engine will derive low directionality for vendors and high directionality for users and developers, amplifying effective extraction for the victim seats and damping it for the beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom imperative reading resists mandatrophy mislabeling by refusing to credit the coordination story (investment, support, QA) as justification for the steady state. It treats the founding problem (financing software) as either solved by alternative means or as a cover story for ongoing extraction. The R5 genealogy records that the founding problem is contested, preventing the regime from inheriting automatic legitimacy from its origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Does the freedom_imperative_reading of the software_control_legitimacy kernel capture the full structural reality of proprietary control, or does its categorical deontology obscure coordination functions that a tangled_rope framing would reveal?',
    'Comparative analysis across the four sibling readings of this kernel; evaluation of whether any proprietary regime can be authored with descriptively low Îµ without changing the observable.',
    'If the coordination function is irreducible and separable from extraction, this reading overstates the case for snare and the engine will compute a tangled_rope or mountain classification for the beneficiary seat; if the coordination is inseparable from control denial, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committing-frame ambiguity for the freedom imperative reading within the software_control_legitimacy kernel.').

omega_variable(
    coordination_extraction_separability,
    'Can the financing and coordination functions of proprietary software production be separated from the regime''s freedom-denying extraction, or are they structurally fused?',
    'Empirical comparison of free software and proprietary business models at equivalent scale; natural experiments in jurisdictions with strong public software funding.',
    'If separable, the high Îµ authored here is partially the price of coordination and the constraint is tangled_rope; if fused, the extraction is intrinsic and snare classification is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and extraction are separable in proprietary software.').

omega_variable(
    suppression_source_ambiguity,
    'Is the persistence of proprietary software dominance driven primarily by active legal-technical suppression (enforcement, DRM, litigation), or by passive network effects and user inertia?',
    'Measure exit elasticity in the absence of legal barriers: if migration surges when enforcement is removed, suppression is active; if migration remains low, dominance is inertial.',
    'Active suppression would validate the high suppression score and snare classification; inertial dominance would suggest a piton or degraded rope dynamic with lower active extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Active enforcement versus inertial lock-in as the source of proprietary persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t11, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 11, 0.2).
narrative_ontology:measurement(soft_tr_t22, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 22, 0.28).
narrative_ontology:measurement(soft_tr_t33, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 33, 0.32).
narrative_ontology:measurement(soft_tr_t44, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 44, 0.35).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soft_be_t11, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 11, 0.65).
narrative_ontology:measurement(soft_be_t22, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 22, 0.75).
narrative_ontology:measurement(soft_be_t33, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 33, 0.82).
narrative_ontology:measurement(soft_be_t44, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 44, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(soft_su_t11, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 11, 0.6).
narrative_ontology:measurement(soft_su_t22, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 22, 0.72).
narrative_ontology:measurement(soft_su_t33, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 33, 0.78).
narrative_ontology:measurement(soft_su_t44, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 44, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_control_legitimacy kernel. Sibling constraints (pragmatic_openness_reading, property_rights_reading, commons_reading) model the same kernel through different normative frames. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
