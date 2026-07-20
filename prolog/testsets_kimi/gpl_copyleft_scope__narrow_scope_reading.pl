% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Scope Reading
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   This constraint story instantiates the narrow_scope_reading of the
 *   gpl_copyleft_scope kernel. Under this reading, GPL Section 2(b) applies
 *   only to direct derivative works as defined by traditional copyright
 *   doctrine, excluding mere aggregation, plugin architectures, and many
 *   dynamic linking scenarios. Commercial firms retain flexibility to
 *   integrate GPL components with proprietary layers. The constraint
 *   functions as coordination infrastructure for mixed codebases. Copyleft
 *   advocates' expectations of universal code sharing are structurally
 *   weakened. Enforcement against dynamic linking patterns is rare. This
 *   reading coexists with strong_copyleft_reading and
 *   enforcement_vacuum_reading as competing interpretations of the same
 *   license text.
 *
 * KEY AGENTS:
 *   - commercial_integrators (beneficiary): retain proprietary flexibility by avoiding derivative work status
 *   - gpl_component_authors (beneficiary): gain wider adoption through integrator-friendly boundaries
 *   - judicial_interpreters (agenda_setter): enforce narrow copyright boundary through case law
 *   - strong_copyleft_advocates (excluded): marginalized expectation of universal reciprocal sharing
 *   - proprietary_end_users (beneficiary): access mixed ecosystem products
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.22).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Scope Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software licensing / intellectual property / open source governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'db96c662-07fd-43f4-b094-bad8c752a1ad').
narrative_ontology:cs_kernel_codification('db96c662-07fd-43f4-b094-bad8c752a1ad', formalized).
narrative_ontology:cs_authority_grounding('db96c662-07fd-43f4-b094-bad8c752a1ad', lineage).
narrative_ontology:cs_interpretation_layer_present('db96c662-07fd-43f4-b094-bad8c752a1ad').
narrative_ontology:cs_reading_relation('db96c662-07fd-43f4-b094-bad8c752a1ad', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('db96c662-07fd-43f4-b094-bad8c752a1ad', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('db96c662-07fd-43f4-b094-bad8c752a1ad', foundational, statutory_derivative_work_standard_governs_gpl).
narrative_ontology:cs_axiom_status(statutory_derivative_work_standard_governs_gpl, holdable).
narrative_ontology:cs_axiom_grounding('db96c662-07fd-43f4-b094-bad8c752a1ad', statutory_derivative_work_standard_governs_gpl, conventional).
narrative_ontology:cs_axiom('db96c662-07fd-43f4-b094-bad8c752a1ad', secondary, license_cannot_expand_beyond_copyright_boundary).
narrative_ontology:cs_axiom_status(license_cannot_expand_beyond_copyright_boundary, holdable).
narrative_ontology:cs_axiom_grounding('db96c662-07fd-43f4-b094-bad8c752a1ad', license_cannot_expand_beyond_copyright_boundary, conventional).
narrative_ontology:cs_reference_frame('db96c662-07fd-43f4-b094-bad8c752a1ad', traditional_copyright_doctrine).
narrative_ontology:cs_drift_state('db96c662-07fd-43f4-b094-bad8c752a1ad', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('db96c662-07fd-43f4-b094-bad8c752a1ad', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, gpl_component_authors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Combine GPL components with proprietary application layers without triggering copyleft obligations, provided they do not create direct derivative works. They retain legal flexibility to distribute mixed codebase products under proprietary terms and can choose integration architectures that preserve separation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    powerful, biographical, mobile, global).

% Publish code under GPL and benefit from wider adoption because commercial integrators can incorporate their components into larger products without fear of contaminating proprietary layers. They receive contributions, visibility, and maintenance support but not reciprocal code sharing for adjacent proprietary modules.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_component_authors, beneficiary,
    moderate, biographical, mobile, global).

% Courts and legal scholars who apply traditional copyright doctrine to determine whether a software work constitutes a derivative work under GPL Section 2(b). Their rulings establish the enforceable boundary between direct modification and permissible aggregation or linking.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Promote the view that GPL copyleft should extend to all forms of software coupling including dynamic linking and plugin architectures. Under the narrow reading, their expectation of universal reciprocal code sharing is structurally weakened and their position is marginalized in commercial and judicial interpretive contexts.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_advocates, excluded,
    organized, generational, constrained, global).

% Benefit from a larger software ecosystem where proprietary applications can integrate robust open source components without license contagion, preserving consumer choice and product diversity across mixed codebase offerings.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_end_users, beneficiary,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables mixed codebase development by clarifying that mere aggregation, plugin architectures, and certain dynamic linking forms do not trigger GPL copyleft, creating a predictable legal environment for combining open and proprietary software.
% TRANSFER_FUNCTION: Transfers legal certainty and integration flexibility to commercial developers and end users; transfers broader distribution and adoption opportunities to GPL component authors who would otherwise face integration barriers.
% ABSENT_VOICES: Strong copyleft advocates who argue for universal reciprocal licensing across all coupling forms are not in the room when judicial and commercial actors apply traditional copyright doctrine to limit the derivative work boundary.
% DISAPPEARANCE_RATIONALE: If the narrow scope reading vanished and was replaced by expansive copyleft interpretation, commercial integrators would face immediate relicensing obligations for combined works, the mixed codebase ecosystem would contract, and proprietary software architectures relying on GPL components would require fundamental restructuring.
% FOUNDING_PROBLEM: How to prevent proprietary enclosure of free software while preserving a clear, legally defensible boundary that does not capture independent works or standard integration techniques.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and copyright scholars outside the commercial integration ecosystem attest that the founding problem was historically addressed through traditional infringement remedies; strong copyleft advocates and the Free Software Foundation dispute that the narrow reading adequately solves the enclosure problem, asserting that the founding problem remains live and requires broader copyleft scope.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does impose a genuine boundaryâdirect derivatives must be GPLâbut leaves most integration paths open, minimizing rent extraction. Suppression is low (0.22) because the narrow boundary persists through legal interpretation and commercial practice rather than active enforcement machinery. Theater ratio is low (0.18) because the coordination function (legal certainty for mixed codebases) is structurally real and not performative. Accessibility collapse is moderate (0.42): alternatives such as proprietary clean room implementations or permissive licenses exist but involve switching costs. Resistance is moderate (0.35): strong copyleft advocates contest the reading but lack institutional leverage to shift judicial doctrine.
 *
 * PERSPECTIVAL GAP:
 *   Commercial integrators experience the constraint as enabling flexibility and legal predictability (beneficiary seat). Strong copyleft advocates experience the same license text as a failed promise, but they are structurally excluded from the interpretive framework that defines enforceable obligationsânot victims of extraction, but holders of a displaced reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial integrators, component authors, and end users are positioned as beneficiaries with mobile exit options; their effective extraction is damped. Judicial interpreters sit at analytical exit. Strong copyleft advocates are excluded rather than victims because the narrow reading does not extract resources from them; it simply declines to enforce their preferred normative framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than snare is warranted by the absence of identifiable victims and the absence of concentrated extraction. The coordination functionâenabling predictable mixed source developmentâis genuine and not a cover story. If the constraint were a snare, we would expect active suppression of alternatives and a clear payer class; instead, commercial actors freely choose integration patterns that avoid the copyleft trigger.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the derivative work boundary in GPL Section 2(b) determined by traditional copyright doctrine or by the license text''s own expansive terms?',
    'Definitive appellate or Supreme Court ruling interpreting the scope of derivative works in software under the Copyright Act and its interaction with GPL.',
    'If courts adopt an expansive statutory interpretation, the narrow reading collapses and commercial integrators face higher effective extraction; if courts affirm traditional doctrine, the narrow reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the narrow or expansive reading is legally correct.').

omega_variable(
    dynamic_linking_copyright_status,
    'Does dynamic linking create a derivative work under traditional copyright doctrine?',
    'Federal circuit precedent specifically addressing whether dynamic linking constitutes a derivative work or a separate aggregate.',
    'Would determine whether plugin architectures and dynamic linking fall inside or outside the narrow reading''s boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_linking_copyright_status, empirical, 'Legal status of dynamic linking under copyright law.').

omega_variable(
    enforcement_asymmetry,
    'Does the narrow reading persist because of doctrinal coherence, or because commercial actors have greater litigation resources than copyleft enforcers?',
    'Comparative analysis of enforcement actions, litigation outcomes, and resource asymmetry between FSF-aligned enforcers and industry defendants.',
    'If asymmetry-driven, the constraint is better described by the enforcement_vacuum_reading; if doctrine-driven, the narrow reading is independently stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether narrow reading stability rests on legal merit or enforcement capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_narrow_tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(gpl_narrow_tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(gpl_narrow_tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(gpl_narrow_tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(gpl_narrow_tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gpl_narrow_be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(gpl_narrow_be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(gpl_narrow_be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(gpl_narrow_be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(gpl_narrow_be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel decomposes into three structurally distinct constraints: the narrow_scope_reading (traditional copyright boundary, rope), the strong_copyleft_reading (expansive coupling boundary, contested extraction), and the enforcement_vacuum_reading (scope as artifact of enforcement capacity). Each has a different epsilon, stakeholder structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
