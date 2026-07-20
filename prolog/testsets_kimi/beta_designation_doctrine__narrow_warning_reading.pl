% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Narrow Warning Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates the narrow_warning_reading of the
 *   beta_designation_doctrine kernel. It treats the beta label as a
 *   temporary, good-faith testing disclosure that preserves underlying
 *   product liability and expires when the genuine testing phase concludes.
 *   The kernel is contested: the expansive_shield_reading treats beta as a
 *   comprehensive, indefinite liability waiver, while the
 *   severity_carve_out_reading excludes beta designation entirely for
 *   critical systems. This reading is authored as a low-extraction scaffold
 *   with a built-in sunset, and its metrics are authored independently of the
 *   claimed type.
 *
 * KEY AGENTS:
 *   - software_developers: Primary beneficiary (moderate/mobile) â receive temporary, bounded liability shield for genuine testing.
 *   - end_users: Co-beneficiary (moderate/mobile) â informed participants with preserved liability rights and voluntary exit.
 *   - judiciary: Agenda-setter (institutional/analytical) â defines and enforces the narrow doctrine and its temporal boundaries.
 *   - consumer_protection_advocates: Observer (organized/analytical) â monitor against expansive drift and corroborate founding problem status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.16).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Narrow Warning Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '6381d43c-c6c7-457b-ab55-a74dfeb48f84').
narrative_ontology:cs_kernel_codification('6381d43c-c6c7-457b-ab55-a74dfeb48f84', formalized).
narrative_ontology:cs_authority_grounding('6381d43c-c6c7-457b-ab55-a74dfeb48f84', lineage).
narrative_ontology:cs_interpretation_layer_present('6381d43c-c6c7-457b-ab55-a74dfeb48f84').
narrative_ontology:cs_reading_relation('6381d43c-c6c7-457b-ab55-a74dfeb48f84', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('6381d43c-c6c7-457b-ab55-a74dfeb48f84', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('6381d43c-c6c7-457b-ab55-a74dfeb48f84', foundational, testing_phase_requires_temporal_boundary).
narrative_ontology:cs_axiom_status(testing_phase_requires_temporal_boundary, holdable).
narrative_ontology:cs_axiom_grounding('6381d43c-c6c7-457b-ab55-a74dfeb48f84', testing_phase_requires_temporal_boundary, conventional).
narrative_ontology:cs_axiom('6381d43c-c6c7-457b-ab55-a74dfeb48f84', foundational, disclosure_preserves_base_liability).
narrative_ontology:cs_axiom_status(disclosure_preserves_base_liability, holdable).
narrative_ontology:cs_axiom_grounding('6381d43c-c6c7-457b-ab55-a74dfeb48f84', disclosure_preserves_base_liability, conventional).
narrative_ontology:cs_reference_frame('6381d43c-c6c7-457b-ab55-a74dfeb48f84', good_faith_testing_transition).
narrative_ontology:cs_drift_state('6381d43c-c6c7-457b-ab55-a74dfeb48f84', contemporary_perpetual_beta_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6381d43c-c6c7-457b-ab55-a74dfeb48f84', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy software under a beta designation to conduct real-world user testing, benefiting from a temporary and narrowly construed liability adjustment. Must provide clear disclosure and adhere to good-faith time bounds; upon completion of the genuine testing phase, the shield expires and full ordinary product liability attaches.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, beneficiary,
    moderate, biographical, mobile, national).

% Receive explicit, conspicuous disclosure that the software is in a time-bounded testing phase and that base product liability protections remain fully in force. They voluntarily use the software, retain ordinary legal recourse for defects, and benefit from earlier access and anticipated product improvement.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, end_users, beneficiary,
    moderate, immediate, mobile, national).

% Adjudicates the scope of the beta designation doctrine, enforcing the narrow reading by verifying genuine disclosure, good-faith temporal boundaries, and expiration of the testing-phase shield. Sets precedent that distinguishes legitimate testing from disguised commercial release.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Monitor judicial and legislative treatment of beta designations to prevent drift toward expansive liability waiver. Corroborate that the narrow reading preserves user rights and that the doctrine's sunset remains functional.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables software developers to obtain essential real-world user feedback during pre-release development while providing users with transparent notice of the software's unfinished status, solving the coordination problem between innovation speed and user safety during bounded testing periods.
% TRANSFER_FUNCTION: Temporarily adjusts the litigation risk profile for developers during a defined testing window, without transferring legal liability to users; the primary transfer is information (disclosure of testing status) and time (early access in exchange for known instability).
% ABSENT_VOICES: Users with low technical literacy who cannot meaningfully parse beta disclosures, and plaintiffs in jurisdictions that have adopted the expansive shield reading, are structurally absent from the narrow reading's protections. They would argue that any liability modulation, however bounded, disadvantages the least sophisticated users.
% DISAPPEARANCE_RATIONALE: Developers would lose the legal certainty for bounded public testing, likely forcing testing into costlier closed environments and slowing user-feedback loops. Users would lose the formal guarantee that base liability survives the beta label, and courts would lack a calibrated doctrine to distinguish genuine testing from commercial deployment.
% FOUNDING_PROBLEM: Pre-release software testing requires real-world exposure that standard product liability rules would chill, because subjecting unfinished software to users would expose developers to unbounded litigation risk, thereby stifling innovation and preventing user feedback from improving final products.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection advocates and software industry associations both attest that some mechanism for bounded pre-release testing is necessary to sustain innovation; independent economic analyses of closed-beta cost differentials and innovation timelines corroborate that the problem remains live from outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.16, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.16) because the liability shield is narrow, temporary, and preserves base liability; the transfer is minimal and consensual. Suppression is low (0.22) because users retain legal recourse and can exit by not using the beta. Theater_ratio is low (0.12) because the beta label corresponds to a genuine functional state rather than a performative legal fiction. Accessibility_collapse is low (0.25) because alternatives remain open. Resistance is modest (0.20) reflecting ongoing vigilance against liability-shield expansion. The measurement grid is flat, reflecting a stable constraint under the narrow reading.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat and the user seat should compute similarly under this reading: both are net beneficiaries of the coordination (developers get testing, users get transparency and final product quality), with directionality near the beneficiary end for both. The judiciary seat computes as agenda-setter. There is minimal seat divergence because the narrow reading is designed to be symmetrically beneficial and time-bounded.
 *
 * DIRECTIONALITY LOGIC:
 *   Both software_developers and end_users are declared beneficiaries, deriving low directionality. The judiciary is agenda_setter with analytical exit. No victims are declared. The engine will compute low effective extraction for all seated agents, consistent with the scaffold's transitional coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading's built-in sunset prevents mandatrophy by design: the justification is the transition from testing to release, not the steady state. If the testing phase becomes indefinite or the liability shield expands, the constraint would drift toward tangled_rope or snare under the expansive reading. The authored metrics remain low and stable to test whether the narrow reading, in practice, avoids the mandatrophy that afflicts the kernel's other readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    base_liability_practical_efficacy,
    'Does preserving nominal base product liability during beta testing result in actual compensation for injured users, or does the beta disclosure create a de facto assumption-of-risk defense that extinguishes recovery?',
    'Empirical analysis of beta-related product liability claim outcomes, settlement rates, and jury instructions in jurisdictions adopting the narrow reading.',
    'If users rarely recover despite preserved liability, they are effectively victims and the constraint''s extractiveness is higher than the narrow reading claims; if recovery rates hold, the reading is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_liability_practical_efficacy, empirical, 'Whether preserved liability translates to actual user recovery.').

omega_variable(
    temporal_boundary_enforceability,
    'Can adjudicators reliably distinguish a good-faith, time-bounded testing phase from a commercially deployed product bearing a beta label?',
    'Longitudinal case law analysis tracking duration, user base scale, revenue generation, and feature completeness at time of injury for products bearing beta designations.',
    'If the boundary is unenforceable, the narrow reading collapses into the expansive shield in practice, invalidating its sunset clause and raising theater_ratio as the label becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_boundary_enforceability, empirical, 'Enforceability of the genuine testing phase boundary.').

omega_variable(
    kernel_reading_irreducibility,
    'Is the beta designation doctrine a single constraint with competing interpretations, or do the narrow, expansive, and severity readings represent three structurally distinct constraints?',
    'Comparative legal analysis across jurisdictions to determine if the readings map to different enforceable rules or are merely rhetorical framings of a single doctrine.',
    'If the readings are structurally distinct, the engine must treat them as a constraint family; if they are rhetorical framings, this story''s epsilon may be observer-relative rather than intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_irreducibility, conceptual, 'Whether the kernel decomposes into distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_narrow_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_narrow_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(beta_narrow_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(beta_narrow_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(beta_narrow_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(beta_narrow_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(beta_narrow_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(beta_narrow_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(beta_narrow_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(beta_narrow_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(beta_designation_doctrine__narrow_warning_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This constraint is the narrow_warning_reading of the beta_designation_doctrine kernel, distinguished from the expansive_shield_reading and severity_carve_out_reading by its insistence on good-faith temporal bounds and preserved base liability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
