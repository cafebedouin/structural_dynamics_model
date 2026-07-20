% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Reading: Linking as Derivative Work Trigger
 *   domain: legal/software_governance
 *
 * SUMMARY:
 *   This constraint instantiates the broad copyleft reading of the GPL
 *   derivative work trigger kernel: the legal interpretation that linking
 *   (even dynamic) to a GPL library creates a derivative work, triggering
 *   source disclosure obligations for the combined work. It is one of three
 *   structurally distinct readings of the same kernel; the narrow linking
 *   permissive reading and interface boundary reading are separate
 *   constraints. The broad reading is advanced by the FSF and enforced
 *   through compliance actions and litigation, extracting compliance costs
 *   from proprietary vendors while coordinating source availability for
 *   downstream users. It is claimed as tangled_rope because it carries both a
 *   genuine coordination function (commons-building reciprocity) and
 *   asymmetric extraction (proprietary vendors bear compliance costs and
 *   legal risk).
 *
 * KEY AGENTS:
 *   - fsf_license_stewards (agenda_setter, institutional/identity_locked): stewards the GPL and enforces the broad interpretation
 *   - downstream_users (beneficiary, moderate/constrained): receive source code access enabled by reciprocity obligations
 *   - proprietary_vendors (payer, powerful/mobile): bear compliance costs and legal exposure for proprietary integration
 *   - commercial_integrators (payer, moderate/constrained): face uncertain compliance burden without resources to engineer around dependencies
 *   - permissive_advocates (excluded, organized/mobile): argue against the reading but are marginalized in canonical discourse
 *   - judicial_arbiters (observer, institutional/analytical): adjudicate derivative work boundaries with mixed signals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.62).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.48).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Reading: Linking as Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/software_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '6c5541e2-cb58-4de1-b860-bfc85e8d24f7').
narrative_ontology:cs_kernel_codification('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', fixed_text).
narrative_ontology:cs_authority_grounding('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', lineage).
narrative_ontology:cs_interpretation_layer_present('6c5541e2-cb58-4de1-b860-bfc85e8d24f7').
narrative_ontology:cs_reading_relation('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', foundational, derivative_work_includes_linking).
narrative_ontology:cs_axiom_status(derivative_work_includes_linking, holdable).
narrative_ontology:cs_axiom_grounding('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', derivative_work_includes_linking, conventional).
narrative_ontology:cs_axiom('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', foundational, copyleft_reciprocity_overrides_proprietary_integration).
narrative_ontology:cs_axiom_status(copyleft_reciprocity_overrides_proprietary_integration, holdable).
narrative_ontology:cs_axiom_grounding('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', copyleft_reciprocity_overrides_proprietary_integration, deontological).
narrative_ontology:cs_reference_frame('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', copyleft_reciprocity_framework).
narrative_ontology:cs_drift_state('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', contemporary_proprietary_avoidance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c5541e2-cb58-4de1-b860-bfc85e8d24f7', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_maximalism).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, reciprocal_licensing_ethic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stewards the GPL license text and promotes the broad copyleft interpretation through legal education, compliance engineering guidance, and litigation support. Asserts that dynamic and static linking both create derivative works requiring source disclosure. Their institutional mission and donor relationships center on defending this interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_license_stewards, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive complete corresponding source code for applications linked to GPL libraries, enabling modification, audit, and redistribution. Depend on the constraint's enforcement to ensure their software freedoms are preserved when using products that incorporate free components.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    moderate, biographical, constrained, global).

% Develop and distribute proprietary software that may dynamically or statically link to GPL-licensed libraries. Face the choice of releasing proprietary source code, investing in costly license compliance and separation engineering, or avoiding GPL dependencies entirely. Fund legal challenges and lobby for narrower copyright interpretations.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, mobile, global).

% Small and medium software firms that incorporate GPL components into commercial products, often without full legal review. Face uncertain compliance exposure and potential enforcement action; typically lack resources to re-engineer around dependencies or to litigate the derivative work boundary.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Advocate for permissive open source licenses and argue that the broad reading overextends copyright doctrine and harms software industry interoperability. Their preferred licensing frameworks are structurally marginalized in GPL-dominated infrastructure domains.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_advocates, excluded,
    organized, biographical, mobile, global).

% Federal courts and regulatory bodies that adjudicate copyright boundary disputes involving the GPL. Their rulings on derivative work doctrine and API copyright partially determine the broad reading's legal enforceability, producing mixed and jurisdictionally varied signals.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, judicial_arbiters, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a reciprocity loop ensuring that users who receive software containing GPL-linked components also receive the complete corresponding source code, preserving the ability to modify, audit, and redistribute, thereby growing the software commons.
% TRANSFER_FUNCTION: Moves source code disclosure obligations and compliance engineering costs from proprietary software vendors and commercial integrators to downstream users and the commons; moves modified source code contributions back into the commons from distributors.
% ABSENT_VOICES: Permissive license advocates and proprietary industry representatives argue the reading overreaches copyright doctrine, but are structurally excluded from canonical GPL interpretation; their objections are treated as external hostility rather than interpretive input.
% DISAPPEARANCE_RATIONALE: If the broad reading disappeared, proprietary vendors would integrate GPL libraries without source disclosure obligations, downstream users would lose guaranteed source access for linked proprietary applications, enforcement organizations would lose a primary enforcement target, and the copyleft commons would likely contract as proprietary integration without reciprocity normalized.
% FOUNDING_PROBLEM: Proprietary software vendors were incorporating free software into closed products without contributing modifications back, enabling free-riding that threatened to starve the software commons and deprive users of source access to software they depended on.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and copyleft developers corroborate the ongoing threat of proprietary appropriation. Major technology corporations and permissive license advocates attest the problem is resolved or overstated, and that the remedy now causes more harm than the disease; independent legal scholarship and empirical analyses of license proliferation offer mixed outside corroboration.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the reading imposes significant compliance costs and source disclosure obligations on proprietary actors. Suppression (0.48) is moderate: the constraint suppresses proprietary linking strategies but does not eliminate alternatives (avoidance, clean room, permissive licensing). Theater ratio (0.30) reflects that while enforcement is functional, a substantial portion of FSF rhetoric performs ideological commitment beyond strict legal necessity. Accessibility collapse (0.60) is elevated because once the reading is accepted within a jurisdiction or supply chain, the only alternatives for proprietary actors are disclosure or avoidance. Resistance (0.72) is high due to sustained industry lobbying, litigation, and massive investment in GPL avoidance. Temporal measurements show extraction and suppression rising through the 2000s enforcement era, then moderating slightly as proprietary avoidance became standard practice and enforcement targets became scarcer.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF seat, the constraint is necessary coordination that prevents free-riding and protects user freedom; the compliance costs are legitimate reciprocity, not extraction. From the proprietary vendor seat, the same structure is asymmetric extraction that leverages copyright ambiguity to force disclosure of independently developed code. The commercial integrator seat experiences high uncertainty and fear-driven compliance expenditure. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream_users are structural beneficiaries: the constraint subsidizes their access to source code (low d, extraction damped into subsidy). Proprietary_vendors and commercial_integrators are structural targets: they bear the compliance costs and legal risk (high d, extraction amplified). Fsf_license_stewards sit near the beneficiary end institutionally but are identity-locked to the constraint's persistence; their directionality is structurally low but their exit is fused. Judicial_arbiters are analytical with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because its coordination function is structurally separable from its extraction: the reciprocity loop genuinely grows the commons (real coordination), while the asymmetric imposition on proprietary vendors (real extraction) requires active enforcement to hold. If the coordination function alone were present without the enforcement asymmetry, it would approach rope; if the extraction alone were present without the commons benefit, it would be snare. The temporal data show the constraint has not atrophied into piton â enforcement remains functional and theater is moderate, though the slight post-peak decline in suppression_requirement bears monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_as_derivative_doctrine,
    'Does copyright law itself classify software linking as creating a derivative work, or does the GPL''s power derive solely from contractual threat and party acquiescence?',
    'Judicial ruling on the copyrightability of linking in the abstract, beyond contract, or authoritative appellate decision squarely holding that linking does or does not create a derivative work under the Copyright Act.',
    'If linking is not copyright infringement absent contractual promise, the constraint''s extraction depends on contract law rather than copyright monopoly, reducing its coercive scope and potentially shifting its classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_as_derivative_doctrine, conceptual, 'Whether the broad reading rests on copyright doctrine or contractual threat.').

omega_variable(
    avoidance_neutralization,
    'Has industry-wide avoidance of GPL dependencies rendered the broad reading''s effective extraction lower than its nominal legal scope suggests?',
    'Empirical measurement of GPL dependency trends in proprietary software and quantitative analysis of clean-room replacement investment over the measurement interval.',
    'If avoidance is near-total, the constraint operates more as a boundary marker than an active extraction mechanism, lowering effective extractiveness despite nominal legal breadth; if integration continues, extraction is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(avoidance_neutralization, empirical, 'Whether proprietary avoidance has neutralized nominal extraction.').

omega_variable(
    kernel_interpretation_stability,
    'Is the broad copyleft reading a stable interpretation of the GPL kernel, or does textual ambiguity make it one of several equally defensible readings?',
    'Close textual analysis of GPL v2 section 2 and v3 section 2 ''based on'' language against federal copyright precedent and the kernel''s legislative history.',
    'If the text genuinely underdetermines the linking question, the constraint''s authority derives from institutional assertion rather than kernel content, shifting authority_grounding toward extraction and raising coupling flags for false-summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_stability, conceptual, 'Whether the broad reading is compelled by the GPL text or asserted despite ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_broad_copyleft_tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl_broad_copyleft_tr_t7, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 7, 0.2).
narrative_ontology:measurement(gpl_broad_copyleft_tr_t14, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 14, 0.3).
narrative_ontology:measurement(gpl_broad_copyleft_tr_t21, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 21, 0.35).
narrative_ontology:measurement(gpl_broad_copyleft_tr_t28, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement(gpl_broad_copyleft_tr_t35, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 35, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl_broad_copyleft_be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gpl_broad_copyleft_be_t7, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(gpl_broad_copyleft_be_t14, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(gpl_broad_copyleft_be_t21, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(gpl_broad_copyleft_be_t28, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(gpl_broad_copyleft_be_t35, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gpl_broad_copyleft_su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl_broad_copyleft_su_t7, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 7, 0.4).
narrative_ontology:measurement(gpl_broad_copyleft_su_t14, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(gpl_broad_copyleft_su_t21, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 21, 0.74).
narrative_ontology:measurement(gpl_broad_copyleft_su_t28, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 28, 0.68).
narrative_ontology:measurement(gpl_broad_copyleft_su_t35, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 35, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is the broad copyleft reading of the GPL derivative work trigger kernel, decomposed from the colloquial 'GPL linking question' which conflates structurally distinct legal interpretations. The narrow linking permissive reading and interface boundary reading are separate constraints with different epsilon values, beneficiary structures, and authority groundings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
