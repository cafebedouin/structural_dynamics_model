% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking Permissive Reading of GPL Derivative Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint instantiates the narrow linking permissive reading of the
 *   GPL derivative-work trigger kernel. It holds that static or dynamic
 *   linking to a GPL library is mere aggregation under copyright law, not
 *   creation of a derivative work, and therefore only direct modifications to
 *   GPL code itself trigger source-disclosure obligations. This reading
 *   erects a protective wall around proprietary modules, enabling vendors to
 *   combine closed and open code without reciprocity. It is contested by the
 *   FSF and copyleft advocates, who assert that linking is inherently
 *   derivative and that the GPL's copyleft provision was designed to
 *   propagate to the entire combined work. The reading functions as a
 *   coordination mechanism (it resolves legal uncertainty for developers)
 *   while asymmetrically extracting from the copyleft commons and end-user
 *   rights.
 *
 * KEY AGENTS:
 *   - Proprietary vendors (beneficiary): powerful, mobile exit â preserve closed-source models by linking without disclosure.
 *   - End users (payer): powerless, trapped â lose source availability for software they use.
 *   - Copyleft community (payer): organized, constrained â see their reciprocity expectations undermined.
 *   - Judiciary (agenda setter): institutional, analytical â adjudicates the derivative-work boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.66).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.6).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking Permissive Reading of GPL Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'ba064a21-c621-4e28-90cf-c4c925f7b0e9').
narrative_ontology:cs_kernel_codification('ba064a21-c621-4e28-90cf-c4c925f7b0e9', formalized).
narrative_ontology:cs_authority_grounding('ba064a21-c621-4e28-90cf-c4c925f7b0e9', lineage).
narrative_ontology:cs_interpretation_layer_present('ba064a21-c621-4e28-90cf-c4c925f7b0e9').
narrative_ontology:cs_reading_relation('ba064a21-c621-4e28-90cf-c4c925f7b0e9', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('ba064a21-c621-4e28-90cf-c4c925f7b0e9', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('ba064a21-c621-4e28-90cf-c4c925f7b0e9', foundational, linking_never_derivative).
narrative_ontology:cs_axiom_status(linking_never_derivative, holdable).
narrative_ontology:cs_axiom_grounding('ba064a21-c621-4e28-90cf-c4c925f7b0e9', linking_never_derivative, conventional).
narrative_ontology:cs_axiom('ba064a21-c621-4e28-90cf-c4c925f7b0e9', foundational, modification_only_trigger).
narrative_ontology:cs_axiom_status(modification_only_trigger, holdable).
narrative_ontology:cs_axiom_grounding('ba064a21-c621-4e28-90cf-c4c925f7b0e9', modification_only_trigger, conventional).
narrative_ontology:cs_reference_frame('ba064a21-c621-4e28-90cf-c4c925f7b0e9', modification_only_copyleft).
narrative_ontology:cs_drift_state('ba064a21-c621-4e28-90cf-c4c925f7b0e9', post_gpl_v3_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ba064a21-c621-4e28-90cf-c4c925f7b0e9', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_community).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_model).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, narrow_derivative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute proprietary software that links to GPL libraries. The narrow reading allows them to maintain closed-source distribution without triggering source-disclosure obligations, preserving trade secrets and competitive moats.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_vendors, beneficiary,
    powerful, generational, mobile, global).

% Use software incorporating GPL components linked into proprietary binaries. Under this reading, they receive no source code for the combined work, losing the ability to audit, modify, or redistribute the software they depend on.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Develop and steward GPL-licensed code expecting reciprocal sharing. The narrow reading frustrates copyleft propagation by permitting proprietary enclosure of linked works, eroding the commons of available source code.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_community, payer,
    organized, generational, constrained, global).

% Adjudicates copyright disputes involving software linking and interprets whether such linking creates a derivative work. Their rulings establish the enforceable boundary that determines when GPL obligations activate.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line legal rule for when GPL source-disclosure obligations are triggered, reducing uncertainty for developers who combine proprietary and GPL code.
% TRANSFER_FUNCTION: Transfers freedom-to-operate and enclosure rights from the copyleft commons to proprietary vendors, while transferring the cost of lost source availability to end-users and the cost of eroded reciprocity to the copyleft community.
% ABSENT_VOICES: End-users who depend on source availability for security auditing and long-term maintenance are rarely party to licensing litigation; their interests are represented only indirectly.
% DISAPPEARANCE_RATIONALE: If the narrow linking reading vanished and linking were universally treated as creating a derivative work, proprietary vendors would have to release source or cease linking to GPL libraries; the market for proprietary software built on GPL components would reorganize.
% FOUNDING_PROBLEM: Uncertainty in software copyright law about whether linking to a library constitutes a derivative work, creating legal risk for developers combining open and closed code.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the beneficiary set attest that the derivative-work boundary in software is unsettled. Proprietary vendors attest the uncertainty justifies narrow interpretation, while FSF and copyleft stewards attest the problem was solved by the license text itself. Independent legal commentary corroborates the underlying ambiguity.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) reflects the substantial value transferred from the copyleft commons to proprietary enclosure; suppression (0.60) captures the active legal enforcement required to maintain the boundary against copyleft claims. Theater ratio (0.40) is elevated because legal arguments framing linking as 'aggregation' often performatively legitimize proprietary enclosure while the underlying coordination function (legal certainty) is genuine but secondary. Accessibility collapse (0.45) is moderate: alternatives (avoiding GPL code, demanding source) exist but are costly for users and developers. Resistance (0.55) is moderate because the FSF and copyleft advocates actively litigate, draft licenses, and lobby against this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary vendor seat, this reading is corrective legal clarity that prevents license overreach and preserves innovation incentives. From the copyleft community seat, it is a judicially sanctioned loophole that hollows out the GPL. From the end-user seat, it is an invisible erosion of software freedom that manifests as opaque, unmodifiable products.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors are declared beneficiaries with mobile exit, yielding low directionality (the constraint subsidizes their business model). End users are declared victims with trapped exit, yielding high directionality (the constraint extracts from them via lost rights). The copyleft community are declared victims with constrained exit, also yielding high directionality (their generational project is diluted). The judiciary, as agenda setter with analytical exit, sits near symmetric but is not a material beneficiary or victim of the extraction itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a pure snare because it solves a genuine coordination problem: without a clear derivative-work boundary, developers face crippling legal uncertainty when combining code with different license terms. However, the chosen boundary systematically favors one class of actors (proprietary vendors) over another (copyleft developers and users). The classification as tangled rope captures this dual character â genuine coordination plus asymmetric extraction â and prevents mislabeling it as either pure extraction (which would ignore the uncertainty problem) or pure coordination (which would ignore the enclosure effect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_doctrine_uncertainty,
    'Does prevailing copyright doctrine actually support the narrow reading that linking is never a derivative work, or does it leave the question genuinely open?',
    'Definitive higher-court rulings or statutory clarification on software linking under copyright law.',
    'If copyright law inherently treats linking as derivative, the narrow reading is a constructed legal strategy rather than a faithful interpretation, pushing the constraint toward snare. If copyright law is genuinely ambiguous, the reading functions as a plausible (if contested) coordination boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_doctrine_uncertainty, empirical, 'Whether the narrow reading rests on solid copyright doctrine or exploits doctrinal ambiguity.').

omega_variable(
    user_harm_materiality,
    'Do end-users of proprietary modules linked to GPL code experience material harm from source unavailability, or is the harm theoretical and uncompensated?',
    'Empirical studies of user modification, audit, and migration behavior in markets dominated by proprietary software built on GPL components.',
    'If harm is material, victim status is strengthened and extraction is higher than measured. If harm is theoretical, the constraint''s effective extraction on the end-user seat is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_harm_materiality, empirical, 'Materiality of end-user harm from lost source availability.').

omega_variable(
    reading_family_relationship,
    'Does the narrow reading logically foreclose the interface_boundary_reading, or can they coexist as fallback positions within a single legal framework?',
    'Analysis of judicial opinions and legal briefs that adopt the narrow reading to see if they explicitly reject or simply bypass the interface-boundary test.',
    'If foreclosed, the kernel readings are mutually exclusive and the constraint family is more fractured. If coexistent, the narrow reading is one of several permissive strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_relationship, conceptual, 'Logical relationship between narrow linking and interface boundary readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 33).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(gpl__tr_t33, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 33, 0.4).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(gpl__be_t33, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 33, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(gpl__su_t33, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 33, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gpl_derivative_work_trigger kernel. The natural-language concept 'derivative work under GPL' conflates three structurally distinct legal interpretations with different epsilon values, beneficiary structures, and victim sets. Each reading is authored as a separate constraint story linked via the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
