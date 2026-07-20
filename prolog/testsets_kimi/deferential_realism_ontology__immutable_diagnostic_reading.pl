% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Immutable Diagnostic Reading of Deferential Realism Ontology
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the immutable_diagnostic_reading of the
 *   deferential_realism_ontology kernel. It treats the DR typology as an
 *   observational instrument with fixed referents, epsilon values as
 *   discoverable rather than constructed, and misclassification as an error
 *   correctable through better observation. The reading enforces high
 *   suppression of alternative framings and resolves classification disputes
 *   by appealing to observable metrics. Sibling readings include
 *   hybrid_pragmatic_reading (contested periphery) and
 *   rhetorical_scaffold_reading (normative persuasive vocabulary). The
 *   constraint is authored as a Tangled Rope: it provides genuine
 *   coordination (a shared vocabulary and metric-driven dispute resolution)
 *   while asymmetrically extracting epistemic authority from those who treat
 *   classification as normative.
 *
 * KEY AGENTS:
 *   - framework_maintainers: agenda_setter (institutional/arbitrage) â administer the engine and schema
 *   - empirical_researchers: beneficiary (organized/mobile) â gain stable vocabulary and peer-review streamlining
 *   - institutional_adopters: beneficiary (institutional/constrained) â gain decision cover from apparent objectivity
 *   - normative_theorists: payer (moderate/constrained) â bear suppressed framing legitimacy
 *   - policy_critics: payer (moderate/constrained) â bear delegitimized rhetorical use
 *   - alternative_framework_proponents: excluded (moderate/trapped) â entirely outside the paradigm boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.78).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.85).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable Diagnostic Reading of Deferential Realism Ontology").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '8581e834-4491-4458-ab17-346a6fdc4d91').
narrative_ontology:cs_kernel_codification('8581e834-4491-4458-ab17-346a6fdc4d91', formalized).
narrative_ontology:cs_authority_grounding('8581e834-4491-4458-ab17-346a6fdc4d91', extraction).
narrative_ontology:cs_interpretation_layer_present('8581e834-4491-4458-ab17-346a6fdc4d91').
narrative_ontology:cs_reading_relation('8581e834-4491-4458-ab17-346a6fdc4d91', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_reading_relation('8581e834-4491-4458-ab17-346a6fdc4d91', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('8581e834-4491-4458-ab17-346a6fdc4d91', foundational, epsilon_values_discoverable).
narrative_ontology:cs_axiom_status(epsilon_values_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('8581e834-4491-4458-ab17-346a6fdc4d91', epsilon_values_discoverable, empirically_contingent).
narrative_ontology:cs_axiom('8581e834-4491-4458-ab17-346a6fdc4d91', foundational, typology_is_observational_instrument).
narrative_ontology:cs_axiom_status(typology_is_observational_instrument, holdable).
narrative_ontology:cs_axiom_grounding('8581e834-4491-4458-ab17-346a6fdc4d91', typology_is_observational_instrument, empirically_contingent).
narrative_ontology:cs_reference_frame('8581e834-4491-4458-ab17-346a6fdc4d91', immutable_diagnostic_framework).
narrative_ontology:cs_drift_state('8581e834-4491-4458-ab17-346a6fdc4d91', contested_periphery_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8581e834-4491-4458-ab17-346a6fdc4d91', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, empirical_researchers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, policy_critics).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_invariance_principle).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, structural_realism_in_social_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the schema, compiler, and classification engine. Define valid constraint inputs and adjudicate classification disputes by reference to observable metrics. Their institutional authority and career trajectories depend on the framework's perceived immutability and diagnostic fixity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the DR typology as a stable, objective vocabulary for constraint classification. Benefit from the elimination of normative disputes over epsilon values, which streamlines peer review, funding justification, and cross-study comparability.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, empirical_researchers, beneficiary,
    organized, biographical, mobile, global).

% Adopt DR outputs for organizational decision-making and policy design. Rely on the framework's claim of observational fixity to insulate decisions from charges of political bias or arbitrariness.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters, beneficiary,
    institutional, generational, constrained, national).

% Argue that constraint classification inherently involves normative judgment and that epsilon values are constructed by the choice of observable. Within the immutable reading, their framings are treated as misclassifications or conceptual errors correctable through better observation rather than legitimate dissent.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_theorists, payer,
    moderate, biographical, constrained, global).

% Deploy DR vocabulary for policy critique but are constrained by the requirement that categories like snare and mountain be discovered rather than declared. Their persuasive, rhetorical use of the framework is delegitimized as epistemic error.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_critics, payer,
    moderate, immediate, constrained, national).

% Propose entirely different ontologies for analyzing social constraints. Their perspectives are structurally excluded from DR discourse because the immutable reading frames alternative epistemologies as outside the paradigm, rendering them uninterpretable rather than refutable.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_framework_proponents, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, framework_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the DR research community and institutional adopters around a shared, stable vocabulary for classifying constraints, providing a dispute-resolution mechanism that appeals to observable metrics rather than normative argument.
% TRANSFER_FUNCTION: Moves epistemic authority and discursive legitimacy from normative theorists and policy critics to empirical researchers and institutional adopters, by framing the former's judgments as misclassifications correctable through better observation.
% ABSENT_VOICES: Alternative framework proponents and radical constructivists are excluded; they would argue that the entire typology is a contingent historical artifact rather than a discovery, but are treated as outside the paradigm.
% DISAPPEARANCE_RATIONALE: If the immutable reading vanished, the DR community would fracture into openly contested hybrid and rhetorical readings, institutional adopters would lose their objective cover, and classification disputes would become normative rather than observational.
% FOUNDING_PROBLEM: The need for a rigorous, non-arbitrary way to classify social constraints that avoids collapsing into pure normative assertion or ad hoc critique.
% FOUNDING_PROBLEM_CORROBORATION: No uninterested corroborator exists outside the dispute: DR engineers assert a purely analytical founding problem, while historians and sociologists of science attest that all such typologies carry normative freight. We state plainly that no external party attests the purely observational founding without controversy.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the immutable reading extracts epistemic authority by treating constructed methodological choices as discovered facts. Suppression (0.85) is higher because the constraint's persistence depends on actively excluding hybrid and rhetorical readings. Theater_ratio (0.45) reflects moderate performative maintenance: the observational-instrument framing is partly genuine (real metrics exist) and partly theatrical (the immutability claim is enforced by institutional boundary-keeping rather than empirical proof). Accessibility_collapse (0.82) is high because once the immutable reading is accepted, alternative epistemologies become invisible as errors. Resistance (0.60) reflects ongoing contestation from normative theorists and the live existence of sibling readings.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (framework_maintainers) experiences the constraint as necessary methodological discipline that protects the framework from normative dilution. The payer seats (normative_theorists, policy_critics) experience the same structure as epistemic capture that excludes legitimate contestation. The beneficiary seats (empirical_researchers, institutional_adopters) experience a subsidized coordination tool. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework_maintainers are the structural agenda-setters (low d, authority is subsidized by the constraint). Empirical_researchers and institutional_adopters are declared beneficiaries (low d, receive epistemic subsidy). Normative_theorists and policy_critics are declared victims (high d, bear extraction manifested as suppressed framing legitimacy). Alternative_framework_proponents are excluded, bearing the full externalized cost of the paradigm boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring active enforcement (high suppression) and naming both coordinated parties (empirical researchers, institutional adopters) and paying parties (normative theorists, policy critics). Without the victim declarations, the high suppression and fixed-referent claims might masquerade as a Mountain (natural law of classification); the named victims and enforcement requirement force classification toward Tangled Rope. The founding problem status is contested, not dead, so the constraint is not yet a Piton â it still has live defenders who believe the problem is unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_epsilon,
    'Are epsilon values genuinely discoverable structural properties of constraints, or are they constructed by the choice of observable and framing?',
    'Historical analysis of epsilon revisions across the DR corpus â if epsilon values change when measurement protocols or schema versions change, they are constructed rather than discovered.',
    'If epsilon is constructed, the immutable reading collapses toward the hybrid reading; if discoverable, the hybrid reading is a misclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_epsilon, conceptual, 'Whether epsilon is ontologically discovered or methodologically constructed').

omega_variable(
    suppression_as_methodology,
    'Does the suppression of alternative framings serve scientific clarity (excluding confusion) or epistemic capture (excluding legitimate dissent)?',
    'Tracking the fate of internally consistent alternative readings within DR discourse â whether they are refuted on empirical grounds or excluded by definitional fiat.',
    'If exclusion-by-fiat dominates, the constraint computes as a snare; if empirical refutation dominates, it is a rope with high standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_methodology, empirical, 'Whether suppression is methodological discipline or capture').

omega_variable(
    identity_lock_of_maintainers,
    'Is the framework maintainer community identity-locked to the immutable reading through career path dependence and institutional reputation?',
    'Career trajectory analysis of DR maintainers â assessing whether they have viable professional paths outside the immutable reading.',
    'If identity-locked, the reading may persist by inertia even if evidence against immutability accumulates, creating a piton pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_of_maintainers, empirical, 'Identity-lock dynamics among framework maintainers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.73).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the deferential_realism_ontology kernel. The immutable_diagnostic_reading treats the kernel as a fixed observational instrument; its siblings treat the kernel as normatively constructed or hybrid. Decomposition was required because the kernel's epsilon varies by reading: high suppression and fixed-referent claims here, versus negotiable periphery in hybrid_pragmatic_reading and pure persuasion in rhetorical_scaffold_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
