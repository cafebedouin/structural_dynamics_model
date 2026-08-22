% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Limited Reading: Substantial Effects with Economic Nexus
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the substantial-effects limited reading of
 *   the Commerce Clause kernel (commerce_clause_text). It holds that federal
 *   power reaches intrastate activity only when that activity is economic in
 *   nature and has substantial effects on interstate commerce, and that
 *   regulation must be non-pretextual. Articulated in United States v. Lopez
 *   (1995) and Morrison (2000), it carves out a protected zone of state
 *   police power over non-economic conduct. It sits between the expansive
 *   federal reading (Wickard/aggregate effects without categorical limits)
 *   and the originalist narrow reading (commerce limited to trade crossing
 *   state lines). The reading is actively enforced by the federal judiciary
 *   and contested by federal legislators and progressive scholars.
 *
 * KEY AGENTS:
 *   - federal_judiciary (agenda_setter, institutional/constrained): Administers the substantial-effects test and draws the economic/non-economic boundary.
 *   - state_governments (beneficiary, institutional/constrained): Retain police power over non-economic intrastate activity.
 *   - federal_legislators (payer, institutional/constrained): Must append jurisdictional findings and avoid non-economic regulation.
 *   - non_economic_intrastate_actors (beneficiary, moderate/constrained): Shielded from federal regulation of local non-commercial conduct.
 *   - constitutional_scholars (observer, analytical): Debate the coherence and legitimacy of the distinction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.62).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Limited Reading: Substantial Effects with Economic Nexus").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'e902dc83-b391-4f88-ba86-14770f909720').
narrative_ontology:cs_kernel_codification('e902dc83-b391-4f88-ba86-14770f909720', fixed_text).
narrative_ontology:cs_authority_grounding('e902dc83-b391-4f88-ba86-14770f909720', lineage).
narrative_ontology:cs_interpretation_layer_present('e902dc83-b391-4f88-ba86-14770f909720').
narrative_ontology:cs_reading_relation('e902dc83-b391-4f88-ba86-14770f909720', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e902dc83-b391-4f88-ba86-14770f909720', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('e902dc83-b391-4f88-ba86-14770f909720', foundational, substantial_effects_requires_economic_nexus).
narrative_ontology:cs_axiom_status(substantial_effects_requires_economic_nexus, holdable).
narrative_ontology:cs_axiom_grounding('e902dc83-b391-4f88-ba86-14770f909720', substantial_effects_requires_economic_nexus, conventional).
narrative_ontology:cs_axiom('e902dc83-b391-4f88-ba86-14770f909720', foundational, police_power_reserved_to_states).
narrative_ontology:cs_axiom_status(police_power_reserved_to_states, holdable).
narrative_ontology:cs_axiom_grounding('e902dc83-b391-4f88-ba86-14770f909720', police_power_reserved_to_states, conventional).
narrative_ontology:cs_reference_frame('e902dc83-b391-4f88-ba86-14770f909720', dual_federalism_economic_limit).
narrative_ontology:cs_drift_state('e902dc83-b391-4f88-ba86-14770f909720', post_new_deal_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e902dc83-b391-4f88-ba86-14770f909720', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, non_economic_intrastate_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, federal_legislators).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, dual_federalism_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, economic_activity_nexus_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the substantial-effects test and polices the economic/non-economic boundary through judicial review, invalidating federal statutes that lack a sufficient jurisdictional nexus or regulate non-economic intrastate activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Retain police power over non-economic intrastate activity and are shielded from federal preemption in areas such as criminal law, education, and family law under the limited reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Must include economic findings and jurisdictional hooks in legislation; face judicial invalidation when attempting to regulate non-economic intrastate activity under the commerce power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_legislators, payer,
    institutional, biographical, constrained, national).

% Individuals and entities engaged in local non-commercial activity whose conduct is insulated from federal commerce regulation by the reading's categorical limit on non-economic activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, non_economic_intrastate_actors, beneficiary,
    moderate, biographical, constrained, local).

% Analyze and debate the coherence of the economic/non-economic distinction, the empirical adequacy of congressional findings, and the reading's fidelity to constitutional text and history.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between federal and state regulatory authority by validating federal power over genuinely interstate economic activity while preserving a categorical zone of state police power over non-economic intrastate conduct.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction over non-economic intrastate activity from the federal legislature to state governments, and moves boundary-policing authority to the federal judiciary.
% ABSENT_VOICES: Victims of local non-economic harms seeking federal legislative remedies; legal realists who regard the economic/non-economic distinction as doctrinally incoherent; and federal legislators whose preferred social policy is blocked by the nexus requirement.
% DISAPPEARANCE_RATIONALE: Federal statutes previously invalidated under Lopez and Morrison could be reenacted without jurisdictional artifice; states would lose categorical insulation from federal regulation in non-economic areas; and the federal judiciary would surrender a primary instrument of federalism boundary enforcement.
% FOUNDING_PROBLEM: The New Deal and Warren Court eras collapsed meaningful limits on the Commerce Clause, permitting federal regulation of virtually all local activity through aggregate-effects reasoning and threatening the structural independence of state police power.
% FOUNDING_PROBLEM_CORROBORATION: Federalism scholars and state attorneys general attest that unlimited commerce power endangers dual sovereignty. Progressive constitutional scholars and dissenting justices attest that the 'problem' was solved federal overreach and that the limited reading manufactures a crisis; they corroborate that the founding problem narrative is contested rather than settled.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because the doctrine extracts significant legislative autonomy from the federal government, but it is not pure extractionâit preserves a coordination function by validating federal economic regulation. Suppression (0.62) reflects the active blocking of alternative federal regulatory frameworks for non-economic harms. Theater_ratio (0.30) captures the formalistic element of the economic/non-economic line, which can appear as doctrinal theater masking value judgment. Accessibility_collapse (0.50) is moderate: alternatives (the expansive reading) are legally marginalized but remain intellectually available and periodically advanced in dissent. Resistance (0.72) is high because the reading is embedded in a live constitutional struggle between nationalist and federalist jurisprudential camps.
 *
 * PERSPECTIVAL GAP:
 *   From the federal legislative seat, the constraint reads as a tangled rope or near-snare: it actively suppresses democratic majorities' capacity to address local harms through federal legislation and forces jurisdictional artifice. From the state-government seat, it reads as a rope or scaffold protecting sovereignty. From the judicial seat, it reads as a necessary enforcement mechanism preventing commerce-clause creep. The engine should compute these divergent per-seat types from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal legislators are the primary targets (high d): they bear the direct cost of legislative invalidation and must internalize jurisdictional constraints. State governments and non-economic actors are beneficiaries (low d): the constraint subsidizes their regulatory autonomy. The federal judiciary sits as agenda_setter with moderate d: it administers the constraint and gains institutional power from boundary policing, but is itself constrained by precedent and the need for doctrinal justification.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by requiring both a genuine coordination function (preserving a federalism balance) and identifiable asymmetric extraction (federal legislative power curtailed, judicial power augmented). Without the coordination function, it would be a pure snare on federal democracy; without the extraction asymmetry, it would be a simple rope of jurisdictional clarity. The active enforcement requirement (judicial review) is the gate that holds it in tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_coherence,
    'Is the distinction between economic and non-economic activity a coherent, administrable legal standard, or a formalistic theater that masks judicial value judgments?',
    'Comparative doctrinal analysis tracking lower-court application rates and reversal rates in cases turning on the economic/non-economic boundary.',
    'If incoherent, the reading''s coordination function collapses into judicial discretion, pushing classification toward snare; if coherent, the boundary policing is a genuine constraint on federal overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_coherence, conceptual, 'Coherence of the economic/non-economic distinction').

omega_variable(
    kernel_reading_foreclosure,
    'Does the substantial-effects limited reading logically foreclose the expansive federal reading, or do they persist as permanently rival interpretations within the same constitutional order?',
    'Analysis of judicial behavior: whether justices switch between readings or whether the readings are locked to incompatible interpretive communities.',
    'If foreclosed, the kernel is closer to a commitment system with a determinate answer; if coexisting, the indeterminacy is structural and the constraint''s classification varies by which faction controls the Court.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between limited and expansive readings').

omega_variable(
    federalism_coordination_or_judicial_aggrandizement,
    'Does the limited reading genuinely coordinate a stable federalism balance, or does it primarily extract legislative power to the judiciary under the cover of federalism?',
    'Empirical comparison of state regulatory output before and after Lopez; measurement of judicial invalidation rates of federal statutes on commerce grounds versus other grounds.',
    'If state autonomy does not measurably increase while judicial power does, the coordination story is cover for extraction, and the beneficiary structure is misidentified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_coordination_or_judicial_aggrandizement, empirical, 'Whether the doctrine coordinates federalism or extracts power to courts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccl_limited_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ccl_limited_tr_t6, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(ccl_limited_tr_t12, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(ccl_limited_tr_t18, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(ccl_limited_tr_t24, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(ccl_limited_tr_t30, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ccl_limited_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ccl_limited_be_t6, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ccl_limited_be_t12, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(ccl_limited_be_t18, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(ccl_limited_be_t24, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(ccl_limited_be_t30, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 30, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_text__substantial_effects_limited_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
