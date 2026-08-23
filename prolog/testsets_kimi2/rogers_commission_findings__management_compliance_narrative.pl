% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Management Compliance Narrative
 *   domain: organizational safety / technology governance / regulatory compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster can be read in
 *   three structurally distinct ways. This constraint instantiates the
 *   management compliance narrative: the findings establish a process whereby
 *   program managers must demonstrate documented risk awareness and
 *   mitigation efforts, after which they retain launch authority. This
 *   reading coordinates safety information flow while asymmetrically
 *   extracting veto power from engineering. It is a contested kernel reading;
 *   the siblings are engineering_absolute_threshold (flight must cease until
 *   redesign certified) and actuarial_risk_acceptance (acceptable to fly if
 *   failure probability documented and accepted).
 *
 * KEY AGENTS:
 *   - Program managers (agenda_setter/institutional/constrained): Administer the compliance documentation process and retain ultimate launch authority.
 *   - Engineering safety officers (payer/moderate/constrained): Bear the loss of informal veto power; their objections are channeled into documents that management can override.
 *   - Contractor engineers (payer/moderate/constrained): Provide technical risk data that is incorporated but does not halt launches.
 *   - Flight crews (payer/powerless/constrained): Bear the physical consequences of decisions made through the process.
 *   - Congressional overseers (observer/institutional/analytical): Monitor NASA safety culture from outside the operational decision chain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational safety / technology governance / regulatory compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'aca5eb58-1576-49f0-8b8d-1d68b21585cb').
narrative_ontology:cs_kernel_codification('aca5eb58-1576-49f0-8b8d-1d68b21585cb', formalized).
narrative_ontology:cs_authority_grounding('aca5eb58-1576-49f0-8b8d-1d68b21585cb', lineage).
narrative_ontology:cs_interpretation_layer_present('aca5eb58-1576-49f0-8b8d-1d68b21585cb').
narrative_ontology:cs_reading_relation('aca5eb58-1576-49f0-8b8d-1d68b21585cb', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('aca5eb58-1576-49f0-8b8d-1d68b21585cb', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('aca5eb58-1576-49f0-8b8d-1d68b21585cb', foundational, documented_mitigation_sufficient_for_proceed).
narrative_ontology:cs_axiom_status(documented_mitigation_sufficient_for_proceed, holdable).
narrative_ontology:cs_axiom_grounding('aca5eb58-1576-49f0-8b8d-1d68b21585cb', documented_mitigation_sufficient_for_proceed, conventional).
narrative_ontology:cs_axiom('aca5eb58-1576-49f0-8b8d-1d68b21585cb', foundational, management_retains_launch_discretion).
narrative_ontology:cs_axiom_status(management_retains_launch_discretion, holdable).
narrative_ontology:cs_axiom_grounding('aca5eb58-1576-49f0-8b8d-1d68b21585cb', management_retains_launch_discretion, conventional).
narrative_ontology:cs_reference_frame('aca5eb58-1576-49f0-8b8d-1d68b21585cb', post_accident_reform_governance).
narrative_ontology:cs_drift_state('aca5eb58-1576-49f0-8b8d-1d68b21585cb', post_challenger_operational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aca5eb58-1576-49f0-8b8d-1d68b21585cb', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_managers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_officers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, contractor_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, flight_crews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the post-Rogers compliance process by requiring and reviewing documented risk assessments and mitigation plans before launch. Retain ultimate authority to approve launch despite technical objections, using documentation as institutional justification for schedule-driven decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_managers, agenda_setter,
    institutional, generational, constrained, national).

% Must formalize safety concerns into risk documentation for management review. Technical objections can be overridden if program managers judge the documented mitigation sufficient. They retain their roles but lose the informal or absolute veto power they previously exercised.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_officers, payer,
    moderate, biographical, constrained, national).

% Provide technical data and risk assessments to program management. Their component-level warnings are incorporated into risk documents but do not automatically trigger launch holds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_engineers, payer,
    moderate, biographical, constrained, national).

% Bear the physical consequences of launch decisions authorized through the compliance process. They do not review risk documentation and have no structural input into go/no-go determinations.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, flight_crews, payer,
    powerless, biographical, constrained, national).

% Review NASA safety culture and adherence to Rogers recommendations through hearings and oversight. Can mandate process changes but do not intervene in individual launch decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congressional_overseers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, program_managers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces ad-hoc pre-launch communication with a formal process requiring documented risk awareness and mitigation plans, ensuring safety information reaches decision-makers in a structured format.
% TRANSFER_FUNCTION: Transfers launch authority from technical safety officers to program management by allowing management to proceed if documented risk awareness and mitigation are deemed sufficient, overriding engineering objections.
% ABSENT_VOICES: Engineers who hold an absolute safety threshold view are included in the process but structurally subordinated; flight crews bear the consequences but are not present in the decision room.
% DISAPPEARANCE_RATIONALE: Without the compliance process, program management would lack documented cover to override engineering objections, and launch authority would likely revert to technical safety officers or require absolute certification before proceeding.
% FOUNDING_PROBLEM: The Challenger disaster revealed a broken decision-making process where critical safety information did not reach management in a structured way, and informal concerns were overridden by schedule pressure.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission documented the communication breakdown. However, post-Challenger safety analysts and subsequent accident investigations attest that management override persists through the new documentation process, suggesting the founding problem evolved rather than resolved.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the process allows management to override technical objections with documentation. Suppression (0.58) reflects the active displacement of engineering veto power. Theater ratio (0.45) captures the growing performative dimension of risk documentation as institutional cover. Accessibility collapse (0.50) indicates that alternatives like absolute engineering thresholds are technically imaginable but institutionally displaced. Resistance (0.60) reflects persistent engineering community pushback against management override. The metrics are authored independently of the claimed type; the engine will compute per-seat divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the program management seat, the constraint is a legitimate reform that structures safety information and preserves program momentum. From the engineering safety officer seat, it is a procedural mechanism that legitimizes the override of technical judgment. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Program managers are beneficiaries with constrained exit (they are the institution) â d near the beneficiary end, yielding damped effective extraction. Engineering safety officers and contractor engineers are payers with constrained exit â d near the target end, yielding amplified effective extraction. Flight crews are powerless and constrained, placing them at the extreme target end. The structural derivation captures this without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents false summits by acknowledging the genuine coordination problem the Rogers Commission identified: broken communication channels meant safety concerns never reached decision-makers. The compliance process does solve that problem. However, it is not a pure rope because the same structure that coordinates information also extracts engineering veto power. It is not a pure snare because the coordination function is real and not merely cover. The Tangled Rope classification captures this hybridity accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the management compliance narrative, the engineering absolute threshold, or the actuarial risk acceptance reading correctly describe the operative constraint derived from the Rogers Commission findings?',
    'Historical institutional analysis of post-Challenger launch decision records and authority structures to determine which reading''s premises are reflected in actual go/no-go power.',
    'If the engineering absolute threshold reading is operative, this story overstates extraction; if the management compliance narrative dominates, the extraction measure is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Contested kernel reading ambiguity for Rogers findings').

omega_variable(
    engineering_veto_suppression_mechanism,
    'Is the suppression of engineering veto power structural (formal process removes veto rights) or internalized (engineers self-censor because they believe documentation is the proper channel)?',
    'Analysis of post-Rogers dissent records: whether engineers formally lodged objections that were procedurally overridden, or ceased lodging objections under the new process.',
    'If internalized, effective extraction is higher than structural measures suggest; if purely structural, extraction is bounded by formal process visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_veto_suppression_mechanism, empirical, 'Structural vs internalized suppression of engineering safety objections').

omega_variable(
    compliance_theater_evolution,
    'Has the documented risk awareness requirement evolved into genuine organizational learning or into theatrical compliance that legitimizes predetermined launch decisions?',
    'Ethnographic and documentary analysis of risk documentation quality versus actual safety margins and launch outcomes over the interval.',
    'If theatrical, the constraint drifts toward piton; if genuine learning, it remains a coordination-heavy tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_theater_evolution, empirical, 'Whether compliance documentation tracks real safety learning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__management_compliance_narrative, theater_ratio, 5, 0.25).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.3).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__management_compliance_narrative, theater_ratio, 15, 0.35).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.39).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__management_compliance_narrative, theater_ratio, 25, 0.42).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__management_compliance_narrative, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings kernel decomposes into three structurally distinct constraints. They share the same source text but assign different operative meanings: an absolute technical boundary, an actuarial acceptance framework, and a management compliance process. This decomposition follows the epsilon-invariance principle because the readings have different epsilon values, different victim/beneficiary structures, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
