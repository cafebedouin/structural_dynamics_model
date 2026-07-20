% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominant Energy Acceptability
 *   domain: risk_assessment/energy_policy
 *
 * SUMMARY:
 *   This constraint instantiates the catastrophic_tail_dominant reading of
 *   the acceptable_risk_for_energy kernel: the normative rule that
 *   low-probability high-consequence events (nuclear catastrophe,
 *   irreversible radioactive waste) must dominate energy risk acceptability,
 *   overriding expected-value optimization and comparative risk trade-offs.
 *   The constraint is structurally enforced through licensing regimes that
 *   treat probabilistic safety arguments as inadmissible for ultimate
 *   acceptability and mandate ever-more-stringent containment and
 *   waste-isolation standards. Nuclear operators and ratepayers bear the
 *   resulting cost escalation and technology exclusion, while regulators and
 *   intergenerational advocacy groups capture authority and policy influence.
 *   The claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope because it carries a genuine coordination function
 *   (catastrophe prevention) alongside asymmetric extraction, while the
 *   metrics describe a heavily enforced, moderately theatrical, and
 *   substantially extractive regime.
 *
 * KEY AGENTS:
 *   - risk_regulators: Agenda-setter (institutional/constrained) â administers the precautionary framework and captures regulatory authority
 *   - nuclear_operators: Primary payer (powerful/constrained) â bears technology exclusion and cost escalation
 *   - intergenerational_advocacy_groups: Beneficiary (organized/mobile) â gains policy influence through intergenerational framing
 *   - electric_ratepayers: Secondary payer (powerless/constrained) â absorbs energy cost and reliability impacts
 *   - probabilistic_risk_analysts: Excluded seat (moderate/constrained) â structurally suppressed in acceptability determinations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.85).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominant Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '7ea57184-707d-43f8-9be9-3ce354986086').
narrative_ontology:cs_kernel_codification('7ea57184-707d-43f8-9be9-3ce354986086', distributed).
narrative_ontology:cs_authority_grounding('7ea57184-707d-43f8-9be9-3ce354986086', distributed).
narrative_ontology:cs_reading_relation('7ea57184-707d-43f8-9be9-3ce354986086', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('7ea57184-707d-43f8-9be9-3ce354986086', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('7ea57184-707d-43f8-9be9-3ce354986086', foundational, irreversible_harm_non_commensurable).
narrative_ontology:cs_axiom_status(irreversible_harm_non_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('7ea57184-707d-43f8-9be9-3ce354986086', irreversible_harm_non_commensurable, deontological).
narrative_ontology:cs_axiom('7ea57184-707d-43f8-9be9-3ce354986086', foundational, tail_risk_absolute_threshold).
narrative_ontology:cs_axiom_status(tail_risk_absolute_threshold, holdable).
narrative_ontology:cs_axiom_grounding('7ea57184-707d-43f8-9be9-3ce354986086', tail_risk_absolute_threshold, conventional).
narrative_ontology:cs_reference_frame('7ea57184-707d-43f8-9be9-3ce354986086', precautionary_risk_governance).
narrative_ontology:cs_drift_state('7ea57184-707d-43f8-9be9-3ce354986086', contemporary_climate_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ea57184-707d-43f8-9be9-3ce354986086', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_regulators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_advocacy_groups).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, electric_ratepayers).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_supremacy).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the risk-acceptability framework for energy licensing, mandating that low-probability high-consequence events dominate siting and design decisions. Derive institutional authority and budget from the precautionary mandate. Cannot easily abandon the framework without legislative reversal or a major accident discrediting the precautionary stance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Must finance ever-more-expensive safety systems, redundant containment, and long-term waste stewardship to satisfy tail-risk criteria. Project cancellations and cost overruns accumulate because probabilistic safety arguments are ruled inadmissible for acceptability. Exit to other energy markets is capital-intensive and politically fraught.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators, payer,
    powerful, biographical, constrained, national).

% Gain policy influence and standing in regulatory hearings by framing nuclear waste and catastrophic accidents as intergenerational injustices. Their participation in licensing challenges is invited and weighted heavily. They can shift attention to other environmental causes but benefit from the current framing's institutionalization.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of forgone nuclear investment through higher electricity rates and continued reliance on fossil backup. They have no direct voice in the risk-calculus framework and limited ability to opt out of the regulated grid.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, electric_ratepayers, payer,
    powerless, immediate, constrained, national).

% Their methodologies (PRA, core damage frequency analysis) are technically admitted but normatively suppressed in acceptability determinations; when tail-risk dominates, probabilistic arguments are treated as irrelevant to the ultimate go/no-go decision. They remain employed but their policy influence is structurally subordinate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_regulators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic low-probability energy accidents by mandating that tail-risk events dominate acceptability decisions, coordinating society away from technologies with irreversible downside and toward precautionary design standards.
% TRANSFER_FUNCTION: Moves decision-making authority from probabilistic engineers and project developers to precautionary regulators and intergenerational advocates; moves cost burden from potential accident victims (future) to present-day project developers and ratepayers via project cancellation, cost escalation, and technology exclusion.
% ABSENT_VOICES: Probabilistic risk analysts and nuclear engineers who treat waste disposal as a solvable engineering problem and accident risk as quantifiable and tradeable against climate benefits; their framing is structurally excluded from regulatory acceptability determinations despite technical competence.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant framing vanished overnight, nuclear projects would be evaluated on comparative risk and expected-value metrics, licensing timelines would compress, waste disposal would revert to an engineering optimization problem, and the present regulatory bottleneck would dissolve â the energy mix and institutional authority would reorganize around open probabilistic trade-offs.
% FOUNDING_PROBLEM: Early nuclear accidents (Three Mile Island, Chernobyl) revealed that probabilistic risk assessment underestimated organizational failure modes and institutional complacency; a framework was needed to prevent 'normalization of deviance' from culminating in catastrophic, irreversible harm.
% FOUNDING_PROBLEM_CORROBORATION: Organizational sociologists (independent academic field, e.g., Perrow, Vaughan) corroborate the structural live-ness of catastrophic failure modes in complex systems. International climate and energy agencies (independent of the precautionary regulatory complex, e.g., IEA, IPCC) corroborate that modern nuclear safety engineering has substantially reduced the founding problem's severity, supporting the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72) is high because the constraint moves substantial resources from operators and ratepayers to regulatory and advocacy institutions via technology exclusion and cost escalation, decoupled from marginal safety returns. Suppression (0.85) is higher because the constraint's persistence depends on actively excluding expected-value and comparative-risk framings from regulatory discourse. Theater ratio (0.45) reflects moderate performative maintenance: some safety requirements yield genuine risk reduction, but a growing share defends the precautionary narrative rather than demonstrated hazard reduction. Accessibility collapse (0.75) is high because probabilistic alternatives are not merely disadvantaged but structurally inadmissible in the acceptability framework. Resistance (0.70) is high because nuclear operators, climate economists, and some publics actively contest the framing. The temporal grid shows monotonic intensification as the precautionary framework hardened from post-accident origin through contemporary climate-policy contestation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) and the payer seats (operators, ratepayers) should compute as different constraint types from the same structural data. From the regulator position, the arrangement is genuine catastrophe prevention whose rigidity is justified by irreversibility. From the operator and ratepayer positions, the same structure is actively enforced extraction that suppresses quantified safety evidence and cheaper alternatives. The engine measures this divergence from beneficiary/victim declarations and exit asymmetry; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk regulators and intergenerational advocacy groups are structural beneficiaries: they collect authority, budget, and policy influence from the constraint's operation, situating them at the low-d end of the derivation chain. Nuclear operators and electric ratepayers are structural targets: they bear the cost of technology exclusion and rate escalation, with constrained or trapped exit options, situating them at the high-d end. Probabilistic risk analysts are excluded rather than coordinated â their suppression is the enforcement object that maintains the beneficiary position. The engine will compute divergent per-seat classifications: the regulator seat will experience coordination-with-overhead, while the operator and ratepayer seats will experience extractive enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â catastrophic nuclear accidents driven by normalized deviance â may be dead (modern designs, empirical safety record) or live (organizational complexity, waste uncertainty). The constraint is not a piton because beneficiaries (regulators, advocates) still capture sufficient authority to actively maintain it; it is not a snare because the coordination function (catastrophe prevention) is not pure cover. Tangled_rope is the classification that prevents mislabeling: it preserves the genuine coordination problem while registering the asymmetric extraction that has layered onto it. If the founding problem is dead and the theater_ratio were higher, the constraint would drift toward piton; if the coordination story were purely cover, it would be a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the catastrophic_tail_dominant reading the only coherent interpretation of the acceptable risk kernel, or do the sibling readings represent equally valid framings that this reading structurally suppresses?',
    'Comparative institutional analysis of jurisdictions adopting each reading; measuring whether catastrophic-tail dominance correlates with lower accident rates at higher system cost, versus expected-value dominance correlating with faster decarbonization but different risk profiles.',
    'If the sibling readings are structurally viable but suppressed, this constraint reads more extractively; if the kernel inherently resolves to catastrophic-tail dominance, the constraint approaches a Mountain-like coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading is one of several live options or the necessary interpretation of the kernel.').

omega_variable(
    probabilistic_suppression_source,
    'Is the suppression of expected-value and probabilistic risk framing achieved through formal regulatory exclusion, or through internalized professional dread-risk culture that persists even when formal rules relax?',
    'Track regulatory text changes versus practitioner survey and licensing hearing data: if formal rules open but probabilistic arguments remain inadmissible in practice, suppression is internalized; if practice opens when rules open, suppression was structural.',
    'Internalized suppression implies higher effective extraction than the structural measure suggests; the constraint persists even after regulatory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probabilistic_suppression_source, empirical, 'Structural versus internalized suppression mechanism for probabilistic framing.').

omega_variable(
    tail_risk_empirical_basis,
    'Does the empirical record of nuclear accidents and waste containment failures support the magnitude of tail-risk weighting, or has the weighting become decoupled from observed failure rates?',
    'Meta-analysis of probabilistic risk assessment accuracy versus observed event frequencies; analysis of waste repository performance data and modern reactor safety statistics.',
    'If decoupled, the constraint''s extractiveness is higher than its coordination function justifies; if coupled, the weighting reflects genuine residual uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_empirical_basis, empirical, 'Whether catastrophic tail-risk weighting tracks empirical failure rates or has become autonomous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_tail_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cat_tail_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.3).
narrative_ontology:measurement(cat_tail_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.36).
narrative_ontology:measurement(cat_tail_tr_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.4).
narrative_ontology:measurement(cat_tail_tr_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.43).
narrative_ontology:measurement(cat_tail_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(cat_tail_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cat_tail_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(cat_tail_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(cat_tail_be_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(cat_tail_be_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(cat_tail_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cat_tail_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cat_tail_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(cat_tail_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(cat_tail_su_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(cat_tail_su_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.83).
narrative_ontology:measurement(cat_tail_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_for_energy kernel. The catastrophic_tail_dominant reading decomposes from the expected_value_dominant and comparative_risk_dominant readings because its epsilon, beneficiary/victim structure, and suppression profile are structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
