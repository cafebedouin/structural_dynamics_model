% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This story instantiates the simulation_sufficiency reading of the
 *   competence_occupation kernel: the claim that simulation-based drills,
 *   properly optimized for frequency and fidelity, are sufficient to occupy
 *   the competence kernel that guards against catastrophic skill decay in
 *   high-reliability organizations. Under this reading, training compliance
 *   (hours logged, scenario completion, pass rates) becomes the operative
 *   observable, and the problem of skill decay is treated as tractable
 *   through engineering — better simulators, more frequent refreshers,
 *   higher-fidelity scenario design — rather than as a problem requiring
 *   exposure to authentic incident conditions. The simulation vendor industry
 *   and the compliance-reporting apparatus that certifies against simulation
 *   metrics become the primary structural beneficiaries. This is a distinct
 *   constraint from the sibling readings real_incident_necessity (which
 *   denies simulation can ever occupy the kernel) and hybrid_occupation
 *   (which denies any single mechanism suffices) — each of those is authored
 *   as its own story with its own ε.
 *
 * KEY AGENTS:
 *   - simulation_vendor_industry: primary beneficiary (organized/arbitrage) — revenue scales with mandated drill-hours regardless of outcome
 *   - training_department_management: agenda-setter (institutional/mobile) — sets and certifies the drill calendar, incentivized toward auditable throughput
 *   - regulatory_compliance_officers: beneficiary/co-agenda-setter (institutional/constrained) — certify based on simulation records because they are administratively defensible
 *   - frontline_operators: primary payer (moderate/constrained) — bear the consequence if simulated competence does not transfer to real crisis conditions
 *   - downstream_public_safety_beneficiaries: secondary payer (powerless/trapped) — bear tail risk with zero visibility or standing
 *   - safety_researchers: excluded voice (moderate/analytical) — document the fidelity gap but lack standing in standard-setting
 *   - senior_incident_commanders: observer (powerful/mobile) — can compare simulated to lived experience but face institutional friction raising doubts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.52).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '65876da5-ac4f-4f8b-9335-b7e2852bdc83').
narrative_ontology:cs_kernel_codification('65876da5-ac4f-4f8b-9335-b7e2852bdc83', distributed).
narrative_ontology:cs_authority_grounding('65876da5-ac4f-4f8b-9335-b7e2852bdc83', practice).
narrative_ontology:cs_interpretation_layer_present('65876da5-ac4f-4f8b-9335-b7e2852bdc83').
narrative_ontology:cs_reading_relation('65876da5-ac4f-4f8b-9335-b7e2852bdc83', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('65876da5-ac4f-4f8b-9335-b7e2852bdc83', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('65876da5-ac4f-4f8b-9335-b7e2852bdc83', foundational, simulated_exposure_is_functionally_equivalent_to_real_exposure).
narrative_ontology:cs_axiom_status(simulated_exposure_is_functionally_equivalent_to_real_exposure, holdable).
narrative_ontology:cs_axiom_grounding('65876da5-ac4f-4f8b-9335-b7e2852bdc83', simulated_exposure_is_functionally_equivalent_to_real_exposure, empirically_contingent).
narrative_ontology:cs_axiom('65876da5-ac4f-4f8b-9335-b7e2852bdc83', secondary, skill_decay_is_solvable_by_frequency_and_fidelity_optimization).
narrative_ontology:cs_axiom_status(skill_decay_is_solvable_by_frequency_and_fidelity_optimization, holdable).
narrative_ontology:cs_axiom_grounding('65876da5-ac4f-4f8b-9335-b7e2852bdc83', skill_decay_is_solvable_by_frequency_and_fidelity_optimization, instrumental).
narrative_ontology:cs_reference_frame('65876da5-ac4f-4f8b-9335-b7e2852bdc83', drill_based_certification_regime).
narrative_ontology:cs_drift_state('65876da5-ac4f-4f8b-9335-b7e2852bdc83', post_high_profile_incident_reviews, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('65876da5-ac4f-4f8b-9335-b7e2852bdc83', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendor_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_department_management).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_compliance_officers).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, downstream_public_safety_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sells simulator hardware, scenario libraries, certification software, and refresh contracts to operators. Revenue scales directly with the number of mandated drill-hours and the fidelity tier purchased. Has no exposure if skill decay persists undetected between simulated and real conditions — the contract is for delivering the exercise, not for the outcome of an actual incident.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendor_industry, beneficiary,
    organized, generational, arbitrage, national).

% Designs and certifies the drill calendar, sets pass/fail thresholds on simulated scenarios, and reports compliance metrics upward. Career incentives favor demonstrable, auditable throughput (hours logged, pass rates) over harder-to-measure real-world readiness. Can quietly increase simulation frequency to answer any audit finding without needing to justify a costlier or riskier live-exercise regime.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_department_management, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, training_department_management, beneficiary).

% Certify organizations as compliant based on simulation completion records. Simulation-based metrics are auditable, standardized, and legally defensible in a way that observed real-world performance is not — this makes the sufficiency reading administratively convenient regardless of whether it tracks actual readiness.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, agenda_setter).

% Undergo repeated simulated drills that satisfy certification requirements but often diverge from the sensory, cognitive, and stakes-based texture of a genuine catastrophic event. Bears the consequence if the kernel is not actually occupied — decision paralysis, procedural lapses, or misapplied training under real stress — while the organization's compliance record remains clean regardless.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% The public, patients, passengers, or plant-adjacent communities who depend on frontline operators performing correctly during an actual crisis. Have no visibility into whether the certifying drill regime tracks real competence and no standing to demand a different training architecture; they bear the tail risk if simulation sufficiency is a false summit.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, downstream_public_safety_beneficiaries, payer,
    powerless, biographical, trapped, regional).

% Publish evidence on the fidelity gap between simulated and real catastrophic conditions (physiological stress response, ambiguous information, irreversible consequence) and argue simulation-only regimes systematically under-train for genuine crisis cognition. Rarely seated on the bodies that set certification standards; their findings are cited selectively when convenient and shelved when they would require costlier architecture changes.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_researchers, excluded,
    moderate, generational, analytical, national).

% Have lived through actual catastrophic events and can compare felt experience against the simulated equivalent. Their testimony is solicited for program legitimacy but their more skeptical observations about the gap between drill performance and real performance are institutionally hard to act on without conceding the sufficiency reading is wrong.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, senior_incident_commanders, observer,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_vendor_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, auditable mechanism for maintaining and demonstrating operator readiness without requiring operators to be exposed to actual catastrophic conditions, which are rare, dangerous, and cannot be scheduled for training purposes.
% TRANSFER_FUNCTION: Moves training budget from the organization to the simulation industry in exchange for compliance artifacts (completion records, pass rates); moves risk exposure from the organization's audit trail (which looks clean) onto frontline operators and the downstream public, who bear the consequences if simulated competence does not transfer to real conditions.
% ABSENT_VOICES: Safety researchers documenting the simulation-reality fidelity gap are rarely given standing in certification-standard-setting bodies. Frontline operators who privately doubt their own readiness after a drill have no formal channel to flag that the exercise did not feel like it occupied the competence kernel without appearing to fail the assessment.
% DISAPPEARANCE_RATIONALE: Training departments and regulators would need to build an entirely new certification architecture (the compliance apparatus depends on simulation as the measurable unit), so for them the world rearranges substantially. But if the underlying claim is false — if simulation was never actually occupying the competence kernel — then for frontline readiness and public safety outcomes the world may already be closer to 'unchanged' by the disappearance of a credential that was not doing the work it claimed to do. The parties dispute which of these is true, which is exactly the contested kernel this story is one reading of.
% FOUNDING_PROBLEM: Real catastrophic incidents are too rare, too dangerous, and too irreversible to serve as the primary vehicle for training frontline operators; some repeatable, safe, and schedulable substitute was needed to build and verify crisis competence.
% FOUNDING_PROBLEM_CORROBORATION: Training management and the simulation vendors attest the problem is solved — drills reliably build and certify competence. Independent safety researchers and several senior incident commanders who have lived through real events attest, from outside the certifying and vending institutions, that the fidelity gap between simulated and real catastrophic conditions remains substantial and under-acknowledged; their corroboration is the basis for treating founding-problem status as contested rather than resolved.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, contested).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but partial coordination function: drills genuinely build procedural familiarity and reduce some categories of error, so this is not a pure snare — but the sufficiency claim (that simulation alone occupies the kernel) is contested and convenient for parties who profit from and administer the simulation regime, which is the tangled-rope signature. Theater ratio is high and rising (0.28→0.61) because as the compliance apparatus matures, an increasing share of drill activity is oriented toward producing auditable completion records rather than toward closing the fidelity gap the safety researchers keep identifying. Suppression is moderate (0.52): there is no coercive barrier stopping researchers from publishing the fidelity-gap evidence, but there is a structural suppression of that evidence's uptake into standard-setting bodies, and frontline operators who privately doubt their readiness have no low-cost channel to register that doubt without appearing to fail certification.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and training/compliance management sit near the beneficiary end: they collect revenue or administrative legitimacy from the arrangement and bear none of the downside if simulated competence fails to transfer. Frontline operators and the downstream public sit near the target end: they are structurally required to rely on the certified competence being real, with constrained or trapped exit respectively — an operator cannot simply opt out of certification, and the public cannot select which operators were adequately trained. Safety researchers and incident commanders are positioned analytically/observationally; their exclusion from standard-setting is itself part of the structure that keeps the sufficiency reading uncontested where it is administratively convenient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare, dangerous, unschedulable real incidents cannot serve as primary training vehicle) remains genuinely live — this is not a pure zombie mandate. What is contested is whether the SPECIFIC solution (simulation deemed fully sufficient) still tracks that problem or has drifted into a self-certifying compliance loop where the observable (drill completion) has replaced the target (actual crisis competence) as the thing being optimized. Classifying this as tangled_rope rather than snare preserves the genuine coordination value of drills (avoiding a real-incident-only regime that would be far more costly and dangerous) while still naming the asymmetric extraction: vendors and administrators capture value regardless of whether the kernel is actually occupied, and operators/public bear the risk if it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_transfer_question,
    'Does competence demonstrated under simulated catastrophic conditions actually transfer to genuine incident performance, or does the sufficiency claim substitute a measurable proxy (drill completion) for an unmeasured target (real crisis competence)?',
    'Longitudinal comparison of operators'' simulated-drill performance against their documented performance in actual incidents (where available), controlled for incident severity; meta-analysis of existing fidelity-transfer research from aviation, nuclear, and medical high-reliability domains.',
    'If transfer is strong, the sufficiency reading is closer to a genuine rope with modest extraction from compliance overhead. If transfer is weak, the arrangement is closer to a snare wearing tangled-rope coordination language — the simulation industry and compliance apparatus would be capturing value from a credential that does not certify what it claims to certify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_transfer_question, empirical, 'Whether simulated competence measurably transfers to real catastrophic-incident performance.').

omega_variable(
    committer_kernel_reading_location,
    'This story is one of three readings of the competence_occupation kernel (simulation_sufficiency, real_incident_necessity, hybrid_occupation). Where exactly does the disagreement between readings live — is it empirical (does simulation transfer?), institutional (who bears the cost of a hybrid regime?), or definitional (what counts as ''occupying'' the kernel at all)?',
    'Structured elicitation from safety researchers, training administrators, and incident commanders across all three reading-communities to locate whether their disagreement is resolvable by more data, by resource allocation decisions, or is a genuine definitional impasse about what ''occupying the competence kernel'' means.',
    'If the disagreement is purely empirical, evidence could eventually collapse the readings toward consensus. If institutional or definitional, the three readings will persist as coexisting positions indefinitely regardless of evidence, which changes how any regulatory body should treat calls to ''resolve'' the kernel dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_location, conceptual, 'Where the disagreement between sibling kernel readings is structurally located.').

omega_variable(
    vendor_capture_of_standard_setting,
    'To what extent do simulation vendors, directly or through funded research and advisory seats, influence the certification standards that define what counts as ''sufficient'' simulation exercise?',
    'Disclosure audit of funding relationships between simulation vendors and the bodies that set drill-frequency and fidelity-tier certification standards; comparison of standard-setting outcomes in jurisdictions with vendor-funded advisory input versus those without.',
    'High capture would support classifying this as more snare-like (extraction dominant, coordination function largely cover); low capture would support the tangled-rope reading where genuine coordination and extraction are more evenly mixed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_capture_of_standard_setting, empirical, 'Degree of simulation-industry influence over the standards that certify its own sufficiency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__simulation_sufficiency, theater_ratio, 4, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__simulation_sufficiency, theater_ratio, 8, 0.42).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.49).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__simulation_sufficiency, theater_ratio, 16, 0.54).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.58).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__simulation_sufficiency, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t4, competence_occupation__simulation_sufficiency, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(comp_be_t8, competence_occupation__simulation_sufficiency, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_occupation__simulation_sufficiency, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_occupation__simulation_sufficiency, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(comp_su_t4, competence_occupation__simulation_sufficiency, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(comp_su_t8, competence_occupation__simulation_sufficiency, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(comp_su_t16, competence_occupation__simulation_sufficiency, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comp_su_t24, competence_occupation__simulation_sufficiency, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.1).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency (this story) treats simulation as sufficient and makes training compliance the observable; real_incident_necessity denies simulation can ever occupy the kernel and treats only authentic catastrophic exposure as valid; hybrid_occupation denies any single mechanism suffices and requires multi-mechanism exercise without settled configuration. Each has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type — they are not the same constraint measured differently; they are three structurally different claims sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
