% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Distributed Learning for Catastrophe Avoidance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability industries, particularly commercial aviation, maintain
 *   catastrophe-avoidance competence through a distributed learning system
 *   that aggregates near-miss reports, foreign incident analyses, and
 *   high-realism simulator drills. The constraint is the institutionalized
 *   requirement that competence retention depends on this hybrid approach
 *   rather than on simulation alone or on actual catastrophic selection.
 *   Industries with robust cross-organizational learning networks (aviation,
 *   nuclear) demonstrate sustained safety performance; those without (much of
 *   medicine) exhibit competence decay and recurring failure modes. This
 *   story authors the hybrid_near_miss_learning reading of the
 *   catastrophe_avoidance_retention kernel.
 *
 * KEY AGENTS:
 *   - aviation_regulators: Agenda-setter (institutional/analytical) â designs and enforces reporting frameworks
 *   - frontline_operators: Primary target (moderate/constrained) â bears reporting burden and exposure
 *   - operating_organizations: Dual-positioned payer/beneficiary (organized/constrained) â funds infrastructure, gains safety
 *   - safety_dependent_public: Diffuse beneficiary (organized/constrained) â receives safety value without direct participation
 *   - medical_practitioners: Excluded seat (moderate/identity_locked) â outside the learning network, represents the null case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.45).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.48).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Distributed Learning for Catastrophe Avoidance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '2f111bb4-66d6-4820-af07-7fbe9d2fd7a8').
narrative_ontology:cs_kernel_codification('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', implicit).
narrative_ontology:cs_authority_grounding('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', expertise).
narrative_ontology:cs_interpretation_layer_present('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8').
narrative_ontology:cs_reading_relation('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_axiom('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', foundational, near_miss_learning_necessary_for_competence).
narrative_ontology:cs_axiom_status(near_miss_learning_necessary_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', near_miss_learning_necessary_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', foundational, distributed_reporting_supersedes_siloed_safety).
narrative_ontology:cs_axiom_status(distributed_reporting_supersedes_siloed_safety, holdable).
narrative_ontology:cs_axiom_grounding('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', distributed_reporting_supersedes_siloed_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', distributed_just_culture_learning).
narrative_ontology:cs_drift_state('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', contemporary_multi_industry_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f111bb4-66d6-4820-af07-7fbe9d2fd7a8', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_dependent_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_researchers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set mandatory and voluntary reporting requirements through ICAO standards and national regulations. Operate and oversee ASRS-like programs and mandate Safety Management Systems. Legitimacy derives from sustained improvements in safety metrics. Could alter reporting requirements by regulatory fiat but are constrained by international treaty frameworks and public safety expectations.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Pilots, air traffic controllers, and maintenance personnel who must file mandatory incident reports and participate in debriefs and high-realism drills. Bear the time cost, psychological burden of self-reporting errors, and potential career exposure even under just-culture policies. Exit is constrained by professional licensing requirements and industry culture that stigmatizes non-participation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Airlines and operators that must invest in reporting infrastructure, employ safety officers, and share data with industry bodies. Bear significant compliance costs and transparency exposure. Benefit from reduced accident liability and insurance costs when safety improves, but the cost-benefit asymmetry varies by fleet size and operational complexity.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations, beneficiary).

% Passengers and overflown communities who receive safety benefits from the reporting system but do not participate in it and cannot easily opt out of air travel for many routes. Pay indirectly through ticket prices that reflect reporting-system costs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_dependent_public, beneficiary,
    organized, biographical, constrained, global).

% Academic and institutional researchers who analyze aggregated near-miss data to publish safety science and build careers on the high-reliability-organization paradigm. Their professional identity and funding are tied to the continuation of rich reporting datasets.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_researchers, beneficiary,
    moderate, generational, mobile, global).

% Practitioners in high-risk medical specialties who lack equivalent cross-institutional reporting networks. They would benefit from or critique the aviation model but are structurally outside the safety-science discourse that legitimates it; their own error-reporting remains largely internal and punitive.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, medical_practitioners, excluded,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic system failures in high-reliability domains by maintaining operator competence through shared learning from operational near-misses, foreign incidents, and high-realism drills without requiring actual catastrophes to occur.
% TRANSFER_FUNCTION: Moves operational incident data, causal analysis, and corrected practices from frontline reporters and investigating organizations to the broader industry and future crews, at the cost of reporter effort, organizational transparency, and infrastructure investment.
% ABSENT_VOICES: Medical practitioners and other high-risk industry workers who lack cross-organizational reporting networks; they would argue for or against the transferability of the aviation model but are outside the safety-science conversation that legitimizes it.
% DISAPPEARANCE_RATIONALE: If the distributed near-miss learning system vanished, aviation safety competence would decay toward reactive catastrophe-driven learning or simulation-only training, incident rates would rise, and the industry's safety record would reorganize around less effective retention mechanisms.
% FOUNDING_PROBLEM: Catastrophic accidents in early aviation revealed that organizational memory of rare failure modes decayed between generations of operators, and single organizations could not generate enough internal exposure to maintain recognition competence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by independent safety-science research (Dekker, Reason, Perrow) and by the historical accident record; it is attested from outside the benefiting regulator class by frontline unions and by international accident investigation boards that predate specific regulatory mandates.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the system genuinely prevents catastrophes but imposes significant asymmetric costs on frontline reporters and operating organizations. Suppression (0.48) reflects the active enforcement of reporting mandates and the deliberate suppression of blame cultures that would otherwise undermine reporting. Theater ratio (0.25) is low-moderate: most activity is functional, though bureaucratic reporting layers create some performative overhead. Accessibility collapse (0.55) is moderate because once distributed learning is institutionalized, reverting to siloed or catastrophe-only learning becomes cognitively and organizationally difficult to justify. Resistance (0.38) comes from frontline reluctance to report and organizational resistance to transparency.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and researcher seats, the constraint is essential coordination that solves the collective-action problem of safety-data sharing. From the frontline operator seat, the same structure reads as enforced extraction of time, attention, and professional vulnerability. The engine computes this divergence from the structural asymmetry in exit options and cost-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators are the primary targets: they pay the concentrated costs of reporting effort, psychological burden, and career exposure while receiving only diffuse safety benefits (d near target end). Operating organizations sit ambiguously: they pay infrastructure costs but also capture safety and liability benefits (d near symmetric). Regulators and researchers are beneficiaries of the system's continuation. The public receives pure benefit but has no voice in system design. Medical practitioners are structurally outside the constraint and do not contribute to its directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy by clearly distinguishing genuine coordination (catastrophe prevention through shared learning) from pure extraction. If the reporting burden exceeded the safety value, it would drift toward snare; if the coordination function atrophied entirely, it would become piton. Current measurements show stable extractiveness with moderate theater, indicating the coordination function remains live but carries real asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cross_industry_generalizability,
    'Is distributed near-miss learning a universal structural requirement for high-reliability systems, or is it contingent on aviation''s specific error profile and professional culture?',
    'Comparative longitudinal studies of safety outcomes across industries adopting vs rejecting aviation-style reporting networks.',
    'If contingent, the constraint''s claimed necessity is a domain-specific rope rather than a universal tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_industry_generalizability, conceptual, 'Whether the mechanism is universal or aviation-specific').

omega_variable(
    reporting_burden_vs_learning_value,
    'Does the marginal reporting burden on frontline operators exceed the marginal safety value of additional near-miss data as systems mature?',
    'Econometric analysis of incident-reporting saturation points and safety outcome elasticity.',
    'If burden exceeds value, the constraint has shifted from coordination toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_burden_vs_learning_value, empirical, 'Marginal cost versus marginal benefit of additional reporting').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_avoidance_retention kernel. Would the same structural arrangement classify differently under the simulation_as_proxy_catastrophe or catastrophe_as_necessary_selector readings?',
    'Comparative classification of the same institutional arrangements under alternative kernel readings.',
    'Under simulation-sufficiency, the near-miss reporting infrastructure is unnecessary overhead (higher extraction); under catastrophe-necessity, it is ineffectual theater (different failure mode).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural framing dependence on kernel reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.19).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
