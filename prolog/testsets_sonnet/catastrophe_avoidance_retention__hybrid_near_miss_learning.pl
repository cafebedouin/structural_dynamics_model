% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss/Incident-Sharing Competence Retention Network
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'catastrophe
 *   avoidance retention' kernel: that competence in high-reliability systems
 *   is maintained neither by simulation alone nor by actual catastrophe
 *   alone, but by a hybrid, distributed learning architecture drawing on
 *   near-misses, foreign incidents, and high-realism drills. Aviation is the
 *   paradigm success case (ASRS, mandatory incident reporting, cross-carrier
 *   data sharing, recurrent simulator training); the same mechanism appears
 *   structurally weaker in medicine, where punitive liability exposure chills
 *   the near-miss disclosure the model depends on. The constraint is claimed
 *   as tangled_rope: it genuinely coordinates a real learning function
 *   (someone doesn't have to die for the lesson to be learned) but does so by
 *   transferring disclosure risk onto individual frontline reporters and
 *   under-resourced smaller operators, and requires active enforcement
 *   (mandatory reporting regimes, non-punitive protections, drill standards)
 *   to keep functioning.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.38).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss/Incident-Sharing Competence Retention Network").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '3f48f4f5-7d1d-4646-9ca4-3f32de143346').
narrative_ontology:cs_kernel_codification('3f48f4f5-7d1d-4646-9ca4-3f32de143346', distributed).
narrative_ontology:cs_authority_grounding('3f48f4f5-7d1d-4646-9ca4-3f32de143346', practice).
narrative_ontology:cs_interpretation_layer_present('3f48f4f5-7d1d-4646-9ca4-3f32de143346').
narrative_ontology:cs_reading_relation('3f48f4f5-7d1d-4646-9ca4-3f32de143346', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('3f48f4f5-7d1d-4646-9ca4-3f32de143346', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, influences).
narrative_ontology:cs_axiom('3f48f4f5-7d1d-4646-9ca4-3f32de143346', foundational, distributed_disclosure_sufficiency).
narrative_ontology:cs_axiom_status(distributed_disclosure_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('3f48f4f5-7d1d-4646-9ca4-3f32de143346', distributed_disclosure_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('3f48f4f5-7d1d-4646-9ca4-3f32de143346', secondary, cross_organizational_transfer_required).
narrative_ontology:cs_axiom_status(cross_organizational_transfer_required, holdable).
narrative_ontology:cs_axiom_grounding('3f48f4f5-7d1d-4646-9ca4-3f32de143346', cross_organizational_transfer_required, empirically_contingent).
narrative_ontology:cs_reference_frame('3f48f4f5-7d1d-4646-9ca4-3f32de143346', distributed_incident_sharing_norm).
narrative_ontology:cs_drift_state('3f48f4f5-7d1d-4646-9ca4-3f32de143346', contemporary_cross_domain_comparison, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f48f4f5-7d1d-4646-9ca4-3f32de143346', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, industry_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_and_manufacturer_safety_departments).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_reporting_frontline_workers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, smaller_operators_without_reporting_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, distributed_learning_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, surgeons, control-room operators whose competence is kept sharp by a steady diet of near-miss reports, foreign incident bulletins, and realistic drills. They benefit from the accumulated pattern library but are also the ones required to file honest near-miss reports, sit through recurrent simulator sessions, and absorb the reputational or career risk of disclosure when the reporting culture is weaker than advertised.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, payer).

% Passengers and patients who never see the incident-sharing machinery but whose survival odds depend on whether it functions. They cannot audit whether the near-miss pipeline is real or theatrical; they simply inherit whatever competence level the system actually sustained.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public, beneficiary,
    powerless, biographical, trapped, global).

% Mandate reporting systems (ASRS-style, mortality and morbidity conferences, incident databases), set drill and recurrency requirements, and administer the confidentiality/non-punitive protections that make honest reporting possible. They can strengthen or weaken the enforcement machinery and bear reputational cost if it fails publicly.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, industry_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Aggregate cross-organizational incident data, run high-realism simulator programs, and translate near-miss patterns into training updates. They benefit from the shared learning pool (free-riding on competitors' disclosed incidents) while controlling how much of their own incident data actually gets shared upward.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_and_manufacturer_safety_departments, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_and_manufacturer_safety_departments, agenda_setter).

% The individual who files the near-miss report bears the personal exposure — career risk, blame exposure, litigation risk in less-protected systems (medicine especially) — while the benefit of the disclosed pattern flows diffusely to the whole industry. In domains without strong non-punitive protections, this asymmetry actively suppresses reporting even though the system's stated function depends on it.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_reporting_frontline_workers, payer,
    powerless, biographical, trapped, national).

% Regional carriers, rural hospitals, and small manufacturers lack the data infrastructure and staff time to participate fully in incident-sharing networks or run high-realism drills. They pay compliance costs to nominally participate but capture less of the learning benefit than large, well-resourced peers, and are disproportionately exposed when a foreign-incident lesson never reaches them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, smaller_operators_without_reporting_infrastructure, payer,
    moderate, biographical, constrained, regional).

% Study whether the hybrid model actually sustains competence, comparing high-reliability industries (aviation) against weaker cross-organizational learners (medicine) to test whether the mechanism generalizes or is domain-contingent.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates near-misses, foreign incidents, and drill data across many organizations so that no single organization has to experience a catastrophe to learn its lesson — distributing the cost of learning across the whole reporting population instead of concentrating it in whoever happens to have the accident.
% TRANSFER_FUNCTION: Moves disclosure risk and reporting labor from the collective benefit pool onto individual frontline reporters and under-resourced smaller operators, while the aggregated competence gains flow to the whole industry, regulators' safety records, and the traveling public's outcomes.
% ABSENT_VOICES: Frontline workers in weakly-protected reporting cultures (notably medicine, where malpractice exposure chills disclosure) would object that the model presumes a non-punitive reporting culture that does not uniformly exist; they are rarely in the room when regulators and industry associations design the reporting architecture.
% DISAPPEARANCE_RATIONALE: If the incident-sharing networks, near-miss databases, and cross-organizational drill standards vanished, competence retention would fall back on isolated organizational memory and occasional catastrophes as the only teacher — accident rates in domains that currently benefit most from shared learning (commercial aviation) would be expected to drift upward over a period of years as the pattern library stopped refreshing.
% FOUNDING_PROBLEM: Catastrophic accidents were historically the primary (and often only) mechanism by which an industry learned that a specific failure mode existed; this was unacceptably costly in lives and capital, and did not transfer well across organizational or national boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Aviation regulators and independent accident investigators (NTSB, ICAO panels) attest the founding problem was substantially real and that the hybrid model has measurably reduced fatal accident rates since its adoption. Patient-safety researchers outside the hospital administrations that run M&M conferences attest the same founding problem persists largely unsolved in medicine, where punitive liability exposure suppresses the very disclosure the model depends on — supporting a reading that the constraint's success is domain-contingent rather than universal.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.38) and rising slowly: the system genuinely distributes learning cost across the industry rather than concentrating it in catastrophe, but the asymmetry between who discloses and who benefits from the aggregated pattern library is real and mildly intensifying as data-sharing infrastructure professionalizes and becomes more bureaucratically demanding of frontline time. Suppression (0.42) reflects the real coercive element — mandatory reporting requirements, career consequences for non-disclosure in some jurisdictions — but is well below snare-level because non-punitive protections and voluntary professional norms do most of the work in the successful cases. Theater ratio (0.28) is moderate-low: most of the incident-sharing and drill activity is functionally real, though a growing share (rising to 0.28 by interval end) is compliance-oriented reporting that satisfies documentation requirements without generating actionable lessons — the Goodhart risk the T17 trigger would flag if this climbed further.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/safety-department seat, this looks like well-functioning coordination: shared learning that avoided the alternative (learning only from catastrophe). From the individual frontline reporter's seat in a weakly-protected reporting culture, the same structure can look like being asked to absorb disclosure risk for a collective good that accrues elsewhere. The engine should compute these as genuinely different seat classifications from the same structural data, not as a disagreement to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators and large safety departments sit near the beneficiary/agenda-setter end: they design and administer the reporting architecture and capture the aggregated competence gains without bearing individual disclosure risk. The traveling public is a pure structural beneficiary with zero agency in the mechanism. Individual frontline reporters and smaller operators sit nearer the target end: they bear the concentrated cost (career exposure, compliance burden) of a system whose benefits are diffuse and industry-wide. This is the structural asymmetry that makes the tangled_rope claim coherent rather than a pure rope — a genuine coordination function riding on an uncompensated transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe as sole teacher) is not dead — it remains the counterfactual the whole architecture exists to avoid — but its status is contested precisely along the cross-domain fault line the kernel context predicts: live and functioning in aviation, closer to unresolved-in-practice in medicine, where the mandate exists on paper but the underlying disclosure culture the mandate depends on has not actually formed. This divergence is exactly the structural delta the hybrid_near_miss_learning reading predicts, and is why this reading, rather than the catastrophe-as-selector or simulation-as-proxy readings, is the one that best explains cross-industry variance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_generalizability_of_hybrid_mechanism,
    'Does the hybrid near-miss/foreign-incident/drill learning mechanism generalize across industries, or is its apparent success in aviation an artifact of aviation-specific conditions (strong unions protecting reporters, no-fault reporting culture, federal preemption of liability) that do not transfer to medicine or other high-consequence domains?',
    'Comparative longitudinal analysis of accident/adverse-event rate trajectories across industries with strong vs. weak cross-organizational incident-sharing infrastructure, controlling for baseline hazard complexity and regulatory maturity.',
    'If the mechanism is domain-contingent rather than general, the constraint should be understood as a conditional coordination technology (works only given specific enabling conditions) rather than a universal competence-retention law, which would change how much weight the tangled_rope classification should carry versus the sibling readings for domains lacking those conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_generalizability_of_hybrid_mechanism, empirical, 'Whether hybrid learning success is aviation-specific or a general mechanism.').

omega_variable(
    reporting_culture_authenticity,
    'Is the observed near-miss reporting volume in any given industry a genuine signal of disclosure culture, or partly an artifact of compliance-driven reporting that satisfies documentation mandates without producing actionable safety lessons?',
    'Compare the ratio of actionable-lesson-generating reports to total filed reports over time; a widening gap alongside rising raw report volume would indicate Goodhart drift (theater rising, function flat or declining).',
    'If a substantial share of reporting is compliance theater, the true effective learning rate is lower than raw reporting metrics suggest, and the theater_ratio trajectory understates the risk — this bears directly on whether the constraint is closer to a Piton (theatrical maintenance of a decayed function) in specific sub-domains even while remaining a functioning tangled_rope industry-wide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_culture_authenticity, empirical, 'Whether rising report volume reflects genuine learning or compliance theater.').

omega_variable(
    kernel_framing_alternative_axis,
    'Is the more defensible framing of this kernel really three discrete mechanisms (catastrophe, simulation, hybrid), or is it better modeled as a single continuous variable (proportion of learning signal drawn from real vs. simulated vs. distributed-disclosed events) with the three readings marking regions on that continuum rather than genuinely distinct constraints?',
    'Attempt to construct a single unified metric (share of competence-relevant lessons attributable to each signal source) across multiple industries; if the metric varies continuously and industries do not cluster into discrete regimes, the three-reading decomposition may be over-fitted to the aviation/medicine contrast rather than a structural fact about the kernel.',
    'If the continuous framing is more accurate, the ε-invariance decomposition into three sibling stories may need revision toward a single constraint with a graded parameter rather than three discrete constraint_ids — though per DP-001 this would itself require re-decomposition rather than folding the readings back together.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_axis, conceptual, 'Whether the three-reading kernel decomposition is structurally correct or an artifact of the aviation/medicine contrast used to motivate it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 8, 0.18).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 16, 0.21).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.24).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 32, 0.26).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_avoidance_retention kernel. simulation_as_proxy_catastrophe claims high-fidelity drills alone suffice for competence retention (a lower-suppression, more rope-like reading, since it requires no punitive disclosure infrastructure). catastrophe_as_necessary_selector claims only real catastrophe provides adequate selection pressure (a reading with no coordination function to speak of — closer to an inevitability claim than a constraint humans maintain). This hybrid reading occupies the middle: it requires active enforcement (reporting mandates, drill standards) and produces genuine but asymmetric coordination, which is why it alone is classified tangled_rope while the siblings would be expected to classify differently (rope-leaning for simulation-as-proxy; near-mountain/fatalistic for catastrophe-as-selector, pending their own authored metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
