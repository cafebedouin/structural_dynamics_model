% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation Exercises as Sufficient Catastrophe-Equivalent Practice
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story instantiates one reading within the
 *   catastrophe_proxy_sufficiency kernel contest: the claim that simulation
 *   exercises are categorically sufficient to maintain operational competence
 *   for catastrophe-grade decision-making, indefinitely, without requiring
 *   exposure to actual catastrophic events. Under this reading, the
 *   coordination function is real and largely uncontaminated by extraction —
 *   simulation solves a genuine problem (rare-event competence cannot be
 *   trained on real events without incurring the harm being trained against)
 *   and the parties who benefit (regulators, operators, frontline staff, the
 *   public) are not offset by an identifiable victim class. The sibling
 *   readings — catastrophe_necessity_reading, hybrid_degradation_reading, and
 *   simulation_fidelity_threshold — dispute this sufficiency claim on
 *   different grounds and are authored as separate constraint stories with
 *   their own ε values, per the ε-invariance principle; this file does not
 *   average over them or hedge its own ε to accommodate them.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: institutional beneficiary/agenda_setter — grants certification, gains liability protection
 *   - operating_organizations: institutional beneficiary — runs simulation programs, avoids seeking unavailable real catastrophic exposure
 *   - frontline_operators: moderate-power beneficiary — trains on simulators, under this reading genuinely gains competence
 *   - simulation_industry_vendors: organized beneficiary — sells the proxy infrastructure this reading validates
 *   - public_and_downstream_populations: powerless beneficiary — receives safety benefit if the reading is accurate, has no visibility into the underlying kernel dispute
 *   - safety_researchers: analytical observer — the seat most likely to generate evidence bearing on the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.18).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation Exercises as Sufficient Catastrophe-Equivalent Practice").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '02e14a8a-eb03-479c-96da-71bb67a28ba2').
narrative_ontology:cs_kernel_codification('02e14a8a-eb03-479c-96da-71bb67a28ba2', distributed).
narrative_ontology:cs_authority_grounding('02e14a8a-eb03-479c-96da-71bb67a28ba2', expertise).
narrative_ontology:cs_interpretation_layer_present('02e14a8a-eb03-479c-96da-71bb67a28ba2').
narrative_ontology:cs_reading_relation('02e14a8a-eb03-479c-96da-71bb67a28ba2', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('02e14a8a-eb03-479c-96da-71bb67a28ba2', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('02e14a8a-eb03-479c-96da-71bb67a28ba2', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('02e14a8a-eb03-479c-96da-71bb67a28ba2', foundational, simulation_equivalence_categorical).
narrative_ontology:cs_axiom_status(simulation_equivalence_categorical, holdable).
narrative_ontology:cs_axiom_grounding('02e14a8a-eb03-479c-96da-71bb67a28ba2', simulation_equivalence_categorical, empirically_contingent).
narrative_ontology:cs_axiom('02e14a8a-eb03-479c-96da-71bb67a28ba2', secondary, competence_maintenance_indefinite_without_real_exposure).
narrative_ontology:cs_axiom_status(competence_maintenance_indefinite_without_real_exposure, holdable).
narrative_ontology:cs_axiom_grounding('02e14a8a-eb03-479c-96da-71bb67a28ba2', competence_maintenance_indefinite_without_real_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('02e14a8a-eb03-479c-96da-71bb67a28ba2', post_war_simulator_based_certification_regime).
narrative_ontology:cs_drift_state('02e14a8a-eb03-479c-96da-71bb67a28ba2', contemporary_high_fidelity_simulation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('02e14a8a-eb03-479c-96da-71bb67a28ba2', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_industry_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, public_and_downstream_populations).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies operators as competent based on documented simulation-exercise hours and drill performance rather than requiring exposure to actual catastrophic events (which cannot ethically or practically be arranged). This certification regime shields the regulator from the charge that it demands the impossible, and shields it from liability if a certified operator later fails, since the certification followed the accepted proxy standard.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, beneficiary).

% Run nuclear plants, air traffic control centers, hospital emergency departments, and similar high-reliability operations. They invest in simulation programs that satisfy regulatory requirements and, in the reading this constraint instantiates, genuinely rehearse the decision architecture required in an actual catastrophe. They have no exit from needing SOME competence-maintenance regime; the simulation-sufficiency reading is the one that lets them run their existing training infrastructure without seeking access to real catastrophic events, which is not available to seek.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations, beneficiary,
    institutional, generational, constrained, national).

% Control room staff, pilots, surgeons, and emergency responders who undergo the simulation exercises. Under this reading, the exercises build and maintain the procedural and decision-making competence that keeps them and the public safe; they experience the training as substantively useful rather than as compliance theater. Their exit from the training regime is not really at issue in this reading because the training is read as functioning.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, beneficiary,
    moderate, biographical, constrained, regional).

% Build and sell the simulators, scenario libraries, and certification software that operationalize this reading. They benefit directly from any regime that treats simulation as sufficient, since insufficiency claims would drive demand toward alternative competence-maintenance mechanisms (or toward higher-fidelity, more expensive systems, which they also sell — so their interest is somewhat aligned across readings, but most strongly aligned with sufficiency claims that keep current-generation product viable).
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_industry_vendors, beneficiary,
    organized, biographical, mobile, global).

% Live near reactors, fly on aircraft, and receive emergency care from the operators this regime certifies. If the reading is accurate, they receive genuine safety benefit from a competence-maintenance system that does not require them to be exposed to an actual catastrophe first. They have no visibility into whether the simulation is in fact sufficient and no direct role in adjudicating the kernel contest; their situation depends entirely on which reading is structurally true, a question this story does not resolve.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, public_and_downstream_populations, beneficiary,
    powerless, generational, trapped, regional).

% Study near-miss data, post-incident reviews, and simulator fidelity to assess whether simulation-trained competence holds up under real catastrophic stress. They are the seat most likely to surface evidence for or against this reading, but their findings feed into the kernel contest rather than this constraint alone.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a scarce and dangerous resource — exposure to catastrophe-grade decision stress — by substituting a repeatable, controllable proxy (simulation) for events that cannot be produced on demand without causing the harm they are meant to prevent. This lets an entire high-reliability workforce train to a common competence standard without anyone needing to survive an actual disaster first.
% TRANSFER_FUNCTION: Moves the burden of competence verification from lived catastrophic experience (unavailable, unethical to manufacture) to documented simulation performance. No direct monetary or resource transfer between victim and beneficiary classes is asserted by this reading; what moves is epistemic credit — simulation performance is credited as equivalent to catastrophe-tested competence.
% ABSENT_VOICES: Survivors and investigators of actual catastrophes where certified, simulation-trained personnel nonetheless failed under real stress conditions are the closest thing to an absent voice against this reading — their testimony would bear directly on whether the equivalence claim holds, but post-incident review processes are typically institutionally separate from the certification regime that this reading defends.
% DISAPPEARANCE_RATIONALE: If this reading were rejected wholesale (i.e., if simulation were found insufficient as a matter of settled fact), the entire competence-certification architecture for nuclear operations, aviation, and emergency medicine would need to be rebuilt around some alternative sufficiency standard — regulators would lose their current liability shield, vendors would need to redesign or justify their product category, and operating organizations would face an unresolved question about how to certify anyone at all, since real catastrophes remain unavailable to manufacture.
% FOUNDING_PROBLEM: High-reliability organizations needed a way to verify and maintain operator competence for catastrophic scenarios that, by definition, occur too rarely (and are too costly to manufacture) to provide adequate real-world practice.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers and post-incident investigation bodies (e.g., aviation and nuclear incident review boards) attest that the underlying problem — rare-event competence maintenance — remains unsolved in principle; this attestation supports the founding problem's continued liveness but does not by itself corroborate THIS reading's specific claim that simulation is a sufficient (rather than partial) solution. No party fully outside the beneficiary set (regulators, operators, vendors) has affirmed the sufficiency claim itself; the sufficiency claim is corroborated mainly from within the arrangement it justifies.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading, taken on its own terms, describes a coordination mechanism without an identifiable extraction target: no group is named as bearing a disproportionate cost through the same structure that delivers the coordination benefit. Suppression is low-moderate (0.22) because alternatives to simulation-based certification are not actively foreclosed by this reading's own logic — it is a sufficiency claim, not an enforcement claim, though certification regimes built on it do carry regulatory weight. Theater ratio starts moderate (0.20) and drifts mildly upward (0.28) over the interval, reflecting a plausible dynamic where simulation programs accumulate compliance-oriented scenario libraries and box-checking elements alongside substantively useful training as certification bureaucracies mature — this is authored as a mild drift, not a dominant feature, because the reading's core claim is that the practice remains substantively sufficient throughout.
 *
 * DIRECTIONALITY LOGIC:
 *   All named parties are declared beneficiaries under this reading: regulators (liability protection, administrative tractability), operating organizations (a workable and affordable competence-maintenance mechanism), frontline operators (genuine skill development, in this reading's own terms), vendors (a validated product category), and the public (safety benefit if the reading holds). No victim group is declared because the reading's own structural claim is that competence is maintained, not merely that costs are shifted — a victim declaration would import the hybrid_degradation_reading's or catastrophe_necessity_reading's premises into this story, which the ε-invariance principle forbids. This produces low derived directionality (d near the beneficiary end) for every named stakeholder except the analytical observer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare-event competence maintenance) remains live by the researchers' own account, so this is not a case of a mandate persisting after its problem vanished. The genuine open question — is the mandate being discharged by the CURRENT mechanism, or merely by a mechanism that looks like discharge — is precisely the kernel contest this story is one reading within; that question is deliberately not resolved here and is instead routed to the omega variables below, consistent with Rule 2.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_conditional_sufficiency,
    'Is simulation sufficiency a categorical property of the practice (this reading''s claim) or a conditional property dependent on simulation fidelity crossing an unspecified threshold (the simulation_fidelity_threshold sibling reading''s claim)?',
    'Longitudinal comparison of post-incident performance between operators trained on high-fidelity vs. low-fidelity simulation programs, controlled for years since last real-catastrophe exposure; convergence of outcomes across fidelity tiers would support the categorical reading, divergence would support the threshold reading.',
    'If sufficiency is conditional on fidelity, this reading''s flat, low ε profile is only accurate for high-fidelity programs — lower-fidelity simulation regimes claiming the same sufficiency would in fact be instances of the hybrid_degradation_reading or catastrophe_necessity_reading wearing this reading''s justification as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conditional_sufficiency, empirical, 'Whether sufficiency is a categorical property of simulation-as-such or conditional on fidelity crossing a threshold.').

omega_variable(
    tacit_knowledge_degradation_horizon,
    'Does tacit, embodied stress-response competence degrade across operator generations who have never faced a real catastrophe, even when procedural competence (as measured by simulation performance) remains stable — the hybrid_degradation_reading''s core claim?',
    'Comparative analysis of decision quality and stress-response latency in real incidents, stratified by operator generation (those with vs. without any career exposure to a real catastrophic event within the domain), holding simulation training hours constant.',
    'If degradation is detected in generations with zero real-catastrophe exposure, this reading''s claim of indefinite sufficiency is falsified for sufficiently long time horizons, and the true structure is better described as scaffold (temporary support, adequate for one generation, requiring periodic real-event ''topping up'') rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_degradation_horizon, empirical, 'Whether procedural competence stability masks tacit stress-response degradation absent real catastrophic exposure.').

omega_variable(
    regulatory_liability_shield_incentive,
    'Does the regulatory body''s benefit from this reading (liability protection, administrative tractability) create an incentive to prefer and propagate this reading independent of its truth — i.e., is the sufficiency claim partly self-serving rather than purely evidentiary?',
    'Examine whether regulatory certification standards update in response to post-incident evidence of simulation-trained failure, or whether the sufficiency standard remains institutionally sticky regardless of such evidence — stickiness despite disconfirming evidence would indicate the reading is partly maintained for liability-shield reasons rather than tracking competence.',
    'If the reading is found to be substantially incentive-driven rather than evidentiary, this story''s low-extraction classification would need revision toward a tangled_rope reading in which regulators are not merely beneficiaries but active maintainers of a claim that also serves to shield them from accountability at some cost to public safety confidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_liability_shield_incentive, conceptual, 'Whether regulatory self-interest in liability protection biases the sufficiency claim independent of its evidentiary merit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language kernel 'the BGS-style conjecture that simulation is sufficient practice for catastrophe competence.' Each sibling reading authors its own ε, beneficiary/victim structure, and claimed type per the ε-invariance principle: this reading (simulation_as_proxy_catastrophe_reading) authors low ε (0.18) with no victim set, reflecting a coordination-dominant structure; catastrophe_necessity_reading is expected to author a structurally different profile (simulation as inadequate substitute, implying a victim class of operators falsely certified as competent); hybrid_degradation_reading sits between, describing partial sufficiency with a generational degradation victim class; simulation_fidelity_threshold conditionalizes sufficiency on a technological variable rather than treating it categorically. All four are linked bidirectionally via affects_constraints as members of one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
