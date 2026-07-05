% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold as Sufficiency Condition for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'simulation_fidelity_threshold' reading of
 *   the catastrophe_proxy_sufficiency kernel: the claim that competence
 *   retention through simulation is neither categorically impossible (the
 *   necessity reading) nor categorically achieved (the proxy reading), but
 *   depends on whether the simulation technology crosses a specific,
 *   technology-bound fidelity threshold matching the stress and uncertainty
 *   profile of the real catastrophe. This makes sufficiency an engineering
 *   question rather than a philosophical one, and creates a genuine
 *   coordination function: organizations, vendors, and regulators can
 *   converge on measurable specification targets instead of relitigating
 *   whether simulation can ever substitute for catastrophe. The coordination
 *   is real, but it also creates a beneficiary class (simulation vendors,
 *   training administrators) whose interests are served by the
 *   threshold-is-approachable framing regardless of whether any given fielded
 *   simulator has actually crossed it for a given failure mode — hence a
 *   modest but real and rising extractiveness as underfunded organizations
 *   certify against a threshold their equipment has not met.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold as Sufficiency Condition for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '03475916-9905-4e25-b9d3-036333e3f255').
narrative_ontology:cs_kernel_codification('03475916-9905-4e25-b9d3-036333e3f255', distributed).
narrative_ontology:cs_authority_grounding('03475916-9905-4e25-b9d3-036333e3f255', expertise).
narrative_ontology:cs_interpretation_layer_present('03475916-9905-4e25-b9d3-036333e3f255').
narrative_ontology:cs_reading_relation('03475916-9905-4e25-b9d3-036333e3f255', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_reading_relation('03475916-9905-4e25-b9d3-036333e3f255', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('03475916-9905-4e25-b9d3-036333e3f255', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('03475916-9905-4e25-b9d3-036333e3f255', foundational, sufficiency_is_technology_conditional_not_categorical).
narrative_ontology:cs_axiom_status(sufficiency_is_technology_conditional_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('03475916-9905-4e25-b9d3-036333e3f255', sufficiency_is_technology_conditional_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('03475916-9905-4e25-b9d3-036333e3f255', secondary, fidelity_threshold_is_measurable_engineering_target).
narrative_ontology:cs_axiom_status(fidelity_threshold_is_measurable_engineering_target, holdable).
narrative_ontology:cs_axiom_grounding('03475916-9905-4e25-b9d3-036333e3f255', fidelity_threshold_is_measurable_engineering_target, instrumental).
narrative_ontology:cs_reference_frame('03475916-9905-4e25-b9d3-036333e3f255', engineering_specification_sufficiency).
narrative_ontology:cs_drift_state('03475916-9905-4e25-b9d3-036333e3f255', post_high_fidelity_simulator_proliferation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('03475916-9905-4e25-b9d3-036333e3f255', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators_under_underfunded_sims).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, technology_dependent_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and sell high-fidelity simulators (full-motion cockpit rigs, VR nuclear control-room replicas, adversarial cyber ranges). Their commercial case depends on the claim that crossing a specific fidelity threshold makes simulation sufficient for competence retention — a claim that justifies continued capital investment in ever-higher-fidelity hardware. They benefit whether or not the threshold is precisely locatable, because the framing keeps the investment question open and recurring.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, generational, arbitrage, global).

% Airlines, nuclear plant operators, aircraft carrier crews, air traffic control agencies. They decide how much to invest in simulator fidelity and how to certify crews using simulated rather than real catastrophic exposure. They administer training budgets and set internal doctrine about what counts as sufficient practice, and they bear direct liability if competence degrades between real incidents.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, national).

% Design curricula and certification standards around simulator hours logged. Their professional standing and budget justification depend on the technology-dependent-threshold framing being taken as settled: it gives them a defensible, auditable sufficiency criterion instead of an unfalsifiable claim about real catastrophe being irreplaceable.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators, agenda_setter).

% Pilots, control room operators, and crews trained on simulators that have not actually crossed the relevant fidelity threshold for their failure modes, but are certified as if they had because the organization purchased or maintained lower-fidelity equipment. They bear the risk of an undetected competence gap during a real event, without the ability to independently verify whether their training crossed the threshold or not.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators_under_underfunded_sims, payer,
    moderate, biographical, trapped, national).

% Set minimum simulator standards (e.g. FAA Level D certification, NRC training requirements) and audit whether organizations meet them. They rely on engineering specifications and incident post-mortems to judge whether a given simulator's fidelity is sufficient for the failure modes it claims to train against.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulators_and_certification_bodies, observer,
    institutional, generational, analytical, national).

% Operators who lived through an actual catastrophic event and whose tacit, stress-tempered competence is treated by this reading as reproducible by sufficiently advanced simulation. Their testimony about what real catastrophic stress does that no simulator has yet replicated for them personally rarely enters procurement or certification decisions, which are made on engineering-spec grounds.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, past_catastrophe_survivors, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives organizations, regulators, and vendors a shared, engineering-tractable criterion for how much simulation investment is 'enough' — replacing an otherwise unfalsifiable dispute (can any simulation ever substitute for a real catastrophe?) with a measurable, technology-dependent target that can be certified, audited, and improved over successive equipment generations.
% TRANSFER_FUNCTION: Moves capital from training budgets toward simulation-technology vendors, and moves certification authority from experience-based judgment toward specification-based engineering sign-off; in organizations that under-invest, it moves undetected competence risk onto frontline operators who are certified as threshold-crossing when the underlying hardware has not actually reached the relevant fidelity band.
% ABSENT_VOICES: Frontline operators facing real events after training on simulators later shown to be sub-threshold have no seat in the fidelity-standard-setting process; survivors of actual catastrophes whose tacit stress-competence exceeds anything measured on a simulator spec sheet are rarely consulted when 'sufficiency' is defined in engineering terms.
% DISAPPEARANCE_RATIONALE: If the fidelity-threshold framing disappeared, training investment decisions would revert either to unbounded skepticism (simulation can never suffice, forcing exposure to real risk or accepting permanent competence uncertainty) or to unbounded confidence (any simulation suffices, removing the investment discipline the threshold currently imposes). Procurement standards, certification regimes, and vendor R&D roadmaps are all built around the assumption that a locatable threshold exists and can be approached technologically.
% FOUNDING_PROBLEM: High-consequence, low-frequency catastrophic events (reactor meltdowns, mid-air engine failures, carrier flight-deck fires) cannot ethically or practically be reproduced for training purposes, yet organizations must maintain operator competence for exactly these events between real occurrences.
% FOUNDING_PROBLEM_CORROBORATION: Aviation and nuclear safety researchers outside the simulator industry (human factors academics, NTSB and INPO post-incident analysts) corroborate that the underlying problem — competence decay absent real catastrophic exposure — remains live and unresolved; their independent post-incident reviews (e.g. crew performance analyses following near-miss events) are the primary source used to argue that some currently-fielded simulators have NOT crossed the sufficiency threshold, even as vendors and program administrators treat the threshold as approached or met.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-low (0.30) and rises slowly to 0.42 over the interval, reflecting a slow drift where the threshold framing becomes a budget-justification mechanism as much as an engineering target — organizations increasingly cite 'threshold-crossing' investment as satisfying competence obligations even where audits of actual fidelity lag. Suppression is low (0.28): no one is coercively prevented from questioning whether a given simulator meets the threshold; the constraint persists because it is a genuinely useful coordination frame, not because dissent is blocked. Theater ratio is low-moderate and rising modestly (0.12 to 0.22), consistent with some certification activity becoming compliance-oriented (logging simulator hours) rather than fidelity-verification-oriented. Accessibility collapse (0.35) and resistance (0.40) are both moderate — organizations retain real alternatives (invest more, use hybrid programs, consult post-incident researchers) and resistance exists from frontline operators and independent safety researchers who push back on premature sufficiency claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and training administrators sit near the beneficiary end: the threshold framing sustains recurring capital investment and gives administrators a defensible certification criterion, regardless of whether the threshold is precisely met. High-reliability organizations are agenda-setters who both benefit (avoid unfalsifiable necessity claims) and bear liability risk, placing them near symmetric. Frontline operators trained on sub-threshold simulators are the structural targets: trapped by employment and licensing structures, they cannot independently verify fidelity and bear the tail risk if the threshold claim is wrong for their specific training program.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as either pure extraction (the necessity reading's implicit charge that all simulation-based sufficiency claims are cover for underinvestment) or pure coordination (the proxy reading's claim that simulation is unconditionally sufficient). By making sufficiency technology-dependent and threshold-based, the reading preserves a real coordination function — a shared, auditable investment target — while remaining honest that under-resourced implementations of the same framework produce real victims. The founding problem (competence maintenance without reproducing catastrophe) remains live; the risk is that 'threshold crossed' becomes a self-certifying claim asserted by the same parties who benefit from asserting it, without independent verification from post-incident researchers or affected operators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_locatability,
    'Can the fidelity threshold that this reading posits actually be located and measured for a given failure mode, or is ''threshold-crossing'' an unfalsifiable claim asserted by interested parties?',
    'Compare simulator-trained crew performance against real-incident performance data (post-incident analyses, NTSB/INPO reports) for specific failure modes, checking whether performance gaps close monotonically with fidelity investment or plateau/discontinue at some measurable point.',
    'If the threshold is empirically locatable and organizations can verify crossing it, this reading is well-grounded and the coordination function is genuine. If the threshold is not independently measurable and ''crossing'' claims are asserted only by vendors and administrators who benefit from the claim, the reading collapses toward the proxy reading''s overclaim, dressed in technology-dependent language that makes the same overclaim harder to falsify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_locatability, empirical, 'Whether the sufficiency threshold is empirically locatable or a self-serving assertion.').

omega_variable(
    sibling_reading_divergence_locus,
    'Where exactly does this reading''s premise (technology-dependent, threshold-based sufficiency) diverge from the sibling readings, and which sibling would the evidence favor if the threshold turns out to be unreachable with current or foreseeable technology?',
    'Track whether fidelity-threshold-chasing organizations show declining real-incident performance over generational timescales despite increasing simulator investment (favoring hybrid_degradation_reading or catastrophe_necessity_reading) versus stable or improving performance (favoring this reading or simulation_as_proxy_catastrophe_reading).',
    'If evidence shows a persistent, non-closing gap despite technology investment, resources and legitimacy should shift toward the hybrid_degradation or necessity readings; if the gap closes with investment, this reading''s coordination frame is vindicated and vendor investment claims gain credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_divergence_locus, conceptual, 'Where the four kernel readings'' predictions diverge and what evidence would discriminate among them.').

omega_variable(
    vendor_asserted_sufficiency_bias,
    'Given that simulation technology vendors are structural beneficiaries of the threshold-is-approachable-via-investment framing, how much of the ''threshold crossed'' certification language in the industry reflects genuine engineering verification versus vendor-influenced self-certification?',
    'Audit certification decisions for independence — whether the body declaring threshold-crossing has a financial relationship with the equipment vendor, and whether independent regulators or academic human-factors researchers concur.',
    'High vendor influence over sufficiency declarations would push the constraint''s true operation toward tangled_rope (genuine coordination function co-located with vendor rent-seeking); independent verification would support the cleaner rope classification claimed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_asserted_sufficiency_bias, empirical, 'Whether sufficiency certification is independently verified or vendor-influenced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 8, 0.15).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 16, 0.17).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the catastrophe_proxy_sufficiency kernel. simulation_as_proxy_catastrophe_reading claims unconditional sufficiency (no threshold, no gap); catastrophe_necessity_reading claims categorical insufficiency (no technology closes the gap); hybrid_degradation_reading locates the gap in generational tacit-knowledge decay independent of fidelity engineering. This reading (simulation_fidelity_threshold) occupies the middle ground: sufficiency is real but conditional on a measurable, technology-bound threshold. Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged into one constraint with a measurement parameter, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
