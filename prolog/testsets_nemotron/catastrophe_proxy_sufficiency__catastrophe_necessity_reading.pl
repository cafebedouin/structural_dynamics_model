% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint asserts that only actual catastrophic events — not
 *   simulations, drills, or exercises — generate the irreducible stress and
 *   uncertainty necessary to maintain genuine operational competence in
 *   high-reliability domains (nuclear, aviation, chemical, space). The claim
 *   is structural: human psychological and physiological stress-response
 *   systems, institutional learning mechanisms, and organizational memory all
 *   require the genuine possibility of irreversible loss to maintain
 *   calibration. Simulation, no matter how high-fidelity, lacks the
 *   ontological weight of real consequence. Competence decays in
 *   catastrophe-free periods — the 'safety margin' erodes silently as
 *   operators, managers, and regulators gradually normalize deviance, defer
 *   maintenance, and under-invest in resilience. The victims are not agents
 *   per se but the operational safety margins that protect frontline
 *   operators and future affected populations. Beneficiaries include
 *   regulatory bodies that can claim 'safety culture' without funding
 *   full-spectrum simulation, senior management that externalizes training
 *   costs, and simulation vendors who sell fidelity that cannot reach the
 *   necessary threshold.
 *
 * KEY AGENTS:
 *   - operational_safety_margins: Primary victim (mountain/universal) — competence decays without real catastrophe
 *   - frontline_operators: Secondary victim (powerless/biographical/trapped) — bear consequences when margins erode
 *   - future_affected_populations: Tertiary victim (powerless/generational/trapped) — bear consequences of systemic competence decay
 *   - regulatory_bodies: Primary beneficiary (institutional/generational/arbitrage) — claim safety culture without full simulation investment
 *   - senior_management: Secondary beneficiary (powerful/biographical/arbitrage) — externalize training/resilience costs
 *   - safety_consultancies: Tertiary beneficiary (organized/biographical/mobile) — sell compliance frameworks that substitute for real stress-testing
 *   - simulation_vendors: Quaternary beneficiary (organized/biographical/mobile) — sell fidelity that cannot reach catastrophe-equivalence
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'f2f1131d-7868-4bd0-acfe-abd90537f8e1').
narrative_ontology:cs_kernel_codification('f2f1131d-7868-4bd0-acfe-abd90537f8e1', distributed).
narrative_ontology:cs_authority_grounding('f2f1131d-7868-4bd0-acfe-abd90537f8e1', practice).
narrative_ontology:cs_interpretation_layer_present('f2f1131d-7868-4bd0-acfe-abd90537f8e1').
narrative_ontology:cs_reading_relation('f2f1131d-7868-4bd0-acfe-abd90537f8e1', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('f2f1131d-7868-4bd0-acfe-abd90537f8e1', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2f1131d-7868-4bd0-acfe-abd90537f8e1', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('f2f1131d-7868-4bd0-acfe-abd90537f8e1', foundational, catastrophe_stress_ontologically_distinct).
narrative_ontology:cs_axiom_status(catastrophe_stress_ontologically_distinct, holdable).
narrative_ontology:cs_axiom_grounding('f2f1131d-7868-4bd0-acfe-abd90537f8e1', catastrophe_stress_ontologically_distinct, empirically_contingent).
narrative_ontology:cs_axiom('f2f1131d-7868-4bd0-acfe-abd90537f8e1', foundational, competence_decay_without_real_loss_irreversible).
narrative_ontology:cs_axiom_status(competence_decay_without_real_loss_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('f2f1131d-7868-4bd0-acfe-abd90537f8e1', competence_decay_without_real_loss_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('f2f1131d-7868-4bd0-acfe-abd90537f8e1', pre_simulation_era_competence_model).
narrative_ontology:cs_drift_state('f2f1131d-7868-4bd0-acfe-abd90537f8e1', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2f1131d-7868-4bd0-acfe-abd90537f8e1', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, senior_management).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_consultancies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_vendors).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, future_affected_populations).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, irreducible_uncertainty_principle).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, stress_inoculation_theory).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, competence_decay_without_catastrophe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The irreducible buffer between normal operations and catastrophic failure. These margins decay silently during catastrophe-free periods as organizations normalize deviance, defer maintenance, and lose the stress-calibration that real events provide. They cannot exit — they are the structural condition that either holds or collapses.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).

% Operators who work daily in high-hazard systems. They bear the consequences when safety margins erode — they are the ones managing the system when competence gaps surface. Their exit options are trapped: leaving the profession means abandoning their expertise; staying means bearing the risk of an organization that has lost its catastrophe calibration.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, frontline_operators, payer,
    powerless, biographical, trapped, global).

% Communities and populations downstream of high-hazard facilities who bear the consequences of competence decay. They have no voice in the organizational decisions that maintain or erode safety margins, and no exit from the geographic risk.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, future_affected_populations, payer,
    powerless, generational, trapped, global).

% Regulators who can claim 'robust safety culture' and 'continuous improvement' based on simulation exercises and procedural compliance, without funding or mandating the investment levels needed for true catastrophe-equivalent fidelity. They benefit from the narrative that simulation suffices — it makes regulation cheaper and politically easier.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, regulatory_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Executives who control budgets for training, simulation, and resilience investment. They benefit from treating simulation as sufficient — it converts open-ended catastrophe-preparation costs into bounded, auditable line items. Their career horizons are shorter than competence-decay timescales, creating arbitrage: they collect the savings now; the costs arrive after they've moved on.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, senior_management, beneficiary,
    powerful, biographical, arbitrage, global).

% Firms selling safety culture assessments, compliance frameworks, and 'resilience maturity models' that substitute procedural checklists for the irreducible stress-testing that only real catastrophe provides. They profit from the simulation-as-proxy narrative because it makes their products look sufficient.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_consultancies, beneficiary,
    organized, biographical, mobile, global).

% Companies selling high-fidelity simulators and training platforms. They benefit from the narrative that fidelity improvements will eventually reach catastrophe-equivalence — it justifies continued procurement. Their interest is in moving the fidelity threshold, not in acknowledging a fundamental ceiling.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% The analytical seat that sees the full structure: the mountain constraint (catastrophe necessity) is real; the beneficiary seats are not beneficiaries OF the constraint but beneficiaries of DENYING it. The constraint extracts from no one — it is the reality that competence decays without real stress. The extraction comes from actors who pretend otherwise.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself does not coordinate — it is the irreducible reality that competence maintenance requires genuine catastrophe stress. The COORDINATION PROBLEM is solved by the sibling readings (simulation_as_proxy, hybrid_degradation, fidelity_threshold), which attempt to coordinate safety investment without waiting for disasters. This reading says those coordination attempts are structurally insufficient.
% TRANSFER_FUNCTION: The arrangement (treating simulation as sufficient) transfers risk from regulators/management/vendors (who avoid full catastrophe-preparation costs) to frontline operators and future populations (who bear the competence-decay consequences). The transfer is not the constraint's operation — it is the operation of DENYING the constraint.
% ABSENT_VOICES: Frontline operators and future affected populations are structurally excluded from the budget and policy decisions that determine simulation investment levels. They would object to simulation-only regimes but are not in the rooms where 'safety culture' is defined and procurement decisions made. Their exclusion is what allows the simulation-as-proxy narrative to persist.
% DISAPPEARANCE_RATIONALE: If the catastrophe-necessity constraint disappeared (i.e., if simulation truly became catastrophe-equivalent), the entire safety investment calculus of high-reliability industries would reorganize: regulators would mandate simulation-only certification, management would cut physical drill budgets, vendors would race on fidelity metrics, and the 'waiting for catastrophe' problem would be solved. The world would rearrange around simulation sufficiency.
% FOUNDING_PROBLEM: High-hazard industries (nuclear, aviation, chemical, space) discovered that operators and organizations lose the capacity to respond to genuine catastrophe after extended periods without real events — procedures are followed but judgment, improvisation, and stress-calibration atrophy. The founding problem: how to maintain genuine catastrophe-response competence without requiring actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources outside the beneficiary set: (1) Nuclear industry post-TMI/Chernobyl/Fukushima analyses (INSAG reports) documenting competence decay during quiet periods; (2) Aviation safety studies on 'startle effect' and automation surprise in pilots with high simulator hours but low real-emergency exposure; (3) NASA/spaceflight literature on 'operational forgetting' between high-risk missions; (4) Organizational learning research (Weick, Sutcliffe, Vaughan) on normalization of deviance. No beneficiary-group source is the sole attestation.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores as a Mountain on the NL profile: accessibility_collapse = 0.92 (alternatives to real catastrophe for maintaining competence are nearly completely closed once the claim is understood — no amount of simulation investment has yet demonstrated catastrophe-equivalent stress inoculation); resistance = 0.03 (almost no active resistance to the claim itself — the constraint is the reality of competence decay, not a policy being fought); emerges_naturally = true (the stress-response requirement is a biological/organizational fact). Extractiveness is low (0.12) because the constraint itself does not extract — it is the reality that competence requires real catastrophe. The beneficiaries identified are agents who benefit from *pretending* simulation suffices, not from the constraint itself. This is a false-summit candidate: the mountain claim (catastrophe necessity is natural law) coexists with identifiable beneficiaries who profit from treating simulation as sufficient.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical seat, the constraint is a mountain — competence decay without catastrophe is a fact of biology and organizational dynamics. From the beneficiary seats (regulators, management, vendors), the same reality is experienced as a coordination problem: 'how do we maintain safety without waiting for disasters?' This creates a structural tension where beneficiaries have incentive to treat simulation-as-proxy readings as valid, even though the mountain constraint says they are not. The engine will compute different types per seat: mountain for analytical, tangled_rope or snare for beneficiaries who enforce simulation-only regimes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (regulatory_bodies, senior_management, safety_consultancies, simulation_vendors) collect from the *pretense* that simulation suffices — they avoid the cost of real catastrophe and the investment in true high-fidelity simulation. The constraint itself (catastrophe necessity) does not extract from them; rather, their *denial* of the constraint extracts from victims. Victims (operational_safety_margins, frontline_operators, future_affected_populations) bear the cost when competence decays because the constraint was denied. Directionality for beneficiaries is near 0.0 (they benefit from ignoring the mountain); for victims it is near 1.0 (they bear the full cost of the mountain's operation when it is denied).
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy — the constraint is not a degraded institution but a persistent reality. The founding problem (maintaining competence in high-hazard domains) remains live. The false summit risk is that the mountain claim is used to justify *inaction* on simulation investment ('since only real catastrophe works, why invest in simulation?'), which would be a perverse reading. The genuine reading: simulation is necessary but insufficient; competence requires both continuous high-fidelity simulation AND the irreducible calibration that only real events provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_necessity,
    'Is the claim that only real catastrophes maintain competence a genuine natural/psychological law, or a constructed constraint that benefits identifiable agents who avoid investing in high-fidelity simulation?',
    'Longitudinal studies of high-reliability organizations with varying catastrophe exposure and simulation investment; controlled experiments on stress-response retention with/without real-event exposure.',
    'If constructed, the constraint is a false summit masking extraction by regulators/management who externalize simulation costs; if natural law, competence decay is irreducible and any simulation-only regime is structurally unsafe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_necessity, conceptual, 'Whether catastrophe necessity is physical/psychological law or institutional cover story').

omega_variable(
    simulation_fidelity_ceiling,
    'Is there a fundamental ceiling to simulation fidelity for stress/uncertainty generation, or does the ceiling reflect current technology investment priorities?',
    'Track simulation fidelity improvements over decades in nuclear, aviation, and space domains; measure stress-response metrics in operators trained exclusively on simulation vs. those with real-event exposure.',
    'If ceiling is fundamental, the mountain claim holds; if ceiling is investment-dependent, the constraint is a tangled rope where simulation vendors and regulators negotiate fidelity thresholds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation''s inability to replicate catastrophe stress is permanent or contingent').

omega_variable(
    kernel_reading_framing,
    'This constraint is the catastrophe_necessity_reading of kernel catastrophe_proxy_sufficiency. Sibling readings: simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading, simulation_fidelity_threshold. What structural elements do readings disagree on?',
    'Map each reading''s beneficiary/victim structure, extractiveness profile, and claimed type; identify which structural atoms differ.',
    'If readings share referent but differ on ε and victim sets, they are distinct constraints per ε-invariance; the kernel is a linguistic conflation, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-system framing: this reading''s structural delta vs. siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cps_cnr_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(cps_cnr_tr_t0, observed).
narrative_ontology:measurement(cps_cnr_tr_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(cps_cnr_tr_t20, observed).
narrative_ontology:measurement(cps_cnr_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(cps_cnr_tr_t40, observed).
narrative_ontology:measurement(cps_cnr_tr_t60, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(cps_cnr_tr_t60, observed).
narrative_ontology:measurement(cps_cnr_tr_t80, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(cps_cnr_tr_t80, observed).
narrative_ontology:measurement(cps_cnr_tr_t100, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(cps_cnr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cps_cnr_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(cps_cnr_be_t0, observed).
narrative_ontology:measurement(cps_cnr_be_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement_basis(cps_cnr_be_t20, observed).
narrative_ontology:measurement(cps_cnr_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement_basis(cps_cnr_be_t40, observed).
narrative_ontology:measurement(cps_cnr_be_t60, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement_basis(cps_cnr_be_t60, observed).
narrative_ontology:measurement(cps_cnr_be_t80, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement_basis(cps_cnr_be_t80, observed).
narrative_ontology:measurement(cps_cnr_be_t100, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(cps_cnr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cps_cnr_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(cps_cnr_su_t0, observed).
narrative_ontology:measurement(cps_cnr_su_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 20, 0.04).
narrative_ontology:measurement_basis(cps_cnr_su_t20, observed).
narrative_ontology:measurement(cps_cnr_su_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement_basis(cps_cnr_su_t40, observed).
narrative_ontology:measurement(cps_cnr_su_t60, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement_basis(cps_cnr_su_t60, observed).
narrative_ontology:measurement(cps_cnr_su_t80, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement_basis(cps_cnr_su_t80, observed).
narrative_ontology:measurement(cps_cnr_su_t100, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 100, 0.05).
narrative_ontology:measurement_basis(cps_cnr_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency__hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% Kernel catastrophe_proxy_sufficiency decomposes into four constraint stories with distinct ε and victim/beneficiary structures. This reading (catastrophe_necessity) is the mountain anchor — low extraction, high accessibility collapse, natural law claim. Sibling readings are progressively more extractive: simulation_as_proxy (rope/tangled_rope), hybrid_degradation (tangled_rope), simulation_fidelity_threshold (rope/scaffold depending on technology trajectory). All linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, powerful, 0.2).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organized, 0.25).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
