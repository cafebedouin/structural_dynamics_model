% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Catastrophe Necessity for Genuine Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'catastrophe_necessity_reading' of
 *   the contested kernel 'catastrophe_proxy_sufficiency'. The reading asserts
 *   a categorical Mountain: only actual catastrophic events generate the
 *   irreducible stress and uncertainty necessary for genuine competence;
 *   simulation is fundamentally insufficient regardless of fidelity. The
 *   constraint extracts from operational safety margins — competence decays
 *   during catastrophe-free periods when organizations substitute simulation
 *   for real experience. The claimed_type is 'mountain' with
 *   emerges_naturally=true, but declared beneficiaries
 *   (catastrophe_experience_advocates, traditional_hro_practitioners) and
 *   victims (operational_safety_margins, high_reliability_organizations)
 *   create a False Summit Mountain candidate. The engine will evaluate
 *   whether this is a genuine natural law or a constructed constraint
 *   benefiting identifiable agents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.72).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Genuine Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'ffac27d2-d4bf-4cfb-b4de-f39685231a8e').
narrative_ontology:cs_kernel_codification('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', distributed).
narrative_ontology:cs_authority_grounding('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', practice).
narrative_ontology:cs_interpretation_layer_present('ffac27d2-d4bf-4cfb-b4de-f39685231a8e').
narrative_ontology:cs_reading_relation('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_axiom('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', foundational, catastrophe_irreducible_stress_necessary).
narrative_ontology:cs_axiom_status(catastrophe_irreducible_stress_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', catastrophe_irreducible_stress_necessary, empirically_contingent).
narrative_ontology:cs_axiom('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', foundational, simulation_fundamentally_inadequate_for_tacit_knowledge).
narrative_ontology:cs_axiom_status(simulation_fundamentally_inadequate_for_tacit_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', simulation_fundamentally_inadequate_for_tacit_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', pre_simulation_era_competence_paradigm).
narrative_ontology:cs_drift_state('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', contemporary_simulation_advancement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ffac27d2-d4bf-4cfb-b4de-f39685231a8e', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_experience_advocates).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, traditional_hro_practitioners).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, irreducible_uncertainty_principle).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, stress_inoculation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The buffer of competence and readiness that erodes during catastrophe-free periods when organizations rely on simulation. This margin bears the full cost of the constraint: when real catastrophe eventually occurs, degraded competence leads to worse outcomes. It cannot exit the constraint — it is the substrate on which the constraint operates.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, biographical, trapped, global).

% Organizations in nuclear, aviation, chemical, and healthcare domains that must maintain competence for rare catastrophic events. They pay through competence decay during quiet periods and through the cost of maintaining readiness without real events. They benefit from the constraint's warning function but are trapped by the structural impossibility of generating real catastrophes for training. Exit would mean accepting lower safety standards or investing in unproven simulation alternatives.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations, beneficiary).

% Safety professionals and theorists who argue that only real catastrophe experience produces genuine competence. They benefit intellectually and professionally from the constraint's validation — their expertise in post-catastrophe analysis and veteran knowledge transfer is valued precisely because simulation is deemed insufficient. They can exit by shifting to simulation-fidelity research.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_experience_advocates, beneficiary,
    moderate, biographical, mobile, national).

% Senior operators, trainers, and institutional leaders whose professional identity and authority are fused with catastrophe-derived competence. They set training agendas, certification standards, and regulatory expectations around the necessity of real-event experience. Their identity_locked exit reflects that abandoning the catastrophe-necessity frame would dissolve the epistemic basis of their authority and career-long practice.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, traditional_hro_practitioners, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, traditional_hro_practitioners, agenda_setter).

% Companies developing high-fidelity simulators, VR training systems, and synthetic environment platforms. They are structurally excluded from the competence-maintenance conversation because the constraint declares their products categorically insufficient. They would argue for simulation sufficiency but are kept out by the categorical natural-law claim. They have arbitrage-grade exit — they can pivot to adjacent markets (gaming, entertainment, non-safety-critical training).
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_technology_vendors, excluded,
    powerful, biographical, arbitrage, global).

% Regulatory bodies (NRC, FAA, OSHA, etc.) that set training requirements and evaluate competence standards. They observe the constraint's operation through incident investigations and license renewals. They cannot easily change the constraint because it is presented as a natural limit, not a regulatory choice. Their analytical exit allows them to commission research but not to override the claimed physical/psychological limit.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of genuine competence for rare catastrophic events across generations of operators by establishing that only real catastrophe experience provides the irreducible stress/uncertainty needed — preventing false confidence from simulation-only training.
% TRANSFER_FUNCTION: Transfers the burden of competence decay from the simulation industry (which would profit from selling sufficiency) onto operational safety margins and HROs, which must absorb the cost of degraded readiness during catastrophe-free periods and invest in veteran knowledge transfer, disaster drills, and cross-domain learning to compensate.
% ABSENT_VOICES: Simulation technology vendors and next-generation operators who have never experienced a real catastrophe are structurally excluded. Vendors would argue for fidelity-threshold sufficiency; junior operators would question why they must wait for catastrophe to gain 'genuine' competence. Both are absent from the authority structure that defines competence standards.
% DISAPPEARANCE_RATIONALE: If the catastrophe-necessity constraint vanished overnight, HROs would rapidly shift to simulation-dominant training regimens, certification standards would rewrite around fidelity metrics, the veteran knowledge-transfer economy would collapse, and safety margins would initially rise (more training volume) then unpredictably fall when the first real catastrophe reveals whether simulation truly sufficed. The entire competence-maintenance architecture would reorganize.
% FOUNDING_PROBLEM: How to maintain genuine operational competence for catastrophic events that occur too rarely for any individual operator to experience directly, yet frequently enough that societal survival depends on readiness.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by nuclear industry veterans (Three Mile Island, Chernobyl, Fukushima), aviation safety boards (post-accident competence analyses), and organizational learning scholars (Weick, Sutcliffe, Perrow) from outside the direct beneficiary set. However, simulation-fidelity proponents (NASA's simulation divisions, modern VR training consortia) contest that the problem remains live, citing advances in stress-inoculation fidelity. No consensus exists across the kernel's readings.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Extractiveness (0.72) is high because the standing arrangement — organizations relying on simulation during catastrophe-free periods — systematically degrades safety margins, transferring risk to future operations. Suppression (0.58) is moderate: the constraint suppresses simulation-sufficiency alternatives not through direct coercion but by defining them as categorically impossible (natural law claim). Theater_ratio (0.32) rises over time as simulation exercises increasingly perform 'catastrophe-like' scenarios without the irreducible stakes. Accessibility_collapse (0.88) is near-mountain level: if the constraint is true, no alternative can work; if false, the entire simulation industry is a viable alternative. Resistance (0.12) is low because the constraint is presented as physics/psychology, not policy — few actively resist a claimed natural limit.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional_hro_practitioner seat, the constraint appears as a Mountain — a natural law they've built careers around. From the operational_safety_margins seat, it appears as a Snare — competence decays inexorably without real catastrophes, and the constraint offers no escape. From the simulation_vendor seat, it appears as a Snare's enforcement mechanism — their exclusion is the suppression that maintains the constraint. The engine computes this seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Operational_safety_margins and high_reliability_organizations are payers (d near 1.0): they bear competence decay and cannot exit the need for genuine competence. Catastrophe_experience_advocates are beneficiaries (d near 0.0): their expertise gains value from the constraint's validation. Traditional_hro_practitioners are identity_locked beneficiaries/agenda_setters (d ~0.15): their professional identity fuses with catastrophe-derived authority, making exit unthinkable. Simulation_vendors are excluded (trapped by the categorical claim, but with arbitrage exit to other markets). Regulators are analytical observers (d=0.5). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence for rare catastrophes) is contested — simulation proponents argue it is substantially solved. The constraint persists despite this contestation, suggesting mandatrophy: the original mandate (learn from real catastrophes) has expanded into a categorical prohibition on simulation sufficiency that benefits the veteran knowledge-transfer economy. The rising theater_ratio and extractiveness over the interval support this — simulation exercises increasingly simulate the simulation of catastrophe rather than building genuine stress-response capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine Mountain (natural law of human psychology/physics) or a constructed Snare/Tangled Rope benefiting catastrophe_experience_advocates and traditional_hro_practitioners?',
    'Longitudinal study of organizations that transition to simulation-dominant training without real catastrophes: if competence metrics hold, the natural-law claim is falsified; if they degrade catastrophically, the Mountain claim is supported.',
    'If Mountain: the constraint is immutable, classification stands, beneficiaries are incidental. If constructed: FSM triggers reclassification to tangled_rope (coordination + extraction) or snare (pure extraction), beneficiaries become structural evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Natural-law vs. constructed status of the catastrophe-necessity claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression of simulation alternatives structural (regulatory standards, certification requirements) or internalized (practitioners genuinely believe simulation cannot work, self-censor research)?',
    'Post-exit suppression trajectory: track organizations that adopt high-fidelity simulation despite the constraint — if suppression persists (regulatory pushback, insurance penalties), it''s structural; if they succeed and the constraint weakens, it was internalized.',
    'If internalized, effective suppression is higher than structural measures suggest — practitioners carry the constraint with them. If structural, regulatory reform could reduce suppression without changing the constraint''s epistemic claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression of simulation alternatives').

omega_variable(
    competence_decay_measurement,
    'Can ''genuine competence'' and its decay be measured independently of catastrophe occurrence, or is the constraint tautological (competence is defined by catastrophe performance)?',
    'Develop proxy metrics for stress-response capacity (physiological markers, decision latency under uncertainty, team coordination entropy) and track them in catastrophe-free periods across simulation-dominant vs. veteran-rich organizations.',
    'If measurable independently, extractiveness can be quantified without waiting for catastrophe. If tautological, the constraint is unfalsifiable — a hallmark of constructed Mountains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_measurement, empirical, 'Operationalizability of competence decay without catastrophe occurrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cps_cnr_tr_t1970, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cps_cnr_tr_t1985, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(cps_cnr_tr_t1995, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(cps_cnr_tr_t2005, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(cps_cnr_tr_t2015, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(cps_cnr_tr_t2025, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(cps_cnr_be_t1970, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(cps_cnr_be_t1985, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(cps_cnr_be_t1995, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(cps_cnr_be_t2005, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(cps_cnr_be_t2015, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(cps_cnr_be_t2025, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cps_cnr_su_t1970, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(cps_cnr_su_t1985, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(cps_cnr_su_t1995, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(cps_cnr_su_t2005, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(cps_cnr_su_t2015, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(cps_cnr_su_t2025, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. It claims categorical Mountain status (natural law). The simulation_fidelity_threshold reading claims technology-dependent sufficiency (Tangled Rope or Rope). The hybrid_degradation_reading claims generational degradation (Tangled Rope). The simulation_as_proxy_catastrophe_reading claims categorical sufficiency (Rope or Mountain). They form a constraint family linked by the kernel. This reading's high extractiveness and declared beneficiaries make it an FSM candidate; the siblings' lower extractiveness would compute differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organized, 0.75).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
