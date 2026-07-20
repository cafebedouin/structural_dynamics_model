% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the absolute_non_intervention reading of the
 *   contested westphalia_sovereignty kernel. Under this reading, territorial
 *   sovereignty is categorical and per se inviolable regardless of a state's
 *   internal conduct. The norm coordinates the interstate system by
 *   preventing constant territorial revisionism, but simultaneously extracts
 *   from civilian populations under authoritarian control by foreclosing
 *   external humanitarian intervention. The structural asymmetry is sharp:
 *   state elites claim territorial monopoly and benefit from impunity, while
 *   at-risk populations are denied a voice in the international rules that
 *   seal their fate. The claim/metric gap is deliberate â the constraint is
 *   claimed by its beneficiaries as necessary coordination while the authored
 *   metrics capture the substantial extraction and active enforcement that
 *   shield atrocity.
 *
 * KEY AGENTS:
 *   - state_elites: Primary agenda-setter and beneficiary (institutional/arbitrage) â claim territorial monopoly, enforce non-intervention reciprocally, and collect impunity.
 *   - civilian_populations_at_risk: Primary target (powerless/trapped) â bear the cost of foreclosed intervention during mass atrocities.
 *   - r2p_advocacy_coalition: Excluded voice (organized/constrained) â articulates conditional-responsibility alternative but is structurally marginalized in binding decision fora.
 *   - great_power_veto_holders: Secondary agenda-setter/beneficiary (institutional/arbitrage) â uphold the norm asymmetrically, retaining capacity to block enforcement against themselves or allies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.6).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '1ed9fec5-8a7a-459f-9e9d-921994897167').
narrative_ontology:cs_kernel_codification('1ed9fec5-8a7a-459f-9e9d-921994897167', formalized).
narrative_ontology:cs_authority_grounding('1ed9fec5-8a7a-459f-9e9d-921994897167', lineage).
narrative_ontology:cs_interpretation_layer_present('1ed9fec5-8a7a-459f-9e9d-921994897167').
narrative_ontology:cs_reading_relation('1ed9fec5-8a7a-459f-9e9d-921994897167', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('1ed9fec5-8a7a-459f-9e9d-921994897167', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('1ed9fec5-8a7a-459f-9e9d-921994897167', foundational, non_intervention_categorical_norm).
narrative_ontology:cs_axiom_status(non_intervention_categorical_norm, holdable).
narrative_ontology:cs_axiom_grounding('1ed9fec5-8a7a-459f-9e9d-921994897167', non_intervention_categorical_norm, conventional).
narrative_ontology:cs_axiom('1ed9fec5-8a7a-459f-9e9d-921994897167', foundational, sovereign_equality_non_forfeitable).
narrative_ontology:cs_axiom_status(sovereign_equality_non_forfeitable, holdable).
narrative_ontology:cs_axiom_grounding('1ed9fec5-8a7a-459f-9e9d-921994897167', sovereign_equality_non_forfeitable, conventional).
narrative_ontology:cs_reference_frame('1ed9fec5-8a7a-459f-9e9d-921994897167', westphalian_mutual_recognition_order).
narrative_ontology:cs_drift_state('1ed9fec5-8a7a-459f-9e9d-921994897167', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ed9fec5-8a7a-459f-9e9d-921994897167', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, great_power_veto_holders).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, civilian_populations_at_risk).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, un_charter_article_2_7).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, sovereign_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control state apparatus and claim territorial monopoly. They formulate, defend, and diplomatically enforce the absolute non-intervention norm through UN forums, treaty practice, and reciprocal recognition. They benefit from impunity for internal conduct and from the institutional barrier to external interference.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, state_elites, beneficiary).

% Live under governments committing or permitting mass atrocities. They are structurally denied external intervention because the absolute non-intervention norm treats their suffering as a domestic matter. Exit via refugee flows is costly, dangerous, and often blocked by the same sovereign control.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, civilian_populations_at_risk, payer,
    powerless, immediate, trapped, national).

% Humanitarian organizations, international lawyers, and victim-advocacy groups that promote the Responsibility to Protect and conditional sovereignty. They are present in UN discourse but their preferred norm is structurally excluded by the absolute non-intervention reading from binding enforcement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, r2p_advocacy_coalition, excluded,
    organized, biographical, constrained, global).

% Permanent members of the UN Security Council who formally uphold the non-intervention norm while retaining the unilateral capacity to block enforcement actions against themselves or allies. They derive structural benefit from a rule that constrains weaker states more reliably than it constrains them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, great_power_veto_holders, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, great_power_veto_holders, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constant interstate warfare by establishing territorial boundaries as inviolable, allowing states to govern without perpetual external interference, and creating a stable recognition order for diplomatic relations.
% TRANSFER_FUNCTION: Transfers security and accountability from civilian populations at risk of domestic atrocities to the state elites controlling territory, by removing external intervention as a check on internal conduct.
% ABSENT_VOICES: Civilian populations under authoritarian control are excluded from the international conversation that defines sovereignty; their preference for external protection over domestic impunity is structurally unrepresented. Humanitarian advocacy coalitions speak on their behalf but are marginalized in binding decision-making.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention norm disappeared, the barrier to humanitarian intervention would collapse, great-power spheres of influence would destabilize as reciprocal territorial guarantees eroded, and state elites would face external accountability for internal conduct â the entire architecture of interstate recognition and impunity would reorganize.
% FOUNDING_PROBLEM: The Wars of Religion and Thirty Years' War demonstrated that confessional interference across borders produced protracted, catastrophic conflict; a system was needed to insulate domestic governance from external coercion.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians of the Thirty Years' War attest the severity of the founding conflict, but international relations scholars and human rights advocates outside the state-elite beneficiary set attest that the contemporary problem structure has shifted to internal atrocity, making the absolute form obsolescent.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the absolute form of the norm decouples territorial inviolability from internal conduct, creating a systematic transfer of security from populations to elites. Suppression (0.60) reflects the active institutional work required to marginalize R2P and conditional-sovereignty alternatives through UNSC vetoes, non-intervention doctrines, and diplomatic pressure. Theater ratio (0.45) captures the growing performative dimension: states ritualistically invoke sovereignty while great powers intervene selectively, yet the norm remains functionally enforced against weaker states and humanitarian actors. Accessibility collapse (0.50) registers that alternatives like humanitarian intervention have been articulated and partially institutionalized but are blocked at the enforcement stage. Resistance (0.55) reflects sustained advocacy from human rights networks and some democratic states. The temporal series shows gradual intensification as decolonization entrenched the norm, the Cold War froze exceptions, and post-1990 R2P was captured by veto politics.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elite seat, the constraint appears as the indispensable foundation of interstate peace â without it, great powers would constantly redraw borders by force. From the at-risk-population seat, the same constraint appears as a sealed door during genocide or ethnic cleansing. The engine computes this divergence from the structural data: the state elite has generational time-horizon and arbitrage-grade exit, while the at-risk population is trapped with immediate stakes. The r2p_advocacy_coalition observes the full structure but is excluded from the agenda-setting that could alter it.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and great-power veto holders are structural beneficiaries: the constraint subsidizes their territorial control and domestic impunity. Civilian populations at risk are structural targets: they bear the effective extraction in the form of denied rescue. The r2p advocacy coalition sits between â not a direct payer, but a constrained mobilizer whose preferred alternative is suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than rope is driven by the co-presence of a genuine coordination function (territorial stability) with asymmetric extraction (atrocity impunity). If we classified it as rope, we would miss the victim population entirely. If we classified it as snare, we would erase the real coordination benefit that prevents constant interstate warfare. The temporal measurements show extraction accumulating over time as the founding problem (religious war) became obsolete and the norm was repurposed to shield post-colonial and authoritarian consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_operativity,
    'Does contemporary international practice still treat sovereignty as categorically absolute, or has the kernel drifted toward conditional or graded readings in operative state behavior?',
    'Systematic review of UN Security Council deliberations, state reactions to humanitarian crises, and ICJ jurisprudence on intervention to distinguish rhetorical adherence from behavioral exceptions.',
    'If practice has drifted conditional, the absolute reading''s high extraction is largely performative and its classification shifts toward piton; if absolute practice remains operative, the extraction is actively enforced and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operativity, empirical, 'Whether the absolute non-intervention reading remains operative or is now theater').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination benefit of territorial stability separable from the extraction of elite impunity, or does the absolute form of the norm structurally require both?',
    'Comparative analysis of sovereignty regimes that incorporate humanitarian exceptions versus absolute regimes, measuring territorial stability outcomes and internal atrocity rates.',
    'If separable, the absolute reading is a tangled rope layering extraction onto genuine coordination; if inseparable, the extraction is the necessary price of the coordination and effective extractiveness is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether territorial coordination and elite impunity are structurally separable').

omega_variable(
    great_power_enforcement_asymmetry,
    'Does the absolute non-intervention norm constrain great powers symmetrically, or is it enforced primarily against weaker states while powerful states retain intervention capacity?',
    'Quantitative analysis of intervention patterns by great powers versus weaker states, and veto usage to shield allies from enforcement action.',
    'If enforcement is asymmetric, the norm functions as a snare for weak states and their populations while being a rope for great powers, suggesting inter-institutional divergence in classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(great_power_enforcement_asymmetry, empirical, 'Whether the norm''s enforcement is structurally asymmetric by power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_abs_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.25).
narrative_ontology:measurement(westphalia_abs_tr_t10, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 10, 0.28).
narrative_ontology:measurement(westphalia_abs_tr_t20, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 20, 0.32).
narrative_ontology:measurement(westphalia_abs_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.35).
narrative_ontology:measurement(westphalia_abs_tr_t40, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 40, 0.38).
narrative_ontology:measurement(westphalia_abs_tr_t50, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 50, 0.4).
narrative_ontology:measurement(westphalia_abs_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.42).
narrative_ontology:measurement(westphalia_abs_tr_t70, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 70, 0.44).
narrative_ontology:measurement(westphalia_abs_tr_t80, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(westphalia_abs_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(westphalia_abs_be_t10, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(westphalia_abs_be_t20, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(westphalia_abs_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(westphalia_abs_be_t40, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(westphalia_abs_be_t50, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(westphalia_abs_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(westphalia_abs_be_t70, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(westphalia_abs_be_t80, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_abs_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(westphalia_abs_su_t10, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(westphalia_abs_su_t20, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(westphalia_abs_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(westphalia_abs_su_t40, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(westphalia_abs_su_t50, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(westphalia_abs_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.57).
narrative_ontology:measurement(westphalia_abs_su_t70, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 70, 0.59).
narrative_ontology:measurement(westphalia_abs_su_t80, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 80, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, identity_coordination).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalia_sovereignty kernel. The natural-language label 'Westphalian sovereignty' conflates three structurally distinct claims: absolute non-intervention (categorical inviolability, high extraction from at-risk populations), conditional responsibility (atrocities trigger forfeiture, moderate extraction with exception mechanisms), and graded sovereignty (scalar capacity, extraction calibrated to state weakness). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
