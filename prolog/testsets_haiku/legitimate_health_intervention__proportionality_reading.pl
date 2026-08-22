% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Constraint on Health Interventions
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of legitimate health intervention grounds
 *   legitimacy in a conditional calculus: intervention severity must match
 *   disease threat (transmissibility + case-fatality rate). Both population
 *   harm and individual autonomy are weighted, but the weight on autonomy
 *   increases as the threat decreases. This reading differs from
 *   bodily_autonomy_primary (which rejects any mandate as illegitimate) and
 *   from public_health_primary (which treats aggregate benefit as overriding
 *   autonomy). The proportionality reading is one specific framing of when
 *   the state may legitimately override medical choice. The constraint's
 *   extractiveness (0.52 at interval end) reflects real coordination benefit
 *   (disease prevention) coupled with genuine burden (forced medical
 *   intervention): tangled rope, not pure snare.
 *
 * KEY AGENTS:
 *   - public_health_authority — sets epidemiological thresholds; enforces through licensing and conditional access
 *   - population_at_risk_high_transmissibility — trapped beneficiaries, cannot opt out of exposure
 *   - vaccine_hesitant_individuals — constrained payers, contest proportionality judgment
 *   - medical_exemption_claimants — identity-locked payers at the conditional boundary
 *   - clinical_epidemiologists — observers whose threat assessment drives the threshold
 *   - bodily_autonomy_advocates — excluded from the proportionality frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.52).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.48).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Constraint on Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '9e00128e-c4bc-45c6-adca-524bda536507').
narrative_ontology:cs_kernel_codification('9e00128e-c4bc-45c6-adca-524bda536507', distributed).
narrative_ontology:cs_authority_grounding('9e00128e-c4bc-45c6-adca-524bda536507', lineage).
narrative_ontology:cs_interpretation_layer_present('9e00128e-c4bc-45c6-adca-524bda536507').
narrative_ontology:cs_reading_relation('9e00128e-c4bc-45c6-adca-524bda536507', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('9e00128e-c4bc-45c6-adca-524bda536507', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_axiom('9e00128e-c4bc-45c6-adca-524bda536507', foundational, proportionality_principle_constitutional).
narrative_ontology:cs_axiom_status(proportionality_principle_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('9e00128e-c4bc-45c6-adca-524bda536507', proportionality_principle_constitutional, conventional).
narrative_ontology:cs_axiom('9e00128e-c4bc-45c6-adca-524bda536507', foundational, dual_weighting_autonomy_and_population_benefit).
narrative_ontology:cs_axiom_status(dual_weighting_autonomy_and_population_benefit, holdable).
narrative_ontology:cs_axiom_grounding('9e00128e-c4bc-45c6-adca-524bda536507', dual_weighting_autonomy_and_population_benefit, instrumental).
narrative_ontology:cs_reference_frame('9e00128e-c4bc-45c6-adca-524bda536507', proportional_health_intervention_doctrine).
narrative_ontology:cs_drift_state('9e00128e-c4bc-45c6-adca-524bda536507', post_pandemic_threshold_reassessment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9e00128e-c4bc-45c6-adca-524bda536507', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, population_at_risk_high_transmissibility).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authority).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, medical_exemption_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets thresholds for mandatory intervention based on disease epidemiology (transmissibility, case-fatality rate, available alternatives). Justifies the threshold as a proportionality calculus: intervention severity calibrated to threat magnitude. Enforces through licensing, school attendance rules, and conditional access to public goods.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from mandatory vaccination/intervention when the disease has high transmissibility and case-fatality (measles, pertussis). Cannot opt out of exposure; their protection depends on others complying with the mandate. Lacks voice in threshold-setting.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, population_at_risk_high_transmissibility, beneficiary,
    powerless, biographical, trapped, national).

% Subject to mandatory intervention even when disease risk is low (flu-like illnesses with minimal case-fatality). They contest the proportionality judgment: argue the intervention is more severe than the threat warrants. Their medical choices are constrained by state-enforced thresholds they did not participate in setting.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Have documented medical or religious reasons for refusing intervention. The proportionality reading treats their refusal as legitimate when disease threat is low or alternatives exist, but illegitimate when threat is high. They occupy the conditional boundary: sometimes payer, sometimes integrated, depending on epidemiological thresholds they cannot control.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_exemption_claimants, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, medical_exemption_claimants, excluded).

% Provide the empirical input (transmissibility, case-fatality, intervention side-effect burden) that the proportionality calculus depends on. Their assessment is the mechanism by which intervention severity is calibrated to threat. Analytically central; institutionally removed from the enforcement chain.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, clinical_epidemiologists, observer,
    institutional, biographical, analytical, global).

% Object to any mandated medical intervention as a violation of bodily integrity, regardless of proportionality. Would argue for informed consent as the binding constraint instead. Structurally excluded from the proportionality reading's framework — their core premise (autonomy trumps aggregate harm) is not engaged by the proportionality calculus.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Organize resistance to mandatory health interventions; challenge both the epidemiological inputs and the legitimacy of the threshold-setting process itself. Their exit option is political mobilization and litigation; they sit outside the proportionality frame.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individual_liberty_coalitions, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns intervention severity to disease threat magnitude: population protection through vaccination/intervention is genuine coordination; the proportionality constraint calibrates enforcement intensity to epidemiological facts rather than blanket mandates, solving the joint problem of reducing disease spread while respecting individual medical choice at low-threat boundaries.
% TRANSFER_FUNCTION: Moves bodily autonomy from individuals to the state when disease threat is high (transmissibility + case-fatality cross a threshold); the proportionality reading transfers less bodily autonomy when threat is low, creating a conditional transfer structure based on epidemiological parameters.
% ABSENT_VOICES: Bodily autonomy advocates and anti-mandate coalitions are structurally excluded from the proportionality reading's framework — they would argue that bodily integrity cannot be traded off against population benefit at any threat level, and that proportionality as a calculus is illegitimate. Their objection is not a measurement dispute but a framing rejection.
% DISAPPEARANCE_RATIONALE: If the proportionality constraint vanished, public health authorities would revert to threat-agnostic mandates or ad-hoc thresholds; disease-specific policy would disappear, replaced by either blanket intervention (autonomy advocates' fear) or no coordination (public health advocates' fear). The contested verdict reflects genuine disagreement on what the world would look like: beneficiaries of herd immunity see coordination collapse; autonomy advocates see liberation.
% FOUNDING_PROBLEM: Early pandemic response revealed a coordination failure: public health authorities imposed identical intervention burdens for diseases with vastly different threat profiles (mandatory quarantine for flu-equivalent illness; mandatory vaccination for low-transmissibility endemic disease). No mechanism existed to weight individual autonomy against population benefit conditional on epidemiological facts.
% FOUNDING_PROBLEM_CORROBORATION: Clinical epidemiologists, constitutional law scholars studying proportionality doctrine (European jurisprudence, Canadian Charter cases), and public health ethicists from outside the advocacy camps attest the problem remains: threshold-setting remains contested and evidence-resistant. Public health authorities attest thresholds are evidence-based; bodily autonomy advocates attest the problem is misidentified — the real problem is that no burden justifies forced medical intervention.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, contested).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint does coordinate genuine population protection AND legitimately constrains individual choice at low-threat boundaries — this is neither pure extraction nor pure coordination, but asymmetric: beneficiaries gain protection they cannot individually exit; payers bear intervention burden calibrated to threat but lose input into threshold-setting. Suppression (0.48) is moderate because the constraint operates through rule enforcement, not internalized belief — resistance is real (59% at interval end) and growing. Theater (0.22, increasing to peak 0.23) is low because the proportionality calculus is genuinely evidence-dependent: when threat re-assessments lower, thresholds adjust downward, showing functional calibration rather than pure performance. The measurement series show extractiveness rising then stabilizing (peaking at t=15, declining slightly at t=20) — this reflects post-pandemic normalization: early overreach followed by evidence-driven correction, consistent with a working proportionality mechanism rather than institutional capture. Time points are aligned across all three metrics: every metric is authored at 0, 5, 10, 15, 20.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority's seat, the proportionality reading is a genuine constraint on legitimate action: it prevents threat-agnostic mandates and requires continuous calibration to epidemiological facts. From vaccine_hesitant_individuals' seats, the same structure is extractive overreach: the threshold for 'high threat' is set by a body they cannot influence, and 'proportionality' becomes cover for suppression. From bodily_autonomy_advocates' seat, the entire proportionality reading is incoherent — there is no proportional threshold that justifies forced medical intervention. The engine computes these divergent classifications from the structural data (power, exit, beneficiary/victim status) without resolving the disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority: d near beneficiary end (0.1–0.2); they set and enforce thresholds, collect the legitimacy dividend of evidence-based policy. Population at risk: d near beneficiary end (0.15–0.25); they gain protection, cannot exit, but their preference for protection is presumed rather than expressed. Vaccine hesitant: d near target end (0.65–0.80); constrained exit, forced intervention, no threshold-setting voice. Medical exemption claimants: d conditional on disease severity — when measles (high threat): d→0.75 (target); when flu (low threat): d→0.40 (moderate burden accepted as proportional). Bodily autonomy advocates: excluded from the framework, not computed on the directionality grid.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows NO signs of mandatrophy. The founding problem (unequal intervention burden across vastly different threat profiles) remains live and actively managed: threshold-setting continues to incorporate epidemiological updates, intervention burdens fluctuate with disease prevalence, and resistance is genuine and visible. Theater ratio remains low (0.22), indicating the proportionality calculus drives actual policy choices, not ceremonial justification. If theater_ratio had risen above 0.5 with extractiveness stable, that would signal cover-story drift (mandatrophy candidate); instead, both extractiveness and suppression show evidence-responsive movement. The constraint is a working tangled rope, not a degraded shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_specification_ambiguity,
    'What transmissibility/case-fatality thresholds convert a low-threat disease from ''individual choice'' to ''mandated intervention''? Is there a defensible, empirically stable boundary, or does the threshold depend on political/institutional factors not captured by epidemiology?',
    'Comparative analysis of threshold-setting across jurisdictions with access to identical epidemiological data: do they converge on similar thresholds? If not, the threshold is not epidemiologically determined. Longitudinal analysis of threshold changes within a jurisdiction: do thresholds shift when epidemiology shifts, or do they remain stable despite new evidence?',
    'If thresholds are empirically grounded and responsive to evidence, the proportionality reading is a working constraint on legitimate action. If thresholds are sticky or divergent despite evidence, the proportionality framing becomes a legitimation cover for institutional capture — the constraint becomes snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_specification_ambiguity, empirical, 'Whether proportionality thresholds are evidence-determined or institutionally constructed.').

omega_variable(
    reading_foreclosure_bodily_autonomy,
    'Does the proportionality reading logically foreclose the bodily autonomy reading? That is, if one accepts that SOME proportional level of coercion is legitimate, does that commitment necessarily reject the claim that bodily integrity is inviolable?',
    'Philosophical analysis: can a coherent framework hold both (a) bodily integrity as a fundamental right, and (b) proportional coercion as legitimate under specified conditions? Or do these premises necessarily contradict?',
    'If they foreclose each other, the two readings cannot coexist in a single coherent framework — this is a logical gate, not a political disagreement. If they coexist (both are live positions people can hold), the classification shifts to coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_bodily_autonomy, conceptual, 'Whether proportionality reading and bodily autonomy reading are logically incompatible or merely competing values.').

omega_variable(
    epidemiological_uncertainty_in_proportionality,
    'When epidemiological estimates are uncertain (case-fatality rate has a wide confidence interval, transmissibility models diverge), how is the proportionality threshold set? Is it set conservatively (assume high threat), liberally (assume low threat), or via some intermediate rule?',
    'Document actual decision-making in the presence of epidemiological uncertainty: what thresholds were adopted when data were ambiguous? Post-hoc analysis: when uncertainty resolved, did policy adjust in the predicted direction?',
    'Conservative threshold-setting (assume high threat) favors population protection but increases suppression of hesitant individuals. Liberal setting (assume low threat) favors autonomy but increases disease burden. The choice reveals whether ''proportionality'' is applied symmetrically or with a systematic bias toward one value over the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epidemiological_uncertainty_in_proportionality, empirical, 'Whether uncertainty in epidemiological estimates biases the proportionality calculus.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.48 at interval end) primarily structural (coercion through licensing and access rules) or internalized (hesitant individuals have absorbed health authority judgment as legitimate)? Would suppression persist after the licensing rules were removed?',
    'Natural experiment: if a jurisdiction removes mandatory vaccination requirements, do vaccine acceptance rates fall, remain stable, or shift based on other factors? Compare pre- and post-removal hesitancy among the same cohorts.',
    'If suppression is structural, removing the rules should reduce measured suppression. If largely internalized, hesitancy may persist because individuals have adopted health authority thresholds as their own. This distinguishes a working constraint (suppression as necessary enforcement) from one that has crossed into internalized legitimacy crisis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression in vaccine resistance is structural coercion or internalized institutional belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__proportionality_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__proportionality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__proportionality_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__proportionality_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(legi_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__proportionality_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__proportionality_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(legi_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__proportionality_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__proportionality_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(legi_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__proportionality_reading, 0.18).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimate_health_intervention kernel. The sibling readings bodily_autonomy_primary and public_health_primary are separate constraint stories, each instantiating a different framing of when state coercion in medical choice is legitimate. All three stories share the same referent (mandated vaccination/intervention policy) but differ in what counts as legitimacy: proportionality (this story), bodily integrity (sibling), population benefit (sibling). The readings coexist as live positions held by different institutional and advocacy seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, powerless, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
