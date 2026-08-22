% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Reading of Substance Control
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-reduction reading of the
 *   substance_control_kernel: substance use is treated as a chronic health
 *   condition requiring pragmatic intervention to reduce harm, independent of
 *   abstinence. Users exit the criminal victim set but enter a medicalized
 *   governance regime; enforcement recedes on possession and use while the
 *   supply chain remains criminalized. The constraint is structurally
 *   distinct from the prohibition reading (which punishes use as moral
 *   transgression) and the legalization reading (which treats use as
 *   individual liberty).
 *
 * KEY AGENTS:
 *   - Public health system (agenda_setter/institutional/analytical): administers interventions, controls protocols, receives funding and mandate.
 *   - Drug users (payer/powerless/constrained): receive health services but surrender autonomy to paternalistic protocols and clinical monitoring.
 *   - Illicit supply chain (payer/moderate/trapped): remains fully criminalized, bearing legal risk while demand is managed rather than suppressed.
 *   - General public (beneficiary/organized/mobile): receives public health externalities without direct interaction.
 *   - Legislators and regulators (agenda_setter/institutional/analytical): maintain the hybrid regime balancing health intervention against supply prohibition.
 *   - Civil liberties observers (observer/organized/analytical): track autonomy costs of medicalized governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.55).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Reading of Substance Control").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'da9f9db8-72ef-49ab-9235-f7c02da0a05e').
narrative_ontology:cs_kernel_codification('da9f9db8-72ef-49ab-9235-f7c02da0a05e', formalized).
narrative_ontology:cs_authority_grounding('da9f9db8-72ef-49ab-9235-f7c02da0a05e', expertise).
narrative_ontology:cs_interpretation_layer_present('da9f9db8-72ef-49ab-9235-f7c02da0a05e').
narrative_ontology:cs_reading_relation('da9f9db8-72ef-49ab-9235-f7c02da0a05e', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('da9f9db8-72ef-49ab-9235-f7c02da0a05e', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('da9f9db8-72ef-49ab-9235-f7c02da0a05e', foundational, substance_use_medicalization).
narrative_ontology:cs_axiom_status(substance_use_medicalization, holdable).
narrative_ontology:cs_axiom_grounding('da9f9db8-72ef-49ab-9235-f7c02da0a05e', substance_use_medicalization, empirically_contingent).
narrative_ontology:cs_axiom('da9f9db8-72ef-49ab-9235-f7c02da0a05e', foundational, abstinence_independence_principle).
narrative_ontology:cs_axiom_status(abstinence_independence_principle, holdable).
narrative_ontology:cs_axiom_grounding('da9f9db8-72ef-49ab-9235-f7c02da0a05e', abstinence_independence_principle, instrumental).
narrative_ontology:cs_reference_frame('da9f9db8-72ef-49ab-9235-f7c02da0a05e', public_health_pragmatism).
narrative_ontology:cs_drift_state('da9f9db8-72ef-49ab-9235-f7c02da0a05e', contemporary_fentanyl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da9f9db8-72ef-49ab-9235-f7c02da0a05e', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_system).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, general_public).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_supply_chain).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, drug_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers harm reduction programs including needle exchanges, supervised consumption sites, and opioid substitution therapy. Sets clinical protocols, receives dedicated public health funding, and holds expanded mandate under the framing of substance use as a chronic health condition. Controls eligibility criteria and service access.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_system, agenda_setter,
    institutional, generational, analytical, national).

% Receive medicalized services and reduced criminal penalties for personal use, but are subject to health monitoring, substitution requirements, and paternalistic protocols that condition autonomy on compliance with treatment norms. Their behavior is reclassified from criminal to pathological, subjecting them to continuous clinical management.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, drug_users, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, drug_users, beneficiary).

% Remains subject to criminal enforcement, arrest, and asset seizure despite the decriminalization or diversion of users. Bears the full legal risk of a continuing prohibition on production and distribution while demand persists under health-system management.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, illicit_supply_chain, payer,
    moderate, immediate, trapped, national).

% Receives public health externalities including reduced infectious disease transmission and fewer overdose deaths in public spaces. Funds the system through taxation but does not interact with the constraint directly.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Establishes the hybrid legal framework that reframes substance use as a health issue while preserving supply-side criminalization. Allocates budget and institutional authority between criminal justice and public health agencies.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legislators_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Monitors the tension between health-based paternalism and individual autonomy, documenting cases where medical surveillance and conditional service access replicate coercive control under a service-provider framing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_system).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective management of infectious disease and overdose risk among active substance users by providing centralized access to sterile equipment, supervised consumption, substitution pharmacotherapy, and emergency medical response, replacing uncoordinated individual risk management.
% TRANSFER_FUNCTION: Moves public health funding and institutional authority from criminal justice to medical agencies; moves autonomy from active users to health-system protocols; moves legal risk from retail users to supply-chain operators.
% ABSENT_VOICES: Autonomous-user collectives and full-legalization advocates who reject both criminalization and medical paternalism are structurally sidelined; their exclusion enables the framing of policy debate as a binary between health intervention and criminal punishment.
% DISAPPEARANCE_RATIONALE: If the harm-reduction constraint vanished overnight, overdose and infectious disease transmission would rise absent alternative coordination, public health infrastructure would lose its mandate and funding for these services, and the criminal justice system would likely re-expand to fill the vacuum or legalization pressures would intensify.
% FOUNDING_PROBLEM: Unregulated substance use produced concentrated epidemics of infectious disease and preventable overdose death, while mass criminalization of users produced overcrowded courts and prisons without reducing use rates or harms.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists attest the problem remains live. Criminal justice reformers and user-advocacy groups attest the founding problem has partially shifted: overdose is now driven by adulterated supply (a product of prohibition) and disease risk is manageable, suggesting the arrangement persists partly to sustain institutional budgets and professional authority.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: lower than pure prohibition because criminal penalties on users are reduced, but substantial because paternalistic health intervention extracts autonomy and the supply chain remains violently criminalized. Suppression (0.55) reflects the active enforcement required to maintain supply prohibition and the soft coercion embedded in conditional service access. Theater ratio (0.25) is relatively low because the coordination function (disease prevention, overdose reversal) is operational and visible, though some performative maintenance of supply-side enforcement persists for political cover. Accessibility collapse (0.45) is moderate: once the health framing is accepted, alternatives like autonomous use or full legalization are marginalized in policy discourse. Resistance (0.50) reflects ongoing contestation from prohibition advocates, legalization advocates, and some user groups.
 *
 * PERSPECTIVAL GAP:
 *   The public health apparatus experiences this constraint as genuine coordination with moderate overhead; from the user seat it registers as paternalistic management with limited exit; from the supply-chain seat it is indistinguishable from prohibition. The engine computes these divergences from the structural data (role, power, exit, scope) rather than from authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health system and general public sit at low directionality (benefit from reduced disease burden and institutional mandate). Drug users sit at high directionality despite receiving services because the constraint structurally subjects their behavior to clinical management and conditions autonomy on compliance. Illicit supply chain sits at very high directionality (full legal exposure, trapped exit). The legislators sit near the center as regime maintainers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because it possesses both a genuine coordination function (demonstrable reduction in HIV and overdose mortality) and identifiable asymmetric extraction (user autonomy loss, continued supply-chain violence). Neither pure extraction nor pure coordination would capture the hybrid structure. The founding problem is contested: while the original epidemics were real, the current persistence of the arrangement may serve institutional maintenance as much as harm reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalistic_extraction_ambiguity,
    'Is the health-system management of substance use a voluntary service relationship or a coercive paternalistic intervention that extracts autonomy?',
    'Comparative analysis of program design: voluntary-access models versus mandatory-treatment or conditional-benefit models; user-exit surveys and legal autonomy metrics.',
    'If primarily coercive, effective extraction for drug_users is higher than the base metric suggests and the constraint tilts toward snare; if genuinely voluntary, extraction is lower and coordination dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalistic_extraction_ambiguity, conceptual, 'Whether health intervention is voluntary service or coercive paternalism').

omega_variable(
    supply_side_residual_enforcement,
    'Does the continued criminalization of the supply chain represent an inseparable enforcement component of the harm reduction regime, or a politically necessary but structurally distinct adjacent constraint?',
    'Jurisdictional comparison: in regimes that decriminalize or legalize supply alongside harm reduction, does the health intervention function change structurally?',
    'If inseparable, the constraint''s extraction and suppression are higher and the coordination story is partially cover for continued war-on-drugs enforcement; if separable, the health constraint is a cleaner coordination mechanism with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_residual_enforcement, conceptual, 'Whether supply criminalization is integral or adjacent to harm reduction').

omega_variable(
    harm_reduction_stability,
    'Is the harm reduction reading a stable policy equilibrium or a transitional scaffold pressured toward either full legalization or reversion to prohibition?',
    'Longitudinal political economy analysis of jurisdictions with mature harm reduction: do they trend toward legalization, retrenchment, or equilibrium?',
    'If transitional, the constraint should carry scaffold features; if stable, it represents a distinct tangled_rope equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_reduction_stability, empirical, 'Whether harm reduction is a stable equilibrium or transitional scaffold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sc_hr_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sc_hr_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(sc_hr_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(sc_hr_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(sc_hr_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(sc_hr_tr_t25, substance_control_kernel__harm_reduction_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(sc_hr_tr_t30, substance_control_kernel__harm_reduction_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(sc_hr_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sc_hr_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(sc_hr_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(sc_hr_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(sc_hr_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sc_hr_be_t25, substance_control_kernel__harm_reduction_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(sc_hr_be_t30, substance_control_kernel__harm_reduction_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sc_hr_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sc_hr_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(sc_hr_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(sc_hr_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(sc_hr_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(sc_hr_su_t25, substance_control_kernel__harm_reduction_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(sc_hr_su_t30, substance_control_kernel__harm_reduction_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_kernel. It is decomposed from the colloquial label 'drug policy' into three structurally distinct constraints because the prohibition, harm-reduction, and legalization framings have different epsilon values, different beneficiary/victim structures, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
