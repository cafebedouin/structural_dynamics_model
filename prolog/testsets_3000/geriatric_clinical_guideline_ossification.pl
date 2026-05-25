% ============================================================================
% CONSTRAINT STORY: geriatric_clinical_guideline_ossification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geriatric_clinical_guideline_ossification, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geriatric_clinical_guideline_ossification
 *   human_readable: Geriatric Clinical Guideline Ossification
 *   domain: healthcare_policy/geriatric_medicine
 *
 * SUMMARY:
 *   Geriatric clinical guideline ossification represents a constraint where
 *   coordination mechanisms designed for population-level disease management
 *   become extractive when applied rigidly to complex elderly patients with
 *   multiple comorbidities, cognitive limitations, and competing care goals.
 *   Guidelines originally emerged as genuine coordination solutions: they
 *   standardized care, reduced variation, enabled training, and aligned
 *   incentives toward evidence-based treatment. However, over time, the
 *   theatrical elements (defensive documentation, institutional gatekeeping
 *   of deviation, malpractice liability systems) have accumulated while the
 *   genuine coordination function has ossified. Bedside clinicians
 *   increasingly follow guidelines not because they optimize patient outcomes
 *   but because deviating creates documentation burden and liability risk.
 *   Elderly patients receive medically inappropriate combinations of
 *   medications justified by protocol adherence. Guideline-maintaining
 *   institutions benefit from simplified liability exposure and standardized
 *   operations. Reform movements are building alternative coordination
 *   mechanisms (shared decision-making, deprescribing protocols,
 *   individualized care frameworks) with explicit permission for guideline
 *   deviation. The constraint exhibits all six DR types from different
 *   structural positions, making it a diagnostic exemplar for how
 *   institutional inertia converts coordination into extraction.
 *
 * KEY AGENTS:
 *   - Elderly Patients with Comorbidities: Primary victims (powerless/trapped) — receive guideline-mandated polypharmacy and protocols designed for simpler cases; cannot exit or refuse without violating care standards
 *   - Bedside Clinicians: Secondary victims/mixed (moderate/constrained) — genuinely enabled by guidelines but also constrained by liability and documentation requirements; can deviate but at cost
 *   - Guideline-Maintaining Institutions: Primary beneficiaries (institutional/arbitrage) — hospitals, professional societies, accreditation bodies that benefit from standardized protocols and simplified liability exposure
 *   - Malpractice Liability System: Enforcer (institutional/arbitrage) — creates defensive documentation incentives that ossify guideline adherence; can exit whenever insurance/legal regimes change
 *   - Geriatric Medicine Reform Movement: Organized challengers (organized/constrained) — geriatricians, patient advocates, health services researchers building alternative coordination mechanisms with scaffold logic
 *   - Guideline Committees: Institutional gatekeepers (institutional/arbitrage) — control what counts as evidence for guideline updates; benefit from slow update cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geriatric_clinical_guideline_ossification, 0.58).
domain_priors:suppression_score(geriatric_clinical_guideline_ossification, 0.65).
domain_priors:theater_ratio(geriatric_clinical_guideline_ossification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geriatric_clinical_guideline_ossification, extractiveness, 0.58).
narrative_ontology:constraint_metric(geriatric_clinical_guideline_ossification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geriatric_clinical_guideline_ossification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geriatric_clinical_guideline_ossification, tangled_rope).
narrative_ontology:human_readable(geriatric_clinical_guideline_ossification, "Geriatric Clinical Guideline Ossification").
narrative_ontology:topic_domain(geriatric_clinical_guideline_ossification, "healthcare_policy/geriatric_medicine").

domain_priors:requires_active_enforcement(geriatric_clinical_guideline_ossification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geriatric_clinical_guideline_ossification, guideline_maintaining_institutions).
narrative_ontology:constraint_beneficiary(geriatric_clinical_guideline_ossification, liability_risk_minimizers).
narrative_ontology:constraint_victim(geriatric_clinical_guideline_ossification, elderly_patients_with_comorbidities).
narrative_ontology:constraint_victim(geriatric_clinical_guideline_ossification, front_line_clinicians).
narrative_ontology:constraint_victim(geriatric_clinical_guideline_ossification, individualized_treatment_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY PATIENT (SNARE) — Trapped by rigid protocols that were designed for simpler cases. A patient with hypertension, mild cognitive impairment, atrial fibrillation, and renal disease receives guideline-mandated polypharmacy that guidelines themselves acknowledge may be inappropriate for this exact combination. Exit is impossible: refusing treatment violates care standards; questioning guidelines is framed as rejecting evidence-based medicine. Maximum extraction — receives medically suboptimal treatment justified by institutional need to follow protocols.
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BEDSIDE CLINICIAN (TANGLED ROPE) — Constrained by malpractice liability and institutional credentialing: deviation from guidelines creates documentation burden, peer review risk, and potential legal exposure. But guidelines also provide genuine coordination benefit: standardized approaches enable care continuity, reduce decision fatigue, and provide defensive documentation. The constraint is both coordination (guidelines work for typical patients) and extraction (guidelines force suboptimal care for atypical patients).
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GUIDELINE-MAINTAINING INSTITUTION (ROPE) — Hospital systems, professional societies, and accreditation bodies benefit from standardized protocols: they reduce liability exposure, simplify training, enable efficiency metrics, and create institutional identity around 'evidence-based practice.' Experiences guidelines primarily as coordination tool. Can exit whenever they choose (updating guidelines is technically feasible); they choose not to because the status quo generates institutional value.
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GERIATRIC MEDICINE REFORM (SCAFFOLD) — Organized agents (geriatricians, patient advocacy groups, health services researchers) see guideline ossification as a temporary institutional coordination failure with a sunset: deprescribing protocols, shared decision-making frameworks, and individualized care models are building alternative approaches. These alternatives explicitly permit guideline deviation with documented patient preference. This is structured as a scaffold — organizing new coordination mechanisms while constraining the old ones to sunset.
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GUIDELINE-DEVIATION RITUAL (PITON) — When clinicians must deviate from guidelines, institutions require explicit documentation, ethics committee review, or special exemption processes. This ritual is largely performative: it verifies the deviation was deliberate, not negligent, but does not actually improve decision-making. The ritual persists through inertia — guideline-following theater maintains institutional legitimacy while the underlying coordination function has atrophied. Theater ratio (0.68) reflects that much institutional activity around guidelines is defensive documentation rather than clinical improvement.
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, this view naturalizes guideline rigidity as inherent to medicine: standardization is necessary for population health, individual variation creates chaos, and rigorous evidence-based medicine requires defined protocols. This perspective risks treating the constraint as immutable law. However, the structural data reveals this as a false summit — geriatric complexity is not new, deprescribing evidence is accumulating, and alternative coordination mechanisms exist. The 'inherent to medicine' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geriatric_clinical_guideline_ossification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geriatric_clinical_guideline_ossification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geriatric_clinical_guideline_ossification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geriatric_clinical_guideline_ossification, TR),
    TR >= 0.70.

:- end_tests(geriatric_clinical_guideline_ossification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial value from elderly patients (inappropriate polypharmacy, cognitive burden, medication side effects) and from clinicians (documentation burden, moral stress from suboptimal care). However, extraction is not maximal — guidelines do provide real coordination benefits for typical patients, and the asymmetry is partly justified by genuine first-mover coordination problems. The increased measurement from 0.35 to 0.58 reflects theater accumulation: defensive documentation, malpractice gatekeeping, and guideline committee inertia have grown. Suppression (0.65): High. Significant barriers to individualized care include malpractice liability (guideline deviation is high-risk), institutional credentialing (deviation triggers peer review), social norms (evidence-based medicine is culturally mandatory), and information asymmetry (patients often don't know alternatives exist). Barriers are partially material (legal liability) and partially internalized (clinicians internalize 'evidence-based' as moral imperative). Theater ratio (0.68): High. Increasing proportion of guideline activity is defensive: documentation of guideline adherence as liability protection, review rituals that verify deviation was deliberate rather than negligent, institutional gatekeeping of what counts as evidence. Theater has grown over the interval as liability systems have tightened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The elderly patient with comorbidities experiences pure extraction (Snare) — receives medically inappropriate treatment justified by protocol. The bedside clinician experiences mixed coordination and extraction (Tangled Rope) — genuinely helped by guidelines for typical patients but harmed by rigidity for complex patients. The guideline-maintaining institution experiences pure coordination (Rope) — protocols work as intended to standardize care and manage liability. The reform movement experiences a solvable coordination problem with a sunset (Scaffold) — alternative mechanisms are being built. The guideline-deviation ritual appears degraded (Piton) — defensive documentation persists through inertia. The civilizational analytical observer risks naturalizing the constraint as inherent to medicine (false summit Mountain). The perspectival gap reveals that 'evidence-based medicine' is genuinely coordinating at population level while extractively constraining at individual level, and that institutional inertia (theater accumulation, committee gatekeeping) has widened the gap between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position and exit options. Elderly patients with comorbidities face trapped exit (high d → high f(d) → high χ experienced extraction) because they cannot refuse treatment or exit the healthcare system without bearing catastrophic risk. Bedside clinicians face constrained exit: they can deviate from guidelines, but at cost of documentation burden and liability risk (moderate d → moderate f(d)). Guideline-maintaining institutions face arbitrage exit: they can update guidelines whenever they choose; they persist in ossified protocols because status quo generates institutional value (low d → low or negative f(d)). The reform movement faces constrained exit: building alternative coordination mechanisms requires organizing against institutional inertia (moderate d). The directionality computation automatically derives these from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by revealing that institutional coordination mechanisms can generate genuine two-sided value (guidelines improve care for typical patients) while simultaneously extracting from patients with atypical complexity. The constraint is NOT 'guidelines are bad' but 'one-size-fits-all coordination extracts from the margin.' The analytical observer's false summit (naturalizing guidelines as inherent to medicine) masks the contingent institutional choices (committee composition, update frequency, deprescribing evidence thresholds) that convert coordination into extraction. The mandatrophy is resolved by showing: (1) the coordination function is real (Rope perspective is accurate), (2) the extraction is real (Snare perspective is accurate), (3) both occur in the same institutional mechanism, (4) the extraction is not inherent but contingent on institutional design choices (theater accumulation, guideline committee gatekeeping, malpractice liability systems), and (5) alternative coordination mechanisms exist that reduce extraction while preserving coordination (scaffold perspective is structurally plausible). The constraint persists not because one-size-fits-all coordination is necessary, but because the current mechanism distributes power in ways that benefit institutions while harming complex patients.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guideline_update_frequency_threshold,
    'What guideline update frequency would convert ossified protocols into adaptive coordination?',
    'Compare clinical outcomes and deprescribing rates in systems with annual vs triennial vs decennial guideline review cycles; track lag between research publication and guideline incorporation',
    'If threshold < 2 years: ossification is primarily institutional inertia (Scaffold is real). If threshold > 5 years: ossification is structural feature of guideline systems (constraint more severe than measured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guideline_update_frequency_threshold, empirical, 'Guideline update frequency threshold for adaptive coordination').

omega_variable(
    deprescribing_evidence_threshold,
    'At what level of evidence does deprescribing become guideline-endorsed rather than guideline-deviant?',
    'Historical analysis of deprescribing research publication rates and citation by guideline committees; comparison with thresholds used for prescribing recommendations',
    'If asymmetric (higher threshold for deprescribing): institutional bias toward status quo extraction is confirmed. If symmetric: deprescribing is genuinely constrained by evidence quality, not institutional gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deprescribing_evidence_threshold, empirical, 'Evidence threshold for deprescribing endorsement vs prescribing endorsement').

omega_variable(
    shared_decision_making_implementation_feasibility,
    'Can shared decision-making frameworks be operationalized in bedside practice without generating unsustainable documentation burden or liability risk for clinicians?',
    'Pilot implementation studies tracking clinician time burden, liability insurance outcomes, and patient satisfaction in guideline-flexible care models; comparison with traditional protocol-driven care',
    'If feasible: scaffold sunset is structurally plausible and constraint will degrade. If infeasible: bedside clinicians remain constrained regardless of institutional policy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_decision_making_implementation_feasibility, empirical, 'Feasibility of shared decision-making as guideline alternative').

omega_variable(
    guideline_committee_composition_bias,
    'Does guideline committee composition systematically exclude geriatricians, elderly patient advocates, or complexity specialists, creating groupthink toward simple protocols?',
    'Analysis of guideline authorship; comparison of comorbidity-adjusted recommendations with single-disease recommendations; qualitative interviews with excluded specialists',
    'If confirmed: guideline ossification is partly a selection bias problem (fixable through committee reform). If absent: ossification is genuine coordination constraint, not institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guideline_committee_composition_bias, empirical, 'Guideline committee composition and complexity representation bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geriatric_clinical_guideline_ossification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcgo_tr_t0, geriatric_clinical_guideline_ossification, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gcgo_tr_t10, geriatric_clinical_guideline_ossification, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gcgo_tr_t20, geriatric_clinical_guideline_ossification, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(gcgo_be_t0, geriatric_clinical_guideline_ossification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gcgo_be_t10, geriatric_clinical_guideline_ossification, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gcgo_be_t20, geriatric_clinical_guideline_ossification, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geriatric_clinical_guideline_ossification, information_standard).
narrative_ontology:boltzmann_floor_override(geriatric_clinical_guideline_ossification, 0.18).
narrative_ontology:affects_constraint(geriatric_clinical_guideline_ossification, polypharmacy_induced_iatrogenesis).
narrative_ontology:affects_constraint(geriatric_clinical_guideline_ossification, deprescribing_evidence_asymmetry).
narrative_ontology:affects_constraint(geriatric_clinical_guideline_ossification, malpractice_liability_defensive_medicine).

% DUAL FORMULATION NOTE:
% Guideline ossification decomposes into three related constraints: (1) guideline_clinical_guideline_ossification (this story, ε=0.58, Tangled Rope) captures the institutional coordination-extraction hybrid; (2) polypharmacy_induced_iatrogenesis (ε=0.72, Snare) captures the medical harm specifically from guideline-mandated polypharmacy in elderly patients; (3) deprescribing_evidence_asymmetry (ε=0.42, Tangled Rope) captures the structural asymmetry in evidence thresholds for prescribing vs deprescribing, which enables guideline ossification. Each has distinct base_properties and perspectives. They are linked via network.affects_constraints because guideline ossification enables polypharmacy-induced harm and is enabled by deprescribing evidence asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geriatric_clinical_guideline_ossification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
