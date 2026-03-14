% ============================================================================
% CONSTRAINT STORY: antiarrhythmic_guideline_compliance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antiarrhythmic_guideline_compliance, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antiarrhythmic_guideline_compliance
 *   human_readable: Antiarrhythmic Guideline Compliance in Clinical Practice
 *   domain: clinical_medicine/cardiology
 *
 * SUMMARY:
 *   Antiarrhythmic guideline compliance represents a hybrid constraint that
 *   simultaneously coordinates clinical practice and extracts from clinician
 *   discretion and patient individualization. The institutional ecosystem
 *   surrounding clinical practice guidelines — medical associations publish
 *   guidelines, hospitals adopt them as accreditation requirements, courts
 *   treat guideline deviation as prima facie negligence, device manufacturers
 *   design products around guideline recommendations — creates a coercive
 *   standardization mechanism. From the bedside cardiologist's perspective,
 *   the constraint delivers genuine coordination value: guidelines reduce
 *   decision paralysis, distribute liability risk, and align practice. From a
 *   resource-constrained rural facility's perspective, the same constraint is
 *   a snare: rigid protocols mandate expensive equipment and specialist
 *   training with no flexibility for local circumstances. The theater ratio
 *   (0.68) reflects that guideline compliance verification focuses on
 *   documentation audit rather than outcome tracking — accreditation
 *   inspectors verify that charts show guideline adherence, not that patients
 *   improved. The constraint's extractiveness (0.58) reflects genuine
 *   tension: some guidelines reduce harmful variance and coordinate necessary
 *   standardization; others ossify outdated practices and prevent beneficial
 *   individualization. The three-phase temporal progression (extractiveness
 *   0.42→0.50→0.58→0.62) suggests increasing rigidity as guideline density
 *   increases and organizational compliance theater expands.
 *
 * KEY AGENTS:
 *   - Guideline Publishing Organizations (institutional/arbitrage): Coordinate practice and benefit from institutional prestige; arbitrage by publishing competing guidelines or specialty shifts
 *   - Hospital Administration (institutional/arbitrage): Coordinate care delivery and satisfy accreditation requirements; arbitrage through selective guideline emphasis
 *   - Device Manufacturers (powerful/arbitrage): Benefit from guideline recommendations creating predictable device demand; arbitrage through regulatory geography and off-label indications
 *   - Bedside Cardiologists (moderate/constrained): Constrained by liability and credentialing; benefit from reduced decision uncertainty; extract through guideline ossification and documentation burden
 *   - Resource-Constrained Facilities (powerless/trapped): Trapped by accreditation requirements to comply with guidelines they cannot financially implement; no exit options
 *   - Patient Individualization (powerless/trapped): Abstract collective good bearing cost of rigid protocols that prevent beneficial individualization; cannot organize or negotiate
 *   - Clinical Evidence Synthesis Movement (organized/constrained): Building alternative mechanisms (continuous evidence updates, individualized risk models) that could replace static guidelines; constrained by inertia of existing guideline infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antiarrhythmic_guideline_compliance, 0.58).
domain_priors:suppression_score(antiarrhythmic_guideline_compliance, 0.65).
domain_priors:theater_ratio(antiarrhythmic_guideline_compliance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antiarrhythmic_guideline_compliance, extractiveness, 0.58).
narrative_ontology:constraint_metric(antiarrhythmic_guideline_compliance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antiarrhythmic_guideline_compliance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antiarrhythmic_guideline_compliance, tangled_rope).
narrative_ontology:human_readable(antiarrhythmic_guideline_compliance, "Antiarrhythmic Guideline Compliance in Clinical Practice").
narrative_ontology:topic_domain(antiarrhythmic_guideline_compliance, "clinical_medicine/cardiology").

domain_priors:requires_active_enforcement(antiarrhythmic_guideline_compliance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antiarrhythmic_guideline_compliance, guideline_publishing_organizations).
narrative_ontology:constraint_beneficiary(antiarrhythmic_guideline_compliance, hospital_administration).
narrative_ontology:constraint_beneficiary(antiarrhythmic_guideline_compliance, device_manufacturers).
narrative_ontology:constraint_victim(antiarrhythmic_guideline_compliance, clinical_discretion).
narrative_ontology:constraint_victim(antiarrhythmic_guideline_compliance, patient_individualization).
narrative_ontology:constraint_victim(antiarrhythmic_guideline_compliance, resource_constrained_facilities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED FACILITY (SNARE) — Trapped by liability doctrine and accreditation requirements to follow guidelines regardless of local capacity. Cannot afford specialized equipment or staff training mandated by guidelines; bears cost of compliance without ability to negotiate or exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BEDSIDE CARDIOLOGIST (TANGLED ROPE) — Constrained by malpractice liability, licensure boards, and institutional credentialing to follow guidelines. Genuine coordination benefit: guidelines reduce decision paralysis and distribute risk. But also bears extraction: rigid protocols undermine clinical judgment for edge cases; administrative burden increases with each new guideline version; time spent on documentation rather than patient care.
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GUIDELINE PUBLISHING ORGANIZATION (ROPE) — Benefits from guideline adoption (institutional prestige, influence over practice, research funding tied to guideline implementation). Experiences constraint as coordination: disseminating evidence-based standards solves the collective action problem of heterogeneous treatment practices. Net beneficiary with arbitrage exit (can publish competing guidelines, shift specialty focus).
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVICE MANUFACTURER (TANGLED ROPE) — Coordinates market through guideline recommendations (predictable demand for approved devices). Also extracts: guidelines accelerate market consolidation, favor branded solutions over generic alternatives, create adoption lock-in. Powerful agents with exit through regulatory arbitrage (international markets, off-label indications).
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLINICAL EVIDENCE SYNTHESIS MOVEMENT (SCAFFOLD) — Organized agents (academic centers, open-science consortia) developing machine learning-based real-time guideline synthesis that bypasses traditional guideline rigidity. Low extraction because the coalition sees exit: continuous evidence updates and individualized risk models could replace static guidelines. Sunset clause: as AI-driven decision support matures, guideline-dependent compliance becomes unnecessary.
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: JOINT COMMISSION AND ACCREDITATION (PITON) — Maintains guideline compliance as a core accreditation metric through institutional inertia. Theater ratio is high: accreditation inspectors audit documentation of guideline adherence, not patient outcomes. The real verification mechanism (outcome tracking) is decoupled from the performative requirement (documented compliance). Accreditation bodies benefit from the clarity guidelines provide but the compliance theater persists because the institutional machinery is locked in.
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, medical standardization might appear as an inherent requirement of scaling clinical care: complex knowledge cannot be transmitted without protocols, and variance in treatment undermines learning. However, this naturalizes a contingent institutional choice (static guidelines + liability-driven compliance) as a natural law. The false summit detector will identify this as inappropriate naturalization — alternative mechanisms (continuous evidence synthesis, outcome-driven local protocols, shared decision-making frameworks) demonstrate that standardization need not require guideline rigidity.
constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antiarrhythmic_guideline_compliance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antiarrhythmic_guideline_compliance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antiarrhythmic_guideline_compliance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antiarrhythmic_guideline_compliance, TR),
    TR >= 0.70.

:- end_tests(antiarrhythmic_guideline_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Guidelines deliver coordination value (reduced variance, liability distribution, clarity in complex clinical decisions) but also extract through rigidity (prevent beneficial individualization, ossify evidence, lock in expensive device requirements). The measurement trajectory shows increasing extractiveness over time, suggesting that as guideline density increases and organizational compliance machinery expands, the extraction component grows faster than the coordination component. Suppression (0.65): High. Clinicians face strong barriers to deviation: malpractice liability, credentialing board enforcement, hospital privileging restrictions, and documentation requirements. These are structural barriers, not merely regulatory framing. Theater ratio (0.68): High and increasing. Compliance verification focuses on documentation audit (do charts show guideline adherence?) rather than outcome tracking (did patients improve?). The theater increases over time as accreditation bodies add more compliance metrics, making administrative compliance more performative than clinical verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification diversity driven by structural position. The guideline publisher's rope classification reflects their genuine experience of solving a coordination problem. The bedside cardiologist's tangled rope reflects their genuine experience of both coordination (decision support) and extraction (discretion loss). The resource-constrained facility's snare reflects their trapped position with only compliance costs. The scaffold perspective reflects the organized agents building alternative mechanisms. The piton perspective reflects institutional degradation where compliance has become theatrical. The analytical false summit warns against naturalizing institutional choices as medical inevitability. No single type is 'correct' — the perspectival presheaf reveals different aspects of the same structural phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the compliance constraint. Guideline publishers and hospital administration occupy beneficiary positions with arbitrage exit — they can shift focus, publish competing guidelines, or modify compliance emphasis. Device manufacturers occupy powerful beneficiary positions with arbitrage exit through regulatory geography. Bedside cardiologists occupy moderate victim positions with constrained exit — they face genuine barriers (liability, credentialing) but retain some discretion through interpretation of guideline boundaries. Resource-constrained facilities occupy powerless victim positions with trapped exit — no financial ability to comply with guideline-mandated equipment and no alternative credentialing path. The clinical evidence movement occupies an organized victim position with constrained exit — they have agency to build alternative systems but operate within the shadow of entrenched guideline infrastructure. Guideline publishers show the lowest directionality (d ≈ 0.10, high institutional position + arbitrage + beneficiary status); resource-constrained facilities show the highest (d ≈ 0.92, powerless position + trapped + victim status).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint genuinely coordinates clinical practice (reduces harmful variance, distributes liability, provides decision support) while asymmetrically extracting from clinician discretion and patient individualization. The tangled rope gate requires beneficiaries (guideline publishers, hospital administration, device manufacturers — present), victims (clinical discretion, patient individualization, resource-constrained facilities — present), and active enforcement (liability doctrine, accreditation requirements, credentialing boards — present). All three conditions are met. The constraint is not pure extraction disguised as coordination; it is genuine hybrid with asymmetric distribution. The mandatrophy dissolves when we recognize that both the coordination and extraction are real and that different agents experience the ratio differently. The bedside cardiologist's tangled rope is not a misclassification of snare; it is the accurate perspectival reading from their structural position. The false summit at the analytical level alerts that naturalizing this as medical necessity rather than institutional choice would be an error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guideline_evidence_lag,
    'How much extraction is driven by the 3-5 year lag between clinical evidence publication and guideline updating versus the coordination value of standardization?',
    'Retrospective analysis of guideline recommendations versus contemporary evidence quality; tracking of outdated recommendations causing harm; correlation between guideline lag time and practice variance',
    'If lag is minimal: guidelines coordinate effectively and extraction is low. If lag is severe: guidelines become instruments of ossification, extracting from clinicians who cannot deviate to incorporate new evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guideline_evidence_lag, empirical, 'Lag between clinical evidence publication and guideline updating').

omega_variable(
    liability_versus_discretion,
    'To what extent does guideline rigidity reflect genuine medical uncertainty requiring standardization versus medico-legal risk transfer (shifting liability from institutions to individual clinicians who deviate)?',
    'Analysis of malpractice litigation outcomes comparing guideline adherence versus deviation; expert review of cases where deviation produced better outcomes; comparison with legal systems with lower malpractice liability (Europe, Canada)',
    'If liability is primary driver: guidelines are extractive mechanisms disguised as coordination. If genuine uncertainty: guidelines coordinate necessary standardization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_versus_discretion, empirical, 'Liability doctrine as driver of guideline rigidity versus medical necessity').

omega_variable(
    individualization_suppression,
    'What proportion of clinician suppression stems from explicit guideline restrictions versus internalized professional identity with guidelines (identity_locked)?',
    'Survey of clinician deviation patterns in low-surveillance settings versus high-surveillance settings; analysis of clinician attitudes toward deviation (regulatory fear versus professional norm internalization); observation of guideline adherence changes when liability threat is reduced',
    'If identity-locked dominates: suppression is internalized and persists even after institutional barriers are removed. If explicit restriction dominates: suppression declines when oversight is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualization_suppression, empirical, 'Identity-locked versus explicit regulatory suppression in guideline compliance').

omega_variable(
    outcome_correlation,
    'Do strict guideline-compliant practices produce better patient outcomes than clinician-discretion-based practices, controlling for patient selection?',
    'Comparative outcomes research: guideline-strict facilities versus discretion-permitting facilities; propensity matching for patient risk; long-term mortality and arrhythmia recurrence tracking',
    'If outcomes strongly favor guideline compliance: guidelines deliver genuine coordination value. If outcomes are equivalent or favor individualization: guidelines extract without functional benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_correlation, empirical, 'Patient outcome correlation with guideline compliance versus clinical discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antiarrhythmic_guideline_compliance, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antiarr_tr_t0, antiarrhythmic_guideline_compliance, theater_ratio, 0, 0.52).
narrative_ontology:measurement(antiarr_tr_t5, antiarrhythmic_guideline_compliance, theater_ratio, 5, 0.61).
narrative_ontology:measurement(antiarr_tr_t10, antiarrhythmic_guideline_compliance, theater_ratio, 10, 0.68).
narrative_ontology:measurement(antiarr_tr_t15, antiarrhythmic_guideline_compliance, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(antiarr_be_t0, antiarrhythmic_guideline_compliance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(antiarr_be_t5, antiarrhythmic_guideline_compliance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(antiarr_be_t10, antiarrhythmic_guideline_compliance, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(antiarr_be_t15, antiarrhythmic_guideline_compliance, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antiarrhythmic_guideline_compliance, enforcement_mechanism).
narrative_ontology:affects_constraint(antiarrhythmic_guideline_compliance, clinical_liability_doctrine).
narrative_ontology:affects_constraint(antiarrhythmic_guideline_compliance, medical_device_market_consolidation).
narrative_ontology:affects_constraint(antiarrhythmic_guideline_compliance, clinician_professional_identity_formation).

% DUAL FORMULATION NOTE:
% Antiarrhythmic guideline compliance is downstream of both clinical evidence synthesis and institutional liability doctrine. The constraint's extractiveness depends on the lag between guideline publication and clinical evidence updates (upstream) and the severity of liability enforcement (cross-cutting). Device market consolidation reinforces guideline adoption, creating feedback loops. Clinician professional identity formation absorbs guideline compliance into professional norms, potentially creating identity_locked suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(antiarrhythmic_guideline_compliance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
