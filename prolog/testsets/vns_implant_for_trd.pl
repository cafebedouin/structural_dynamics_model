% ============================================================================
% CONSTRAINT STORY: vns_implant_for_trd
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vns_implant_for_trd, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vns_implant_for_trd
 *   human_readable: Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression
 *   domain: medical_technology/psychiatry
 *
 * SUMMARY:
 *   Vagus Nerve Stimulation for treatment-resistant depression represents a
 *   structural tension between desperate patients with exhausted
 *   pharmacological options and a medical technology ecosystem where device
 *   manufacturers, surgical specialists, and regulatory bodies benefit from
 *   permanent implantation regardless of outcome. The constraint demonstrates
 *   hybrid coordination-extraction: genuine coordination function exists
 *   (reducing severe depression outcomes in some patients), but asymmetric
 *   extraction is active (manufacturers guarantee revenue regardless of
 *   responder status, patients bear irreversible surgical risk with uncertain
 *   benefit). The theater ratio (0.64) reflects performative regulatory
 *   oversight (FDA maintains approval on 2005 data despite replication
 *   evidence of lower efficacy) and clinical uncertainty masked by positive
 *   trial design (industry-sponsored studies showing 67% response vs
 *   independent verification showing 20-30%). The constraint exhibits all six
 *   DR types from different perspectives, revealing how a medical technology
 *   can be simultaneously a life-saving coordination mechanism, a temporary
 *   bridge to better treatments, a degraded regulatory ritual, a mixed
 *   extraction-coordination hybrid, pure extraction from the patient
 *   perspective, and a false natural law from the analytical perspective.
 *
 * KEY AGENTS:
 *   - Treatment-Resistant Patients: Primary victims (powerless/trapped) — face irreversible surgical commitment with uncertain efficacy; 70% may be non-responders bearing device risk without benefit
 *   - Device Manufacturers (LivaNova, others): Primary beneficiaries (institutional/arbitrage) — extract guaranteed revenue through implantation volume, service contracts, and battery replacements regardless of patient outcomes
 *   - Implanting Surgeons and Psychiatric Specialists: Secondary beneficiaries (institutional/immediate) — capture reimbursement for procedures and monitoring; institutional incentives favor implantation volume over patient selection rigor
 *   - Healthcare Insurance Systems: Secondary victims/beneficiaries (moderate/constrained) — must fund expensive interventions for majority non-responders while benefiting from minority responders; constrained by cost management pressure
 *   - FDA and Regulatory Bodies: Piton actors (institutional/arbitrage) — maintain approval through inertia despite outdated evidence base; regulatory theater persists but verification function has atrophied
 *   - Patient Advocacy and Bioethics Committees: Organized agents (organized/constrained) — building alternatives through informed consent improvements, outcome transparency, and investment in non-surgical pathways; see VNS as temporary bridge
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent therapeutic incompleteness as inherent neurobiological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vns_implant_for_trd, 0.52).
domain_priors:suppression_score(vns_implant_for_trd, 0.68).
domain_priors:theater_ratio(vns_implant_for_trd, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vns_implant_for_trd, extractiveness, 0.52).
narrative_ontology:constraint_metric(vns_implant_for_trd, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vns_implant_for_trd, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vns_implant_for_trd, tangled_rope).
narrative_ontology:human_readable(vns_implant_for_trd, "Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression").
narrative_ontology:topic_domain(vns_implant_for_trd, "medical_technology/psychiatry").

domain_priors:requires_active_enforcement(vns_implant_for_trd).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, device_manufacturers).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, implanting_surgeons).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, psychiatric_specialists).
narrative_ontology:constraint_victim(vns_implant_for_trd, treatment_resistant_patients).
narrative_ontology:constraint_victim(vns_implant_for_trd, healthcare_systems).
narrative_ontology:constraint_victim(vns_implant_for_trd, insurance_coverage_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREATMENT-RESISTANT PATIENT (SNARE) — After failed medication trials, patient faces irreversible surgical commitment. Cannot exit: reversal surgery carries additional risks, alternative treatments exhausted. Bears maximum cost (surgical risk, device complications, lifelong monitoring, battery replacements) with uncertain therapeutic benefit. No genuine informed consent mechanism addresses the asymmetry between modest efficacy rates (20-30% response in trials) and permanent bodily commitment. Maximum experienced extraction — trapped in choice between despair and high-risk device.
constraint_indexing:constraint_classification(vns_implant_for_trd, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTHCARE INSURANCE (TANGLED ROPE) — Benefits from VNS through reduced long-term psychiatric hospitalizations and emergency interventions for responders. Constrained by cost: device implantation (~$30-40k), annual monitoring ($3-5k), battery replacements every 10 years. Asymmetric: must subsidize majority non-responders while benefiting from minority responders. Active enforcement of coverage decisions and patient selection criteria creates hybrid: coordination function (reducing overall TRD system cost) with extraction asymmetry (insurers bear upfront costs for uncertain outcomes).
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANUFACTURERS & SPECIALISTS (ROPE) — Institutional beneficiaries. Device manufacturers (LivaNova, others) extract revenue through implantation volume and service contracts. Implanting surgeons and psychiatric specialists capture reimbursement for procedures and monitoring. Arbitrage exit: can shift market focus to other conditions (bipolar disorder, chronic pain, epilepsy) if TRD market saturates. Experience the constraint as coordination mechanism: VNS clinical protocols, training standards, device interoperability create standardized market. Minimal suppression experienced — can withdraw resources if reimbursement declines.
constraint_indexing:constraint_classification(vns_implant_for_trd, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES (PITON) — FDA approval pathway for VNS-TRD (granted 2005) was based on modest clinical evidence (67% response rate in industry-sponsored trials; replication studies show 20-30%). The regulatory theater persists: ongoing monitoring reports, post-market surveillance, periodic efficacy reviews maintain the illusion of continuous verification, but the approval infrastructure has largely atrophied into performative ritual. Agencies maintain approval not because evidence base strengthens but through institutional inertia — removing approval would require active political action. Theater ratio high (0.64): regulatory process requires annual reports and device updates, but these are largely procedural rather than re-evaluating efficacy.
constraint_indexing:constraint_classification(vns_implant_for_trd, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PATIENT ADVOCACY & ETHICS (SCAFFOLD) — Organized actors (patient support groups, bioethics boards, emerging digital mental health alternatives) recognize VNS as a temporary solution with a sunset: brain imaging research (fMRI, EEG biomarkers), digital therapeutics, pharmacogenomics, and psychedelic-assisted therapy represent parallel pathways that may eventually reduce irreversible surgical intervention rates. Advocacy groups push for rigorous patient selection, transparent outcome reporting, and investment in non-surgical alternatives. Constraint experienced as temporary: 10-15 year sunset as therapeutic alternatives mature. Active enforcement (ethics review, informed consent improvements) is declining suppression over time.
constraint_indexing:constraint_classification(vns_implant_for_trd, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, treatment-resistant depression represents an irreducible subclass of mood disorders where standard pharmacological interventions fail by neurobiological necessity. VNS targets vagal signaling pathways that medications cannot access through conventional routes. This perspective sees the device constraint as following from the structure of human neurobiology itself: some patients will always require neuromodulation because their pathophysiology is incompatible with pharmacotherapy. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'neurobiological necessity' naturalizes what is actually a contingent ecosystem of incomplete pharmacological development, insufficient patient stratification, and profitable device markets.
constraint_indexing:constraint_classification(vns_implant_for_trd, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vns_implant_for_trd_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vns_implant_for_trd, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vns_implant_for_trd, TR),
    TR >= 0.70.

:- end_tests(vns_implant_for_trd_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The device system extracts from patients (permanent bodily commitment, surgical risk, uncertain benefit) and from healthcare systems (high cost per responder). However, extraction is not maximal because genuine therapeutic benefit exists for responders (20-30% response rate). The increase over the interval (0.38 → 0.52) reflects market expansion and increasing device placement despite stable efficacy data, suggesting extraction through volume rather than individual patient value. Suppression (0.68): High. Significant barriers to refusing implantation include: exhaustion of medication options (systemic filtering), psychiatric crisis severity (decision-making under duress), asymmetric information about efficacy and complications (regulatory theater and industry messaging), and absence of equivalent alternatives (though this is changing). No genuine informed consent mechanism addresses the irreversibility asymmetry. Theater ratio (0.64): Moderate-high. FDA oversight is substantially performative: approval from 2005 based on 67% response rates, yet replication studies show 20-30%; annual regulatory reports are procedural rather than re-evaluating efficacy; device marketing emphasizes hope and clinical protocols rather than honest uncertainty about outcomes. The theater has increased over 20 years as regulatory burden has accumulated without corresponding evidence re-evaluation.
 *
 * PERSPECTIVAL GAP:
 *   Original research showing 67% response (industry-sponsored trials, company-supported studies) vs independent verification showing 20-30% (academic research without industry funding) creates massive perspectival gap. Manufacturers frame the higher number; patients and healthcare systems experience the lower. This gap is structural, not measurement error. The regulatory perspective sees approval protocol satisfied; the patient perspective sees outdated evidence. The scaffold perspective (patient advocacy) sees this gap closing as biomarkers improve patient selection and alternative treatments mature. Over 20 years, the theater ratio increased (0.45 → 0.64) as regulatory burden accumulated without corresponding evidence re-evaluation, and extractiveness increased (0.38 → 0.52) as implant volume grew relative to demonstrated efficacy. The key perspectival feature: an institutional innovation that genuinely helps some patients (responders) is operationally indistinguishable from pure extraction because non-responders cannot be identified pre-implant and face irreversible commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients accumulate negative exit options progressively: medication trials exhaust first (option removal), then psychotherapy failure (option removal), then psychiatric crisis creates decision urgency (temporal pressure). This trajectory systematically moves d → 1.0 (full target). Manufacturers and specialists maintain arbitrage exit throughout: can shift device focus to bipolar, epilepsy, chronic pain if TRD reimbursement changes. This maintains low d (~0.05). Insurance systems are constrained by coverage mandates but not trapped: can adjust authorization criteria, require biomarker testing, restrict to severe cases. This gives intermediate d (~0.55). The directionality overrides are not needed here because the structural derivation correctly captures the relationships: beneficiary + arbitrage → negative chi; victim + trapped → maximum chi; mixed beneficiary-victim with constraints → moderate positive chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy resolution through three gates: (1) Tangled Rope requires beneficiaries (manufacturers, specialists) — satisfied. (2) Tangled Rope requires victims (patients, healthcare systems) — satisfied. (3) Tangled Rope requires active enforcement (regulatory oversight, reimbursement control) — satisfied. However, mandatrophy is NOT fully resolved because the constraint is borderline snare. If efficacy validation studies confirm that efficacy is primarily placebo/irreversibility effect, or if biomarkers emerge showing pre-implant prediction is feasible, the constraint could collapse into pure extraction (Snare). The mandatrophy analysis shows that current classification is contingent on empirical facts (true efficacy rates, complication trajectories, alternative treatment timelines) that are themselves contested. The structural data suggests Tangled Rope is correct: there is genuine coordination (psychiatric crisis reduction in responders), but extraction asymmetry is real (non-responders bear risk without benefit, manufacturers extract regardless of outcome). The false summit (Mountain perspective) reveals the analytical trap: framing TRD as inherently unresponsive to non-device treatments naturalizes what is actually institutional incompleteness in pharmacotherapy development and patient stratification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_measurement_bias,
    'Do post-implant clinical response rates reflect genuine therapeutic effect or placebo/expectancy effects amplified by irreversibility and financial commitment?',
    'Rigorous long-term follow-up data separating early responders from late responders; comparison with sham-surgery control groups (ethically constrained); analysis of response persistence at 5+ year follow-up',
    'If efficacy sustained: justifies moderate risk acceptance. If efficacy decays to 10-15%: device represents pure extraction disguised as medical treatment. Mandatrophy shifts from unresolved to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_measurement_bias, empirical, 'Separation of genuine efficacy from placebo/irreversibility effects').

omega_variable(
    alternative_therapeutic_timeline,
    'Will pharmacogenomics, digital therapeutics, or psychedelic-assisted therapy provide superior outcomes to VNS within 15 years, rendering the irreversible surgical commitment obsolete?',
    'Comparative trial data emerging from parallel research tracks; market adoption rates of novel treatments; reduction in annual VNS implant volume relative to total TRD cases',
    'If yes: scaffold perspective validated—device is temporary bridge to better solutions. If no: VNS remains long-term standard, and patient commitment is not temporary. Affects sunset clause credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_therapeutic_timeline, empirical, 'Whether superior non-surgical alternatives will emerge within 15 years').

omega_variable(
    patient_stratification_feasibility,
    'Can pre-implant biomarkers (neuroimaging, genetic, inflammatory) reliably predict responders from non-responders, reducing the population bearing device risk without benefit?',
    'Prospective biomarker validation studies; predictive accuracy of pre-implant testing; adoption into clinical decision-making guidelines',
    'If feasible: suppression declines dramatically—patients with low response probability can be diverted to alternatives. If not feasible: majority of implanted patients are non-responders bearing extraction risk. Suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_stratification_feasibility, empirical, 'Pre-implant prediction of responder vs non-responder status').

omega_variable(
    device_complication_trajectory,
    'Are reported surgical and device complication rates (infection, migration, loss of efficacy, hardware failure) complete, or do long-term complications emerge after the industry''s post-market surveillance window closes?',
    'Independent long-term registry data beyond manufacturer follow-up; patient-reported outcomes at 10+ year follow-up; revision/explantation rates over time',
    'If high late complication rates: actual burden on patients much higher than marketed. Extraction increases. If complications remain low: patient risk profile is accurate, justifying some acceptance of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(device_complication_trajectory, empirical, 'True long-term complication trajectory independent of manufacturer data').

omega_variable(
    reimbursement_sustainability,
    'Can healthcare systems sustain VNS as routine TRD treatment given the cost per responder (often $200k+ for responders, $1M+ when accounting for non-responders), or will cost pressures force restriction to extreme-case patients?',
    'Comparative cost-effectiveness analysis against emerging alternatives; insurance coverage policy evolution; uptake rates by healthcare system type (public vs private)',
    'If unsustainable: VNS becomes luxury good available only to wealthy or well-insured patients. Suppression remains high for economically vulnerable patients. If sustainable: constraint is genuinely open to broader population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reimbursement_sustainability, empirical, 'Long-term financial sustainability of VNS as standard TRD treatment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vns_implant_for_trd, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vns_tr_t0, vns_implant_for_trd, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vns_tr_t10, vns_implant_for_trd, theater_ratio, 10, 0.58).
narrative_ontology:measurement(vns_tr_t20, vns_implant_for_trd, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(vns_be_t0, vns_implant_for_trd, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vns_be_t10, vns_implant_for_trd, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vns_be_t20, vns_implant_for_trd, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vns_implant_for_trd, enforcement_mechanism).
narrative_ontology:affects_constraint(vns_implant_for_trd, treatment_resistant_depression_pharmacotherapy).
narrative_ontology:affects_constraint(vns_implant_for_trd, psychiatric_device_regulation).
narrative_ontology:affects_constraint(vns_implant_for_trd, digital_mental_health_alternatives).

% DUAL FORMULATION NOTE:
% VNS-TRD is downstream of broader treatment-resistant depression pathology but represents a distinct structural constraint at the medical technology level. The upstream constraint (TRD pharmacotherapy incompleteness) has its own extractiveness reflecting the state of psychiatric medication development; VNS has its own extractiveness reflecting the device ecosystem and irreversibility asymmetry. Network linkage shows how VNS is being displaced by digital therapeutics and pharmacogenomics as upstream constraints resolve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
