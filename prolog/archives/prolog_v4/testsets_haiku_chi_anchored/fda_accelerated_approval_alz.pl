% ============================================================================
% CONSTRAINT STORY: fda_accelerated_approval_alz
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_accelerated_approval_alz, []).

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
 *   constraint_id: fda_accelerated_approval_alz
 *   human_readable: FDA Accelerated Approval Pathway for Alzheimer's Drugs
 *   domain: technological/economic/regulatory
 *
 * SUMMARY:
 *   The FDA's accelerated approval pathway for Alzheimer's drugs creates a
 *   structural conflict between two legitimate goals: enabling early access
 *   to treatments for a devastating disease and maintaining evidence
 *   standards that protect patients from ineffective or harmful therapies.
 *   The constraint operates by allowing drugs to be approved based on
 *   surrogate biomarkers (amyloid plaque reduction) that are 'reasonably
 *   likely' to predict clinical benefit, rather than proven clinical benefit
 *   (cognitive stabilization or functional improvement). This creates
 *   asymmetric extraction: pharmaceutical manufacturers capture market
 *   exclusivity and profit during the approval window regardless of
 *   confirmatory trial outcomes, while patients and post-market evidence
 *   systems bear the risk of inefficacy or harm. The theater ratio (0.64)
 *   reflects that surrogate endpoints are treated rhetorically as clinical
 *   proxies without full evidence; the suppression (0.68) reflects barriers
 *   to exit (desperate patients, regulatory authority), and extractiveness
 *   (0.52) reflects the material benefit capture by manufacturers. This
 *   constraint exhibits all six DR types from different perspectives: a snare
 *   for desperate patients and trial enrollees who cannot exit, a rope for
 *   manufacturers who coordinate on market access, a tangled rope for
 *   regulators and physicians who balance early access against evidence, a
 *   piton for classical clinical trial standards that have been relegated to
 *   secondary importance, and a false mountain for observers who naturalize
 *   the uncertainty-access tradeoff as inherent to neuroscience rather than a
 *   contingent regulatory choice.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — capture market exclusivity, extended marketing period, and patent protection regardless of confirmatory trial outcomes
 *   - Patients with Early Alzheimer's Symptoms: Primary beneficiary + victim (moderate/constrained) — gain access to drugs years earlier, but access depends on unproven surrogate endpoints; experience hope bias
 *   - Patients Enrolled in Confirmatory Trials: Victim (powerless/trapped) — enrolled in post-market studies after drug is already marketed; cannot exit without forfeiting access; bear safety monitoring burden
 *   - FDA Regulatory Agency: Institutional enforcer (organized/constrained) — enforces accelerated pathway; benefits from perceived innovation; constrained by incomplete post-market data and political pressure from patient advocates and industry
 *   - Neurology Medical Community: Institutional prescriber (institutional/constrained) — benefits from early drug availability for seriously ill patients; constrained by liability if outcomes are poor
 *   - Clinical Evidence Standards: Victim/Structural principle (institutional/constrained) — historical standard of proven clinical benefit has been degraded; post-market confirmatory trials are secondary to approval
 *   - Post-Market Safety Surveillance: Victim (powerless/constrained) — tasked with detecting harms after drugs are marketed and patient expectations are set; limited power to enforce withdrawal
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the regulatory choice as inherent uncertainty rather than contingent policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_accelerated_approval_alz, 0.52).
domain_priors:suppression_score(fda_accelerated_approval_alz, 0.68).
domain_priors:theater_ratio(fda_accelerated_approval_alz, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, extractiveness, 0.52).
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_accelerated_approval_alz, tangled_rope).
narrative_ontology:human_readable(fda_accelerated_approval_alz, "FDA Accelerated Approval Pathway for Alzheimer's Drugs").
narrative_ontology:topic_domain(fda_accelerated_approval_alz, "technological/economic/regulatory").

domain_priors:requires_active_enforcement(fda_accelerated_approval_alz).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, patients_with_early_symptoms).
narrative_ontology:constraint_victim(fda_accelerated_approval_alz, post_market_safety_surveillance).
narrative_ontology:constraint_victim(fda_accelerated_approval_alz, clinical_evidence_standards).
narrative_ontology:constraint_victim(fda_accelerated_approval_alz, patients_enrolled_in_confirmatory_trials).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS IN CONFIRMATORY TRIALS (SNARE) — Enrolled in post-market studies under ethical obligation; cannot exit without forfeiting access to marketed drug. Bear the full risk of off-label use before confirmatory efficacy is proven. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALZHEIMER'S PATIENTS SEEKING ANY OPTION (SNARE) — Cognitive decline creates urgency; constrained by disease timeline and hope bias. Limited exit options: refuse the drug and watch decline, or accept unproven surrogate endpoint. Suppress information about uncertainty through medical authority. d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURERS (ROPE) — Primary beneficiary. Accelerated approval reduces time-to-market by 3-5 years, capturing first-mover advantage and extended market exclusivity. Market conditions are favorable post-approval regardless of confirmatory trial outcome. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary through arbitrage (exit via patent expiration, alternative indications).
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FDA REGULATORY AGENCY (TANGLED ROPE) — Enforcer of accelerated pathway; benefits from perceived innovation (public health theater), constrained by incomplete post-market data and political pressure. Coordination function: enabling early access to serious disease drugs. Extraction: approving drugs before efficacy proven, then maintaining approval regardless of confirmatory trial results (Aducanumab precedent). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NEUROLOGY MEDICAL COMMUNITY (TANGLED ROPE) — Coordination: early drug availability to treat desperately ill patients. Extraction: pressure to prescribe drugs with unproven clinical benefit; reputational risk from adverse outcomes; constrained by liability if confirmatory trials later show harm. d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.46.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLINICAL EVIDENCE STANDARDS (PITON) — Historically required proven clinical benefit for approval. Accelerated pathway created exception for surrogate endpoints. Theater ratio 0.64: surrogate endpoints are treated as proxies for clinical benefit (performative claim). Institutional inertia: pathway persists even when confirmatory trials show no benefit (Aducanumab approved 2023, confirmatory trial inconclusive/halted). d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Argues that uncertainty in Alzheimer's biomarkers and complex pathophysiology is inherent: surrogate endpoints can never perfectly predict clinical benefit in neurodegeneration. Trade-off between early access and certainty is immutable law of medical science. However, base properties (ε=0.52, suppression=0.68, theater=0.64) contradict mountain classification — the false summit reveals that the 'inherent uncertainty' framing naturalizes what is actually a regulatory policy choice.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_accelerated_approval_alz_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fda_accelerated_approval_alz, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fda_accelerated_approval_alz, TR),
    TR >= 0.70.

:- end_tests(fda_accelerated_approval_alz_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The manufacturers capture real economic value: 3-5 year earlier market entry, patent extension, and first-mover pricing power. Even if confirmatory trials later show marginal or no benefit (as with Aducanumab), the approval itself is not retrospectively withdrawn — extraction is locked in. The extractiveness is not maximal (0.66+) because the pathway does enable some early access benefit to patients, and some drugs (lecanemab, donanemab) show modest clinical effects in confirmatory trials. Suppression (0.68): Moderate-to-high. Significant barriers to exit: desperate patients have limited alternatives; regulatory authority suppresses information about surrogate endpoint uncertainty through medical framing ('reasonably likely' predictor); post-market data collection is insufficient to detect harms before widespread adoption. Theater ratio (0.64): Moderate-high. The surrogate endpoint framing treats biomarker changes as clinical benefit through rhetorical substitution rather than proven causation. The FDA's approval decision is performative in that it signals 'confidence' without evidence; the confirmatory trial is subsequently treated as optional rather than mandatory. Over the measurement interval (years 0-20), theater has increased from 0.35 to 0.64 because regulatory language has shifted from 'exceptional circumstance' to routine use of surrogate endpoints, and confirmatory trial enforcement has weakened (Aducanumab precedent).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Manufacturers see a coordination mechanism (Rope) — the pathway solves the problem of getting promising drugs to market efficiently. Desperate patients see opportunity (partial Rope) but also entrapment (partial Snare) — they have no real exit. Trial-enrolled patients see pure extraction (Snare) — they are in studies for drugs already approved based on surrogate endpoints. Regulators see a hybrid (Tangled Rope) — early access is real coordination, but the loss of post-market enforcement is extraction. Physicians see the same hybrid with different emphasis (Tangled Rope) — they coordinate on patient care but face liability. Classical evidence standards see degradation (Piton) — the surrogate endpoint pathway has relegated confirmatory trials to secondary status, maintained through institutional inertia despite weak enforcement. The analytical observer risks false naturalization (Mountain) — treating the uncertainty-access tradeoff as inherent to neuroscience rather than a regulatory policy. The perspectival gap is resolvable through the ε-invariance test: the extracted value (manufacturer profit, extended monopoly) is identical across all perspectives, confirming the constraint is real and tangled_rope is the correct type.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Desperate patients: Beneficiary (access) + victim (false hope) + constrained → d≈0.60, f(d)≈0.82. Mixed experience but more victimization because hope bias and disease timeline constrain exit. Trial enrollees: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit without losing drug access. FDA: Beneficiary (innovation credit) + victim (enforcement burden) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction because agency has structural power but faces conflicting mandates. Physicians: Beneficiary (early tools) + victim (liability) + constrained → d≈0.62, f(d)≈0.88. Moderate extraction due to professional liability and prescribing pressure. Evidence standards: Victim (degraded) + institutional (no exit) + constrained → d≈0.70, f(d)≈1.05. High extraction through institutional substitution. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit; the constraint is regulatory policy, not inherent science.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy between 'early access' (coordination) and 'unproven efficacy' (extraction) by mapping to tangled_rope classification, which contains BOTH functions as structural properties. The FDA accelerated approval pathway is genuinely a coordination mechanism — it enables early patient access to serious disease drugs — AND genuinely an extraction mechanism — manufacturers capture profit and market exclusivity before confirmatory trials, and the extraction persists even if trials fail (Aducanumab precedent). The tangled_rope classification is not a compromise or ambiguity; it is the accurate structural description: 0.52 extractiveness captures the real profit capture, 0.68 suppression captures real barriers to exit, 0.64 theater captures real rhetorical substitution of surrogate endpoints for clinical benefit. The mandatrophy is resolved by recognizing that BOTH the beneficiary (manufacturers) and victims (patients, evidence standards) are correct — they occupy different structural positions in the same constraint. The policy question ('should we allow accelerated approval?') is not resolved by classification; the classification merely disambiguates the structural facts. The Aducanumab case (approved 2023, confirmatory trial inconclusive, remained approved despite professional criticism) confirms the tangled_rope reading: the coordination function (early access) is real, but the extraction function (manufacturer profit and regulatory authority independence from evidence) persists independent of efficacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surrogate_endpoint_validity,
    'Does amyloid plaque reduction reliably predict meaningful clinical benefit (cognitive stability/functional preservation) in Alzheimer''s disease?',
    'Long-term outcome correlation studies: link baseline amyloid PET imaging to longitudinal cognitive decline trajectories; cross-validate against lecanemab and donanemab confirmatory trial results; meta-analysis of surrogate-to-clinical correlation coefficients',
    'If correlation > 0.70: surrogate endpoint classification valid (accelerated pathway justified). If correlation < 0.40: surrogate is theater; pathway becomes pure snare. If 0.40-0.70: tangled rope classification confirmed (mixed coordination and extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surrogate_endpoint_validity, empirical, 'Predictive validity of amyloid reduction as clinical benefit proxy').

omega_variable(
    confirmatory_trial_enforcement,
    'Will FDA enforce withdrawal or label restriction if post-market confirmatory trials show no clinical benefit?',
    'Historical precedent analysis: compare Aducanumab (approved 2023, confirmatory trial inconclusive, remains approved) with hypothetical future failures; examine regulatory language for mandatory withdrawal clauses vs discretionary continuation',
    'If enforcement strict: pathway becomes true scaffold with sunset (enforcement risk constrains extraction). If enforcement weak: pathway becomes pure snare (approval is terminal regardless of evidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confirmatory_trial_enforcement, empirical, 'Whether FDA enforces withdrawal on failed confirmatory trials').

omega_variable(
    patient_harm_attribution,
    'How much cognitive decline and adverse drug events in accelerated-approval patients are attributable to drug vs. disease natural history?',
    'Propensity-matched cohort comparison: accelerated-approval recipients vs. historical controls or parallel patients on placebo in confirmatory trials; time-to-event analysis for cognitive decline and ARIA (amyloid-related imaging abnormalities) adverse events',
    'If drug harms > disease natural history: snare extraction clearly documented, pathway faces legitimacy crisis. If drug inert: coordination benefit (early access) is pure theater; pathway degrades to piton. If drug modestly slows decline: tangled rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_harm_attribution, empirical, 'Magnitude of drug-specific cognitive decline vs. natural disease progression').

omega_variable(
    regulatory_capture_mechanism,
    'To what extent does FDA decision-making on Alzheimer''s drugs reflect pharmaceutical industry lobbying vs. genuine scientific evidence?',
    'Regulatory affairs analysis: track FDA advisory committee voting patterns pre/post industry presentations; cross-correlate approval decisions with NIH/industry funding flows to advisory committee members; compare FDA decision thresholds for Alzheimer''s vs. other serious diseases',
    'If capture high (>60%): pathway is pure snare enforced through regulatory capture. If capture low (<20%): pathway is genuine compromise between early access and evidence. If capture moderate: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Extent of pharmaceutical industry influence on FDA Alzheimer''s approval decisions').

omega_variable(
    hope_bias_magnitude,
    'How much do Alzheimer''s patients overestimate the clinical benefit of drugs approved on surrogate endpoints vs. realistic expectations?',
    'Patient survey cohort: pre- and post-approval expectations vs. actual outcomes; qualitative interviews on decision regret; measurement of placebo effect in actual prescribing (prescriber optimism bias) vs. inert drug response',
    'If hope bias >> realistic benefit: patients experience snare (trapped by false expectations). If hope bias marginal: coordination function is legitimate (early access justified). If hope bias moderate: tangled rope (some benefit, some extraction via false hope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hope_bias_magnitude, empirical, 'Magnitude of patient hope bias regarding surrogate endpoint benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_accelerated_approval_alz, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fda_alz_tr_t0, fda_accelerated_approval_alz, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fda_alz_tr_t10, fda_accelerated_approval_alz, theater_ratio, 10, 0.52).
narrative_ontology:measurement(fda_alz_tr_t20, fda_accelerated_approval_alz, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(fda_alz_be_t0, fda_accelerated_approval_alz, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fda_alz_be_t10, fda_accelerated_approval_alz, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(fda_alz_be_t20, fda_accelerated_approval_alz, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_accelerated_approval_alz, resource_allocation).
narrative_ontology:affects_constraint(fda_accelerated_approval_alz, biomarker_validation_uncertainty).
narrative_ontology:affects_constraint(fda_accelerated_approval_alz, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(fda_accelerated_approval_alz, post_market_surveillance_adequacy).

% DUAL FORMULATION NOTE:
% The accelerated approval pathway is downstream of surrogate biomarker validation claims and upstream of pricing and post-market enforcement mechanisms. Upstream constraint (biomarker_validation_uncertainty) determines whether surrogate endpoints are legitimate proxies; downstream constraints (pricing_power, post_market_adequacy) determine whether the extraction persists even if efficacy fails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fda_accelerated_approval_alz, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
