% ============================================================================
% CONSTRAINT STORY: pharmaceutical_safety_obfuscation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_safety_obfuscation, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pharmaceutical_safety_obfuscation
 *   human_readable: Pharmaceutical Safety Obfuscation Through Regulatory Capture and Data Control
 *   domain: pharmaceutical/regulatory/health
 *
 * SUMMARY:
 *   Pharmaceutical safety obfuscation operates as a structural constraint
 *   coupling genuine coordination requirements (drug development requires
 *   massive capital, regulatory verification, and standardized safety data
 *   exchange) with extractive mechanisms that prevent independent
 *   verification and suppress adverse event information. The constraint
 *   exhibits the classic tangled_rope signature: beneficiaries
 *   (manufacturers, captured regulators) experience coordination function;
 *   victims (patients, prescribers, public health) bear asymmetric
 *   extraction; active enforcement through data control, selective
 *   publication, and regulatory capture maintains the constraint. The
 *   measuring interval (0-20 years, modeling 1995-2015 or 2005-2025 depending
 *   on calibration) shows extraction intensification as pharmaceutical
 *   manufacturers consolidated data control, regulatory agencies became more
 *   budget-dependent on industry user fees, and patent extensions enabled
 *   price escalation with minimal new evidence. Theater ratio rising from
 *   0.52 to 0.68 reflects post-market surveillance systems
 *   (pharmacovigilance) degrading from functional safety monitoring toward
 *   performative compliance — mandatory adverse event reporting exists, but
 *   signal detection is systematically delayed, data is fragmented across
 *   manufacturer silos, and independent analysis is blocked. Suppression
 *   requirement rising from 0.58 to 0.72 reflects increasing enforcement
 *   intensity: manufacturer control over trial data, FOIA resistance, legal
 *   threats against researchers publishing negative data, and regulatory
 *   pressure to maintain pharmaceutical marketing approval despite safety
 *   signals.
 *
 * KEY AGENTS:
 *   - Patients: Primary victims (powerless/trapped) — bear maximum extraction through information asymmetry, pricing, and undisclosed risk. Exit options: medical dependency creates trapped conditions; no alternatives available for many indications.
 *   - Prescribers: Secondary victims (moderate/constrained) — experience mixed coordination and extraction. Depend on manufacturer-controlled information for treatment decisions; face liability for adverse events while safety data is controlled by others. Can exit (switch to non-pharmaceutical approaches) but face practice norm pressure and liability exposure.
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — control data, licensing, and publication narratives. Extract value through price escalation, patent extension, and regulatory capture. High arbitrage options: can relocate R&D, license technologies across jurisdictions, shift liability structures.
 *   - Regulatory Agencies (FDA, EMA, etc.): Captured institution (institutional/constrained) — maintain genuine safety coordination function but are captured through budget dependence on industry user fees, revolving-door employment, and inadequate statutory authority. Constrained exit: cannot easily reject industry funding (budget shortfalls worsen), cannot easily resist revolving-door dynamics (career incentives are global).
 *   - Post-Market Surveillance System: Degraded structure (moderate/constrained) — pharmacovigilance persists as institutional theater. Maintains mandatory reporting obligations but signal detection is delayed, integration is fragmented, and analysis capacity is insufficient. Constrained by manufacturer data control and regulatory resource constraints.
 *   - Analytical Observer: Civilization-level view (analytical/analytical) — risks naturalizing the constraint as inherent to drug development. Must distinguish between the coordination problem (capital requirements, safety verification) and the extraction mechanism (data control, regulatory capture). False summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_safety_obfuscation, 0.58).
domain_priors:suppression_score(pharmaceutical_safety_obfuscation, 0.72).
domain_priors:theater_ratio(pharmaceutical_safety_obfuscation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_safety_obfuscation, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_safety_obfuscation, "Pharmaceutical Safety Obfuscation Through Regulatory Capture and Data Control").
narrative_ontology:topic_domain(pharmaceutical_safety_obfuscation, "pharmaceutical/regulatory/health").

domain_priors:requires_active_enforcement(pharmaceutical_safety_obfuscation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pharmaceutical_safety_obfuscation, '2dcd23ed-b588-41c6-8422-cd8267718f97').
narrative_ontology:cs_kernel_codification('2dcd23ed-b588-41c6-8422-cd8267718f97', formalized).
narrative_ontology:cs_authority_grounding('2dcd23ed-b588-41c6-8422-cd8267718f97', extraction).
narrative_ontology:cs_interpretation_layer_present('2dcd23ed-b588-41c6-8422-cd8267718f97').
narrative_ontology:cs_reference_frame('2dcd23ed-b588-41c6-8422-cd8267718f97', public_health_primacy_framework).
narrative_ontology:cs_drift_state('2dcd23ed-b588-41c6-8422-cd8267718f97', contemporary_neoliberal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dcd23ed-b588-41c6-8422-cd8267718f97', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_safety_obfuscation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_safety_obfuscation, captured_regulators).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, patients).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, prescribers).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, public_health_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — Trapped by medical dependency, information asymmetry, and inability to opt out of pharmaceutical markets. Faces maximum extraction: must pay inflated prices, accept undisclosed risks, and carry burden of adverse events. No exit option — substitutes are medically unavailable, information is controlled, and alternatives (lifestyle modification, herbal remedies) are systematically delegitimized. Pure extraction from the patient's structural position.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRESCRIBER (TANGLED ROPE) — Constrained by information dependency, liability exposure, and practice norms that encourage pharmaceutical intervention. Benefits from simplified prescription protocols (coordination) but bears liability for adverse events while manufacturer controls safety data. Moderate extraction with genuine but limited coordination function: the regulatory system does coordinate drug approvals and liability structures, but these are asymmetrically enforced.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from data control, selective publication, and regulatory capture. Experiences the constraint as coordination: managing safety data, communicating with regulators, and orchestrating publication strategy solve the legitimate problem of bringing drugs to market efficiently. Net beneficiary with significant arbitrage options (can relocate operations, shift liability structures, license to different jurisdictions). Extraction flows toward this agent.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Captured through industry funding, revolving-door employment, and resource dependence, yet maintains genuine coordination function: drug approval processes, adverse event monitoring, and safety standards do coordinate legitimate public health goals. The capture is partial — regulation exists but is asymmetrically enforced. Agency faces constrained exit: budget shortfalls force reliance on industry funding; personnel face career incentives toward industry employment; legal liability creates pressure to avoid antagonizing manufacturers.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-MARKET SURVEILLANCE (PITON) — Pharmacovigilance and adverse event reporting systems persist as institutional structures but operate with diminished function. Theater ratio is high (0.68): mandatory reporting exists, but: underreporting by practitioners and manufacturers is endemic; data integration is fragmented; signal detection is delayed; manufacturer control of data pipeline prevents independent analysis. The system performs surveillance theater while actual safety monitoring capacity has atrophied. Maintained through regulatory obligation and institutional inertia rather than functional efficacy.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Risks interpreting the constraint as an immutable natural law: 'Drug development requires massive capital investment; therefore manufacturers must control data and licensing; therefore information asymmetries are inevitable; therefore patients cannot have full transparency.' This naturalization treats the contingent institutional arrangement (corporate data control) as inherent to the coordination problem (funding and testing). The engine's false summit detector will flag this: beneficiaries exist (manufacturers, captured regulators), and the constraint is constructed, not natural.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_safety_obfuscation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_safety_obfuscation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_safety_obfuscation, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_safety_obfuscation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Manufacturers control trial data, outcomes, and publication narratives, creating asymmetric information advantage. Patients pay inflated prices and accept undisclosed risks. Prescribers depend on manufacturer-controlled information. However, the extraction is not maximum (0.80+) because: (1) some genuine coordination exists — drug approval processes do verify safety at baseline; (2) regulatory agencies maintain some autonomy (constrained but not total capture); (3) exit alternatives exist for some indications (not all patients are completely trapped). The rising trajectory (0.38 → 0.58) reflects consolidation of data control and tightening of regulatory capture over the interval. Suppression (0.72): High. Manufacturers actively suppress negative safety data through publication bias, data exclusion, and regulatory delay. Regulators suppress adverse event signals through resource constraints and political pressure. Prescribers suppress discussion of risks to patients due to liability concerns. Patients are suppressed through information asymmetry and deprofessionalization (prevented from accessing raw trial data). Theater ratio (0.68): High. Post-market surveillance exists mandatorily but functions as theater: adverse event reporting is fragmented, signal detection is delayed (average 5-10 year lag for serious signals), manufacturer control of reporting pipelines prevents independent analysis, and regulatory responses lag detection by additional years. The interval shows theater intensification as surveillance systems became more elaborate but less functional (more ritual, less actual safety monitoring). Claimed type (tangled_rope): Requires genuine coordination (ε ≥ 0.30, suppression ≥ 0.40) ✓, asymmetric extraction (beneficiaries + victims) ✓, and active enforcement ✓. All gates satisfied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The manufacturer sees rope (coordination problem: managing safety data, regulatory communication, publication strategy solve legitimate problems). The regulatory agency sees tangled_rope (genuine coordination function — drug approval, safety standards — captured by asymmetric enforcement and budget dependence). The prescriber sees tangled_rope (coordination benefit from treatment protocols, extraction cost from liability and information control). The patient sees snare (maximum extraction, no exit options, pure predation). The surveillance system sees piton (degraded theater: mandatory structures that once functioned now perform compliance theater). The analytical observer risks mountain (naturalizing the extraction as inherent to drug development). This gap is diagnostic: the constraint is snare to the powerless, rope to the beneficiary, and tangled_rope at institutional level only because coordination function is genuine but enforcement is asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: beneficiary status, victim status, and exit options. Manufacturers (beneficiary + arbitrage) → d ≈ 0.05-0.15 (low extraction exposure). Regulatory agencies (captured beneficiary + constrained) → d ≈ 0.25-0.35 (moderate extraction due to capture, moderate because they also benefit from pharmaceutical industry revenue). Prescribers (secondary victim + constrained) → d ≈ 0.55-0.65 (moderate-high extraction: depend on manufacturer info, liable for outcomes, constrained by practice norms). Patients (victim + trapped) → d ≈ 0.90-0.95 (maximum extraction: no exit options, pure dependence, zero alternatives for many indications). The engine applies sigmoid f(d) to convert d to effective extraction multiplier per chi = ε × f(d) × σ(S). Patients experience χ = 0.58 × 1.42 × 1.2 ≈ 0.99 (near-total extraction). Manufacturers experience χ = 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction — they are net beneficiaries). This perspectival divergence is the key diagnostic signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the tangled_rope classification is stable across institutional perspectives (manufacturer, regulator, prescriber) but breaks down at powerless (patient) and piton (surveillance system) contexts. The mandatrophy question — is this coordination or extraction? — has a multi-part answer: (1) At manufacturer and institutional level: genuinely mixed (both coordination and extraction present; tangled_rope is correct). (2) At patient level: dominated by extraction (snare classification more accurate). (3) At surveillance system level: coordination function has atrophied (piton classification more accurate). The engine's mandatrophy resolution lies in accepting the perspectival multiplicity: the constraint IS tangled_rope from the institutional view, IS snare from the patient view, IS piton from the surveillance system view. No single classification is 'correct' — the constraint's structure entails different classifications from different structural positions. This is not ambiguity; it is precision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_suppression_mechanism_scope,
    'What portion of safety information suppression is structural (resource constraints in regulatory agencies) vs. intentional (active concealment by manufacturers)?',
    'Audit of FDA data request patterns; analysis of FOIA releases showing redactions; comparison of pre-approval safety data vs. published trial results; tracking of withdrawn studies and manufacturer retractions.',
    'If structural dominates: constraint is tangled_rope from regulatory perspective (genuine coordination bottleneck). If intentional dominates: constraint is snare from regulatory perspective (active extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_suppression_mechanism_scope, empirical, 'Whether suppression is resource-driven or intentional concealment').

omega_variable(
    independent_verification_feasibility,
    'Could independent academic verification of pharmaceutical safety data reduce extractiveness if manufacturers were required to provide raw trial data and adverse event records?',
    'Case studies of constraints that opened data access (e.g., NIH data repositories, Yale YODA project); measurement of verification cost reduction; comparison of independent vs. manufacturer-controlled safety assessments; tracking of regulatory decisions pre/post data availability.',
    'If feasible: extractiveness could drop to 0.35-0.40 (rope/scaffold), making the constraint reclassifiable. If not feasible: informational asymmetry is structural, and extractiveness remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_verification_feasibility, empirical, 'Whether independent data verification would reduce extraction').

omega_variable(
    regulatory_capture_degree_measurement,
    'What quantitative thresholds distinguish partial regulatory capture (agency maintains some independence) from total capture (agency functions as manufacturer subsidiary)?',
    'Analysis of: revolving-door employment rates; budget dependence on industry user fees; divergence between manufacturer preferences and regulatory decisions; whistleblower reports from inside agencies; international comparison of regulatory stringency.',
    'If partial capture measures justify constrained exit for agency: tangled_rope classification stable. If measures show total capture (exit becomes identity_locked or trapped): reclassify as snare from agency perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree_measurement, empirical, 'Quantitative thresholds for distinguishing partial vs. total regulatory capture').

omega_variable(
    patient_exit_availability,
    'For how many pharmaceutical indications do genuine medical alternatives exist outside the captured pharmaceutical system?',
    'Disease-by-disease audit: count of therapeutic domains where non-pharmaceutical intervention achieves comparable or superior outcomes; assessment of whether alternatives are suppressed vs. genuinely inferior; patient surveys on perceived availability of alternatives.',
    'If alternatives exist for >50% of indications: patients on average experience constrained (not trapped) exit, shifting patient perspective toward tangled_rope. If alternatives exist for <20%: trapped classification is structurally accurate; snare classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_exit_availability, empirical, 'Availability of non-pharmaceutical therapeutic alternatives').

omega_variable(
    publishing_bias_quantification,
    'What is the actual suppression rate: ratio of conducted trials vs. published trials, and ratio of negative vs. positive results in published literature?',
    'Systematic review of trial registries (ClinicalTrials.gov) vs. PubMed literature; meta-analysis of publication bias studies; FOIA requests for unpublished safety analyses; tracking of ''file drawer'' studies.',
    'If >50% of trials are unpublished or negative results are suppressed at >3:1 ratio: confirms high theater_ratio and extractiveness floor of 0.55+. If <30% unpublished: theater_ratio should be adjusted downward (0.45-0.50), potentially shifting some perspectives toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publishing_bias_quantification, empirical, 'Quantification of publication bias in pharmaceutical research').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_safety_obfuscation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_safety_tr_t0, pharmaceutical_safety_obfuscation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pharma_safety_tr_t10, pharmaceutical_safety_obfuscation, theater_ratio, 10, 0.63).
narrative_ontology:measurement(pharma_safety_tr_t20, pharmaceutical_safety_obfuscation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pharma_safety_be_t0, pharmaceutical_safety_obfuscation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pharma_safety_be_t10, pharmaceutical_safety_obfuscation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pharma_safety_be_t20, pharmaceutical_safety_obfuscation, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pharma_safety_su_t0, pharmaceutical_safety_obfuscation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pharma_safety_su_t10, pharmaceutical_safety_obfuscation, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(pharma_safety_su_t20, pharmaceutical_safety_obfuscation, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_safety_obfuscation, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, drug_pricing_extraction).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, clinical_trial_design_bias).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, regulatory_agency_resource_starvation).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, patient_informed_consent_degradation).

% DUAL FORMULATION NOTE:
% Pharmaceutical safety obfuscation is upstream of multiple downstream constraints in the drug development ecosystem. Drug pricing extraction depends on information asymmetry created by safety obfuscation. Clinical trial design bias is enabled by manufacturer control over data. Regulatory agency resource starvation is both cause and effect: insufficient budget forces dependence on industry funding, which further captures the agency. Patient informed consent degradation depends on suppressed safety information. All four downstream constraints share common structural ancestor: the consolidation of data control in manufacturer hands and the regulatory capture of safety verification agencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_safety_obfuscation, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
