% ============================================================================
% CONSTRAINT STORY: acip_hep_b_infant_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acip_hep_b_infant_mandate, []).

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
 *   constraint_id: acip_hep_b_infant_mandate
 *   human_readable: ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)
 *   domain: public_health/vaccination_policy
 *
 * SUMMARY:
 *   The ACIP universal hepatitis B vaccination mandate for all infants,
 *   implemented in 1991, represents a foundational public health coordination
 *   mechanism that has prevented an estimated 80 million cases of hepatitis B
 *   infection and 25 million deaths from HBV-related cirrhosis and
 *   hepatocellular carcinoma globally. The constraint exhibits tension
 *   between two legitimate structural forces: (1) the epidemiological
 *   necessity of achieving population-level immunity to prevent vertical
 *   transmission, and (2) the parental autonomy interest in informed choice
 *   over infant medical intervention. The mandate emerged from genuine
 *   cost-benefit analysis in 1991, when hepatitis B prevalence among
 *   childbearing-age women in the US was approximately 1.3% — high enough
 *   that risk-stratified vaccination would have missed substantial infection
 *   risk. Over 34 years, maternal prevalence has declined to 0.3%, shifting
 *   the epidemiological calculus but not the policy mandate. The constraint's
 *   extractiveness is moderate (0.28) because the coordination function is
 *   real and highly beneficial (near-elimination of hepatitis B in vaccinated
 *   cohorts), but it achieves this by suppressing parental choice at a
 *   critical decision point (hospital discharge) without robust informed
 *   consent mechanisms. The theater ratio (0.42) reflects the gap between
 *   formal informed consent procedures and actual parental decision-making
 *   autonomy: consent documents are provided, but typically in the context of
 *   post-partum fatigue, time pressure, and social enforcement that reduces
 *   meaningful choice. The constraint evolves over its interval: early years
 *   (1991-2000) had higher extractiveness because the epidemiological case
 *   was stronger and parental vaccine hesitancy was minimal; middle years
 *   (2001-2015) show plateauing theater as anti-vaccine movements emerge;
 *   recent years (2016-2025) show increasing theater as hospitals formalize
 *   consent procedures in response to criticism while maintaining the de
 *   facto mandate.
 *
 * KEY AGENTS:
 *   - Vaccinated Infants: Primary beneficiary (powerless/trapped) — receive genuine protection against hepatitis B; no exit option; experience constraint as beneficial coordination despite loss of parental choice at birth
 *   - Public Health Authority (CDC/ACIP): Secondary beneficiary (institutional/arbitrage) — benefits from simplified implementation and measurable coverage; can exit or modify mandate if epidemiology changes
 *   - Vaccine-Hesitant Parents: Primary victim (moderate/constrained) — lose choice over infant medical intervention; constrained by legal requirement and social enforcement; may experience extraction even if vaccine is objectively beneficial
 *   - Pharmaceutical Manufacturers: Secondary beneficiary (institutional/arbitrage) — guaranteed market through universal mandate; can exit hepatitis B market if margins decline
 *   - Medical Ethics Community: Organized reformers (organized/constrained) — pushing toward informed-choice architecture with strong recommendation rather than mandate; see sunset mechanism in improved consent procedures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a policy choice (universal vaccination) as an epidemiological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acip_hep_b_infant_mandate, 0.28).
domain_priors:suppression_score(acip_hep_b_infant_mandate, 0.35).
domain_priors:theater_ratio(acip_hep_b_infant_mandate, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, extractiveness, 0.28).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acip_hep_b_infant_mandate, rope).
narrative_ontology:human_readable(acip_hep_b_infant_mandate, "ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)").
narrative_ontology:topic_domain(acip_hep_b_infant_mandate, "public_health/vaccination_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, vaccinated_infants).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, pharmaceutical_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINATED INFANT (ROPE) — No direct exit option; vaccination is administered at hospital discharge. However, the constraint solves a genuine coordination problem: universal vaccination eliminates the screening burden (testing every mother's HBsAg status) and provides reliable prophylaxis. From the infant's long-term perspective, the vaccination is a net benefit — protection against vertical transmission and future exposure. The constraint appears as coordination rather than extraction because the beneficiary (the vaccinated child) genuinely gains protection. The suppression of parental choice at birth is offset by the epidemiological benefit.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AUTHORITY (ROPE) — CDC/ACIP experience the mandate as a coordination solution: universal vaccination eliminates the administrative burden of identifying and testing high-risk mothers, replaces it with a single protocol applicable to all births, and reduces hepatitis B incidence by 95%. The authority benefits from simplified implementation, measurable coverage metrics, and declining disease burden. Exit option is arbitrage — they can shift to risk-stratified vaccination if epidemiology or cost-benefit calculus changes. No extraction is experienced; the coordination function is genuine.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: VACCINE-HESITANT PARENT (TANGLED ROPE) — Constrained by legal requirement (hospital discharge protocols) and social enforcement (risk of losing custody). Experiences extraction: the mandate removes parental choice over infant medical intervention without informed consent dialogue or refusal option. Also constrained by asymmetric information — parents lack technical knowledge to independently verify hepatitis B transmission risk in low-risk populations. However, also experiences genuine coordination benefit if the parent values the protection the vaccine provides. The constraint has both coordination (reduced disease burden) and extraction (removed choice) components simultaneously.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VACCINE MANUFACTURER (ROPE) — Guaranteed market for hepatitis B vaccine through universal infant mandate. Exit option is arbitrage — can exit hepatitis B vaccine market if profit margins decline or if regulatory mandate changes. Benefits from predictable demand and large-scale production efficiency. Coordination function: the manufacturer's incentive to produce reliably aligns with public health's need for consistent vaccine supply. No extraction perceived — the manufacturer experiences the mandate as a stable coordination contract that ensures market demand.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL ETHICS/BIOETHICS ADVOCATES (SCAFFOLD) — Organized agents (bioethics journals, institutional review boards, informed-consent frameworks) see the mandate as a temporary arrangement being reformed toward informed choice architecture. Recent developments (improved risk-benefit communication, expanded parental education, opt-out procedures in some jurisdictions) represent a sunset mechanism: moving from mandatory-by-default to informed-choice-with-strong-recommendation. The constraint has high theater (performative informed consent forms that appear to offer choice while the vaccine is already administered). Theater ratio reflects this — protocols maintain the appearance of choice while enforcement is structural.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPIDEMIOLOGICAL NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, universal hepatitis B vaccination appears as an immutable public health requirement: vertical transmission of HBV creates lifelong chronic infection in 90% of infected infants, carrier state enables oncogenic progression, and the only intervention window is at birth. From this view, the mandate is not a policy choice but an epidemiological constraint — you cannot achieve population-level HBV elimination without universal infant vaccination. However, the structural data contradicts pure mountain classification: the mandate is a policy choice (the US could implement risk-stratified vaccination as some low-incidence countries do), not an irreducible physical limit. The analytical perspective risks naturalizing a legitimate public health judgment as an immutable law.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acip_hep_b_infant_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(acip_hep_b_infant_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The constraint has genuine coordination function — universal vaccination eliminates the administrative burden of identifying and testing all pregnant women for HBsAg status, provides reliable protection during the critical transmission window, and has achieved near-elimination of hepatitis B in vaccinated cohorts. The extraction component reflects the suppression of parental choice without robust informed consent, but this is offset by the objective benefit to the vaccinated infant. The metric reflects that 1991-era epidemiology (1.3% maternal HBsAg prevalence) strongly justified the coordination, but contemporary epidemiology (0.3% prevalence) makes the extractiveness more apparent — the cost-benefit calculus has shifted toward risk-stratified approaches. Suppression (0.35): Moderate. Parents cannot refuse vaccination at hospital discharge without legal and social consequences (mandatory screening, child welfare involvement in some cases). However, suppression is not total — some jurisdictions allow philosophical/religious exemptions, and parental refusal does not trigger criminal penalties. The suppression reflects institutional power (hospital protocols, state vaccination requirements) but not coercive intensity. Theater ratio (0.42): Moderate. Hospital informed consent procedures exist but are often performative: consent documents are provided post-delivery during fatigue and time pressure, vaccine is frequently pre-administered before consent is obtained, and parental comprehension of hepatitis B transmission risk is typically low. However, theater is not high (< 0.7) because some parents do make informed choices, some jurisdictions have improved consent processes, and the public health function (disease elimination) is demonstrably real. The theater ratio has increased over the interval as informed-consent criticism has mounted and hospitals have formalized procedures, but the underlying enforcement mechanism (mandatory vaccination) remains intact.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates indexical variation across power positions. The vaccinated infant (powerless/trapped) experiences rope — genuine protection despite loss of choice. The public health authority (institutional/arbitrage) experiences rope — the constraint solves their coordination problem of identifying high-risk mothers. The vaccine manufacturer (institutional/arbitrage) experiences rope — guaranteed market through mandate. The vaccine-hesitant parent (moderate/constrained) experiences tangled rope — the constraint has both genuine coordination function (HBV protection) and extraction (removed choice). The medical ethics community (organized/constrained) experiences scaffold — viewing the mandate as a temporary arrangement being reformed toward informed-choice architecture. The analytical observer (analytical/analytical) experiences mountain — risks naturalizing a policy choice as an epidemiological inevitability. The perspectival gap reveals that the constraint is misclassified as 'immutable public health necessity' when it is actually a policy choice that was well-justified in 1991 but requires re-evaluation in 2025, given changing epidemiology and evolving informed-consent norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: beneficiary/victim status and exit options. Vaccinated infants are beneficiaries (d ≈ 0.15) with trapped exit — they experience the constraint as beneficial despite loss of choice; the engine derives low extraction from beneficiary + trapped status because the objective benefit is clear. Public health authority is beneficiary (d ≈ 0.05) with arbitrage exit — they can modify the mandate if cost-benefit changes; the engine derives near-zero or negative extraction (negative chi) from institutional beneficiary + arbitrage status. Vaccine-hesitant parents are victims (d ≈ 0.60) with constrained exit — they experience extraction from the loss of choice, but constrained exit (not fully trapped) means the extraction is moderate, not maximal; the engine derives moderate chi from moderate/constrained/victim status. The tangled rope classification for the vaccine-hesitant parent emerges from the combination of real coordination function (HBV protection) and real extraction (choice suppression) simultaneously present in the constraint's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate resolves the mandatrophy by distinguishing between the epidemiological claim ('universal hepatitis B vaccination is necessary for population-level immunity') and the policy claim ('the government should enforce universal vaccination of infants regardless of parental preference'). The epidemiological claim is strong (vertical transmission risk is genuine, prevention window is at birth, no alternative timing exists). The policy claim is weaker (many countries achieve near-universal coverage through recommendation + informed choice, not mandate). The constraint's extractiveness (0.28) reflects this resolution: the coordination function is real and justified, but the enforcement mechanism extracts parental choice. The analytical perspective risks mandatrophy by naturalizing the policy choice as epidemiological necessity ('universal vaccination is inherent to HBV elimination'). The true structure is: genuine coordination (HBV elimination requires high coverage) + legitimate extraction (enforcement via mandate removes choice). The mandate is justified if and only if informed choice would produce suboptimal coverage and the epidemiological gain justifies the extraction cost. In 1991 (1.3% maternal prevalence), the calculation was clear. In 2025 (0.3% maternal prevalence), risk-stratified vaccination with high compliance through recommendation might achieve equivalent outcomes with less extraction. The scaffold perspective captures this — the mandate is becoming a temporary coordination solution, not a permanent necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vertical_transmission_threshold,
    'At what population prevalence of maternal HBsAg should infant vaccination switch from universal to risk-stratified?',
    'Cost-effectiveness analysis comparing universal vs risk-stratified vaccination across different HBsAg prevalence scenarios; comparison with other developed nations using risk-stratified approaches',
    'If threshold < 0.5% prevalence: current US mandate is economically irrational (extraction of parental choice exceeds benefit). If threshold > 2% prevalence: universal mandate is epidemiologically justified. Current US prevalence ~0.3% places this ambiguity directly in the payload.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vertical_transmission_threshold, empirical, 'Threshold prevalence for switching universal to risk-stratified vaccination').

omega_variable(
    parental_refusal_consequences,
    'Does enforced vaccination (vs informed refusal option) measurably improve hepatitis B elimination outcomes compared to high-coverage voluntary vaccination?',
    'International comparison: countries with mandatory vs voluntary high-coverage vaccination; longitudinal tracking of disease elimination timelines and coverage rates',
    'If no measurable difference: mandate is pure extraction of choice (reclassify toward snare). If mandate provides significant tail protection: mandate is justified coordination (rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_refusal_consequences, empirical, 'Whether enforcement improves outcomes vs voluntary high-coverage vaccination').

omega_variable(
    vaccine_adverse_event_causality,
    'What is the true causal rate of serious adverse events from hepatitis B vaccine in infants, independent of temporal association bias?',
    'Large prospective cohort with matched unvaccinated controls; systematic review of adverse event causality assessments; Bayesian inference on background rate vs vaccine rate',
    'If causal rate > 1 per 100,000: some parental hesitation reflects accurate risk perception (mandate becomes more extractive). If causal rate < 1 per 1,000,000: parental hesitation reflects misperception (mandate is protective even without choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_adverse_event_causality, empirical, 'True causal rate of serious adverse events from infant HBV vaccine').

omega_variable(
    informed_consent_sufficiency,
    'Do current hospital-based informed consent procedures for hepatitis B vaccination actually enable parental decision-making, or are they performative theater?',
    'Observational study of consent encounters; comprehension testing of parents post-vaccination; comparison of decision-making autonomy in consent-present vs consent-absent hospitals',
    'If consent is performative (comprehension < 40%, decisions constrained by time/social pressure): theater_ratio should increase toward 0.7+ (scaffold classification). If consent enables genuine choice: theater_ratio should remain < 0.5 (rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informed_consent_sufficiency, empirical, 'Whether hospital informed consent enables genuine parental decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acip_hep_b_infant_mandate, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hepb_tr_t0, acip_hep_b_infant_mandate, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hepb_tr_t17, acip_hep_b_infant_mandate, theater_ratio, 17, 0.38).
narrative_ontology:measurement(hepb_tr_t34, acip_hep_b_infant_mandate, theater_ratio, 34, 0.42).

% Extraction over time
narrative_ontology:measurement(hepb_be_t0, acip_hep_b_infant_mandate, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hepb_be_t17, acip_hep_b_infant_mandate, base_extractiveness, 17, 0.24).
narrative_ontology:measurement(hepb_be_t34, acip_hep_b_infant_mandate, base_extractiveness, 34, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acip_hep_b_infant_mandate, resource_allocation).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, vaccine_mandates_school_entry).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, informed_consent_hospital_protocols).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, maternal_hbsag_screening).

% DUAL FORMULATION NOTE:
% The hepatitis B infant mandate decomposes into two structurally distinct claims: (1) Vertical HBV transmission creates serious health risk in infants (ε ≈ 0.08, Mountain), and (2) Government should enforce universal vaccination regardless of parental preference (ε ≈ 0.28, Rope). Claim 1 is epidemiological (high confidence, stable across contexts). Claim 2 is policy (higher uncertainty, varies with prevailing epidemiology and autonomy values). The JSON story addresses the policy claim; the epidemiological claim is upstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acip_hep_b_infant_mandate, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
