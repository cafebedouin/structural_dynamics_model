% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Disease-Proportional Vaccine Mandate Framework (Proportionality Reading)
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the PROPORTIONALITY READING of the
 *   vaccine_mandate_balance kernel. The reading holds that vaccine mandates
 *   are permissible only when disease severity, transmission risk, and
 *   vaccine safety meet strict proportionality thresholds, and exemptions
 *   must be robust (medical, religious, philosophical). This is distinct from
 *   autonomy-absolutist reading (mandates never permissible regardless of
 *   disease severity) and public-health-primary reading (mandates permissible
 *   whenever they advance herd immunity and protect vulnerable populations).
 *   The proportionality reading is the dominant framework in contemporary
 *   constitutional law and medical ethics. The constraint operates as a
 *   tangled rope: it coordinates protection for vulnerable unvaccinatable
 *   populations (genuine coordination function) AND extracts bodily autonomy
 *   from mandate-resistant adults through state enforcement (asymmetric
 *   extraction). The extraction is conditional on disease parameters: a
 *   mandate for smallpox would be highly justifiable under this reading; a
 *   mandate for seasonal influenza would be unjustifiable. The ε value (0.58)
 *   reflects the reading's middle-ground position: less extractive than
 *   public-health-maximalism, more extractive than autonomy-absolutism. The
 *   measurement series track enforcement intensification during high-threat
 *   periods (pandemic waves) and de-escalation during endemic phases.
 *
 * KEY AGENTS:
 *   - Public health officials: institutional agenda-setters authorized to enforce mandates conditionally on proportionality justification; retain discretion over exemption robustness and threshold application.
 *   - Mandate-resistant adults: moderate power but constrained exit; subject to vaccination requirements for employment, school, facilities; exempt options exist but variably robust.
 *   - Vulnerable unvaccinatable populations: powerless beneficiaries (infants, immunocompromised, medical contraindications); depend on community immunity; their welfare justifies the mandate when disease severity warrants it.
 *   - Vaccine safety researchers: institutional observers providing empirical safety data that proportionality determination depends on; their independence is structurally essential.
 *   - Epidemiologists: institutional observers providing disease severity and transmission-risk data; their measurement of pathogen-specific parameters is the empirical substrate of the threshold test.
 *   - Bodily autonomy advocates: excluded from this reading's framing; would argue no threshold test is coherent.
 *   - Public-health maximalists: excluded from this reading's framing; would reject the threshold test as paralyzing necessary mandates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.62).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Disease-Proportional Vaccine Mandate Framework (Proportionality Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '3638f648-2667-4d5e-ae1c-993e257e47eb').
narrative_ontology:cs_kernel_codification('3638f648-2667-4d5e-ae1c-993e257e47eb', formalized).
narrative_ontology:cs_authority_grounding('3638f648-2667-4d5e-ae1c-993e257e47eb', expertise).
narrative_ontology:cs_interpretation_layer_present('3638f648-2667-4d5e-ae1c-993e257e47eb').
narrative_ontology:cs_reading_relation('3638f648-2667-4d5e-ae1c-993e257e47eb', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('3638f648-2667-4d5e-ae1c-993e257e47eb', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('3638f648-2667-4d5e-ae1c-993e257e47eb', foundational, proportionality_constraint_required).
narrative_ontology:cs_axiom_status(proportionality_constraint_required, holdable).
narrative_ontology:cs_axiom_grounding('3638f648-2667-4d5e-ae1c-993e257e47eb', proportionality_constraint_required, deontological).
narrative_ontology:cs_axiom('3638f648-2667-4d5e-ae1c-993e257e47eb', foundational, exemptions_substantively_robust).
narrative_ontology:cs_axiom_status(exemptions_substantively_robust, holdable).
narrative_ontology:cs_axiom_grounding('3638f648-2667-4d5e-ae1c-993e257e47eb', exemptions_substantively_robust, deontological).
narrative_ontology:cs_reference_frame('3638f648-2667-4d5e-ae1c-993e257e47eb', proportionality_constrained_authority).
narrative_ontology:cs_drift_state('3638f648-2667-4d5e-ae1c-993e257e47eb', contemporary_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3638f648-2667-4d5e-ae1c-993e257e47eb', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_unvaccinatable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_officials).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, mandate_resistant_adults).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the override of individual medical decision-making authority. At 0.58, it reflects that the override is conditional: it applies only when disease severity (hospitalization rate, fatality rate), transmission risk (R value, population immunity gaps), and vaccine safety (adverse event rate, severity) meet specified thresholds. A seasonal flu mandate (low severity, low vaccine risk but also low disease risk) would have lower ε (0.30-0.40); a smallpox mandate (extreme severity, high transmissibility, excellent safety) would have higher ε (0.75+) because the proportionality justification is overwhelming and exemptions would be narrower. The 0.58 is calibrated to a 'middle-case' moderate-threat pathogen (e.g., measles in a low-immunity population, or pandemic influenza with moderate CFR). Suppression is higher (0.62) because mandate enforcement requires active suppression: employment requirements, school exclusions, legal penalties for non-compliance, limitation of exemptions to narrowly defined classes. Theater ratio (0.41) reflects that 'proportionality justification' has become ceremonial in some contexts: officials invoke thresholds to legitimize mandates they have already decided to impose, rather than genuinely testing the mandate against threshold data. The proportionality language sometimes obscures rather than constrains the mandate decision. Accessibility collapse (0.68) reflects constrained exit: refusing vaccination means meaningful social and economic exclusion, but alternatives exist (religious exemption, remote work, private schooling) and the constraint is conditional on disease parameters, so alternatives become viable if the threat passes below the threshold. Resistance (0.72) reflects sustained contestation from autonomy advocates and mandate-skeptics; the proportionality reading does not eliminate resistance, it channels it into threshold disputes and exemption boundaries.
 *
 * PERSPECTIVAL GAP:
 *   The official seat will compute as near-rope (genuine coordination, minor asymmetry). The payer seat will compute as tangled-rope or snare (asymmetric extraction with conditional coordination for others). The observer seats will compute as rope or mountain (they inform the constraint without being constrained by it). The engine's per-seat type computation captures why the same constraint (the proportionality threshold rule) looks like justified coordination from the official's position and like state overreach from the payer's position.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health officials (agenda-setter, institutional, arbitrage exit) benefit from the proportionality framework because it authorizes them to mandate vaccination while appearing constrained by thresholds. Their directionality is low (near beneficiary end, d~0.2) because they collect authority and legitimacy from the framework and their exit is high (they can reinterpret thresholds or declare emergencies). Mandate-resistant adults (payer, moderate power, constrained exit) bear the extraction (forced medical intervention, employment conditionality, exemption limitation). Their directionality is high (near target end, d~0.75) because the extraction is imposed without their consent and their exit options are severely constrained: refusing vaccination means job loss, school exclusion, or social restriction. Vulnerable unvaccinatable populations (beneficiary, powerless, trapped) have low directionality (d~0.15) because the framework protects them without extracting from them; however, their power is so low that the protection is contingent on officials' continued commitment to the proportionality threshold. If officials pivot to public-health-maximalism, vulnerable populations become invisible again (extracted from by disease, not protected by mandate). Vaccine safety researchers and epidemiologists (observers, institutional, analytical exit) are symmetrically positioned (d~0.5) relative to the constraint: they inform it but do not depend on it. Their independence is essential, so they have no beneficiary or victim status in the classical sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading prevents misclassification of state medical authority as pure coordination. The founding problem (vulnerable populations unprotected by voluntary vaccination) is live: pandemic experience confirms voluntary uptake falls short of herd immunity for severe pathogens. The reading's proportionality test is not mandatrophy (not a vestigial function maintained theatrically) because it is actively used to adjudicate mandate decisions: officials do reject mandates when disease severity is low (seasonal flu, endemic measles in high-immunity countries). However, there is drift toward theater: as the reading's legitimacy became established, the proportionality language sometimes becomes decorative (thresholds are declared met without genuine measurement). The 0.41 theater ratio reflects this drift. The mandatrophy risk is that the proportionality reading will persist as ceremonial legitimacy even if actual enforcement practices diverge from threshold justification. The constraint would persist because it vindicates the constitutional proportionality doctrine (a vindicated proposition), not because it genuinely coordinates or constrains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_determination_capture,
    'Who determines whether disease severity, transmission risk, and vaccine safety meet the proportionality thresholds? Can the institution that benefits from mandates (public health officials) objectively measure and apply the thresholds, or does institutional interest bias the measurement?',
    'Empirical observation of mandate decisions when disease parameters are ambiguous (moderate severity, moderate transmission, moderate vaccine risk). If mandates are approved inconsistently based on threshold data, the institutional capture is occurring; if the same pathogen profile leads to consistent rejection or approval decisions, thresholds are genuinely constraining.',
    'If threshold determination is captured, the proportionality reading becomes a legitimacy performance (theater) and the constraint reclassifies from tangled_rope to snare (conditional coordination cover for unconstrained extraction). If thresholds genuinely constrain, the reading is analytically coherent and the tangled_rope classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_determination_capture, empirical, 'Whether proportionality thresholds are objectively applied or institutionally captured.').

omega_variable(
    exemption_robustness_variability,
    'When officials declare mandates ''proportional'', what exemptions do they offer in practice? Are medical exemptions genuinely available, or are they narrowed to eliminate functional alternatives? Are religious exemptions honored, or are they reinterpreted as philosophical and rejected? Is philosophical exemption recognized at all?',
    'Comparative analysis of exemption policies across jurisdictions and time periods; survey of applicants denied exemptions to measure the scope of refusal; longitudinal tracking of exemption availability as disease threat changes.',
    'If exemptions are genuinely robust, mandate-resistant adults have meaningful exit (identity_locked → constrained), and the constraint''s extraction is tempered by alternatives. If exemptions are narrowed or illusory, exit becomes trapped (identity_locked → trapped), and the constraint becomes snare (asymmetric extraction without genuine alternative, despite proportionality framing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_robustness_variability, empirical, 'Whether exemptions are robust alternatives or theater.').

omega_variable(
    reading_coherence_small_vs_large_diseases,
    'Is the proportionality reading internally coherent across disease extremes? Can the same threshold framework justify smallpox mandates (extreme severity, high benefit, excellent vaccine safety) AND reject seasonal flu mandates (low severity, minimal benefit, acceptable vaccine risk)? Or does the reading incoherently claim both mandates are ''proportional''?',
    'Examine the proportionality reading''s application to canonical high-severity (smallpox) and low-severity (seasonal flu) pathogens. If the framework produces mandate approval for smallpox and mandate rejection for flu, it is internally coherent; if it produces mandate approval for both despite severe disease-parameter differences, the reading is incoherent (thresholds are not genuinely constraining).',
    'If incoherent, the proportionality reading is conceptually broken and should be replaced by either autonomy-absolutism (no mandates) or public-health-primacy (mandates whenever beneficial). If coherent, the reading is a defensible middle ground, and the tangled_rope / snare classification depends on empirical questions about capture (omega 1) and exemption robustness (omega 2).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coherence_small_vs_large_diseases, conceptual, 'Whether proportionality thresholds are internally consistent across disease severity spectrum.').

omega_variable(
    sibling_reading_empirical_contest,
    'Do the three sibling readings (proportionality, autonomy-primary, health-primary) make empirically different predictions about vaccine uptake, disease outcome, and social conflict? Or are the readings purely normative framings of identical operational constraints?',
    'Examine jurisdictions that have adopted each reading (proportionality-constrained mandates vs. autonomy-protection policies vs. health-maximalist policies) and compare vaccine uptake, herd immunity achievement, disease incidence, and social polarization. If different readings produce different outcomes, the readings are structurally distinct; if outcomes are identical, the readings are performative reframings of the same practical constraint.',
    'If readings are empirically distinct, decomposing them into separate constraint stories is justified and the constraint family structure is real. If readings are empirically identical, they are merely different narratives about one constraint, and the family decomposition is premature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_contest, empirical, 'Whether sibling readings generate empirically different constraint operations.').

omega_variable(
    proportionality_suppression_mechanism,
    'Is the measured suppression (0.62) structural (external coercion: job loss, school exclusion, legal penalties) or internalized (the unvaccinated believe they deserve exclusion, or have fused their identity with resistance)? Or both?',
    'Post-mandate-repeal trajectory: if mandate-resistant individuals who were externally suppressed maintain the resistance after the mandate vanishes (no job threat, no school exclusion), suppression was internalized. If resistance collapses once external incentives are removed, suppression was structural.',
    'If suppression is mostly structural, removing the mandate removes the constraint. If suppression is internalized, the constraint persists through normative capture (identity fusion, internalized shame, belief in deserved exclusion), and reclassify from tangled_rope to snare (persistent extraction through internalized suppression, not just external coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_suppression_mechanism, empirical, 'Mechanism of suppression: structural coercion vs. internalized normative capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__proportionality_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__proportionality_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__proportionality_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(vacc_tr_t35, vaccine_mandate_balance__proportionality_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(vacc_be_t35, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(vacc_su_t35, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.18).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% vaccine_mandate_balance is a contested kernel with three readings: proportionality (this story), bodily_autonomy_primary, and public_health_primary. The three readings share the same domain (vaccine mandate legitimacy) but instantiate structurally distinct constraints with different ε values, different victim sets (conditional on disease parameters vs. categorical), and different authority justifications (proportionality-constrained vs. unconstrained autonomy vs. health-maximalist). Each reading is generated as a separate constraint story and linked via this network edge. The sibling stories are not alternative measurements of one constraint; they are competing institutional framings of the same kernel, each producing different operational constraints. The ε variation by pathogen (smallpox vs. seasonal flu) is captured within this story's omega variables (threshold_determination_capture, reading_coherence_small_vs_large_diseases); the structural difference between readings is captured in the separate constraint files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
