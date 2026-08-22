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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Disease-Proportionate Vaccine Mandate Framework (Proportionality Reading)
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the PROPORTIONALITY READING of the
 *   vaccine_mandate_balance kernel. The proportionality reading holds that
 *   states may compel vaccination only when disease severity, transmission
 *   risk, and vaccine safety meet strict thresholds, and exemptions must be
 *   robust. It occupies a middle position between two sibling readings:
 *   bodily_autonomy_primary (mandates always illegitimate) and
 *   public_health_primary (collective protection supersedes autonomy). The
 *   proportionality reading claims to resolve the tension by making mandate
 *   legitimacy conditional on pathogen severity and proportionate to the
 *   threat. The constraint story evaluates this reading on its own lights: ε
 *   measures the extraction inherent to state-compelled medical intervention
 *   even under proportionality constraints; suppression measures the
 *   enforcement machinery needed to sustain mandate compliance; theater_ratio
 *   measures the extent to which threshold-setting becomes political theater
 *   rather than epidemiological assessment. The claim/metric independence
 *   rule is deliberately applied: this constraint is CLAIMED as tangled_rope
 *   (genuine coordination function protecting vulnerable populations +
 *   asymmetric extraction from the unvaccinated + active enforcement) while
 *   the metrics describe a constraint whose proportionality is contestable
 *   and whose suppression grows over time.
 *
 * KEY AGENTS:
 *   - public_health_authority (institutional, analytical exit): agenda-setter; claims proportionality constrains its discretion; legitimacy depends on threshold credibility
 *   - vulnerable_populations_protected_by_herd_immunity (powerless, trapped): beneficiaries; depend on herd immunity thresholds; face mortality risk if thresholds fall
 *   - unvaccinated_persons_subject_to_mandate (moderate, constrained): payers; subject to medical intervention when thresholds triggered; exit via exemption or jurisdictional relocation
 *   - conscientious_objectors_with_genuine_medical_contraindications (powerless, constrained): payers + excluded; cannot receive vaccines; nominally protected by exemptions but often excluded from review processes
 *   - courts_reviewing_mandate_authority (institutional, analytical): observers; provide corrective feedback; enforce threshold compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.38).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.41).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Disease-Proportionate Vaccine Mandate Framework (Proportionality Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'ff63fe8e-214e-4b53-8d81-f551790bf5d9').
narrative_ontology:cs_kernel_codification('ff63fe8e-214e-4b53-8d81-f551790bf5d9', formalized).
narrative_ontology:cs_authority_grounding('ff63fe8e-214e-4b53-8d81-f551790bf5d9', lineage).
narrative_ontology:cs_interpretation_layer_present('ff63fe8e-214e-4b53-8d81-f551790bf5d9').
narrative_ontology:cs_reading_relation('ff63fe8e-214e-4b53-8d81-f551790bf5d9', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('ff63fe8e-214e-4b53-8d81-f551790bf5d9', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_axiom('ff63fe8e-214e-4b53-8d81-f551790bf5d9', foundational, proportionality_gates_mandate_authority).
narrative_ontology:cs_axiom_status(proportionality_gates_mandate_authority, holdable).
narrative_ontology:cs_axiom_grounding('ff63fe8e-214e-4b53-8d81-f551790bf5d9', proportionality_gates_mandate_authority, deontological).
narrative_ontology:cs_axiom('ff63fe8e-214e-4b53-8d81-f551790bf5d9', foundational, bodily_autonomy_constrained_not_eliminated).
narrative_ontology:cs_axiom_status(bodily_autonomy_constrained_not_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('ff63fe8e-214e-4b53-8d81-f551790bf5d9', bodily_autonomy_constrained_not_eliminated, deontological).
narrative_ontology:cs_reference_frame('ff63fe8e-214e-4b53-8d81-f551790bf5d9', proportionality_constrained_state_authority).
narrative_ontology:cs_drift_state('ff63fe8e-214e-4b53-8d81-f551790bf5d9', contemporary_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff63fe8e-214e-4b53-8d81-f551790bf5d9', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations_protected_by_herd_immunity).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authority_legitimacy).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, unvaccinated_persons_subject_to_mandate).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, conscientious_objectors_with_genuine_medical_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vaccinated_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate thresholds and enforces them via health department authority. Claims the proportionality framework prevents both overreach (seasonal flu mandates) and underreach (smallpox-level threats). Bears the burden of continuous epidemiological assessment and threshold calibration. Legitimacy depends on threshold credibility and transparent exemption review.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Immunocompromised individuals, infants, and persons allergic to vaccine components depend on herd immunity thresholds for protection. Cannot receive certain vaccines themselves. Benefit from mandates calibrated to severe pathogens but not applied to minor respiratory viruses. Face heightened mortality risk if herd immunity falls below protective thresholds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations_protected_by_herd_immunity, beneficiary,
    powerless, immediate, trapped, national).

% Subject to mandate when thresholds are met. Face medical intervention compelled by state authority. Options include vaccination, seeking exemption (if criteria met), employment mobility restrictions if refusing, or jurisdictional relocation. Compliance is enforced through workplace, school, and public facility access restrictions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, unvaccinated_persons_subject_to_mandate, payer,
    moderate, biographical, constrained, national).

% Persons with documented allergies to vaccine components or medical contraindications who cannot receive the vaccine. The proportionality framework nominally protects them via robust exemption processes, but exemption review is often politicized and opaque. They face mandate compliance demands despite medical inability to comply.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, conscientious_objectors_with_genuine_medical_contraindications, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, conscientious_objectors_with_genuine_medical_contraindications, excluded).

% Achieve protection through vaccination; benefit from collective immunity thresholds. Can opt out of vaccination where mandates do not apply. Indirectly benefit from mandate-driven thresholds that protect vulnerable persons they may live with or encounter.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).

% The evolving scientific understanding of disease severity, transmission dynamics, and vaccine safety profiles. The proportionality framework binds mandate legitimacy to this evidence base. Recorded as non-agent: the evidence is the criterion, not a party.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, epidemiological_evidence_base, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_balance__proportionality_reading, epidemiological_evidence_base).

% Challenge mandate authority on bodily autonomy grounds. Would argue that no threshold of public benefit justifies compelled medical intervention; they are structurally excluded from threshold-setting processes because they reject the proportionality framing itself as fundamentally illegitimate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% Provide evidence for threshold calibration: disease severity metrics, transmission models, vaccine efficacy and safety data. Their epistemic authority is what the proportionality framework depends on. Occupy an analytical seat; do not collect from the arrangement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_epidemiologists, observer,
    institutional, generational, analytical, global).

% Adjudicate whether specific mandates meet the proportionality thresholds. Their review authority is what makes the framework judicially enforceable. Provide corrective feedback when the public health authority overshoots or undershoots thresholds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_reviewing_mandate_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates protection of vulnerable populations who cannot vaccinate themselves against severe, transmissible pathogens. Solves the collective-action problem where individual vaccination decisions do not account for herd immunity thresholds needed to protect immunocompromised persons. Establishes a disease-specific assessment mechanism to prevent overapplication to minor threats.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from individuals to public health institutions for the duration of a declared health emergency. In exchange, vulnerable populations receive protection from severe communicable disease and the unvaccinated receive the benefit of proportionality constraints (mandates only for severe pathogens, robust exemptions for genuine medical contraindications).
% ABSENT_VOICES: Bodily-autonomy-primary reading holders are structurally excluded: they reject the entire proportionality framing as illegitimate. Persons with rare, undocumented, or ideologically-motivated medical contraindication claims are often excluded from exemption review processes despite the framework's nominal commitment to robust exemptions. Future generations affected by precedent-setting for emergency authority are absent from threshold debates.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished, vulnerable populations would lose institutionalized protection against severe pathogen transmission; herd immunity for those unable to vaccinate would fall, increasing their mortality risk. Conversely, the absence of the framework would also eliminate a precedent for state-compelled medical intervention, shifting decision-making entirely to individual choice and medical autonomy.
% FOUNDING_PROBLEM: Severe communicable diseases (smallpox, polio, measles) pose lethal risks to immunocompromised persons who cannot vaccinate. Voluntary vaccination alone fails to reach herd immunity thresholds when a significant portion of the population refuses vaccination. The state needs authority to compel vaccination during genuine health emergencies without that authority extending to minor respiratory illnesses where collective action is not necessary.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists and vulnerable-population advocates attest the founding problem of severe-pathogen transmission and herd immunity thresholds remains live. Bodily-autonomy advocates contest whether the founding problem justifies compelled medical intervention under ANY proportionality threshold. Courts have split: some jurisdictions accept proportionality frameworks; others reject them as unconstitutional regardless of threshold severity. Legislative testimony and epidemiological literature support context-dependent mandate legitimacy; civil liberties organizations argue the problem is overstated and the cure (state medical authority) creates worse harms.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the proportionality framework nominally constrains state authority: mandates apply only to severe pathogens, exemptions exist for medical contraindications, and courts review thresholds. However, extraction rises from 0.28 to 0.39 over the first 24 time points as political pressure accumulates for mandate expansion beyond the original pathogen (the measles spike occurred during points 16–24); extraction stabilizes at 0.38–0.39 thereafter, suggesting the framework holds but under continual strain. Suppression is lower (0.41) than extraction because many unvaccinated persons comply voluntarily or perceive vaccination as legitimate, not purely coerced. Theater rises from 0.08 to 0.23 over the interval as threshold-setting increasingly becomes a negotiation between public health authority and political constituencies rather than a technical epidemiological assessment. The measurements are authored on a single shared time grid (all metrics sampled at t=0, 8, 16, 24, 32, 40), enabling the lifecycle drift detection to observe the trade-off: extraction stabilizes while theater continues rising, suggesting the framework's epidemiological credibility erodes even as mandate persistence holds steady.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority seat and the unvaccinated_persons seat should compute differently: the authority sees genuine coordination (protecting vulnerable populations) plus bounded extraction (proportionality thresholds); the unvaccinated seat experiences state-compelled medical intervention with opaque threshold-setting and political drift. From the vulnerable_populations seat, the constraint is almost purely beneficial (herd immunity protection); from the conscientious_objectors seat, it is extraction without exit (medical contraindication exemptions are nominally available but often denied). Courts occupy an observer position, reviewing whether thresholds are met but not participating in the coordination or bearing the extraction directly. These divergent experiences flow from the structural data: differential power (institutional vs. powerless), differential exit options (mobile vs. trapped), differential roles (agenda-setter vs. payer). The engine computes per-seat types from this structural variation; the claim (tangled_rope) reflects the proportionality reading's own self-understanding, which diverges from how powerless payers experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The public_health_authority derives directionality near 0.0 (beneficiary): it sets rules, collects legitimacy from the arrangement, and can exit via policy change. Vulnerable populations derive d slightly above 0.0 (minor beneficiary): they receive protection but cannot participate in threshold-setting and bear risk if herd immunity thresholds are underestimated. Unvaccinated persons derive d near 1.0 (full target): they are compelled into medical intervention, bear the bodily autonomy cost, and exit options are constrained (exemption + relocation + litigation, all imperfect). Conscientious objectors with genuine medical contraindications derive d near 1.0 but with suppression amplified: they cannot comply even if they wished to, and exemption review is opaque, making their exit_options identity_locked rather than merely constrained. The beneficiary/victim split is conditional on disease parameters: for a severe pathogen like smallpox, unvaccinated persons bear high d (mandates are broadly viewed as legitimate) and vulnerable populations collect substantial benefit; for a minor respiratory illness, the extraction is viewed as unjustified and unvaccinated persons experience higher suppression (political backlash and social pressure intensify). The base_properties metrics are authored for a mixed scenario (mandates applied to moderately severe pathogens with some political drift), averaging across the conditional variation.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading avoids misclassifying extraction as pure coordination by explicitly naming the asymmetry: mandates impose bodily autonomy costs on the unvaccinated in exchange for public health authority's commitment to proportionality constraints. The constraint does NOT claim pure coordination (that would be the public_health_primary reading: 'herd immunity is paramount'). The constraint DOES claim bounded extraction: the extraction is justified when proportionality thresholds are met and proportionate to disease severity. The mandatrophy risk is temporal: if thresholds erode over time (theater_ratio rising from 0.08 to 0.23, suppression_requirement rising over the first 24 points), the founding problem (severe pathogen protection) may become decoupled from the enforcement mechanism (mandate authority as a general instrument), turning genuine coordination into inertial extraction. The measurement series signals this risk: extraction stabilizes at 0.38–0.39 while theater continues rising, suggesting threshold-setting is becoming increasingly political. A mandatrophy verdict would be triggered if founding_problem_status shifts from 'live' (severe pathogens remain a genuine collective-action problem) to 'dead' (vaccines are universally available and herd immunity is maintained voluntarily), but the constraint persists due to institutional inertia. The proportionality reading forestalls this by conditioning mandate legitimacy on continuous pathogen-specific assessment, but the measurements suggest that assessment is slipping into political theatre.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_specification,
    'What specific epidemiological thresholds (R₀, case fatality rate, severe outcomes incidence, vaccine efficacy ≥X%, adverse event rate ≤Y%) operationalize proportionality, and who determines them?',
    'Legislative codification of thresholds or judicial precedent establishing bright-line rules; comparison of mandate implementation across pathogens to infer operative thresholds; epidemiological modeling showing which parameter changes would cross the threshold boundary.',
    'Vague thresholds allow political discretion masquerading as proportionality, making the constraint extractive despite the framework; operationalized thresholds make the framework judicially reviewable and reduce political rent-seeking in threshold-setting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_specification, empirical, 'Whether the proportionality framework''s thresholds are specified precisely enough to constrain discretion.').

omega_variable(
    exemption_robustness_vs_political_erosion,
    'Are robust exemption processes—medical contraindications, conscientious objection, documented allergy—actually honored, or are they systematically narrowed during political pressure for compliance?',
    'Audit of exemption approval rates and appeal outcomes by jurisdiction and time period; qualitative interviews with persons denied exemptions despite meeting stated criteria; comparison of stated exemption policy to actual implementation.',
    'If exemptions are robust, persons with genuine contraindications are protected and the constraint operates as tangled_rope (coordination + asymmetric but bounded extraction). If exemptions erode during political pressure, the constraint drifts toward snare (extraction with suppressed exit options).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_vs_political_erosion, empirical, 'Whether exemption promises are honored in practice or systematically violated.').

omega_variable(
    pathogen_specificity_vs_categorical_authority,
    'Does the proportionality framework truly bind mandate authority to each pathogen''s severity profile, or does it function as post-hoc legitimation for categorical state authority to compel vaccination regardless of pathogen characteristics?',
    'Historical analysis: for which pathogens were mandates invoked and declined, and why? Did thresholds actually gate decisions, or did political interests drive mandate invocation and threshold narratives were constructed afterward? Counterfactual: what would it take for public health authority to decline a mandate under this framework?',
    'If pathogen-specific, the constraint is genuinely proportionate and victims'' losses scale with disease severity. If categorical authority disguised as proportionality, the framework is a reading-level false summit (beneficiaries are the authority, not the vulnerable population), and ε should be reclassified upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_specificity_vs_categorical_authority, conceptual, 'Whether proportionality thresholds actually gate mandate decisions or merely rationalize categorical authority.').

omega_variable(
    kernel_reading_contest,
    'This is one reading of the vaccine_mandate_balance kernel. The bodily_autonomy_primary reading holds that state-compelled medical intervention is never justified; the public_health_primary reading holds that collective protection supersedes individual consent. Which reading''s core premise is structurally true?',
    'This is a conceptual omega: no empirical data resolves it. The readings represent different normative commitments about the relationship between state authority and bodily autonomy. Omega documents that this constraint instantiates ONE reading among contested alternatives, none of which can be falsified by evidence alone.',
    'If bodily_autonomy_primary is adopted, mandates are always illegitimate regardless of pathogen severity, and this constraint''s entire framework becomes incoherent. If public_health_primary is adopted, the proportionality framework is unnecessary—public health needs supersede autonomy automatically. The proportionality reading occupies a middle position that both sibling readings reject.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel-level disagreement about whether bodily autonomy or collective protection is foundational.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.41) structural—enforcement machinery that ceases when the external mandate is lifted—or internalized—persons internalize the judgment that they have a duty to vaccinate and experience suppression as guilt or shame even absent external coercion?',
    'Post-mandate trajectory analysis: when mandates are lifted, do vaccination rates drop sharply (structural suppression) or persist at mandate-era levels (internalized norm adoption)? Qualitative interviews exploring whether persons experience the mandate as external coercion or as vindication of a principle they came to endorse.',
    'If structural, the constraint''s suppression is reversible. If internalized, the constraint carries psychological burden beyond its formal scope, and the measured suppression understates its actual impact on persons'' autonomy and self-conception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression persists after external mandate mechanisms are removed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).
narrative_ontology:measurement(vacc_tr_t32, vaccine_mandate_balance__proportionality_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t32, observed).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_balance__proportionality_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(vacc_be_t24, observed).
narrative_ontology:measurement(vacc_be_t32, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(vacc_be_t32, observed).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(vacc_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(vacc_su_t24, observed).
narrative_ontology:measurement(vacc_su_t32, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement_basis(vacc_su_t32, observed).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(vacc_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel decomposes into three structurally distinct constraints corresponding to three readings of state authority over vaccination. The proportionality_reading (this constraint) conditions mandate legitimacy on pathogen severity and robust exemptions. The bodily_autonomy_primary reading treats mandates as categorically illegitimate. The public_health_primary reading treats herd immunity as superseding individual consent. These are not the same constraint viewed from different angles—their ε values and beneficiary/victim structures differ substantially depending on how severely the kernel's commitment is read. Link all three stories via network.affects_constraints to document the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
