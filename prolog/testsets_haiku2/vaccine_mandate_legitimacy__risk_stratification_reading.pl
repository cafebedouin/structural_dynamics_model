% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy Framework
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the risk-stratification reading of vaccine
 *   mandate legitimacy: the claim that state authority to mandate vaccination
 *   is proportional and constitutionally permissible when the mandate is
 *   calibrated to actuarial risk thresholds, but becomes illegitimate
 *   (arbitrary, extractive) when applied blanket across all risk strata. The
 *   reading sits between the extremes: neither absolute bodily autonomy
 *   (which would forbid all mandates regardless of risk) nor unrestricted
 *   public health authority (which would justify blanket mandates). The
 *   constraint's extracted measurement series shows rising extractiveness
 *   early (as threshold definitions tightened and low-risk mandates
 *   persisted) and plateau as the system normalized into its stable targeting
 *   regime. The theater ratio reflects a modest performative component: early
 *   public communication about risk stratification that later reveals the
 *   thresholds have drifted toward inclusivity rather than true targeting.
 *
 * KEY AGENTS:
 *   - public_health_authority: institutional agenda-setter, designs and enforces threshold policy
 *   - high_risk_population: moderate power, identity-locked beneficiary (age/condition determines benefit), gains disease protection
 *   - low_risk_unvaccinated_individuals: moderate power, constrained payers, forced to bear vaccination burden despite minimal personal risk
 *   - vaccine_hesitant_moderate_risk_groups: organized payers at the threshold boundary, legitimacy of mandate is contested for this stratum
 *   - courts_judiciary: institutional observer, adjudicates whether threshold definition satisfies proportionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.38).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy Framework").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '3a73dbb2-2d1f-42da-8b4a-073e691bc711').
narrative_ontology:cs_kernel_codification('3a73dbb2-2d1f-42da-8b4a-073e691bc711', formalized).
narrative_ontology:cs_authority_grounding('3a73dbb2-2d1f-42da-8b4a-073e691bc711', lineage).
narrative_ontology:cs_interpretation_layer_present('3a73dbb2-2d1f-42da-8b4a-073e691bc711').
narrative_ontology:cs_reading_relation('3a73dbb2-2d1f-42da-8b4a-073e691bc711', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a73dbb2-2d1f-42da-8b4a-073e691bc711', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('3a73dbb2-2d1f-42da-8b4a-073e691bc711', foundational, proportionality_principle_constrains_mandate_authority).
narrative_ontology:cs_axiom_status(proportionality_principle_constrains_mandate_authority, holdable).
narrative_ontology:cs_axiom_grounding('3a73dbb2-2d1f-42da-8b4a-073e691bc711', proportionality_principle_constrains_mandate_authority, deontological).
narrative_ontology:cs_axiom('3a73dbb2-2d1f-42da-8b4a-073e691bc711', foundational, actuarial_risk_stratification_empirically_defensible).
narrative_ontology:cs_axiom_status(actuarial_risk_stratification_empirically_defensible, holdable).
narrative_ontology:cs_axiom_grounding('3a73dbb2-2d1f-42da-8b4a-073e691bc711', actuarial_risk_stratification_empirically_defensible, empirically_contingent).
narrative_ontology:cs_reference_frame('3a73dbb2-2d1f-42da-8b4a-073e691bc711', proportional_mandate_authority_framework).
narrative_ontology:cs_drift_state('3a73dbb2-2d1f-42da-8b4a-073e691bc711', threshold_scope_creep_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a73dbb2-2d1f-42da-8b4a-073e691bc711', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_moderate_risk_groups).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_principle_in_public_health).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, actuarial_risk_stratification_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces mandate policy based on actuarial risk thresholds. Sets the threshold definition (e.g., age ≥65, immunocompromised status, occupational exposure). Justifies thresholds as protecting vulnerable populations and healthcare system capacity while respecting proportionality. Maintains the apparatus for classifying individuals into risk strata and enforcing differential requirements.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Elderly, immunocompromised, or high-exposure individuals who benefit from community vaccination via mandate enforcement. Their risk of severe disease is genuine; mandate reduces their exposure to disease in shared spaces. Locked into age/condition identity; cannot exit the risk category. Benefit from the constraint even if they initially hesitate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_population, beneficiary,
    moderate, biographical, identity_locked, national).

% Young, healthy, unvaccinated people with minimal actuarial risk who are required to vaccinate (or face employment/school/travel restrictions) under the mandate. They bear the constraint's cost (vaccination, compliance monitoring) despite low individual risk. Exit options include vaccine compliance (absorbing the intervention), geographic relocation, or occupational/educational exit — all costly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Individuals in moderate-risk strata (e.g., age 45-65, some chronic conditions) who are vaccine-hesitant or hold cultural/religious objections to the intervention. They face the mandate but sit at an actuarial threshold where the legitimacy claim is contested — their risk level sits at the boundary where proportionality becomes unclear. Resistance is organized; exemptions and deferrals are negotiable.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_moderate_risk_groups, payer,
    organized, biographical, constrained, national).

% Reduction in severe disease burden reduces hospital ICU occupancy and workforce strain. Not an actor; benefits accrue to the institutional system's operational resilience. A non-agent entity kept for narrative completeness.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity, beneficiary,
    powerless, immediate, analytical, national).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity).

% Some societies advocate for individualized clinical judgment and tiered risk assessment rather than categorical mandates. Their medical expertise in risk stratification is invoked to support mandate design but their dissent from blanket approaches is structurally marginalized by political pressure to show visible compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, medical_professional_societies, excluded,
    organized, generational, constrained, national).

% Argue that mandatory medical intervention violates bodily autonomy even if risk-stratified. Their objections to the mandate's legitimacy are heard but not centered in policy debate; they are excluded from threshold-setting authority despite having standing in constitutional discourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, civil_liberties_organizations, excluded,
    organized, generational, constrained, national).

% Evaluate mandate constitutionality under proportionality tests. Review whether the threshold definition matches the articulated public health need and whether less-restrictive alternatives exist. Their role is to adjudicate the reading itself: whether risk stratification actually satisfies proportionality or is a veneer for expanded state coercion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects high-vulnerability populations from disease transmission by establishing vaccination thresholds calibrated to actuarial risk. Coordinates individual compliance decisions around a shared epidemiological model that targets intervention where risk-benefit analysis justifies it.
% TRANSFER_FUNCTION: Transfers bodily autonomy burden from low-risk individuals (who must vaccinate despite minimal personal risk) to the system's higher-risk beneficiary populations (who gain protection). The mechanism: mandatory vaccination becomes a condition of employment, education, or public space access for those below threshold exemption.
% ABSENT_VOICES: Vaccine-hesitant moderate-risk individuals whose medical objections or cultural values dissent from the mandate; dissident medical professionals who advocate tiered individualized judgment; civil liberties organizations advocating bodily autonomy as absolute. These voices are excluded from threshold-setting authority even though they would argue the threshold definition is pretextual or too broad.
% DISAPPEARANCE_RATIONALE: Public health proponents argue disappearance would cause measurable harms: hospital surge, excess mortality in high-risk strata, occupational hazards in healthcare/long-term care settings. Autonomy advocates argue disappearance would restore individual choice and reduce state coercive apparatus. The actual rearrangement depends on whether the threshold was empirically justified or was cover for blanket coercion.
% FOUNDING_PROBLEM: Early pandemic uncertainty about transmission and risk stratification led to undifferentiated mandates. As epidemiological evidence matured, the founding problem shifted from 'need to maximize vaccination' to 'need to target vaccination where risk-benefit tilts toward intervention.' Risk stratification is framed as solving this matured problem: proportionality within legitimate public health authority.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest that risk stratification is scientifically sound and ethically superior to blanket mandates. Autonomy advocates and some constitutional scholars attest that the founding problem (pandemic uncertainty justifying extraordinary measures) has passed, and the constraint persists as normalized state coercion. Threshold-independent empirical measures (hospitalization by age/risk stratum, vaccine effectiveness by risk group) corroborate the risk structure; the legitimacy of USING that structure to mandate intervention is the contested remainder.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at plateau) because the constraint operates partially as genuine coordination (high-risk protection) and partially as coercion (low-risk mandates). The measurement series show extraction rising early as thresholds drift downward (capturing more low-risk individuals) and plateauing once the stable targeting regime is established. Theater is low (0.22) because risk stratification is genuinely calculated from epidemiological data; the performative component is modest — early communication about proportionality that later reveals thresholds have expanded. Suppression is moderate (0.38) because the constraint's persistence depends on both active enforcement (vaccination verification, employment/school access gates) and legitimacy claims (that the threshold satisfies proportionality). The payer seats are constrained, not trapped — they can exit via vaccination compliance or relocation, though both are costly. The reading's core claim is that proportionality CAN be achieved through thresholds; if thresholds collapse toward blanket mandates, extractiveness would rise sharply as the legitimacy claim fails.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority's seat, the constraint is genuine coordination: thresholds are calibrated to risk, protect vulnerable populations, and respect individual autonomy by exempting low-risk groups. From the low-risk payer's seat, the same structure operates as coercion: vaccination is mandated despite minimal personal benefit, justified by abstract public health rather than individual risk-benefit. From the moderate-risk boundary's seat, legitimacy is uncertain — the threshold definition is the entire question. The engine computes this divergence from the structural data: the same constraint produces different effective extraction values for different power atoms and exit positions. The authorized claim is tangled_rope; the metrics support it: genuine coordination function (high-risk protection) paired with asymmetric extraction (low-risk mandates without proportionate justification at that stratum).
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk populations are near the beneficiary end (d ≈ 0.1–0.2): they benefit from disease protection without bearing mandate costs (their medical status exempts most from vaccine hesitation). Low-risk individuals are near the target end (d ≈ 0.7–0.8): they bear mandate costs (vaccination, compliance burden) despite minimal personal risk; their exit options are constrained. Public health authority is the agenda-setter (d ≈ 0.3): it collects authority and legitimacy from mandate enforcement but also bears the burden of threshold adjudication and legal challenge. The moderate-risk boundary payers sit at d ≈ 0.5–0.6: the legitimacy of their mandate is precisely contested, making their directionality uncertain. This uncertainty is routed to omega variables rather than collapsed into a single d value.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap (dead founding problem, persist by inertia) because its founding problem is genuinely contested, not dead. The early pandemic crisis (uncertainty justifying extraordinary measures) has passed, but the question of whether risk-stratified mandates address a live public health need remains empirically open and normatively contested. The classification as tangled_rope (not piton) reflects this: if the founding problem were clearly dead, theater would rise sharply and suppression would depend purely on inertia; instead, suppression remains tied to active enforcement of thresholds because the thresholds themselves are the legitimacy apparatus. The risked plateau (extractiveness and suppression level off at time 24+) reflects the system settling into a stable regulatory regime, not degrading into pure performance. Mandatrophy would appear as rising theater and suppression with flat or declining coordination function — the measurement series show no such pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_legitimacy,
    'What threshold definition (age cutoff, medical criteria, occupational exposure) actually satisfies proportionality, and does the authorized threshold match that definition or does it drift toward blanket inclusion?',
    'Empirical analysis of mandate scope creep: compare the initial threshold definition to the de facto scope of enforcement over time. If low-risk populations are included in practice despite official threshold exemptions, the threshold is pretextual and the constraint has collapsed toward blanket coercion.',
    'If thresholds are maintained as defined, the constraint remains tangled_rope (genuine coordination + targeted extraction). If thresholds drift and enforcement becomes blanket, extractiveness rises sharply and the constraint reclassifies toward snare (pure extraction under coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_definition_legitimacy, empirical, 'Whether the declared threshold definition is actually enforced or has drifted toward blanket mandates.').

omega_variable(
    actuarial_risk_stratification_validity,
    'Are the actuarial risk calculations that define the thresholds empirically sound and updated as epidemiological evidence changes?',
    'Comparison of threshold definitions across time and jurisdictions; external peer review of risk models; analysis of whether thresholds adjust when epidemiological evidence (vaccine effectiveness waning, variant risk, improved treatment) changes.',
    'Sound stratification with evidence-based updates supports the proportionality claim; static thresholds that ignore changed evidence suggest the constraint persists for legitimacy maintenance rather than public health adaptation (rising theater_ratio, potential reclassification toward piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actuarial_risk_stratification_validity, empirical, 'Whether risk stratification reflects current epidemiological evidence or is theater.').

omega_variable(
    proportionality_threshold_ambiguity,
    'Is proportionality a coherent test for distinguishing legitimate from illegitimate mandates, or does the concept admit multiple readings that different stakeholders invoke to reach opposed conclusions?',
    'Constitutional analysis of precedent: do courts apply proportionality consistently across cases, or do different judges reach different mandate verdicts under the same test?',
    'If proportionality is ambiguous, the risk-stratification reading lacks a stable anchor and may collapse into either bodily_autonomy_primacy (courts exclude proportionality as a limiting principle) or public_health_primacy (courts apply proportionality permissively). If proportionality is coherent, the reading remains stable as a middle path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Proportionality principle coherence in public health mandate doctrine.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (enforcement machinery, legal penalties for non-compliance) or internalized (individuals accept mandate legitimacy and comply voluntarily)?',
    'Post-enforcement survey: if suppression persists after enforcement machinery is withdrawn, the suppression is partially internalized. If compliance drops sharply, suppression is primarily structural.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests — the reading has been legitimized and individuals carry acceptance even absent enforcement. If structural, the constraint depends on active state power and would collapse if enforcement ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in mandate compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).
narrative_ontology:measurement(vacc_tr_t32, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t32, observed).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t40, observed).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(vacc_be_t24, observed).
narrative_ontology:measurement(vacc_be_t32, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement_basis(vacc_be_t32, observed).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(vacc_be_t40, observed).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 48, 0.42).
narrative_ontology:measurement_basis(vacc_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement_basis(vacc_su_t24, observed).
narrative_ontology:measurement(vacc_su_t32, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 32, 0.38).
narrative_ontology:measurement_basis(vacc_su_t32, observed).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(vacc_su_t40, observed).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 48, 0.38).
narrative_ontology:measurement_basis(vacc_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__risk_stratification_reading, 0.18).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_legitimacy kernel decomposes into three structurally distinct constraints, each with different ε, beneficiary/victim structures, and stakeholder seats. This reading (risk_stratification) claims moderate extraction justified by proportionate targeting; bodily_autonomy_primacy claims categorical impermissibility (zero extraction justified); public_health_primacy claims unrestricted authority (high extraction justified by collective benefit). The three readings coexist in contemporary constitutional discourse and are linked via network.affects_constraints: risk_stratification_reading influences both extremes, as it occupies the middle ground; any shift in threshold definition or proportionality doctrine affects the legitimacy conditions of sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
