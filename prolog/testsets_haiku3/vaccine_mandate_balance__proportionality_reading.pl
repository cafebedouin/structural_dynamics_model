% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Vaccine Mandate Proportionality Balance (Proportionality Reading)
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The proportionality reading of the vaccine mandate kernel establishes
 *   that mandates are legitimate only when disease severity, transmission
 *   risk, and vaccine safety meet transparent, evidence-based thresholds.
 *   Unlike the public_health_primary reading (which prioritizes collective
 *   protection categorically) or the bodily_autonomy_primary reading (which
 *   rejects mandates entirely), this reading attempts to balance state
 *   authority to protect vulnerable populations against individual consent by
 *   conditioning mandate legitimacy on pathogen-specific proportionality. The
 *   constraint's extractiveness is moderate (0.42) because it compels
 *   vaccination for many who would refuse, but remains bounded by the
 *   thresholds themselves — mandates for low-severity pathogens would fail
 *   the constraint; robust exemptions reduce the net coercive burden.
 *   Suppression is lower (0.38) because the reading explicitly requires
 *   transparent thresholds and robust exemption processes, both of which
 *   reduce concealed extraction. Theater rises over the interval as
 *   enforcement increasingly focuses on demonstrating threshold-compliance
 *   rhetoric rather than adapting thresholds to changing epidemiology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.38).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Balance (Proportionality Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '54405766-db2f-499d-a67e-01765e01ea32').
narrative_ontology:cs_kernel_codification('54405766-db2f-499d-a67e-01765e01ea32', fixed_text).
narrative_ontology:cs_authority_grounding('54405766-db2f-499d-a67e-01765e01ea32', lineage).
narrative_ontology:cs_interpretation_layer_present('54405766-db2f-499d-a67e-01765e01ea32').
narrative_ontology:cs_reading_relation('54405766-db2f-499d-a67e-01765e01ea32', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('54405766-db2f-499d-a67e-01765e01ea32', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('54405766-db2f-499d-a67e-01765e01ea32', foundational, proportionality_gates_state_power).
narrative_ontology:cs_axiom_status(proportionality_gates_state_power, holdable).
narrative_ontology:cs_axiom_grounding('54405766-db2f-499d-a67e-01765e01ea32', proportionality_gates_state_power, deontological).
narrative_ontology:cs_axiom('54405766-db2f-499d-a67e-01765e01ea32', foundational, disease_parameters_determine_legitimacy).
narrative_ontology:cs_axiom_status(disease_parameters_determine_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('54405766-db2f-499d-a67e-01765e01ea32', disease_parameters_determine_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('54405766-db2f-499d-a67e-01765e01ea32', proportional_state_authority).
narrative_ontology:cs_drift_state('54405766-db2f-499d-a67e-01765e01ea32', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('54405766-db2f-499d-a67e-01765e01ea32', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_population).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_contraindications).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elderly, immunocompromised, and those who cannot receive vaccines depend on population immunity thresholds to avoid lethal disease. Benefit from mandates only when the disease risk and vaccine efficacy justify the collective constraint; have no exit from exposure if herd immunity falls below critical levels.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Cannot receive live vaccines and depend entirely on others' immunity. Benefit directly when mandates are proportional to disease threat; harmed when mandates are deployed for low-severity pathogens or when exemption scope expands beyond medical contraindications.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, constrained, national).

% Subject to mandate compliance or face employment, education, or civic access restrictions. Under proportionality reading, mandates are legitimized only when disease severity and transmission risk justify the imposition; hesitancy itself does not create a medical contraindication, so exemptions are conditional on meeting proportionality thresholds for the specific pathogen. They bear the direct bodily burden and access restrictions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_population, payer,
    organized, biographical, constrained, national).

% Face exclusion from civic participation when mandates apply, but the proportionality reading requires robust exemption processes that recognize medical contraindications. They bear the burden of accessing and demonstrating exemption status; their exemption depends on whether mandate legitimacy itself is established through proportionality thresholds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_contraindications, payer,
    moderate, biographical, constrained, national).

% Set the evidentiary and proportionality standards for mandate deployment; decide which thresholds (severity, transmission, safety) trigger legitimacy; administer exemption review. Under this reading, they are constrained by proportionality requirements that require transparent, evidence-based thresholds rather than categorical mandates. They collect authority to compel vaccination but are bound by the duty to justify it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Expected to assess individual contraindications and issue exemptions; required to attest proportionality assessments. Under proportionality reading, they carry both advisory and gatekeeping roles — their clinical judgment is the mechanism for robust exemption, and the field's consensus on safety standards underwrites the legitimacy of mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, medical_professionals, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, medical_professionals, agenda_setter).

% Authorize mandate policy and set or defer proportionality standards. Under proportionality reading, they are required to establish or reference transparent thresholds (disease severity, transmission rates, vaccine safety profiles) rather than issuing categorical mandates; the reading's legitimacy depends on this constraint being applied prospectively and defensibly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, elected_officials, agenda_setter,
    institutional, generational, analytical, national).

% Contest the premise that state-compelled medical intervention is ever permissible; would argue that proportionality thresholds rationalize what remains unjustifiable coercion. Their position is structurally excluded from the proportionality reading (which accepts mandates in principle) and would reject its thresholds as window-dressing on fundamentally rights-violating policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Propose infection-acquired immunity, targeted protection, or early treatment as alternatives to mass vaccination. Their absence from the proportionality framework means these alternatives are not weighed against mandates in the threshold calculation; proportionality as framed here presumes vaccination is the primary lever.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, alternative_protection_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes epidemiological thresholds for when collective vaccination mandates become legitimate: disease must meet a severity standard (case fatality rate, hospitalization burden) and transmission risk standard (R-value, outbreak potential); vaccine safety must be demonstrated in the target population; exemptions must be robust and prospectively defined. Solves the collective-action problem of achieving herd immunity when voluntary vaccination alone cannot, while constraining state power to demand medical compliance to justified cases.
% TRANSFER_FUNCTION: Moves bodily autonomy (the authority to refuse the vaccine) from individuals to the state when thresholds are met, in exchange for protection of vulnerable populations. The constraint transfers decision-making power from individuals to health authorities when proportionality is demonstrated; hesitant individuals bear the cost of compliance or exclusion; vulnerable populations receive the benefit of population-level protection.
% ABSENT_VOICES: Bodily autonomy absolutists (who contest the premise that proportionality can justify any mandate) and advocates for infection-acquired immunity or alternative protection strategies (whose approaches are not weighed in the proportionality calculus) are structurally excluded. Their participation would reframe whether thresholds are sufficient or whether the coordination function itself is legitimate.
% DISAPPEARANCE_RATIONALE: If proportionality thresholds were removed and no mandate constraint remained, public health authorities would lose the ability to compel vaccination, vulnerable populations would face higher transmission risk if herd immunity fell below critical levels, and disease control would depend entirely on voluntary participation. The constraint's disappearance would reshape herd immunity outcomes and the state's public health authority.
% FOUNDING_PROBLEM: Voluntary vaccination uptake is insufficient to achieve herd immunity for high-transmission, high-severity pathogens (historically: smallpox, measles); vulnerable populations cannot vaccinate and depend on population-level protection; without a mechanism to compel sufficient vaccination, lethal disease spreads to those who cannot protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists and immunologists testify that for certain pathogens (measles, for instance) voluntary uptake historically fell below herd immunity thresholds and vulnerable populations experienced measles mortality. Bodily autonomy advocates and epidemiological critics contest whether the founding problem is still live (citing higher baseline immunity, better early treatment, lower fatality rates in modern contexts) and whether mandates are the only mechanism (arguing for targeted protection, treatment, or voluntary outreach). Legislative testimony and academic literature from outside public health authorities support both the live-problem and contested-legitimacy readings.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts at 0.25 (early interval, when pathogen threat is assessed and thresholds are transparent) and rises to 0.42 as enforcement infrastructure solidifies and the threshold-setting process becomes bureaucratized, less responsive to changing epidemiology. The constraint compels vaccination for those who refuse it absent medical contraindication, but only when proportionality thresholds are met — this conditionality limits extractiveness relative to categorical mandates. Suppression mirrors extractiveness because the constraint's legitimacy depends on transparent thresholds; once thresholds are set, enforcement becomes administrative rather than overtly coercive, but the suppression requirement rises as exemption review tightens and authorities restrict exemption scope beyond medical contraindications. Theater rises modestly (0.08 to 0.22) as the proportionality framing increasingly becomes a rhetorical cover for mandate persistence even as underlying disease threat declines — the constraint's original function (responding to high-severity pathogens) may be completed, but the mandate structures and enforcement machinery persist, defended by appeals to threshold-compliance rather than current epidemiology. Accessibility collapse is moderate (0.48): alternatives exist (infection-acquired immunity, early treatment, targeted protection) but are actively excluded from the proportionality calculation, so once mandates are deployed, individual exit is severely constrained. Resistance is high (0.71) because the constraint directly impinges on perceived bodily autonomy and generates organized opposition; hesitant populations mount sustained resistance through political, legal, and informational channels.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority seat, the constraint is a necessary defense of vulnerable populations — a coordination function that achieves what voluntary vaccination cannot. From the hesitant population seat, the constraint is extraction of bodily autonomy justified by proportionality thresholds that are opaque, contested, and subject to bureaucratic expansion. From the vulnerable population seat, the constraint is protection; from the hesitant population seat, it is coercion. The engine computes these divergent classifications from the structural data: the beneficiary's low directionality and the target's high directionality produce different type verdicts at the same seats. The proportionality reading accepts that both readings are structurally sound; legitimacy depends on whether thresholds are actually met and whether exemptions are actually robust.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations and immunocompromised individuals are structural beneficiaries: they receive the protection of population immunity without bearing the vaccination burden themselves. Their directionality is low (d ~ 0.15–0.25), approaching full beneficiary. Vaccine-hesitant populations are structural targets: they bear the compliance burden (vaccination or exclusion) and exit is constrained by employment, education, and civic participation requirements. Their directionality is high (d ~ 0.75–0.85). Medical professionals and public health authorities are agenda-setters who define thresholds and enforce exemptions; they collect neither the benefits of population immunity nor the costs of compliance, but they do exercise state power and bear the burden of threshold-setting. Their directionality is mid-range (d ~ 0.45–0.55) — they experience the constraint as an expansion of their authority, but the constraint also binds them to proportionality requirements that limit their discretion. Elected officials similarly experience dual directionality: they gain political authority to act on public health but are constrained by the requirement to defend thresholds transparently.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — voluntary vaccination insufficient for herd immunity against high-severity, high-transmission pathogens — is either live or dead depending on current epidemiology. The proportionality reading guards against mandatrophy by tying legitimacy to pathogen-specific thresholds: if the founding problem becomes dead (voluntary uptake rises, or disease severity drops), the threshold gates should close and mandates should end. The measurement series shows extractiveness rising and plateauing by interval-end, and theater rising continuously — a pattern consistent with mandatrophy onset (the founding problem is solved or contested, but enforcement infrastructure persists). If disease threat genuinely declined over the interval but mandates remained defended by proportionality rhetoric, that is mandatrophy: the constraint persists by inertia and is maintained by appeals to thresholds that are no longer met. The constraint avoids pure mandatrophy if thresholds are genuinely re-evaluated and mandates are withdrawn when no longer proportional — but the rising theater ratio (enforcement defending rhetoric rather than function) suggests this is not happening. The proportionality reading is meant to prevent mandatrophy, but the measurement suggests the mechanism is failing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_setting_opacity,
    'Are proportionality thresholds for disease severity, transmission risk, and vaccine safety set transparently and updated as evidence changes, or do they become fixed bureaucratic rules decoupled from current epidemiology?',
    'Audit of threshold-setting process: are thresholds explicitly stated prospectively? Are they revisited when disease burden, transmission rates, or vaccine safety data change? Are mandates withdrawn when thresholds are no longer met?',
    'If thresholds are opaque or fixed, the constraint devolves into a categorical mandate dressed in proportionality language — functionally equivalent to public_health_primary, despite the reading''s stated constraints. If thresholds are transparent and dynamic, the constraint maintains its distinction as a bounded coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_setting_opacity, empirical, 'Whether proportionality thresholds function as real constraints or as post-hoc justifications for predetermined mandate policy.').

omega_variable(
    exemption_robustness_erosion,
    'Are exemption processes robust (low burden, clear criteria, accessible review) or do they erode over time toward categorical denial except for narrow medical contraindications?',
    'Track exemption approval rates, appeal processes, and scope of recognized contraindications over the measurement interval. Compare stated exemption policy at t0 vs. actual practice at tn.',
    'If exemptions erode, the constraint shifts from tangled_rope (coordinating herd immunity with protected individual refusal) toward snare (extraction with no legitimate exit). If exemptions remain robust, the constraint maintains its balance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_robustness_erosion, empirical, 'Whether robust exemption is preserved or whether exemption scope contracts over time.').

omega_variable(
    reading_frame_collapse,
    'Does the proportionality reading remain tenable, or does the debate collapse into a binary between bodily_autonomy_primary and public_health_primary, making the middle-ground reading incoherent?',
    'Monitor elite discourse and policy debate: does the proportionality reading remain defended by credible voices, or do arguments increasingly polarize around autonomy-vs-public-health poles?',
    'If the reading collapses, the constraint''s legitimacy foundation is lost; the actual rule becomes either autonomous refusal (bodily_autonomy wins) or categorical mandate (public_health wins), and the proportionality framing becomes pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_collapse, conceptual, 'Whether the proportionality reading can persist as a live normative position in polarized discourse or whether it collapses into adjacent readings.').

omega_variable(
    pathogen_specificity_variation,
    'Does extractiveness of the mandate genuinely vary by pathogen (high for smallpox/measles, low for seasonal flu), or is enforcement threshold-indifferent once mandate infrastructure exists?',
    'Measure extraction and enforcement intensity for mandates applied to different pathogens. Compare mandate stringency for high-severity vs. low-severity diseases at the same time point.',
    'If extractiveness is pathogen-specific, the constraint is functioning as proportionality theory prescribes (ε varies by disease parameters). If extractiveness is uniform across pathogens, the constraint is not proportional; it is categorical mandate infrastructure.  This tests whether the reading''s core claim (proportionality depends on disease parameters) is structurally true.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathogen_specificity_variation, empirical, 'Whether mandate extractiveness is conditional on disease-specific proportionality thresholds or is uniform regardless of pathogen severity.').

omega_variable(
    committer_frame_sibling_distinction,
    'What structural feature distinguishes this proportionality_reading from the bodily_autonomy_primary and public_health_primary siblings? Do all three coexist as live options, or does one foreclosure relation hold?',
    'Assess whether the readings are held simultaneously by different political/epistemic communities (coexist_with) or whether one reading''s core premise logically contradicts another''s (forecloses). Examine jurisprudential and ethical literature to map the logical relations among reading positions.',
    'If readings coexist, the constraint''s classification depends on the observer''s seat; the engine computes different types for different stakeholders, reflecting their reading position. If one forecloses another, the three-reading kernel resolves into a two-reading space, and the proportionality reading is either vindicated or eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_sibling_distinction, conceptual, 'Logical relations among the three vaccine mandate kernel readings: foreclosure, coexistence, or influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__proportionality_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t5, observed).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t10, observed).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__proportionality_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(vacc_tr_t15, observed).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__proportionality_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t25, observed).
narrative_ontology:measurement(vacc_tr_t35, vaccine_mandate_balance__proportionality_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(vacc_be_t5, observed).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(vacc_be_t10, observed).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(vacc_be_t15, observed).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(vacc_be_t25, observed).
narrative_ontology:measurement(vacc_be_t35, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement_basis(vacc_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(vacc_su_t5, observed).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(vacc_su_t10, observed).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(vacc_su_t15, observed).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(vacc_su_t25, observed).
narrative_ontology:measurement(vacc_su_t35, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(vacc_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, herd_immunity_threshold_maintenance).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel is decomposed into three distinct constraint stories, each representing a different reading of the foundational commitment: 'the state may compel medical intervention to protect public health.' The proportionality_reading (this story) instantiates the middle-ground position: mandates are legitimate only when disease severity and transmission risk meet transparent thresholds, and exemptions must be robust. The bodily_autonomy_primary reading rejects the premise entirely; the public_health_primary reading accepts mandates when herd immunity requires them, regardless of individual burden. All three readings share the same kernel but differ in their core normative axioms and their threshold-setting logic. They are structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. This reading's extractiveness (0.42) is conditional on disease parameters; it is lower than public_health_primary (which would be ~0.65 for categorical mandates) but higher than bodily_autonomy_primary (which would declare ε=0.0 by denying mandate legitimacy). The readings are linked via network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
