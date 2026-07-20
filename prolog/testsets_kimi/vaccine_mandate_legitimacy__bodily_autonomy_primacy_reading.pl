% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Absolute Bodily Autonomy Primacy Against Vaccine Mandates
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint is the bodily_autonomy_primacy_reading of the
 *   vaccine_mandate_legitimacy kernel. The kernel is the contested question
 *   of whether and when state vaccine mandates are legitimate. This reading
 *   instantiates an absolute prohibition: medical self-sovereignty is
 *   inviolable and state coercion is categorically impermissible regardless
 *   of outcome. Sibling readings include public_health_primacy_reading
 *   (collective harm justifies mandate authority) and
 *   risk_stratification_reading (targeted mandates pass proportionality while
 *   blanket mandates fail). The constraint coordinates a genuine
 *   liberty-protection function while asymmetrically extracting health
 *   security from immunocompromised and high-risk populations who depend on
 *   population-level transmission suppression.
 *
 * KEY AGENTS:
 *   - Liberty advocacy movements (agenda-setter/beneficiary): Organized litigation and lobbying power, mobile exit â drive the absolutist reading and collect political legitimacy.
 *   - Immunocompromised populations (payer): Powerless, trapped â bear elevated infection risk when mandates are barred.
 *   - High-risk vulnerable groups (payer): Powerless, constrained â bear the epidemiological externality of reduced population uptake.
 *   - Bioethics scholars (observer): Analytical seat â document the autonomy-solidarity tension and comparative outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.62).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.65).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Absolute Bodily Autonomy Primacy Against Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '61b9f864-3ef1-4884-887a-1d05834bca4d').
narrative_ontology:cs_kernel_codification('61b9f864-3ef1-4884-887a-1d05834bca4d', fixed_text).
narrative_ontology:cs_authority_grounding('61b9f864-3ef1-4884-887a-1d05834bca4d', lineage).
narrative_ontology:cs_interpretation_layer_present('61b9f864-3ef1-4884-887a-1d05834bca4d').
narrative_ontology:cs_reading_relation('61b9f864-3ef1-4884-887a-1d05834bca4d', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('61b9f864-3ef1-4884-887a-1d05834bca4d', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('61b9f864-3ef1-4884-887a-1d05834bca4d', foundational, medical_self_sovereignty_absolute).
narrative_ontology:cs_axiom_status(medical_self_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('61b9f864-3ef1-4884-887a-1d05834bca4d', medical_self_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('61b9f864-3ef1-4884-887a-1d05834bca4d', foundational, state_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('61b9f864-3ef1-4884-887a-1d05834bca4d', state_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('61b9f864-3ef1-4884-887a-1d05834bca4d', absolute_medical_sovereignty).
narrative_ontology:cs_drift_state('61b9f864-3ef1-4884-887a-1d05834bca4d', contemporary_public_health_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61b9f864-3ef1-4884-887a-1d05834bca4d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_risk_vulnerable_groups).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, negative_medical_liberty_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, anti_paternalist_state_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% File constitutional and human-rights litigation to block vaccine mandates at every level of government, frame medical refusal as a fundamental liberty, and mobilize grassroots funding and membership around absolute bodily autonomy. When courts accept the absolutist framing, the movement gains precedential legitimacy, media attention, and donor inflows.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary).

% Depend on population-level herd immunity and low community transmission because their conditions prevent vaccine efficacy or safe exposure. When absolute autonomy blocks mandates, they face higher background transmission without individual recourse, remaining confined to protective isolation or accepting elevated infection risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_populations, payer,
    powerless, biographical, trapped, national).

% Include elderly and clinically fragile people whose mortality and morbidity from vaccine-preventable illness far exceed population averages. They bear the epidemiological externality of reduced uptake, experiencing constrained mobility and heightened anxiety in high-transmission environments where mandates are categorically barred.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_risk_vulnerable_groups, payer,
    powerless, biographical, constrained, national).

% Observe and document the tension between deontological autonomy frameworks and solidarity-based public health ethics, comparing outcome data across jurisdictions with different constitutional balances.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an absolute boundary against state medical coercion, coordinating collective expectations that bodily integrity cannot be overridden by public health claims regardless of outcome severity.
% TRANSFER_FUNCTION: Shifts epidemiological risk from the general population (who gain the liberty to refuse vaccination) to immunocompromised and clinically vulnerable populations (who cannot benefit from direct protection and rely on population-level suppression), while transferring political capital and ideological legitimacy to liberty advocacy movements.
% ABSENT_VOICES: Public health agencies and epidemiologists who would argue for proportionate, risk-stratified coercion are structurally overridden by the absolute prohibition; immunocompromised individuals are often not in the room where the constitutional balance is struck.
% DISAPPEARANCE_RATIONALE: If absolute bodily autonomy vanished, states would regain the policy space to impose risk-stratified or blanket mandates during outbreaks, transmission dynamics would shift toward higher uptake scenarios, and the political economy of medical liberty would reorganize around proportionality rather than absolutism.
% FOUNDING_PROBLEM: Historical medical abuse by state and medical institutions (forced sterilization, non-consensual experimentation, Tuskegee) created a demand for an inviolable individual right against medical coercion.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and bioethicists outside the liberty advocacy movement attest the historical abuses; however, public health ethicists and vulnerable-population advocates contest that an absolute ban is the appropriate remedy, arguing proportionate, supervised coercion would prevent current harm without repeating historical crimes.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial risk externalization to vulnerable populations when population uptake falls below collective-protection thresholds. Suppression (0.65) captures the degree to which the absolute reading forecloses alternative public health architectures (risk-stratified mandates, proportionate coercion). Theater ratio (0.45) recognizes that genuine legal principle is present but is increasingly performed in culture-war politics where the symbolic defense of autonomy displaces nuanced ethical argument. Accessibility collapse (0.72) is high because a categorical legal prohibition causes mandate alternatives to collapse nearly completely once the reading is adopted. Resistance (0.55) reflects sustained opposition from public health institutions, vulnerable-population advocates, and proportionality-focused jurists.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (liberty advocacy movements) experiences the constraint as protective coordination: it successfully limits state overreach and generates organizational resources. The payer seats (immunocompromised and vulnerable groups) experience the identical legal structure as extraction: their health security is compromised by the same rule that empowers the liberty movement. The engine computes this divergence from the structural data â the same constraint yields opposite classifications from opposite seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements are declared beneficiaries and agenda-setters with mobile exit and organized power; the engine derives a low directionality (near-beneficiary, subsidy). Immunocompromised and vulnerable groups are declared victims/payers with trapped or constrained exit and powerless status; the engine derives a high directionality (near-target, amplified extraction). The spatial scope is national, so scope amplification applies modestly. The high extraction + high directionality for vulnerable seats produces severe effective extraction, while the low directionality for the liberty movement dampens or inverts effective extraction into subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the genuine coordination function (protecting against state medical abuse) from the extraction function (risk-shifting to vulnerable populations). A pure mountain reading would deny the beneficiary structure and treat the constraint as a fixed natural law of rights; a pure snare reading would deny the historical legitimacy of the anti-coercion norm. Tangled_rope is the only category that admits both the legitimate founding problem and the present asymmetric extraction, capturing the constraint without collapsing into either apologia or conspiracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_reading_contest,
    'Is the absolute bodily-autonomy reading a genuine legal Mountain (a fixed constitutional boundary) or a Tangled Rope that coordinates liberty advocates while extracting health security from the vulnerable?',
    'Comparative constitutional analysis across jurisdictions with different readings; epidemiological outcome comparison under absolute vs. proportionate mandate regimes.',
    'If the reading operates as a constructed constraint with identifiable beneficiaries and victims, it computes as Tangled Rope rather than Mountain; if it is a genuine fixed limit, it would compute as Mountain from all seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_reading_contest, conceptual, 'Whether the autonomy primacy reading is natural-law-like or a constructed extraction mechanism.').

omega_variable(
    risk_stratification_feasibility,
    'Would a risk-stratified mandate regime (the risk_stratification_reading) practically protect the vulnerable, or would enforcement leakage render the vulnerable''s protection illusory?',
    'Empirical study of compliance and exemption leakage in jurisdictions with targeted mandates (e.g., health-worker mandates with medical exemptions).',
    'If stratified regimes fail to protect due to leakage, the absolute prohibition may be the only regime that avoids false security; if stratified regimes work, the absolute reading''s victim set is avoidable and the extraction is heightened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_stratification_feasibility, empirical, 'Whether targeted mandates can actually protect vulnerable populations.').

omega_variable(
    absolute_prohibition_as_suppression,
    'Does the categorical prohibition on state coercion suppress legitimate public health alternatives by design, or does it merely reflect the absence of legitimate state authority?',
    'Jurisprudential analysis of whether the prohibition functions as a negative liberty (state lacks power) or as an active constraint on democratic public health legislation.',
    'If the prohibition is an active constraint that suppresses alternatives, suppression is high and the reading is more extractive; if it is merely a boundary on illegitimate state power, suppression is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolute_prohibition_as_suppression, conceptual, 'Whether the autonomy constraint actively suppresses public health alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_legitimacy kernel decomposes into three structurally distinct constraints. This reading (absolute bodily autonomy) has low coordination cost in liberty protection but high extraction in risk externalization; siblings have different epsilon profiles, different beneficiary/victim structures, and different directionalities. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
