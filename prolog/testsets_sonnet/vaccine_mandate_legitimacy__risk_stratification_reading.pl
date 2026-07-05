% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Vaccine Mandate Legitimacy — Risk-Stratification (Proportionality) Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the risk-stratification reading of the
 *   vaccine-mandate-legitimacy kernel: the claim that mandate legitimacy is
 *   contingent on satisfying an actuarial risk threshold, such that blanket
 *   population-wide mandates fail proportionality review but mandates
 *   narrowly targeted at demonstrably high-risk settings (healthcare,
 *   congregate care, high-exposure occupations) remain permissible. This is a
 *   distinct constraint from the public-health-primacy reading (which grounds
 *   mandate authority in a general duty to prevent externalities, with no
 *   threshold gate) and the bodily-autonomy-primacy reading (which forecloses
 *   mandate authority categorically regardless of risk). The three readings
 *   are linked via network.affects_constraints and are not merged here — each
 *   has its own beneficiary/victim structure and its own epsilon. The
 *   structural delta for this reading, as anticipated, shows up in the victim
 *   set: because the actuarial threshold is administratively drawn (by job
 *   title, facility type, or occupational code) rather than measured directly
 *   per individual, some workers are swept into 'high-risk' tiers whose
 *   actual exposure does not match the tier's justification. The doctrine's
 *   proportionality promise is therefore only partially self-enforcing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.48).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy — Risk-Stratification (Proportionality) Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '5a90f68b-7519-4f1d-9c2c-3a4ec72de736').
narrative_ontology:cs_kernel_codification('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', distributed).
narrative_ontology:cs_authority_grounding('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', distributed).
narrative_ontology:cs_reading_relation('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', foundational, mandate_legitimacy_requires_actuarial_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_actuarial_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', mandate_legitimacy_requires_actuarial_proportionality, instrumental).
narrative_ontology:cs_axiom('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', secondary, individualized_risk_review_must_be_operative_not_nominal).
narrative_ontology:cs_axiom_status(individualized_risk_review_must_be_operative_not_nominal, holdable).
narrative_ontology:cs_axiom_grounding('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', individualized_risk_review_must_be_operative_not_nominal, empirically_contingent).
narrative_ontology:cs_reference_frame('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', proportionality_gated_mandate_authority).
narrative_ontology:cs_drift_state('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', post_pandemic_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a90f68b-7519-4f1d-9c2c-3a4ec72de736', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_occupational_cohorts).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, health_systems_administrators).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_mandate_targets).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, workers_in_misclassified_risk_tiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and defend the actuarial threshold that separates a permissible targeted mandate from an impermissible blanket one. They set the risk cutoffs (age bands, comorbidity scores, occupational exposure classes) and administer exemption and enforcement machinery. They bear reputational and legal risk if the threshold is challenged as arbitrary, but capture legitimacy and continued mandate authority when courts uphold a proportional design.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, health_systems_administrators, agenda_setter,
    institutional, biographical, analytical, national).

% Health workers, congregate-care staff, and others in demonstrably high-exposure or high-transmission-consequence settings. The targeted mandate reduces their occupational risk and the risk they pose to vulnerable patients/clients; they are the group whose inclusion is least contestable under the proportionality logic.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_occupational_cohorts, beneficiary,
    moderate, immediate, constrained, national).

% Cannot vaccinate themselves effectively or rely on their own immune response; depend entirely on the vaccination status of the workers and caregivers around them. The risk-stratification reading exists substantially to protect this group by justifying targeted (not blanket) mandates aimed at their care environment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Individuals swept into a mandate whose actuarial justification is thin for their specific risk profile (e.g., young, low-exposure, remote workers included in an occupational category defined too broadly). They bear employment, legal, or social consequences of non-compliance without a correspondingly strong individual risk basis — the exact failure mode the proportionality doctrine is supposed to prevent but does not always catch in practice.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_mandate_targets, payer,
    moderate, immediate, constrained, national).

% Placed in a risk category by administrative classification (job title, facility type) that does not track their actual exposure (e.g., remote-facility clerical staff coded under a hospital-wide mandate). They have little recourse to contest the classification and face job loss or exclusion if they do not comply, despite the doctrine's promise that only genuinely high-risk targeting is permissible.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, workers_in_misclassified_risk_tiers, payer,
    powerless, immediate, trapped, regional).

% Adjudicate whether a given mandate's threshold and scope satisfy proportionality — whether it is narrowly tailored to actuarial risk or functions as a blanket mandate in targeted clothing. Their rulings define, case by case, where the risk-stratification line actually sits, and can force administrators to redraw the threshold.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_and_reviewing_bodies, observer,
    institutional, generational, analytical, national).

% Argue that any actuarial threshold is itself illegitimate because it treats bodily autonomy as tradeable against a state-calculated risk score rather than as categorically protected. Their objection is not to a particular threshold but to the whole risk-stratification logic; they are heard in litigation but their categorical objection is structurally foreclosed from altering the doctrine's own terms, since the doctrine's premise is that some threshold-based mandate is legitimate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_advocacy_groups, excluded,
    organized, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and administrative middle path that permits targeted mandates for demonstrably high-risk settings (hospitals, congregate care, high-exposure occupations) while barring undifferentiated population-wide mandates — coordinating protection of vulnerable populations against transmission risk without claiming authority over the general population's medical choices.
% TRANSFER_FUNCTION: Moves compliance burden and bodily-integrity costs from the state's general population onto persons administratively classified as high-risk-adjacent (by occupation, facility, or exposure category), while moving protection benefits to immunocompromised and highly exposed persons in those same settings.
% ABSENT_VOICES: Vaccine-hesitant advocacy groups object to the entire threshold-based framework as illegitimate in principle, not merely miscalibrated; their categorical objection is heard in court but cannot be resolved within a doctrine whose premise is that a threshold-based mandate can be legitimate. Workers placed in misclassified risk tiers rarely have standing or resources to contest their specific classification, so the doctrine's proportionality promise is tested far less often at the margin than at the extremes.
% DISAPPEARANCE_RATIONALE: If the risk-stratification doctrine vanished, mandate authority would collapse to one of the two extreme readings — either blanket mandates would become legally uncontestable (public health primacy) or all mandates would become legally impermissible (bodily autonomy primacy). Hospitals, courts, and employers currently rely on the middle doctrine's threshold tests to design policy; without it, every existing targeted mandate would need to be re-litigated under a different legal theory.
% FOUNDING_PROBLEM: Courts and legislatures needed a principle to resolve mandate disputes that avoided both extremes: unlimited state power to compel medical intervention on any population, and an absolute veto that would leave severely immunocompromised patients and high-exposure workers unprotected. The proportionality/risk-stratification doctrine was built to let mandates survive judicial review only when narrowly tailored to actual risk.
% FOUNDING_PROBLEM_CORROBORATION: Public health administrators attest the actuarial-threshold problem remains live (variant risk, occupational exposure differentials persist). Independent legal scholars outside the administering health agencies and outside the advocacy groups note that in practice, threshold definitions are frequently drawn broadly enough to functionally approximate blanket mandates, and that misclassification litigation is rare because affected workers lack resources to challenge tier assignment — suggesting the doctrine's proportionality promise is only partially operative outside of high-profile test cases.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) and rising slowly over the interval, reflecting gradual threshold-creep: administrators, once granted authority to draw a risk line, face incentive pressure to draw it broadly enough to simplify enforcement, which erodes the doctrine's narrow-tailoring promise over time without ever becoming a full blanket mandate. Suppression is moderate (0.48) — real but bounded, because the doctrine's own logic requires exemption pathways and individualized risk review to survive judicial scrutiny, which caps how coercive enforcement can become before the doctrine collapses into the public-health-primacy reading. Theater ratio rises modestly (0.12 to 0.30) as administrative risk-classification processes accumulate procedural trappings (review boards, appeal processes) that in practice rarely overturn tier assignments — the appearance of individualized proportionality review outpacing its substantive operation. Accessibility collapse is moderate (0.40): exemption and reclassification routes formally exist, but are underused by exactly the powerless workers who need them most. Resistance is meaningfully high (0.55) because both extreme camps — public health absolutists and bodily-autonomy absolutists — attack the middle doctrine from opposite directions, and misclassified workers occasionally litigate their specific tier.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator's seat, this reads as principled, narrowly-tailored public health law — a genuine constraint on state overreach. From the seat of a worker in a misclassified risk tier, the same doctrine reads as a mandate imposed on them with the individualized-risk analysis promised by the doctrine never actually performed in their case. The engine should compute these as different seat-level classifications from the same structural facts; the divergence is the point, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Health systems administrators are the agenda-setters: they draw and defend the threshold, and are structurally the analytical/institutional seat that bears legal risk rather than health risk. High-risk occupational cohorts and immunocompromised patients are genuine beneficiaries — the doctrine exists substantially to let mandates protect them without licensing unlimited state power. Low-risk mandate targets and misclassified-tier workers are the victims: they bear compliance costs disproportionate to their individual risk contribution because the actuarial threshold is applied at the group/classification level rather than the individual level. This group-level application is exactly where the doctrine's proportionality promise is weakest in practice, and where the tangled-rope structure (genuine coordination function plus asymmetric extraction on misclassified workers) is most visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (needing a principle between unlimited mandate authority and an absolute veto) remains partially live — new pathogens and variant risk differentials keep recreating scenarios where a threshold-based approach is needed. But the specific administrative machinery built to implement any given mandate under this doctrine can outlive the acute risk that justified its threshold (e.g., a mandate calibrated to an early pandemic risk profile persisting after risk has fallen for most of a classified tier). The doctrine avoids blanket mandatrophy by requiring periodic threshold reassessment, but nothing in the structure guarantees that reassessment actually happens promptly, which is why founding_problem_status is marked contested rather than clearly live or clearly dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_legitimacy,
    'Is there a principled, non-arbitrary way to set the actuarial risk threshold that separates a legitimate targeted mandate from an illegitimate blanket one, or is any specific threshold ultimately a policy choice dressed in actuarial language?',
    'Comparative judicial review across jurisdictions using different threshold methodologies (relative risk ratios vs. absolute incidence vs. occupational exposure classification) to see whether courts converge on a stable, replicable standard or whether outcomes track political composition of the deciding body instead.',
    'If no principled threshold exists, the risk-stratification reading is itself a contested construction rather than a neutral middle ground — its legitimacy claim would be structurally similar to the extremes it claims to mediate between, weakening its status as a distinct, defensible reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_legitimacy, conceptual, 'Whether the actuarial threshold at the heart of this reading is principled or a disguised policy choice.').

omega_variable(
    misclassification_correction_rate,
    'How often are workers who are administratively misclassified into a high-risk tier successfully reclassified through appeal or exemption processes, versus how often does misclassification simply persist uncontested?',
    'Empirical audit of exemption/appeal request rates, grant rates, and time-to-resolution across a sample of mandate-administering institutions, compared against estimated misclassification prevalence from independent occupational exposure studies.',
    'A low correction rate would confirm that the doctrine''s individualized-proportionality promise is largely theatrical for the powerless victim group, supporting the theater_ratio trajectory authored here and strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misclassification_correction_rate, empirical, 'Whether administrative reclassification actually corrects misclassified risk tiers in practice.').

omega_variable(
    kernel_collapse_direction,
    'Under sustained political or epidemiological pressure, does the risk-stratification reading tend to collapse toward the public-health-primacy reading (threshold drifting toward zero, effectively blanket) or toward the bodily-autonomy-primacy reading (threshold drifting toward one, effectively no mandate survives review)?',
    'Longitudinal tracking of threshold definitions and mandate scope across multiple public health emergencies to observe directional drift, correlated with the political valence of the administering government and prevailing judicial composition.',
    'Directional collapse toward either extreme would indicate the risk-stratification reading functions as an unstable equilibrium rather than a genuinely distinct, stable doctrine — informing whether this constraint should be modeled as an independent long-run attractor or a transitional state between the two extreme readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_collapse_direction, empirical, 'Whether the middle reading is a stable equilibrium or drifts toward one of the two extreme kernel readings over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vaccine_mandate_legitimacy kernel. public_health_primacy_reading grounds mandate authority in collective-harm prevention without an actuarial gate (broader beneficiary set, larger victim set, higher extractiveness expected). bodily_autonomy_primacy_reading forecloses mandate authority categorically (no beneficiaries from coercive mandate machinery; victims are anyone subject to any mandate; likely classifies as snare or tangled_rope depending on enforcement data). This risk_stratification_reading occupies the structural middle, with victim-set size directly sensitive to where the actuarial threshold is administratively drawn — the expected structural delta noted in the generation brief. Each story carries its own epsilon and its own stakeholder set; they are not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
