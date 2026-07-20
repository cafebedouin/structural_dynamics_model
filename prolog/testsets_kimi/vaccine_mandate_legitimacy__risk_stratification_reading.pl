% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy Doctrine
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   A constitutional legal doctrine holding that vaccine mandates are
 *   legitimate only when targeted to actuarial risk thresholds, with blanket
 *   mandates failing proportionality review. The constraint operates in
 *   public health emergencies (notably COVID-19) as a judicially enforced
 *   middle ground between absolute bodily autonomy and categorical public
 *   health primacy. It is actively enforced by constitutional courts
 *   reviewing legislation, and extracts autonomy from targeted high-risk
 *   subgroups while exempting low-risk populations. The kernel is contested:
 *   three readings exist (bodily autonomy absolutism, public health primacy,
 *   and this risk-stratified proportionality reading). This story
 *   instantiates only the risk stratification reading as a clean constraint
 *   with its own structural signature.
 *
 * KEY AGENTS:
 *   - constitutional_courts (agenda_setter, institutional/analytical): enforces proportionality test and determines threshold adequacy
 *   - targeted_mandate_recipients (payer, moderate/trapped): bears coercive cost of risk-stratified mandates
 *   - low_risk_exempted_group (beneficiary, powerless/constrained): spared from blanket coercion by actuarial exemption
 *   - public_health_agencies (beneficiary, institutional/constrained): gains legal tool for targeted mandates but limited from broader action
 *   - state_legislators (agenda_setter, institutional/constrained): crafts mandates within judicial proportionality bounds
 *   - public_health_absolutists (excluded, organized/constrained): advocates for blanket mandates excluded from proportionality framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.62).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.58).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy Doctrine").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'e455e617-decc-4b43-9f97-0ea0ff513826').
narrative_ontology:cs_kernel_codification('e455e617-decc-4b43-9f97-0ea0ff513826', formalized).
narrative_ontology:cs_authority_grounding('e455e617-decc-4b43-9f97-0ea0ff513826', lineage).
narrative_ontology:cs_interpretation_layer_present('e455e617-decc-4b43-9f97-0ea0ff513826').
narrative_ontology:cs_reading_relation('e455e617-decc-4b43-9f97-0ea0ff513826', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e455e617-decc-4b43-9f97-0ea0ff513826', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_axiom('e455e617-decc-4b43-9f97-0ea0ff513826', foundational, actuarial_threshold_governs_legitimacy).
narrative_ontology:cs_axiom_status(actuarial_threshold_governs_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e455e617-decc-4b43-9f97-0ea0ff513826', actuarial_threshold_governs_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('e455e617-decc-4b43-9f97-0ea0ff513826', foundational, blanket_mandate_presumption_disproportionate).
narrative_ontology:cs_axiom_status(blanket_mandate_presumption_disproportionate, holdable).
narrative_ontology:cs_axiom_grounding('e455e617-decc-4b43-9f97-0ea0ff513826', blanket_mandate_presumption_disproportionate, conventional).
narrative_ontology:cs_reference_frame('e455e617-decc-4b43-9f97-0ea0ff513826', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('e455e617-decc-4b43-9f97-0ea0ff513826', post_pandemic_legal_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e455e617-decc-4b43-9f97-0ea0ff513826', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_exempted_group).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_mandate_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise judicial review over vaccine mandate legislation, enforcing a proportionality test that requires actuarial risk thresholds. They strike down blanket mandates and uphold targeted ones, interpreting constitutional rights and state police powers. Their exit is analytical: they can revise doctrine, but only through formal legal reasoning and precedent.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Individuals whose demographic, clinical, or occupational actuarial risk falls below the judicially recognized threshold. They are exempt from vaccine mandates that would apply under a blanket regime, avoiding bodily coercion and employment penalties. They cannot easily alter their risk classification or the legal framework that defines it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_exempted_group, beneficiary,
    powerless, biographical, constrained, national).

% Healthcare workers, elderly in congregate settings, or other high-risk or high-exposure groups who meet the actuarial threshold. They must accept vaccination or face termination, professional licensure restrictions, or social exclusion. Their exit options are limited because the mandate is tied to their occupational or residential identity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_mandate_recipients, payer,
    moderate, biographical, trapped, national).

% Design and implement targeted vaccination campaigns using epidemiological risk stratification. They benefit from legal certainty that proportionately tailored mandates are constitutionally permissible, but are constrained from pursuing broader population-level mandates by judicial proportionality limits.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_agencies, beneficiary,
    institutional, generational, constrained, national).

% Draft public health statutes creating vaccine mandates for high-risk groups. They must tailor legislative coverage to actuarial categories to survive judicial review, losing the option of blanket population mandates. They retain control over threshold calibration and enforcement mechanisms within constitutional bounds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, state_legislators, agenda_setter,
    institutional, generational, constrained, national).

% Advocates and some epidemiologists who argue that any unvaccinated status constitutes a collective harm justifying universal mandates. They are excluded from the risk stratification framework, which rejects blanket coercion regardless of actuarial risk, but they continue to press for broader mandate authority in legislative and public discourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_absolutists, excluded,
    organized, generational, constrained, national).

narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of pandemic vaccination without authorizing unlimited state coercion by limiting mandates to actuarially justified subgroups, preserving a zone of liberty for low-risk individuals while enabling public health protection where risk concentration warrants it.
% TRANSFER_FUNCTION: Moves the burden of vaccination and the risk of penalty from low-risk populations to high-risk or high-exposure subgroups identified by actuarial thresholds; moves legal legitimacy from blanket state police power to conditional, evidence-based mandate authority subject to judicial review.
% ABSENT_VOICES: Absolute bodily autonomy advocates who reject any state-mandated medical intervention regardless of risk, and categorical public health primacy advocates who treat all unvaccinated individuals as externalities warranting universal coercion. Both are structurally excluded from the proportionality framework but remain active outside it.
% DISAPPEARANCE_RATIONALE: Removing the risk stratification requirement would either legitimize blanket mandates (expanding coercion to low-risk groups) or delegitimize all mandates, depending on which extreme reading filled the vacuum. The current legal settlement between public health authority and bodily liberty would dissolve, forcing rearrangement of constitutional doctrine and public health practice.
% FOUNDING_PROBLEM: The collision between emerging vaccine technology, pandemic disease transmission, and constitutional protections for bodily integrity: how to permit effective public health measures during emergency without authorizing unlimited state coercion over medical decisions.
% FOUNDING_PROBLEM_CORROBORATION: Independent constitutional law scholars and civil liberties organizations outside the public health beneficiary set attest that the tension between emergency police power and bodily autonomy remains unresolved. No corroborating consensus exists from neutral parties that the proportionality test has definitively settled the founding collision.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint authorizes and legitimates bodily coercion for targeted subgroups; while it protects low-risk individuals, the extraction from the targeted group is structurally central. Suppression (0.58) reflects the active judicial enforcement required to invalidate blanket mandates and uphold targeted ones, plus the employment and licensure penalties applied to non-compliant targeted individuals. Theater ratio (0.28) is moderate: proportionality review performs genuine legal work but also serves as political cover for contested coercion. Accessibility_collapse (0.48) is moderate because alternative legal frameworks (absolute autonomy or blanket primacy) remain politically and jurisprudentially live. Resistance (0.71) is high because both targeted individuals and public health absolutists actively oppose the constraint from opposite directions. The measurement series show extraction rising as judicial doctrine settled during the pandemic, then stabilizing as the framework matured.
 *
 * PERSPECTIVAL GAP:
 *   Targeted mandate recipients experience the constraint as direct extraction of bodily autonomy (high directionality, high effective extraction), while low-risk exempted individuals experience it as protective coordination (low directionality, subsidy). Constitutional courts experience it as analytical interpretation, and public health agencies experience it as enabling but limiting. The per-seat computed type should diverge sharply between the payer seat (snare-like experience) and the beneficiary seat (rope-like experience), with the story-level classification resolving to tangled_rope because both coordination and extraction are structurally present and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (low_risk_exempted_group, public_health_agencies) receive low directionality because the constraint subsidizes their liberty or legal authority. The targeted_mandate_recipients are the primary victims, receiving high directionality because the constraint authorizes direct coercion against them. State legislators and courts sit near symmetric or beneficiary because the constraint organizes their authority rather than extracting from them. No override is needed: the structural derivation from beneficiary/victim declarations plus exit options (trapped for targeted group, constrained for exempted group) correctly maps the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The risk stratification reading prevents mandatrophy misclassification by making the coordination function (enabling targeted public health action) inseparable from the extraction function (coercing targeted subgroups). A pure rope classification would ignore the trapped exit options and bodily coercion of the targeted group. A pure snare classification would ignore the genuine protective function for low-risk individuals and the legal limits on blanket state power. The tangled_rope classification is warranted because removing the coordination function (the proportionality test) would not remove the extraction (targeted mandates could still exist under public health primacy), and removing the extraction (abolishing all mandates) would abandon the coordination. The constraint requires active enforcement (judicial review) to hold the middle ground, and collapses toward one extreme if enforcement decays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_threshold_indeterminacy,
    'What actuarial risk threshold satisfies the proportionality requirement, and does threshold calibration determine whether the constraint functions as coordination or extraction?',
    'Comparative jurisdictional analysis of judicial threshold selection against independent epidemiological risk curves; correlation between threshold stringency and targeted group size.',
    'If courts set thresholds arbitrarily or politically, the victim set expands and the constraint drifts toward snare; if rigorously epidemiological, the extraction remains bounded and proportional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_indeterminacy, empirical, 'Threshold definition determines victim set size and extractiveness').

omega_variable(
    middle_ground_stability,
    'Can the risk stratification reading persist as a stable legal framework, or will polarization force collapse into bodily autonomy primacy or public health primacy?',
    'Track judicial appointments, legislative composition, and emergency declaration patterns over multiple electoral cycles to observe whether proportionality review survives or is captured.',
    'Collapse would reclassify the constraint family: either toward rope (if autonomy prevails and mandates are barred) or toward snare (if public health primacy prevails and blanket mandates are authorized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_ground_stability, conceptual, 'Stability of the proportionality middle ground against polarizing pressure').

omega_variable(
    foreclosure_of_absolute_autonomy,
    'Does adoption of the risk stratification reading logically foreclose the bodily autonomy primacy reading, or merely narrow its scope?',
    'Analyze judicial opinions for whether bodily autonomy is treated as a presumption rebuttable by actuarial evidence or as a principle categorically subordinated to proportionality.',
    'If autonomy is preserved as a rebuttable presumption, the reading retains a rope-like protective function; if autonomy is treated as fully subordinate, the extraction signature strengthens and the reading edges toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_of_absolute_autonomy, conceptual, 'Whether risk stratification forecloses or merely qualifies absolute autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vml_rsr_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vml_rsr_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(vml_rsr_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(vml_rsr_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(vml_rsr_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(vml_rsr_tr_t30, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(vml_rsr_tr_t36, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(vml_rsr_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vml_rsr_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(vml_rsr_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(vml_rsr_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(vml_rsr_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(vml_rsr_be_t30, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(vml_rsr_be_t36, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 36, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vml_rsr_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vml_rsr_su_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(vml_rsr_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(vml_rsr_su_t18, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(vml_rsr_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(vml_rsr_su_t30, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(vml_rsr_su_t36, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 36, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine_mandate_legitimacy kernel. The kernel decomposes into three structurally distinct constraints because the label 'vaccine mandate legitimacy' conflates three different normative commitments with different victim sets, coordination mechanisms, and extraction profiles. Each reading has its own epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
