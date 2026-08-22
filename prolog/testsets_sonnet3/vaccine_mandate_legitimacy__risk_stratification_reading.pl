% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy (Proportionality-Gated Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the risk-stratification reading of the vaccine
 *   mandate legitimacy kernel: mandate authority is legitimate only where
 *   actuarial risk data justifies the specific targeted population, and
 *   illegitimate where applied as a blanket rule untethered to measured
 *   exposure. This is a proportionality-doctrine reading, distinct from the
 *   public-health-primacy reading (which grounds mandate authority in the
 *   externality itself, not in a threshold test) and the
 *   bodily-autonomy-primacy reading (which denies mandate authority
 *   regardless of risk magnitude). The reading's own internal tension — that
 *   the threshold is drawn at the occupational-category level rather than the
 *   individual-exposure level — is what produces its victim set: workers
 *   formally inside a mandated high-risk category whose actual measured risk
 *   sits below the threshold the doctrine itself claims to require.
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
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy (Proportionality-Gated Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '9a10ce35-ac9b-431b-9e15-a787124db7c4').
narrative_ontology:cs_kernel_codification('9a10ce35-ac9b-431b-9e15-a787124db7c4', distributed).
narrative_ontology:cs_authority_grounding('9a10ce35-ac9b-431b-9e15-a787124db7c4', practice).
narrative_ontology:cs_interpretation_layer_present('9a10ce35-ac9b-431b-9e15-a787124db7c4').
narrative_ontology:cs_reading_relation('9a10ce35-ac9b-431b-9e15-a787124db7c4', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a10ce35-ac9b-431b-9e15-a787124db7c4', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('9a10ce35-ac9b-431b-9e15-a787124db7c4', foundational, mandate_legitimacy_requires_actuarial_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_actuarial_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('9a10ce35-ac9b-431b-9e15-a787124db7c4', mandate_legitimacy_requires_actuarial_proportionality, empirically_contingent).
narrative_ontology:cs_axiom('9a10ce35-ac9b-431b-9e15-a787124db7c4', secondary, blanket_coercion_categorically_disfavored_absent_measured_risk).
narrative_ontology:cs_axiom_status(blanket_coercion_categorically_disfavored_absent_measured_risk, holdable).
narrative_ontology:cs_axiom_grounding('9a10ce35-ac9b-431b-9e15-a787124db7c4', blanket_coercion_categorically_disfavored_absent_measured_risk, instrumental).
narrative_ontology:cs_reference_frame('9a10ce35-ac9b-431b-9e15-a787124db7c4', narrow_tailoring_doctrine_baseline).
narrative_ontology:cs_drift_state('9a10ce35-ac9b-431b-9e15-a787124db7c4', post_pandemic_litigation_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a10ce35-ac9b-431b-9e15-a787124db7c4', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_congregate_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_facility_operators).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, courts_seeking_administrable_standard).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_workers_swept_into_targeted_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, occupational_groups_misclassified_as_high_exposure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the actuarial risk threshold that separates permissible targeted mandates from impermissible blanket ones. Controls the epidemiological modeling, the exposure-tier categories, and the review cycle for reclassification. Bears reputational and legal cost if thresholds are struck down, but faces no direct personal exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Nursing home residents, immunocompromised patients, and others in settings where transmission risk is empirically elevated. Gain protection when mandates are correctly targeted at the settings around them; have no capacity to enforce the targeting themselves and depend entirely on the threshold being drawn accurately.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_congregate_populations, beneficiary,
    powerless, immediate, trapped, national).

% Hospitals and long-term care facilities implement the mandate as a condition of employment and licensure. Benefit from liability protection and outbreak reduction; also administer enforcement (termination, reassignment) against staff who refuse, giving them a dual position as both beneficiary and local enforcer.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_facility_operators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_facility_operators, agenda_setter).

% Employees classified into a mandated occupational tier (e.g. all hospital staff regardless of patient contact, or all K-12 staff regardless of classroom exposure) whose actual individual transmission risk is low. Bear job loss or coercion under a threshold drawn at the occupational-category level rather than the individual-exposure level; exit means leaving the profession or region.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_workers_swept_into_targeted_mandates, payer,
    powerless, biographical, constrained, national).

% Groups such as remote-working clinical staff, administrative hospital employees, or teachers in low-density rural schools who are categorized with their higher-contact colleagues for administrative simplicity. The proportionality logic that legitimizes the mandate for their high-exposure peers is applied to them by category rather than by measured individual risk, producing the same coercive consequence without the same actuarial justification.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, occupational_groups_misclassified_as_high_exposure, payer,
    moderate, biographical, constrained, national).

% Judges reviewing mandate challenges need a workable proportionality test rather than an all-or-nothing rule; the risk-stratification framework gives them a doctrine (narrow tailoring to actuarial risk) that lets them uphold some mandates and strike others without abandoning judicial review of public health measures. They gain a durable analytical tool; they bear no direct cost from either outcome.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_seeking_administrable_standard, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, courts_seeking_administrable_standard, observer).

% Individuals whose personal circumstances (prior infection, remote work, isolated living) place their actual transmission risk below the threshold used to justify the mandate in their occupational category, but who have no individualized process to demonstrate this and are not heard in the categorical rulemaking that sets the threshold.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, unvaccinated_individuals_in_low_risk_settings, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a proportionality standard that lets mandate authority track actual epidemiological risk rather than applying uniformly across all settings — concentrating coercive intervention where transmission consequences are severe (ICUs, nursing homes) and withholding it where they are not.
% TRANSFER_FUNCTION: Moves the burden of mandate compliance (vaccination, employment consequence, or exit from a profession) from the population at large onto whichever occupational or demographic category is drawn inside the actuarial threshold, while shifting protection benefits toward high-risk congregate populations.
% ABSENT_VOICES: Individuals whose actual personal risk profile diverges from their assigned category — the remote clinical worker, the previously-infected teacher, the low-density-school staffer — have no individualized hearing in how the threshold-defining category is drawn; the rulemaking process categorizes by occupation, not by measured individual exposure.
% DISAPPEARANCE_RATIONALE: If the risk-stratification doctrine disappeared, courts would be forced back toward the binary choice between upholding blanket mandates outright or striking mandate authority down categorically — either outcome reorganizes which populations are protected and which employment consequences attach, and removes the current administrable middle path judges and health authorities rely on.
% FOUNDING_PROBLEM: Early pandemic mandates were challenged as either insufficiently protective (public health primacy critique) or categorically coercive (bodily autonomy critique); courts needed a doctrine that could distinguish a mandate narrowly tailored to a genuine, measurable transmission risk from a mandate imposed on the general population regardless of actual exposure.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists outside the litigating parties attest that transmission risk is genuinely heterogeneous across settings, supporting the underlying premise that stratification is not mere doctrine-shopping. However, labor advocates and misclassified occupational groups — who are not party to the threshold-setting process — attest that the categorical (occupation-level) implementation of the threshold does not track the individual-level risk data the doctrine claims to rely on, meaning the founding problem may be only partially solved in practice even where the doctrine is sound in principle.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) and suppression (0.48) are moderate rather than high because the doctrine genuinely narrows coercive scope relative to a blanket mandate — this is real coordination function, not pure cover. But both remain non-trivial because the category-level implementation of the threshold reintroduces coercion against individuals whose measured risk does not meet the stated bar; the doctrine's own proportionality logic is not fully carried through to individual-level determination. Theater ratio rises over the interval (0.15 -> 0.30) as litigation and administrative practice increasingly substitute categorical occupational classification for the individualized risk assessment the doctrine claims to perform — a mild Goodhart drift where the proxy (occupation) replaces the target (measured exposure).
 *
 * PERSPECTIVAL GAP:
 *   From the health-authority and court seats, this doctrine looks like a genuine, moderate Rope: it solves the coordination problem of distinguishing justified from unjustified coercion, and does so with real administrability gains. From the misclassified-worker seat, the same structure looks like a Tangled Rope shading toward Snare: the proportionality promise is real in principle but the categorical implementation extracts compliance from individuals the doctrine's own actuarial logic does not reach. The engine should compute this divergence from the structural exit/power data rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk congregate populations and healthcare facility operators sit near the beneficiary end: they gain outbreak protection and liability shielding from correctly-targeted mandates and bear minimal exit cost themselves. Low-risk workers swept into a mandated occupational tier, and occupational groups misclassified as high-exposure, sit near the target end: they bear the mandate's coercive consequence (employment loss, forced vaccination) without the individualized risk finding the doctrine's own logic would require to justify it on them specifically. Courts occupy an analytical, low-exit seat that benefits from doctrinal workability without bearing either mandate's direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing narrowly-tailored from blanket coercion) remains partially live — transmission risk heterogeneity is real and persists across settings — but the doctrine's administrative implementation has drifted toward categorical occupational sorting, which is easier to litigate and enforce than individualized risk assessment. This is not full mandatrophy (the underlying problem has not disappeared), but it is a genealogical warning sign: a doctrine built to require individualized proportionality has hardened into a categorical proxy, which is exactly the kind of substitution the theater_ratio trend is tracking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_convergence_ambiguity,
    'At what actuarial threshold value does the risk-stratification reading collapse into one of its siblings — either authorizing mandates so broadly it becomes indistinguishable from public_health_primacy_reading, or restricting them so narrowly it becomes indistinguishable from bodily_autonomy_primacy_reading?',
    'Comparative doctrinal analysis across jurisdictions that have set the threshold at different levels, tracking whether courts applying a very low or very high threshold produce outcomes that converge with either sibling reading''s outcome set.',
    'If convergence is easy at plausible threshold values, the risk-stratification reading is less a stable third position than a continuum whose endpoints are the two extreme readings — meaning its distinct identity as a constraint depends on the threshold staying in a genuinely intermediate range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_convergence_ambiguity, conceptual, 'Whether the moderate reading is a stable third position or a continuum collapsing into its siblings at the extremes.').

omega_variable(
    categorical_vs_individual_implementation_gap,
    'Is the gap between the doctrine''s individualized-risk justification and its categorical (occupation-level) implementation a temporary administrative shortcut, or a structural feature that will persist because individualized assessment is administratively infeasible at scale?',
    'Track whether any jurisdiction successfully implements individual-level risk assessment for mandate exemption at scale, and at what administrative cost, versus jurisdictions that formally adopt individualized standards but administer them categorically in practice.',
    'If individualized assessment is infeasible at scale, the risk-stratification reading''s proportionality promise is structurally unfulfillable for a meaningful minority of affected individuals, which would push the constraint''s classification toward tangled_rope permanently rather than as a transitional feature correctable by better administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_individual_implementation_gap, empirical, 'Whether categorical implementation of an individualized-risk doctrine is a fixable defect or a structural limit.').

omega_variable(
    threshold_setting_authority_capture,
    'Is the actuarial threshold itself set by disinterested epidemiological analysis, or does the threshold-setting process respond to the administrative and political convenience of the agenda-setters (health authorities, facility operators) who also benefit from the mandate''s scope?',
    'Compare threshold values across jurisdictions with different institutional structures for threshold-setting (independent scientific panels vs. agency discretion vs. legislative mandate) and check whether threshold stringency correlates with the interests of the setting institution.',
    'If threshold-setting is captured by the interests of agenda-setters, the doctrine''s proportionality claim is partly cover for extraction rather than a genuine constraint on mandate scope, which would raise the story''s effective extractiveness beyond what is authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_setting_authority_capture, empirical, 'Whether the threshold itself is independently derived or shaped by the interests of those who administer the mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the vaccine_mandate_legitimacy kernel. public_health_primacy_reading grounds mandate authority in the externality itself and authors a lower ε for the arrangement it describes (broader mandate authority, framed as duty-fulfillment rather than proportionality-limited extraction). bodily_autonomy_primacy_reading treats any mandate as categorically illegitimate and authors mandate enforcement itself as the extractive constraint (high ε, victims = all coerced individuals regardless of risk). This risk_stratification_reading occupies structural middle ground: its ε (0.42) is lower than bodily_autonomy_primacy_reading's would be (mandate scope is narrower) but higher than public_health_primacy_reading's would be for the same targeted-mandate population (because this reading's own categorical implementation reintroduces coercion against individuals its actuarial logic does not reach). The three stories share no observable — each authors its own beneficiary/victim structure and ε from its own reading's premises, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
