% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Categorical Prohibition on Physician-Assisted Dying (Sanctity-of-Life Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint is the sanctity-of-life reading of a single contested
 *   kernel governing end-of-life authority: who may decide when a life ends,
 *   and on what warrant. Under this reading, the intrinsic and equal value of
 *   every human life is a categorical premise that forecloses weighing that
 *   value against an individual's stated preference, however competent or
 *   persistent the preference. The reading grounds a real, if imperfect,
 *   protective function — shielding vulnerable populations from coercion into
 *   death — while also imposing continued suffering on competent patients who
 *   want assistance and are denied it. This is a genuinely different
 *   constraint from the autonomy reading (which grounds a right to control
 *   the timing of death) and from the slippery-slope mechanism story (which
 *   is an empirical claim about how permissive frameworks drift over time,
 *   not a normative claim about intrinsic value). Each reading is authored as
 *   its own file with its own epsilon; this file does not average over them
 *   or describe the contest internally.
 *
 * KEY AGENTS:
 *   - medical_licensing_boards: agenda_setter (institutional/analytical) — administers and enforces the categorical prohibition
 *   - vulnerable_patients_at_coercion_risk: beneficiary (powerless/trapped) — protected from coercive pressure by the bright line
 *   - competent_terminally_ill_patients_seeking_death: payer (powerless/trapped) — bears the cost of the categorical rule directly
 *   - treating_physicians: agenda_setter/payer (moderate/constrained) — administers the rule at the bedside and bears its moral distress
 *   - legislatures_and_courts: observer (institutional/analytical) — the live site of ongoing contest between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.42).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.58).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Categorical Prohibition on Physician-Assisted Dying (Sanctity-of-Life Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '4f37bd4b-df8d-436b-b4e8-87282ce8abb7').
narrative_ontology:cs_kernel_codification('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', formalized).
narrative_ontology:cs_authority_grounding('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', lineage).
narrative_ontology:cs_interpretation_layer_present('4f37bd4b-df8d-436b-b4e8-87282ce8abb7').
narrative_ontology:cs_reading_relation('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', foundational, life_has_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', life_has_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', foundational, physician_role_categorically_excludes_causing_death).
narrative_ontology:cs_axiom_status(physician_role_categorically_excludes_causing_death, holdable).
narrative_ontology:cs_axiom_grounding('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', physician_role_categorically_excludes_causing_death, conventional).
narrative_ontology:cs_reference_frame('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', hippocratic_non_maleficence_tradition).
narrative_ontology:cs_drift_state('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', post_legalization_wave_contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f37bd4b-df8d-436b-b4e8-87282ce8abb7', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, vulnerable_patients_at_coercion_risk).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_and_pro_life_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_rights_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_death).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, patients_in_intractable_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, families_bearing_prolonged_dying_process).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, treating_physicians).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, physician_do_no_harm_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce professional conduct rules that categorically bar physicians from intentionally hastening death, backed by license revocation and criminal referral. Administers the prohibition and could in principle revise it, but treats the prohibition as constitutive of the profession's identity rather than a policy choice open to revision.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_licensing_boards, agenda_setter,
    institutional, generational, analytical, national).

% Elderly, disabled, and economically disadvantaged patients who might otherwise face subtle or overt pressure from family, insurers, or a cost-conscious healthcare system to choose death. The prohibition removes that option entirely, foreclosing both the pressure and the pressured choice. They cannot advocate for themselves as a class distinct from the terminally-ill patients the prohibition also binds.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, vulnerable_patients_at_coercion_risk, beneficiary,
    powerless, biographical, trapped, national).

% Advocacy organizations and religious bodies whose founding commitments are vindicated by the categorical rule; they lobby to maintain and strengthen it, litigate against exceptions, and derive institutional legitimacy and continued relevance from the prohibition's persistence. They bear none of the prohibition's costs directly.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_and_pro_life_institutions, beneficiary,
    organized, civilizational, arbitrage, national).

% Organizations representing disabled people who argue that any legal assisted-dying pathway inevitably becomes a pathway that devalues disabled lives and pressures disabled people toward death under cost or convenience framing. They benefit from the categorical rule as protection against that dynamic, independent of whether any individual disabled person might want the option.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_rights_organizations, beneficiary,
    organized, generational, arbitrage, national).

% Mentally competent adults facing terminal diagnoses who want medical assistance to end suffering on their own timeline. The prohibition forces them to continue living through a dying process they have judged unbearable, seek unregulated or violent means, or travel to a jurisdiction that permits assistance if they have the means and mobility to do so — an exit unavailable to most.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_death, payer,
    powerless, immediate, trapped, national).

% Patients whose suffering is not adequately controlled by available palliative measures. The rule offers them no individualized exception regardless of the severity or duration of documented suffering; their only lawful path is continued treatment or refusal of treatment (which is permitted) but not active assistance in dying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, patients_in_intractable_suffering, payer,
    powerless, immediate, trapped, national).

% Family members who witness and often provide caregiving through a dying process the patient wished to shorten. They bear emotional, financial, and caregiving costs of a prolongation neither they nor the patient chose, with no lawful mechanism to honor the patient's stated wish.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, families_bearing_prolonged_dying_process, payer,
    powerless, biographical, constrained, national).

% Physicians administer the prohibition daily at the bedside, are bound by professional codes limiting their role to life preservation and comfort care, and bear the moral distress of being unable to act on a patient's explicit, competent, repeated request. They cannot exit the constraint without leaving the profession or jurisdiction.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, treating_physicians, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, treating_physicians, payer).

% Adjudicate constitutional and statutory challenges to the prohibition, hear testimony from all sides, and retain the formal power to legalize exceptions — several jurisdictions have moved to the autonomy reading while others have reaffirmed the prohibition, making this an active site of contest rather than settled law.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice and social trust around a single bright-line rule — physicians preserve life and relieve suffering but never intentionally cause death — which protects against misdiagnosis, coercion, and the erosion of trust in medical institutions that a case-by-case exception regime would risk.
% TRANSFER_FUNCTION: Moves the burden of continued suffering and prolonged dying from a diffuse, hard-to-verify population of at-risk vulnerable patients (who are protected in the aggregate) onto a smaller, individually identifiable population of competent patients who want assistance and are denied it — protection for one group is purchased with suffering imposed on another.
% ABSENT_VOICES: Competent terminally ill patients who want assistance and have already died without it cannot testify to legislatures; disabled and elderly individuals who WOULD want the option, as opposed to the advocacy organizations that claim to represent disabled people categorically, are largely absent from the coalition that defends the prohibition.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, physicians and legislatures would need to build a new consent, competency, and safeguard apparatus from scratch (world rearranges for the profession), but many patients would simply exercise a choice already available to them in other jurisdictions (comparatively little rearranges for them beyond gained access) — the parties genuinely dispute which framing is correct, which is itself evidence this is a live kernel contest rather than settled fact.
% FOUNDING_PROBLEM: Historically: preventing physicians from becoming agents of death, protecting patients from coercion by family or state, and maintaining an unambiguous professional identity in which the healer never kills — arising from documented histories of medical involvement in eugenics and euthanasia abuse.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations and palliative care physicians outside the religious advocacy coalition independently attest the coercion-risk problem remains live, citing documented cases of financial and family pressure in permissive jurisdictions. Independent bioethicists and courts in jurisdictions that have legalized assistance attest the problem is addressable through safeguards short of categorical prohibition — corroboration exists on both sides from outside the core beneficiary coalition, which is precisely what makes this a genuine kernel contest rather than a settled genealogy.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, contested).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).
:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than low or high: the constraint imposes a real and identifiable cost on a specific population (competent patients denied assistance) while also producing a real protective benefit for another population (vulnerable patients at coercion risk) — it is not pure extraction because the coordination function (protecting against coercion) is genuine, but it is not costless coordination either because the cost falls concentratedly on identifiable non-beneficiaries. Suppression is authored moderately high (0.58, declining slightly over the measured interval) because the prohibition is maintained through criminal law, professional licensing sanctions, and in many jurisdictions constitutional rulings — real coercive machinery, not mere convention — though the declining trajectory reflects gradual erosion of enforcement intensity as more jurisdictions carve out exceptions. Theater ratio is low (0.22) because the enforcement apparatus (licensing boards, criminal statutes) is substantively active, not merely performative. Accessibility collapse is moderately high (0.61): once a jurisdiction adopts the categorical rule, the alternative (a case-by-case or autonomy-based system) becomes very difficult for an individual patient to access, though it is not fully collapsed because litigation and cross-jurisdictional travel remain theoretically available to some. Resistance is moderate-high (0.55), reflecting active, organized challenge from patients, physicians, and courts in several jurisdictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable patients at coercion risk, religious/pro-life institutions, and disability rights organizations are declared beneficiaries because the categorical rule vindicates their position and, for the vulnerable population, provides real protective value they cannot obtain any other way given their powerlessness and trapped exit options. Competent terminally ill patients, patients in intractable suffering, and their families are declared victims because the same rule, applied to them, imposes a cost (continued suffering, denial of a wanted choice) that the rule's protective logic does not distinguish from the coercion risk it targets — the rule cannot tell a competent, uncoerced patient from a coerced one without the case-by-case mechanism it forecloses. Treating physicians occupy both agenda_setter and payer roles: they administer the prohibition at the bedside (agenda_setter) but also bear its moral cost when a patient's request cannot be honored (payer) — this dual position is exactly the kind of genuinely dual-positioned agent the schema's secondary_role field exists for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting patients from medical involvement in coercive or eugenic death — remains genuinely live in the presence of documented coercion risk to elderly, disabled, and economically disadvantaged populations; this is not a case of an obsolete mandate persisting by inertia. But the founding_problem_status is authored as contested rather than live because the categorical (as opposed to safeguarded case-by-case) form of the solution is itself contested: critics argue the problem can be solved with narrower safeguards that do not impose a blanket cost on competent patients. This is precisely the kind of divergence the classification is built to surface without resolving — the coordination function is real, the extraction is real, and mandatrophy is not resolved because both readings retain outside corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_natural_or_constructed,
    'Is the intrinsic-value premise a discoverable moral fact independent of human convention, or a constructed doctrine whose persistence is explained by the institutional interests of the parties who benefit from its vindication (religious institutions, professional identity of medicine, disability advocacy organizations)?',
    'No empirical resolution mechanism exists for the moral-realist question itself; the closest available proxy is cross-cultural and cross-temporal convergence analysis on the categorical prohibition versus documented instances of the doctrine being invoked or abandoned in ways that track institutional interest rather than principle.',
    'If constructed and interest-tracking, the tangled_rope classification is well-supported (real coordination function, but also concentrated institutional benefit riding on the same structure). If a genuine discoverable moral fact, the coordination function may be closer to a mountain-adjacent constraint that happens to have distributional effects rather than a hybrid extraction structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_natural_or_constructed, conceptual, 'Whether the sanctity premise is natural-law-like or an interest-serving construction.').

omega_variable(
    coercion_risk_magnitude,
    'How large is the actual coercion risk to vulnerable populations under a safeguarded (non-categorical) assisted-dying regime, versus under the categorical prohibition''s own failure modes (unregulated self-harm, cross-border travel restricted to the wealthy, prolonged unconsented suffering)?',
    'Comparative empirical study of documented coercion incidents and prolongation harms across jurisdictions with categorical prohibitions versus jurisdictions with safeguarded permissive frameworks (e.g., Oregon, Netherlands, Canada) over comparable time periods.',
    'If coercion incidents under safeguarded regimes are rare and detectable, the sanctity reading''s protective claim is weaker than authored and extractiveness/victim-cost should be weighted more heavily; if coercion incidents are common and hard to detect even with safeguards, the protective beneficiary claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_magnitude, empirical, 'Comparative magnitude of coercion risk under categorical versus safeguarded regimes.').

omega_variable(
    kernel_framing_alternative,
    'Is the correct framing of this kernel a single contested moral question (as authored across three sibling readings), or is the sanctity_reading better understood as one horn of a categorical/case-by-case procedural dispute layered on top of a separate, less contested question about coercion protection?',
    'Examine whether jurisdictions that adopt safeguarded frameworks explicitly retain and formalize coercion-protection mechanisms (competency evaluation, waiting periods, independent witness requirements) as a distinct layer from the categorical/permissive choice — if so, the two questions are more separable than the three-reading kernel structure suggests.',
    'If separable, the sanctity_reading''s coordination function (coercion protection) could in principle be preserved under an autonomy-respecting framework, which would weaken the forecloses/coexists_with distinction currently authored and suggest the readings are less structurally opposed than the kernel framing implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the three-reading kernel decomposition itself may conflate a procedural dispute with a normative one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__sanctity_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__sanctity_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__sanctity_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__sanctity_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__sanctity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__sanctity_reading, base_extractiveness, 32, 0.415).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__sanctity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__sanctity_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__sanctity_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the end_of_life_authority kernel. autonomy_reading grounds a right to assisted death in individual autonomy over unbearable suffering (structurally the mirror image of this reading — the same patients who are victims here are beneficiaries there). slippery_slope_mechanism is not a normative reading at all but an empirical claim about institutional drift once an autonomy-based framework is adopted; it is downstream of whichever normative reading a jurisdiction adopts. All three should be read as distinct constraints, each with its own epsilon, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
