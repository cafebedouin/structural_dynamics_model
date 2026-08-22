% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Prohibition on Assisted Dying
 *   domain: medical ethics/bioethics
 *
 * SUMMARY:
 *   The sanctity-of-life reading of end-of-life authority treats the
 *   intrinsic value of human life as generating a categorical prohibition on
 *   intentional life-ending, overriding individual patient preference.
 *   Operationalized through medical licensing, criminal law, and professional
 *   oaths, it structures the physician as a life-preserver and the patient as
 *   a bearer of existence who may not elect death. This constraint is
 *   contested by autonomy-based and slippery-slope readings of the same
 *   kernel. The authored metrics treat the prohibition as substantially
 *   extractive and suppressive despite its genuine coordination function in
 *   preventing non-consensual killing; the claimed type is tangled_rope,
 *   reflecting both functions. The metrics and claim are independently
 *   authored and not tuned to match.
 *
 * KEY AGENTS:
 *   - Medical licensing boards: Primary agenda_setter (institutional/analytical) â enforce the prohibition through discipline and licensure.
 *   - Religious moral authorities: Agenda_setter (organized/analytical) â provide the legitimating framework and sit outside the paying structure.
 *   - Pro-life advocacy networks: Primary beneficiary (organized/mobile) â collect political and social capital from the prohibition.
 *   - Palliative care institutions: Secondary beneficiary (institutional/constrained) â receive demand redirected from assisted dying.
 *   - Suffering terminally ill patients: Primary payer (powerless/trapped) â bear the cost of denied exit from unbearable suffering.
 *   - Pressured vulnerable groups: Secondary payer (powerless/trapped) â elderly, disabled, and economically disadvantaged who are coerced into continued existence.
 *   - Physicians: Dual-positioned agent (organized/constrained) â enforces the constraint but bears moral distress and legal risk.
 *   - Autonomy advocacy groups: Excluded voice (organized/constrained) â structurally absent from policy-making bodies.
 *   - Bioethics observers: Analytical observer (analytical/analytical) â compares regimes without stake in the outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.62).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '96a83b85-4252-466e-8599-086efdc984ee').
narrative_ontology:cs_kernel_codification('96a83b85-4252-466e-8599-086efdc984ee', formalized).
narrative_ontology:cs_authority_grounding('96a83b85-4252-466e-8599-086efdc984ee', lineage).
narrative_ontology:cs_interpretation_layer_present('96a83b85-4252-466e-8599-086efdc984ee').
narrative_ontology:cs_reading_relation('96a83b85-4252-466e-8599-086efdc984ee', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('96a83b85-4252-466e-8599-086efdc984ee', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('96a83b85-4252-466e-8599-086efdc984ee', foundational, human_life_categorically_inviolable).
narrative_ontology:cs_axiom_status(human_life_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('96a83b85-4252-466e-8599-086efdc984ee', human_life_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('96a83b85-4252-466e-8599-086efdc984ee', secondary, physician_may_never_intentionally_kill).
narrative_ontology:cs_axiom_status(physician_may_never_intentionally_kill, holdable).
narrative_ontology:cs_axiom_grounding('96a83b85-4252-466e-8599-086efdc984ee', physician_may_never_intentionally_kill, conventional).
narrative_ontology:cs_reference_frame('96a83b85-4252-466e-8599-086efdc984ee', inviolability_framework).
narrative_ontology:cs_drift_state('96a83b85-4252-466e-8599-086efdc984ee', contemporary_autonomy_legalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('96a83b85-4252-466e-8599-086efdc984ee', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocacy_networks).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_moral_authorities).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, suffering_terminally_ill_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressured_vulnerable_groups).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_human_value_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce professional standards that categorically prohibit physicians from participating in assisted dying; discipline transgressors; derive institutional authority from being the gatekeeper of medical morality and licensure.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_licensing_boards, agenda_setter,
    institutional, generational, analytical, national).

% Provide the theological and philosophical framing that grounds the inviolability of human life; influence policy, professional oaths, and legislative testimony through institutional teaching authority.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_moral_authorities, agenda_setter,
    organized, civilizational, analytical, national).

% Collect social and political capital from the maintenance of categorical prohibitions on assisted dying; mobilize constituencies and funding around the sanctity framing; lose political purpose if the prohibition is relaxed.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocacy_networks, beneficiary,
    organized, generational, mobile, national).

% Receive patients, clinical resources, and legitimating demand that would otherwise flow to assisted dying services; their institutional growth and professional identity are partly contingent on the legal closure of the death-with-dignity option.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_institutions, beneficiary,
    institutional, biographical, constrained, national).

% Are professionally bound to preserve life and refrain from intentional life-ending; bear moral distress when patients suffer unbearably and request death that the physician is barred from providing; face licensure loss and criminal liability if they transgress.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians, payer).

% Experience unbearable suffering at end of life and are denied the option of a medically assisted death; their individual preference is structurally overridden by the prohibition; exit from suffering is blocked by legal, medical, and pharmaceutical barriers.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, suffering_terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Elderly, disabled, and economically disadvantaged individuals who, under the sanctity regime, are subjected to the coercive preservation of life regardless of their own wishes or circumstances; bear the cost of a categorical denial of exit and the medicalization of their continued existence.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressured_vulnerable_groups, payer,
    powerless, immediate, trapped, local).

% Advocate for patient self-determination and legal access to assisted dying; are structurally excluded from ethics councils, licensing boards, and legislative drafting committees in sanctity-dominant jurisdictions; their arguments are treated as outside legitimate medical ethics discourse.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Analyze the tension between sanctity-based and autonomy-based frameworks; document comparative outcomes across jurisdictions; do not bear costs or collect benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice around an absolute prohibition on killing, preventing the normalization of physician-caused death and maintaining a bright-line boundary for professional ethics that is intended to protect vulnerable patients from non-consensual life-ending.
% TRANSFER_FUNCTION: Transfers the burden of continued existence from the social order and medical institutions onto suffering and vulnerable individuals; moves authority over the timing and manner of death from the patient to medical licensing boards and moral authorities.
% ABSENT_VOICES: Autonomy advocacy groups and terminally ill patients seeking assisted dying are structurally excluded from ethics councils and legislative drafting bodies in sanctity-dominant jurisdictions; their exclusion is necessary to maintain the categorical nature of the prohibition because their testimony would introduce preference-based exceptions.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, medical practice would reorganize around assisted dying protocols for competent terminal patients, palliative care institutions would lose the captive demand redirect, vulnerable patients would gain legal exit options, and the professional identity of physicians would shift from absolute life-preservers to agents who may enable death in defined circumstances.
% FOUNDING_PROBLEM: The historical risk of non-consensual medical killing, eugenics programs, and the erosion of public trust in physicians if they were permitted to intentionally end life; the need for a clear moral boundary to prevent abuse of vulnerable patients by medical and state power.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and critical bioethicists outside the pro-life beneficiary set attest that the founding problem of non-consensual medical killing was real in mid-twentieth century eugenics and institutional abuse; however, these same independent observers contest whether the categorical prohibition remains proportionate to the risk once rigorous consent, capacity, and terminal-illness safeguards are introduced, or whether it has outlived its protective function.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the prohibition forces continued existence and suffering on those who would choose death; suppression (0.71) is high because legal criminalization, professional discipline, and social stigma actively close assisted-dying alternatives. Theater ratio (0.32) reflects growing performative maintenance of an absolute line that clinical practice already nuances through palliative sedation and withdrawal of treatment. Accessibility collapse (0.48) is moderate because underground and jurisdictional alternatives exist but are locally suppressed. Resistance (0.58) is significant due to sustained autonomy movements, legalization campaigns in comparative jurisdictions, and dissenting medical professionals. Temporal measurements trace a single aligned grid: extraction rises as medical technology prolongs life against patient preference, theater rises as the absolute line becomes more contested, and suppression requirement rises as autonomy gains force counter-hegemonic pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (medical boards, religious authorities) experience the constraint as necessary moral boundary and professional identity; the payer seats (suffering patients, vulnerable groups) experience it as a coerced preservation of existence. The engine computes this divergence from power and exit asymmetries: the agenda-setters have analytical or mobile exit and institutional power, while the payers are trapped in immediate suffering with no legal exit. Physicians occupy a genuinely dual position: their professional identity is constituted by the constraint, yet they pay its moral costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the institutional and advocacy actors whose authority and mission are vindicated by the prohibition. Victims are the suffering patients and vulnerable groups who bear the cost of denied autonomy. Physicians sit closer to the target end than pure beneficiaries because their enforcement role is instrumental rather than rent-collecting, and their moral distress and legal exposure place them structurally among the cost-bearers.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling by separating the genuine coordination function (preventing abuse, maintaining professional non-killing norms, protecting against non-consensual killing) from the extraction layer (denying exit to competent, suffering patients who request it). Without this separation, the prohibition would be read as pure rope (ignoring the forced suffering) or pure snare (ignoring the protective coordination). The measurement of rising extraction over time alongside rising resistance is the signal that the coordination layer is being captured by extractive enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_risk_direction,
    'Does the sanctity regime''s coercion fall on vulnerable groups by forcing continued existence, or does it protect them from autonomy-regime coercion to die?',
    'Comparative empirical analysis of end-of-life experience in sanctity-dominant versus autonomy-legalized jurisdictions, measuring patient-reported pressure (to live or to die) across vulnerable populations.',
    'If the primary coercion is the prohibition itself, the victim set is correctly identified and directionality holds; if the primary coercion would arise under autonomy, the victim set may need redefinition and the extraction metric would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_direction, empirical, 'Whether vulnerable groups are coerced to live or would be coerced to die.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of non-consensual medical killing been sufficiently addressed by modern consent and capacity frameworks that the categorical prohibition is now disproportionate?',
    'Historical comparison of abuse rates under strict prohibition versus regulated autonomy regimes with capacity safeguards; analysis of whether the prohibition''s extension to competent terminal patients tracks the founding risk.',
    'If the founding problem is solved by safeguards, the constraint''s extraction layer is unjustified and the classification trends toward snare; if the risk remains, the coordination function is live and tangled_rope remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the prohibition has outlived its protective founding problem.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal prohibition, licensure threat) or internalized (physician professional identity fused with non-killing, patient acceptance of suffering as fate)?',
    'Post-legalization trajectory analysis: if physician participation and patient demand shift rapidly after prohibition removal, suppression was primarily structural; if persistence remains, internalization is significant.',
    'If internalized, effective suppression exceeds the structural measure because agents carry the constraint with them after formal exit is available, raising the true extraction and suppressing alternatives even when legal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism.').

omega_variable(
    sanctity_autonomy_foreclosure,
    'Does the sanctity reading''s core axiom logically foreclose the autonomy reading within a single medical ethics framework, or can both be held as live options by the same polity through jurisdictional or institutional pluralism?',
    'Jurisprudential analysis: whether any single legal framework has successfully institutionalized both a categorical prohibition and a right to assisted dying without one being functionally subordinated.',
    'If foreclosure is genuine, the kernel readings are mutually exclusive and the contested domain is zero-sum; if pluralism is possible, the foreclosure relation should be reclassified as coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_autonomy_foreclosure, conceptual, 'Whether sanctity and autonomy are logically mutually exclusive in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eol_sanctity_tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(eol_sanctity_tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(eol_sanctity_tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(eol_sanctity_tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eol_sanctity_be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(eol_sanctity_be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(eol_sanctity_be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(eol_sanctity_be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eol_sanctity_su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(eol_sanctity_su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(eol_sanctity_su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(eol_sanctity_su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one member of the end_of_life_authority kernel family, which decomposes into structurally distinct readings (sanctity, autonomy, slippery_slope) due to divergent epsilon values, stakeholder directionalities, and normative axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
