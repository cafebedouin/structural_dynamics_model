% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Absolute Bodily Autonomy Against Vaccine Mandates
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'bodily_autonomy_primacy_reading' of the
 *   'vaccine_mandate_legitimacy' kernel. It asserts that medical
 *   self-sovereignty is absolute and state coercion for public health is
 *   categorically impermissible, regardless of collective outcomes. This
 *   reading positions individual liberty as the paramount value, even when it
 *   imposes significant health risks on vulnerable populations. The claimed
 *   type is 'mountain' due to its assertion of an absolute, unchangeable
 *   principle, but the presence of beneficiaries and victims will trigger
 *   False Summit Mountain detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.8).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Absolute Bodily Autonomy Against Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'f2e9af69-8dd5-480a-abf1-c5050a984b29').
narrative_ontology:cs_kernel_codification('f2e9af69-8dd5-480a-abf1-c5050a984b29', formalized).
narrative_ontology:cs_authority_grounding('f2e9af69-8dd5-480a-abf1-c5050a984b29', lineage).
narrative_ontology:cs_interpretation_layer_present('f2e9af69-8dd5-480a-abf1-c5050a984b29').
narrative_ontology:cs_reading_relation('f2e9af69-8dd5-480a-abf1-c5050a984b29', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f2e9af69-8dd5-480a-abf1-c5050a984b29', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('f2e9af69-8dd5-480a-abf1-c5050a984b29', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f2e9af69-8dd5-480a-abf1-c5050a984b29', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('f2e9af69-8dd5-480a-abf1-c5050a984b29', foundational, state_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('f2e9af69-8dd5-480a-abf1-c5050a984b29', state_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('f2e9af69-8dd5-480a-abf1-c5050a984b29', unconditional_individual_liberty).
narrative_ontology:cs_drift_state('f2e9af69-8dd5-480a-abf1-c5050a984b29', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2e9af69-8dd5-480a-abf1-c5050a984b29', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_asserting_autonomy).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert their fundamental right to control their own body and medical decisions, free from state interference, viewing this principle as an unalienable right.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_asserting_autonomy, beneficiary,
    powerless, biographical, mobile, local).

% Champion individual rights and freedom from government overreach, leveraging this principle as a core tenet in various policy debates. They benefit from its broad acceptance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, arbitrage, national).

% Bear increased risk of severe illness or death due to reduced herd immunity when vaccine mandates are absent, as their own immune systems cannot provide full protection. They pay with their health.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Experience higher exposure risk and health burden due to the lack of collective protection, often exacerbated by social and economic factors. They pay with increased health risks.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations, payer,
    powerless, biographical, constrained, local).

% Tasked with protecting public health, their ability to implement population-level interventions like vaccine mandates is severely limited by this principle, leading to higher rates of preventable disease. They pay with curtailed efficacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, payer,
    institutional, biographical, constrained, national).

% Adjudicate the balance between individual rights and state power, with their rulings shaping the practical application and legal force of this principle. They interpret its scope and limits.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Grapple with the ethical implications of individual autonomy versus collective well-being, providing guidance and commentary but lacking direct enforcement power over policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_ethics_boards, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for individual medical decision-making, ensuring personal autonomy is the primary coordinating principle for health choices, preventing state-imposed collective health strategies.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from the state and the general population to individuals, particularly the vulnerable, in exchange for absolute individual medical self-determination.
% ABSENT_VOICES: Future generations and those unable to advocate for themselves (e.g., infants, severely disabled) who would benefit from collective immunity are not directly represented in the assertion of this principle, bearing its costs without voice.
% DISAPPEARANCE_RATIONALE: If the principle of absolute bodily autonomy against state coercion vanished, public health authorities would gain significant power to implement mandates, fundamentally altering the balance between individual liberty and collective welfare, leading to a reorganization of public health policy and individual medical choices.
% FOUNDING_PROBLEM: To protect individuals from unwanted medical interventions and state overreach, particularly in historical contexts of forced sterilization, experimentation, or medical discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, bioethicists, and historical records corroborate the ongoing relevance of protecting individuals from medical coercion, citing historical abuses and contemporary concerns about privacy and bodily integrity.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'extractiveness' (0.65) reflects the societal cost borne by vulnerable populations and the public health system when collective protective measures are foreclosed. 'Suppression' (0.8) is high because this principle actively suppresses the state's ability to implement coercive public health interventions. 'Theater ratio' is low (0.1) as the principle is a direct normative claim, not a performative one. The 'claimed_type' is 'mountain' because it asserts an absolute, fundamental right, but its operation creates clear beneficiaries and victims, indicating it functions more like a constructed constraint.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a fundamental, non-negotiable truth (a mountain), while public health authorities and vulnerable populations experience its effects as a significant imposition of risk and a limitation on collective action (more akin to a snare or tangled rope). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and liberty advocacy movements are clear beneficiaries, as the constraint enshrines their core value. Immunocompromised individuals and vulnerable populations are victims, bearing the direct health costs of reduced collective immunity. Public health authorities are also victims, as their institutional mandate to protect the population is curtailed. Constitutional courts act as agenda-setters, interpreting and applying this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is absolute bodily autonomy a genuine natural law (a Mountain) or a constructed legal/ethical principle that benefits identifiable agents (a Snare/Tangled Rope)?',
    'Analysis of its historical contingency, cultural variation, and the identifiable interests it serves. If its scope and application vary significantly across legal traditions or if its primary beneficiaries are specific advocacy groups, it suggests a constructed nature.',
    'If constructed, the FSM detection will correctly reclassify this ''claimed mountain'' to a more appropriate type (e.g., Tangled Rope or Snare), reflecting its extractive and enforced nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity of the constraint''s fundamental nature: natural law vs. constructed principle.').

omega_variable(
    scope_of_self_sovereignty_externalities,
    'Does the principle of absolute bodily autonomy extend to actions that generate significant, unmitigated negative externalities for others, particularly the vulnerable?',
    'Legal and ethical adjudication of the ''harm principle'' in public health contexts. If courts or ethics boards consistently rule that individual autonomy is limited when it directly harms others, the ''absolute'' nature of this reading is challenged.',
    'If externalities are acknowledged as limiting factors, the ''suppression'' of state action would decrease, and the ''extractiveness'' (societal cost) would be re-evaluated, potentially shifting the constraint towards a more conditional or balanced classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_self_sovereignty_externalities, conceptual, 'Whether bodily autonomy is absolute or limited by externalities.').

omega_variable(
    immunocompromised_victim_status,
    'Is the increased risk borne by immunocompromised individuals a direct ''victimization'' by this principle, or an unavoidable consequence of individual liberty?',
    'Ethical frameworks that prioritize collective responsibility or the protection of the most vulnerable. If a framework assigns a moral duty to protect the vulnerable, then the increased risk is a direct cost imposed by the principle''s absoluteness.',
    'If confirmed as direct victimization, the ''extractiveness'' and ''suppression'' metrics are strongly validated, reinforcing a classification that highlights the costs imposed by this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_victim_status, preference, 'Ethical framing of risk to vulnerable populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
